#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
#stop("!")

#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
#2. load objects (initial conditions and yields)
#3. create a list of 100 seeds
#4. select ME and grid cells to optimise on
#5. create meteorology for selected grid cells
#6. iteratively optimise over the list of parameters (with a defined number of iterations)

################################################################################
#get the command line arguments
args=(commandArgs(TRUE))

#evaluate the arguments
for(i in 1:length(args)) {
  eval(parse(text=args[[i]]))
}
#should have read *me_i*, *iter*, *i* (param order) and *j* (step in parameter)
#me_i <- 1; iter <- 1; i <- 1; j <- 1

#source all functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))
source(paste(src.dir,"/glam-utils/calibrate.R",sep=""))
source(paste(src.dir,"/glam-utils/optimise.R",sep=""))
source(paste(src.dir,"/glam-utils/randomise_param_space.R",sep=""))
source(paste(src.dir,"/meteo/extract_weather.R",sep=""))

#input directories
#wd <- "~/Leeds-work/quest-for-robustness"
#wd <- "/nfs/a101/earjr/quest-for-robustness"
wd <- "~/quest-for-robustness"
runs_dir <- paste(wd,"/crop_model_runs",sep="")
calib_dir <- paste(runs_dir,"/ppe_optimisation_t4",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c",sep="")

###
#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_orig <- read.csv(paste(mdata_dir,"/parameter_list_glam.txt",sep=""),sep="\t",header=T)

###
#2. load objects (initial conditions and yields)
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

###
#3. create a list of 10 seeds
set.seed(2302) #fixed seed to make it replicable
seed_list <- round(runif(10, 1000, 9999),0)

###
#4. select ME and grid cells to optimise on
me_list <- unique(xy_main$ME_NEW)
me_sel <- me_list[me_i]

xy_me <- xy_main[which(xy_main$ME_NEW == me_sel),]
set.seed(2059) #randomly choose grid cells (use fixed seed to make it replicable)
xy_sel <- xy_me[sample(1:nrow(xy_me), 10, replace=F),]
row.names(xy_me) <- 1:nrow(xy_me); row.names(xy_sel) <- 1:nrow(xy_sel)

#ensure SAT is not below DUL
corr_loc <- xy_sel$LOC[which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]
corr_fac <- mean((xy_sel$SAT[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]-xy_sel$DUL[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]),na.rm=T)
xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)] <- xy_sel$DUL[which(xy_sel$LOC %in% corr_loc)] + corr_fac
xy_main$SAT[which(xy_main$LOC %in% corr_loc)] <- xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)]

#load all runs data.frame
load(paste(mdata_dir,"/glam-all_optim_runs.RData",sep=""))

####################################################################################
#all the *STEPS and *SEEDS for the given ITER & PARAM can be submitted simultaneously
#each time calibrate() needs to be run. calibrate should take ~36 min
cat("...processing iter=",iter,", parameter sequence i=",i," and param step j=",j,sep="","\n")

#this i need to do somehow in the driver script so i can send the "j" value into R from it
#select seeds*steps to run
dfsel <- dfall[which(dfall$ITER == iter & dfall$PARAM_ORDER == i),]
dfsel$ITER <- NULL

#if iter > 1 or i > 1 iterate to gather output and reconstruct done_param
done_param <- data.frame()
if (iter > 1 | i > 1) {
  cat("...gathering previous output\n")
  for (iter_i in 1:iter) {
    for (i_i in 1:47) {
      dfsel_i <- dfall[which(dfall$ITER == iter_i & dfall$PARAM_ORDER == i_i),]
      for (seed in seed_list) {
        #seed <- seed_list[1]
        tdfsel_i <- dfsel_i[which(dfsel_i$SEED == seed),]
        tparam <- paste(dfall$PARAM_NAME[which(dfall$SEED == seed & dfall$ITER == iter_i & dfall$PARAM_ORDER == i_i & dfall$STEP == 1)])
        
        #outdir and save_file
        out_dir <- paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter_i,sep="")
        save_file <- paste(out_dir,"/opt-",tparam,".RData",sep="")
        
        if (file.exists(save_file)) {
          load(save_file)
          opt_val <- r_list$OPTIMISATION$VALUE[which(r_list$OPTIMISATION$RMSE == min(r_list$OPTIMISATION$RMSE))]
          if (length(opt_val > 1)) {opt_val <- opt_val[ceiling(length(opt_val)/2)]}
          done_param <- rbind(done_param, data.frame(ITER=iter_i, PARAM_ORDER=i_i, 
                                                     SEED=seed, PARAM_NAME=tparam,
                                                     OPT_VALUE=opt_val))
          rm(r_list)
        }
      }
    }
  }
}
rm(list=c("iter_i","i_i","seed","dfsel_i","tparam","opt_val"))

#wrapper function here
seed_step_run <- function(j) {
  #source all needed functions
  source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
  source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
  source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
  source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
  source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
  source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
  source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))
  source(paste(src.dir,"/glam-utils/calibrate.R",sep=""))
  source(paste(src.dir,"/glam-utils/optimise.R",sep=""))
  source(paste(src.dir,"/glam-utils/randomise_param_space.R",sep=""))
  
  #run details
  seed <- dfsel$SEED[j]
  param <- paste(dfsel$PARAM_NAME[j])
  porder <- dfsel$PARAM_ORDER[j]
  pstep <- dfsel$STEP[j]
  
  #get parameters
  rand_out <- randomise_param_space(params=GLAM_get_default(mdata_dir),plist_in=param_orig,seed=seed)
  this_params <- rand_out$PARAMS
  param_list <- rand_out$PARAM_LIST
  
  ###
  #note: to update the parameter set based on previous optimal value(s)
  #select all *iter* and *param* for this seed
  this_opt <- done_param[which(done_param$SEED == seed),]
  #if there have been parameters optimised for this *seed* before, then update the parameter set
  if (nrow(this_opt) > 0) {
    for (k in 1:nrow(this_opt)) {
      #k <- 1
      t_param <- paste(this_opt$PARAM_NAME[k])
      t_sect <- paste(param_list$WHERE[which(param_list$PARAM == t_param)])
      t_val <- this_opt$OPT_VALUE[k]
      if (t_param %in% c("SLA_INI","NDSLA")) {
        this_params[[t_sect]][[t_param]] <- t_val
      } else {
        this_params[[t_sect]][[t_param]][,"Value"] <- t_val
      }
    }
  }
  ###
  
  #create object for optimisation
  opt_data <- list()
  opt_data$CROP <- "maize"
  opt_data$MODEL <- "glam-maiz"
  opt_data$BASE_DIR <- calib_dir
  opt_data$BIN_DIR <- bin_dir
  opt_data$PAR_DIR <- mdata_dir
  opt_data$WTH_DIR <- paste(met_dir,"/ascii_extract_raw",sep="") #for reading .wth files
  opt_data$WTH_ROOT <- "obs_hist_WFD"
  opt_data$LOC <- xy_sel$LOC
  opt_data$ISYR <- 1981
  opt_data$IEYR <- 2000
  opt_data$INI_COND <- xy_main
  opt_data$YLD_DATA <- xy_main_yield
  opt_data$SIM_NAME <- paste("optim_me-",me_sel,"_seed-",seed,"_iter-",iter,"_param-",param,"_step-",pstep,sep="")
  opt_data$RUN_TYPE <- "RFD"
  opt_data$METHOD <- "RMSE"
  opt_data$PARAMS <- this_params
  opt_data$PARAM <- param
  opt_data$SECT <- paste(param_list$WHERE[porder])
  opt_data$NSTEPS <- param_list$NSTEPS[porder]
  opt_data$VALS <- seq(param_list$MIN[porder],param_list$MAX[porder],length.out=opt_data$NSTEPS)[pstep]
  opt_data$USE_SCRATCH <- T
  
  #scratch in /scratch or in /dev/shm
  opt_data$SCRATCH <- "/scratch/earjr"
  
  #run optimiser
  par_optim <- GLAM_optimise(opt_data)
  
  #return object
  rdata_dir <- paste(opt_data$BASE_DIR,"/",opt_data$SIM_NAME,sep="")
  return(rdata_dir)
}

#run only if given *j*, maximum number of *j* jobs = 170
if (j <= nrow(dfsel)) {
  #check what has been done already
  tdfsel <- dfsel[j,]
  seed <- tdfsel$SEED
  tparam <- paste(tdfsel$PARAM_NAME)
  out_dir <- paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,sep="")
  save_file <- paste(out_dir,"/opt-",tparam,".RData",sep="")
  
  #run this *j* if not exists
  if (!file.exists(save_file)) {runstep <- seed_step_run(j)}
}


