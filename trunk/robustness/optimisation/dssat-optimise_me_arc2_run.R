#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")

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
for(arg_i in 1:length(args)) {
  eval(parse(text=args[[arg_i]]))
}
#should have read *csmodel*, *me_i*, *iter*, *i* (param order) and *j* (step in parameter)
#csmodel <- "MZCER045"; me_i <- 1; iter <- 1; i <- 1; j <- 1

#source all functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/dssat-utils/make_xfile.R",sep=""))
source(paste(src.dir,"/dssat-utils/make_soilfile.R",sep=""))
source(paste(src.dir,"/dssat-utils/make_wth.R",sep=""))
source(paste(src.dir,"/dssat-utils/make_parameters.R",sep=""))
source(paste(src.dir,"/dssat-utils/get_parameters.R",sep=""))
source(paste(src.dir,"/dssat-utils/get_xfile.R",sep=""))
source(paste(src.dir,"/dssat-utils/get_soils.R",sep=""))
source(paste(src.dir,"/dssat-utils/run_dssat.R",sep=""))
source(paste(src.dir,"/dssat-utils/calibrate.R",sep=""))
source(paste(src.dir,"/dssat-utils/optimise.R",sep=""))
source(paste(src.dir,"/dssat-utils/randomise_param_space.R",sep=""))

#input directories
#wd <- "~/Leeds-work/quest-for-robustness"
#wd <- "/nfs/a101/earjr/quest-for-robustness"
wd <- "~/quest-for-robustness"
runs_dir <- paste(wd,"/crop_model_runs",sep="")
calib_dir <- paste(runs_dir,"/dssat_",csmodel,"_optimisation_t1",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")

#define bin dir based on node
nname <- Sys.info()[["nodename"]]
if (length(grep("arc2",nname)) == 0) {
  bin_dir <- paste(wd,"/bin/csm45_1_23_bin_ifort_arc1",sep="")
} else {
  bin_dir <- paste(wd,"/bin/csm45_1_23_bin_ifort",sep="")
}
#bin_dir <- paste(wd,"/bin/dssat/csm45_1_23_bin_gfort",sep="")

#load objects
load(paste(mdata_dir,"/initial_conditions_major_dssat.RData",sep=""))
load(paste(mdata_dir,"/yield_major_dssat.RData",sep=""))

###
#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_orig <- read.table(paste(mdata_dir,"/parameter_list_dssat.txt",sep=""),header=T,sep="\t")
param_orig <- param_orig[which(param_orig$MODEL == csmodel),] #select those relevant to model

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

#ensure SAT is not below DUL for all 8 soil layers
for (sl in 1:8) {
  #sl <- 1
  corr_loc <- xy_sel$LOC[which(round(xy_sel[paste("SSAT_",sl,sep="")],3) <= round(xy_sel[paste("SDUL_",sl,sep="")],3))]
  corr_fac <- mean((xy_sel[-which(round(xy_sel[paste("SSAT_",sl,sep="")],3) <= round(xy_sel[paste("SDUL_",sl,sep="")],3)),paste("SSAT_",sl,sep="")]-xy_sel[-which(round(xy_sel[paste("SSAT_",sl,sep="")],3) <= round(xy_sel[paste("SDUL_",sl,sep="")],3)),paste("SDUL_",sl,sep="")]),na.rm=T)
  
  xy_sel[which(xy_sel$LOC %in% corr_loc),paste("SSAT_",sl,sep="")] <- xy_sel[which(xy_sel$LOC %in% corr_loc),paste("SDUL_",sl,sep="")] + corr_fac
  xy_main[which(xy_main$LOC %in% corr_loc),paste("SSAT_",sl,sep="")] <- xy_sel[which(xy_sel$LOC %in% corr_loc),paste("SSAT_",sl,sep="")]
}

#load all runs data.frame
load(paste(mdata_dir,"/dssat_",csmodel,"-all_optim_runs.RData",sep=""))

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
    for (i_i in 1:nrow(param_orig)) {
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
  rm(list=c("iter_i","i_i","seed","dfsel_i","tparam","opt_val"))
}

#wrapper function here
seed_step_run <- function(j) {
  #j <- 1
  #source all needed functions
  source(paste(src.dir,"/dssat-utils/make_xfile.R",sep=""))
  source(paste(src.dir,"/dssat-utils/make_soilfile.R",sep=""))
  source(paste(src.dir,"/dssat-utils/make_wth.R",sep=""))
  source(paste(src.dir,"/dssat-utils/make_parameters.R",sep=""))
  source(paste(src.dir,"/dssat-utils/get_parameters.R",sep=""))
  source(paste(src.dir,"/dssat-utils/get_xfile.R",sep=""))
  source(paste(src.dir,"/dssat-utils/get_soils.R",sep=""))
  source(paste(src.dir,"/dssat-utils/run_dssat.R",sep=""))
  source(paste(src.dir,"/dssat-utils/calibrate.R",sep=""))
  source(paste(src.dir,"/dssat-utils/optimise.R",sep=""))
  source(paste(src.dir,"/dssat-utils/randomise_param_space.R",sep=""))
  
  #run details
  seed <- dfsel$SEED[j]
  param <- paste(dfsel$PARAM_NAME[j])
  porder <- dfsel$PARAM_ORDER[j]
  pstep <- dfsel$STEP[j]
  
  #get parameters
  cul_params <- randomise_culpar(plist_in=param_orig, seed=seed, model=csmodel)
  eco_params <- randomise_ecopar(plist_in=param_orig, seed=seed, model=csmodel)
  spe_params <- randomise_spepar(params=get_spepar(paste(bin_dir,"/MZCER045.SPE",sep="")),
                                 plist_in=param_orig, seed=seed)
  xfi_params <- randomise_xfile(params=get_xfile_dummy(), plist_in=param_orig, seed=seed)
  xfi_params$sim_ctrl$VBOSE <- "0" #write only Summary.OUT outputs (as needed)
  param_list <- randomise_order(plist_in=param_orig, seed=seed)
  
  ###
  #note: to update the parameter set based on previous optimal value(s)
  #select all *iter* and *param* for this seed
  this_opt <- done_param[which(done_param$SEED == seed),]
  #if there have been parameters optimised for this *seed* before, then update the parameter set
  #below needs to be adapted to .CUL, .ECO, .SPE and XFILE
  if (nrow(this_opt) > 0) {
    for (k in 1:nrow(this_opt)) {
      #k <- 1
      t_param <- paste(this_opt$PARAM_NAME[k])
      t_sect <- paste(param_list$WHERE[which(param_list$PARAM == t_param)])
      t_fil <- paste(param_list$FILE[which(param_list$PARAM == t_param)])
      t_val <- this_opt$OPT_VALUE[k]
      
      if (t_fil == "SPE") {
        if (t_param %in% c("SDSZ","RSGRT")) {
          spe_params[[t_sect]][which(gsub(" ","",spe_params[[t_sect]][["PARAM"]]) == t_param),"VALUE"] <- t_val
        } else {
          spe_params[[t_sect]][[t_param]] <- t_val
        }
      } else if (t_fil == "ECO") {
        eco_params[[t_param]] <- t_val
      } else if (t_fil == "CUL") {
        cul_params[[t_param]] <- t_val
      } else if (t_fil == "XFILE") {
        xfi_params[[t_sect]][[t_param]] <- t_val
      }
    }
  }
  ###
  
  #create object for optimisation
  opt_data <- list()
  opt_data$MODEL <- csmodel
  opt_data$BASENAME <- "AFRB" #basename of runs
  opt_data$BASE_DIR <- calib_dir
  opt_data$BIN_DIR <- bin_dir
  opt_data$WTH_DIR <- paste(met_dir,"/ascii_extract_raw",sep="") #for reading .wth files
  opt_data$WTH_ROOT <- "obs_hist_WFD"
  opt_data$LOC <- xy_sel$LOC
  opt_data$ISYR <- 1980 #1 year before GLAM's because of spin-up year needs in CSM
  opt_data$IEYR <- 2001 #1 extra year so as to include in wth file (but this year won't be run)
  opt_data$INI_COND <- xy_main
  opt_data$YLD_DATA <- xy_main_yield
  opt_data$CUL <- cul_params
  opt_data$ECO <- eco_params
  opt_data$SPE <- spe_params
  opt_data$XFILE <- xfi_params
  opt_data$SIM_NAME <- paste("optim_me-",me_sel,"_seed-",seed,"_iter-",iter,"_param-",param,"_step-",pstep,sep="")
  opt_data$PARAM <- param
  opt_data$VALS <- seq(param_list$MIN[porder],param_list$MAX[porder],length.out=param_list$NSTEPS[porder])[pstep]
  opt_data$IFILE <- paste(param_list$FILE[porder])
  opt_data$SECT <- paste(param_list$WHERE[porder])
  opt_data$METHOD <- "RMSE"
  opt_data$USE_SCRATCH <- T
  
  #scratch in /scratch or in /dev/shm
  if (j%%2 == 0) {
    opt_data$SCRATCH <- "/scratch/earjr"
  } else {
    opt_data$SCRATCH <- "/dev/shm/earjr"
  }
  
  #remove junk previous to running (in case of deleted / failed runs)
  trdir <- paste(opt_data$SCRATCH,"/",opt_data$SIM_NAME,sep="")
  if (file.exists(trdir)) {system(paste("rm -rf ",trdir,sep=""))}
  
  #run optimiser
  par_optim <- DSSAT_optimise(opt_data)
  
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
  
  #write procfile file
  procfil <- paste(wd,"/scratch/procfiles/dssat_",csmodel,"_out_",me_i,"_",iter,"_",i,"_",j,".proc",sep="")
  if (!file.exists(procfil)) {pfil <- file(procfil,open="w"); cat("Process completed!\n",file=pfil); close(pfil)}
}


