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
wd <- "/nfs/a101/earjr/quest-for-robustness"
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
me_sel <- me_list[1]

xy_me <- xy_main[which(xy_main$ME_NEW == me_sel),]
set.seed(2059) #randomly choose grid cells (use fixed seed to make it replicable)
xy_sel <- xy_me[sample(1:nrow(xy_me), 10, replace=F),]
row.names(xy_me) <- 1:nrow(xy_me); row.names(xy_sel) <- 1:nrow(xy_sel)

#ensure SAT is not below DUL
corr_loc <- xy_sel$LOC[which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]
corr_fac <- mean((xy_sel$SAT[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]-xy_sel$DUL[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]),na.rm=T)
xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)] <- xy_sel$DUL[which(xy_sel$LOC %in% corr_loc)] + corr_fac
xy_main$SAT[which(xy_main$LOC %in% corr_loc)] <- xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)]

###
#5. create meteorology for selected grid cells
#for (i in 1:nrow(xy_sel)) {
#  loc <- xy_sel$LOC[i]; x <- xy_sel$x[i]; y <- xy_sel$y[i]
#  wval <- extract_weather(cellid=loc, lon=x, lat=y, met_dir=met_dir, data_type="obs", dataset="WFD", 
#                          sce="hist", years=1950:2001)
#}

###
#6. iteratively optimise over the list of parameters (with a defined number of iterations)
nmaxiter <- 10

########################################################
#construct table of sequential steps
if (!file.exists(paste(mdata_dir,"/glam-all_optim_runs.RData",sep=""))) {
  dfall <- data.frame()
  for (i in 1:length(seed_list)) {
    cat("i=",i,"\n")
    #get parameter list
    param_list <- randomise_param_space(params=GLAM_get_default(mdata_dir),
                                        plist_in=param_orig, seed=seed_list[i])$PARAM_LIST
    dfj <- data.frame()
    for (j in 1:nmaxiter) {
      cat("...j=",j,"\n")
      dfk <- data.frame()
      for (k in 1:nrow(param_list)) {
        dfl <- data.frame()
        for (l in 1:param_list$NSTEPS[k]) {
          trow <- data.frame(SEED=seed_list[i],ITER=j,PARAM_ORDER=k,PARAM_NAME=param_list$PARAM[k],STEP=l)
          dfl <- rbind(dfl,trow)
        }
        dfk <- rbind(dfk, dfl)
      }
      dfj <- rbind(dfj, dfk)
    }
    dfall <- rbind(dfall, dfj)
  }
  rm(list=c("dfj","dfk","dfl","trow","param_list","j","k","l"))
  save(list=c("dfall"),file=paste(mdata_dir,"/glam-all_optim_runs.RData",sep=""))
} else {
  load(paste(mdata_dir,"/glam-all_optim_runs.RData",sep=""))
}

#because snow has a limit in number of workers, i decided to drive using two different machines
driver <- Sys.info()[["nodename"]]
if (driver == "eljefe") {
  seed_list <- seed_list[1:5]
  socket_list <- c(rep("localhost",30),rep("foe-linux-01",20),rep("foe-linux-02",20))
} else if (driver == "lajefa") {
  seed_list <- seed_list[6:10]
  socket_list <- c(rep("localhost",30),rep("foe-linux-03",20),rep("foe-linux-04",20))
}
dfall <- dfall[which(dfall$SEED %in% seed_list),]; row.names(dfall) <- 1:nrow(dfall)

#dfall <- dfall[which(dfall$ITER == 1),]

####################################################################################
#all the *STEPS and *SEEDS for the first ITER & PARAM can be submitted simultaneously
#each time calibrate() needs to be run. calibrate should take ~36 min
done_param <- data.frame()
for (iter in 1:nmaxiter) {
  #iter <- 1
  #order of parameters (nparam = 47, hence does not matter not to loop the param name)
  for (i in 1:47) {
    #i <- 1
    cat("...processing iter=",iter," and parameter sequence i=",i,"\n")
    
    #select seeds*steps to run
    dfsel <- dfall[which(dfall$ITER == iter & dfall$PARAM_ORDER == i),]
    #row.names(dfsel) <- 1:nrow(dfsel)
    dfsel$ITER <- NULL
    
    #wrapper function here
    seed_step_run <- function(j) {
      #j <- 1
      #renice only if eljefe/lajefa
      if (driver %in% c("eljefe","lajefa")) {system("renice 19 -u earjr",ignore.stdout=T)}
      
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
      if (driver %in% c("eljefe","lajefa")) {
        if (j%%2 == 0) {
          opt_data$SCRATCH <- "/scratch/earjr"
        } else {
          opt_data$SCRATCH <- "/dev/shm/earjr"
        }
      } else {
        opt_data$SCRATCH <- "~/scratch/earjr"
      }
      
      #run optimiser
      par_optim <- GLAM_optimise(opt_data)
      
      #return object
      rdata_dir <- paste(opt_data$BASE_DIR,"/",opt_data$SIM_NAME,sep="")
      return(rdata_dir)
    }
    
    #check what has been done already
    for (seed in seed_list) {
      #seed <- seed_list[1]
      tdfsel <- dfsel[which(dfsel$SEED == seed),]
      tparam <- paste(dfall$PARAM_NAME[which(dfall$SEED == seed & dfall$ITER == iter & dfall$PARAM_ORDER == i & dfall$STEP == 1)])
      out_dir <- paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,sep="")
      save_file <- paste(out_dir,"/opt-",tdfsel$PARAM_NAME[1],".RData",sep="")
      if (file.exists(save_file)) {dfsel <- dfsel[-which(dfsel$SEED == seed),]}
    }
    
    #if there are param*seeds still not fully done then run the opt function in parallel
    if (nrow(dfsel) > 0) {
      require(snowfall)
      #parallelisation
      sfInit(parallel=T,cpus=70,socketHosts=socket_list,type="SOCK")
      #sfInit(parallel=T,cpus=3)
      sfExport(list=c("dfsel","bin_dir","calib_dir","mdata_dir","met_dir","xy_main","xy_main_yield"))
      sfExport(list=c("me_sel","param_orig","src.dir","xy_sel","wd","iter","done_param","seed_step_run"))
      sfExport(list=c("driver"))
      run_steps <- sfSapply(as.vector(1:nrow(dfsel)), seed_step_run)
      sfStop()
      #for (a in 1:nrow(dfsel)) {cat(a,"\n"); runstep <- seed_step_run(a)}
    }
    
    #determine optimal value per seed for this *iter* and *i* (for parameter "i")
    for (seed in seed_list) {
      #seed <- seed_list[1]
      tdfsel <- dfsel[which(dfsel$SEED == seed),]
      tparam <- paste(dfall$PARAM_NAME[which(dfall$SEED == seed & dfall$ITER == iter & dfall$PARAM_ORDER == i & dfall$STEP == 1)])
      
      #outdir and save_file
      out_dir <- paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,sep="")
      if (!file.exists(out_dir)) {dir.create(out_dir)}
      save_file <- paste(out_dir,"/opt-",tparam,".RData",sep="")
      
      if (nrow(tdfsel) > 0) {
        #if there are rows it means the parameter has just been done (all its steps)
        #hence load and append into single object
        for (k in 1:nrow(tdfsel)) {
          #k <- 1
          load(paste(calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,"_param-",tdfsel$PARAM_NAME[k],"_step-",tdfsel$STEP[k],"/opt-",tdfsel$PARAM_NAME[k],".RData",sep=""))
          out_row <- r_list[[1]]
          ygp_all <- r_list[[2]]
          this_cal <- r_list[[3]]
          yr_out <- r_list[[4]]
          rm(r_list)
          system(paste("rm -rf ",calib_dir,"/","optim_me-",me_sel,"_seed-",seed,"_iter-",iter,"_param-",tdfsel$PARAM_NAME[k],"_step-",tdfsel$STEP[k],sep=""))
          
          if (k == 1) {
            out_all <- out_row
            out_raw <- ygp_all
            cal_all <- this_cal
            cal_raw <- yr_out
          } else {
            out_all <- rbind(out_all,out_row)
            out_raw <- rbind(out_raw,ygp_all)
            cal_all <- rbind(cal_all,this_cal)
            cal_raw <- rbind(cal_raw,yr_out)
          }
        }
        r_list <- list(OPTIMISATION=out_all, RAW_OPTIMISATION=out_raw, 
                       CALIBRATION=cal_all, RAW_CALIBRATION=cal_raw)
        save(list=c("r_list"),file=save_file)
      } else {
        load(save_file)
      }
      #determine optimum value
      opt_val <- r_list$OPTIMISATION$VALUE[which(r_list$OPTIMISATION$RMSE == min(r_list$OPTIMISATION$RMSE))]
      if (length(opt_val > 1)) {opt_val <- opt_val[ceiling(length(opt_val)/2)]}
      
      #append to done param
      done_param <- rbind(done_param, data.frame(ITER=iter, PARAM_ORDER=i, 
                                                 SEED=seed, PARAM_NAME=tparam,
                                                 OPT_VALUE=opt_val))
    }
  }
}



