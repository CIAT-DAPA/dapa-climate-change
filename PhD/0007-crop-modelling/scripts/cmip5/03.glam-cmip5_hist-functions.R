#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Sept 2012

glam_cmip5_hist_wrapper <- function(this_run) {
  #get the run details
  expID <- runs_ref$EXPID[this_run]
  gcm <- paste(runs_ref$GCM[this_run])
  
  #check the existence of three parameters needed for sourcing this script
  if (class(try(get("src.dir"),silent=T)) == "try-error") {
    stop("src.dir needs to be set")
  }
  
  if (class(try(get("bDir"),silent=T)) == "try-error") {
    stop("bDir needs to be set")
  }
  
  if (class(try(get("maxiter"),silent=T)) == "try-error") {
    stop("maxiter needs to be set")
  }
  
  #Read in a dummy GLAM parameter file and create a new one based on a new parameter for
  #running and optimising GLAM
  
  #source all needed functions
  source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
  source(paste(src.dir,"/cmip5/03.glam-cmip5_hist-functions.R",sep=""))
  
  #input directories and model
  cropName <- "gnut"
  cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
  pDir <- paste(cDir,"/params",sep="") #parameter files
  
  #load cell details
  cells <- read.csv(paste(cDir,"/inputs/calib-cells-selection-",selection,".csv",sep=""))
  
  #ci <- 1
  ciList <- 1:nrow(cells) #which(cells$ZONE == zone)
  for (ci in ciList) {
    #get run setup
    #files that were generated
    setup <- list()
    setup$BDIR <- bDir
    setup$SCRATCH <- scratch
    setup$USE_SCRATCH <- use_scratch
    setup$CELL <- cells$CELL[ci]
    setup$ZONE <- cells$ZONE[ci]
    setup$METHOD <- "lin"
    setup$CROPNAME <- "gnut"
    setup$CAL_DIR <- paste(setup$BDIR,"/model-runs/",toupper(setup$CROPNAME),"/runs/cmip5_hist/exp-",expID,"_outputs/",gcm,sep="")
    setup$PRE_DIR <- paste(setup$BDIR,"/model-runs/",toupper(setup$CROPNAME),"/calib/exp-",expID,"_outputs",sep="")
    setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
    setup$YGP_FILE <- "nofile"
    setup$SOW_FILE_RFD <- paste(setup$PRE_DIR,"/gridcells/fcal_",setup$CELL,"/opt_fcal_",setup$CELL,".txt",sep="")
    setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
    setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/rfd_",setup$CELL,sep="")
    setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/irr_",setup$CELL,sep="")
    setup$WTH_ROOT <- "ingc"
    setup$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",setup$CELL,".txt",sep="")
    setup$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
    setup$SIM_NAME <- paste("allin_",setup$CELL,sep="")
    setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
    
    #if using scratch directory instead of nfs
    if (use_scratch) {setup$SCRATCH <- paste(setup$SCRATCH,"/exp-",expID,"_",gcm,sep="")}
    
    cat("\nprocessing cell",setup$CELL,"\n")
    
    #get defaults (parameter set)
    params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
    params$glam_param.mod_mgt$ISYR <- 1966 #start year
    params$glam_param.mod_mgt$IEYR <- 1993 #end year
    params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
    params$glam_param.sim_ctr$NDSLA <- 1
    
    #extract irrigation rates
    irDir <- paste(cDir,"/irrigated_ratio",sep="")
    library(raster)
    ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
    ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
    ir_vls <- as.numeric(ir_vls)
    ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
    ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
    
    ###############################################
    #load the calib.csv, last iteration
    cal_data <- read.csv(paste(setup$PRE_DIR,"/optimisation/z",setup$ZONE,"_rfd_irr/calib.csv",sep=""))
    optimal <- cal_data[which(cal_data$iter==maxiter),]
    
    #update the parameter set
    for (rw in 1:nrow(optimal)) {
      pname <- paste(optimal$param[rw])
      where <- paste(optimal$sect[rw])
      
      if (pname == "TB" | pname == "TO" | pname == "TM") {
        params[[where]][[paste(pname,"FLWR",sep="")]][,"Value"] <- optimal$opt_val[rw]
        params[[where]][[paste(pname,"PODF",sep="")]][,"Value"] <- optimal$opt_val[rw]
        params[[where]][[paste(pname,"LMAX",sep="")]][,"Value"] <- optimal$opt_val[rw]
        params[[where]][[paste(pname,"HARV",sep="")]][,"Value"] <- optimal$opt_val[rw]
      } else {
        params[[where]][[pname]][,"Value"] <- optimal$opt_val[rw]
      }
    }
    
    ###############################################
    # final calibration of YGP for the GCM, using all GCM inputs
    ###############################################
    #run the optimiser for YGP, 100 steps
    parname <- "YGP"
    where <- "glam_param.ygp"
    nstep <- 100
    params[[where]][[parname]][,"Min"] <- 0.01
    params[[where]][[parname]][,"Max"] <- 1.00
    
    if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
      # reset lists of output parameters
      optimal <- list(); optimised <- list()
      
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      #keep only IRR and RFD for YGP=opt & YGP=1
      run_list <- list.files(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),sep=""),pattern="_run-")
      opt_run <- which(optimised[[parname]]$VALUE == optimal[[parname]])
      suffix <- paste("_run-",opt_run,"_",optimal[[parname]],sep="")
      ers_run <- run_list[-grep(suffix,run_list,fixed=T)]
      ers_run <- ers_run[-grep("_run-100_1",ers_run,fixed=T)]
      for (ers in 1:length(ers_run)) {
        system(paste("rm -rf ",paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),"/",ers_run[ers],sep=""),sep=""))
      }
      
      save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
    }
    
    
    ###############################################
    # final calibration of YGP for the GCM, using rainfall from obs
    # this would only hold for the rainfed run, rain is not relevant for irr
    ###############################################
    #copy the irr_rfd data to a temporary location
    #remove data from location if needed
    owth_dir <- paste(setup$SCRATCH,"/rfd_",setup$CELL,sep="")
    if (file.exists(owth_dir)) {system(paste("rm -rf ",owth_dir,sep=""))}
    system(paste("cp -r ",setup$WTH_DIR_RFD," ",setup$SCRATCH,sep=""))
    
    #observed data folders
    obs_dir_rfd <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
    
    #loop years
    for (yr in params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR) {
      #grab obs values
      obs_wth_file <- paste(obs_dir_rfd,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
      obs_data <- read.fortran(obs_wth_file,format=c("I5","F6","3F7"),skip=4)
      names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
      obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
      obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
      wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir,setup$WTH_ROOT,yr,target_var="RAIN",values=obs_data$RAIN)
    }
    
    setup$SIM_NAME <- paste("norain_",setup$CELL,sep="") #"norain_291"
    setup$WTH_DIR_RFD <- owth_dir
    if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
      # reset lists of output parameters
      optimal <- list(); optimised <- list()
      
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      #keep only IRR and RFD for YGP=opt & YGP=1
      run_list <- list.files(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),sep=""),pattern="_run-")
      opt_run <- which(optimised[[parname]]$VALUE == optimal[[parname]])
      suffix <- paste("_run-",opt_run,"_",optimal[[parname]],sep="")
      ers_run <- run_list[-grep(suffix,run_list,fixed=T)]
      ers_run <- ers_run[-grep("_run-100_1",ers_run,fixed=T)]
      for (ers in 1:length(ers_run)) {
        system(paste("rm -rf ",paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),"/",ers_run[ers],sep=""),sep=""))
      }
      
      save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
    }
    setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/rfd_",setup$CELL,sep="")
    setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/irr_",setup$CELL,sep="")
    
    ###############################################
    # final calibration of YGP for the GCM, using tmin/tmax from obs
    ###############################################
    owth_dir_rfd <- paste(setup$SCRATCH,"/rfd_",setup$CELL,sep="")
    if (file.exists(owth_dir_rfd)) {system(paste("rm -rf ",owth_dir_rfd,sep=""))}
    owth_dir_irr <- paste(setup$SCRATCH,"/irr_",setup$CELL,sep="")
    if (file.exists(owth_dir_irr)) {system(paste("rm -rf ",owth_dir_irr,sep=""))}
    system(paste("cp -r ",setup$WTH_DIR_RFD," ",setup$SCRATCH,sep=""))
    system(paste("cp -r ",setup$WTH_DIR_IRR," ",setup$SCRATCH,sep=""))
    
    #observed data folders
    obs_dir_rfd <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
    obs_dir_irr <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
    
    #loop years
    for (yr in params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR) {
      #grab obs values, rainfed
      obs_wth_file <- paste(obs_dir_rfd,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
      obs_data <- read.fortran(obs_wth_file,format=c("I5","F6","3F7"),skip=4)
      names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
      obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
      obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
      wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir_rfd,setup$WTH_ROOT,yr,
                                        target_var="TMIN",values=obs_data$TMIN)
      wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir_rfd,setup$WTH_ROOT,yr,
                                        target_var="TMAX",values=obs_data$TMAX)
      
      #grab obs values, irrigated
      obs_wth_file <- paste(obs_dir_irr,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
      obs_data <- read.fortran(obs_wth_file,format=c("I5","F6","3F7"),skip=4)
      names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
      obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
      obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
      wth_dir_irr <- GLAM_chg_wth_cmip5(owth_dir_irr,setup$WTH_ROOT,yr,
                                        target_var="TMIN",values=obs_data$TMIN)
      wth_dir_irr <- GLAM_chg_wth_cmip5(owth_dir_irr,setup$WTH_ROOT,yr,
                                        target_var="TMAX",values=obs_data$TMAX)
    }
    
    setup$SIM_NAME <- paste("notemp_",setup$CELL,sep="") #"notemp_291"
    setup$WTH_DIR_RFD <- owth_dir
    if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
      # reset lists of output parameters
      optimal <- list(); optimised <- list()
      
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      #keep only IRR and RFD for YGP=opt & YGP=1
      run_list <- list.files(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),sep=""),pattern="_run-")
      opt_run <- which(optimised[[parname]]$VALUE == optimal[[parname]])
      suffix <- paste("_run-",opt_run,"_",optimal[[parname]],sep="")
      ers_run <- run_list[-grep(suffix,run_list,fixed=T)]
      ers_run <- ers_run[-grep("_run-100_1",ers_run,fixed=T)]
      for (ers in 1:length(ers_run)) {
        system(paste("rm -rf ",paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),"/",ers_run[ers],sep=""),sep=""))
      }
      
      save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
    }
    setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/rfd_",setup$CELL,sep="")
    setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/irr_",setup$CELL,sep="")
    
    ###############################################
    # final calibration of YGP for the GCM, using srad from obs
    ###############################################
    owth_dir_rfd <- paste(setup$SCRATCH,"/rfd_",setup$CELL,sep="")
    if (file.exists(owth_dir_rfd)) {system(paste("rm -rf ",owth_dir_rfd,sep=""))}
    owth_dir_irr <- paste(setup$SCRATCH,"/irr_",setup$CELL,sep="")
    if (file.exists(owth_dir_irr)) {system(paste("rm -rf ",owth_dir_irr,sep=""))}
    system(paste("cp -r ",setup$WTH_DIR_RFD," ",setup$SCRATCH,sep=""))
    system(paste("cp -r ",setup$WTH_DIR_IRR," ",setup$SCRATCH,sep=""))
    
    #observed data folders
    obs_dir_rfd <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
    obs_dir_irr <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
    
    #loop years
    for (yr in params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR) {
      #grab obs values, rainfed
      obs_wth_file <- paste(obs_dir_rfd,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
      obs_data <- read.fortran(obs_wth_file,format=c("I5","F6","3F7"),skip=4)
      names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
      obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
      obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
      wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir_rfd,setup$WTH_ROOT,yr,
                                        target_var="SRAD",values=obs_data$SRAD)
      
      #grab obs values, irrigated
      obs_wth_file <- paste(obs_dir_irr,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
      obs_data <- read.fortran(obs_wth_file,format=c("I5","F6","3F7"),skip=4)
      names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
      obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
      obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
      wth_dir_irr <- GLAM_chg_wth_cmip5(owth_dir_irr,setup$WTH_ROOT,yr,
                                        target_var="SRAD",values=obs_data$SRAD)
    }
    
    setup$SIM_NAME <- paste("nosrad_",setup$CELL,sep="") #"nosrad_291"
    setup$WTH_DIR_RFD <- owth_dir
    if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
      # reset lists of output parameters
      optimal <- list(); optimised <- list()
      
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      #keep only IRR and RFD for YGP=opt & YGP=1
      run_list <- list.files(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),sep=""),pattern="_run-")
      opt_run <- which(optimised[[parname]]$VALUE == optimal[[parname]])
      suffix <- paste("_run-",opt_run,"_",optimal[[parname]],sep="")
      ers_run <- run_list[-grep(suffix,run_list,fixed=T)]
      ers_run <- ers_run[-grep("_run-100_1",ers_run,fixed=T)]
      for (ers in 1:length(ers_run)) {
        system(paste("rm -rf ",paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),"/",ers_run[ers],sep=""),sep=""))
      }
      
      save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
    }
    setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/rfd_",setup$CELL,sep="")
    setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",gcm,"/irr_",setup$CELL,sep="")
    if (file.exists(owth_dir_rfd)) {system(paste("rm -rf ",owth_dir_rfd,sep=""))}
    if (file.exists(owth_dir_irr)) {system(paste("rm -rf ",owth_dir_irr,sep=""))}
  }
}



#function to replace the weather files using a set of prescribed values
GLAM_chg_wth_cmip5 <- function(wth_dir,wth_root,yr,target_var="TMIN",values=NA) {
  #cat("transforming",target_var,"...\n")
  #open the file
  wth_file <- paste(wth_dir,"/",wth_root,"001001",yr,".wth",sep="")
  wth_data <- read.fortran(wth_file,format=c("I5","F6","3F7"),skip=4)
  names(wth_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
  wth_data$YEAR <- as.numeric(substr(wth_data$DATE,1,2))
  wth_data$JDAY <- as.numeric(substr(wth_data$DATE,3,5))
  
  sdet_2 <- as.character(read.fortran(wth_file,n=1,format=c("A50")))
  sdet_2 <- strsplit(sdet_2," : ",fixed=T)[[1]][2]
  sdet_1 <- read.fortran(wth_file,skip=2,n=1,format=c("A6","2F9","5F6"))
  sdet_1$V1 <- gsub(" ","",sdet_1$V1)
  s_details <- data.frame(NAME=sdet_2,INSI=sdet_1$V1,LAT=sdet_1$V2,LONG=sdet_1$V3,
                          ELEV=sdet_1$V4,TAV=sdet_1$V5,AMP=sdet_1$V6,
                          REFHT=sdet_1$V7,WNDHT=sdet_1$V8)
  
  wth_data[,toupper(target_var)] <- values
  wth_file <- write_wth(wth_data,outfile=wth_file,site.details=s_details)
  return(wth_dir)
}


