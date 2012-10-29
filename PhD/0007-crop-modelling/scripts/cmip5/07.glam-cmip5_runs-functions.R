#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012


#rcp wrapper
glam_rcp_run_wrapper <- function(RUN_CFG) {
  #check the existence of the environment set up, before running
  if (class(try(get("ENV_CFG"),silent=T)) == "try-error") {
    stop("src.dir needs to be set")
  }
  
  #source functions of interest
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
  source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
  source(paste(src.dir,"/cmip5/07.glam-cmip5_runs-functions.R",sep=""))
  
  #input directories and model
  cropName <- ENV_CFG$CROP_NAME
  cDir <- paste(ENV_CFG$BDIR,"/model-runs/",toupper(ENV_CFG$CROP_NAME),sep="")
  
  #here construct a control file folder
  #out_bdir <- paste(bDir,"/model-runs/",toupper(cropName),"/runs/cmip5_hist",sep="")
  ctrl_dir <- paste(ENV_CFG$OUT_BDIR,"/_process/exp-",RUN_CFG$PARSET,"_",RUN_CFG$SCE,sep="")
  if (!file.exists(ctrl_dir)) {dir.create(ctrl_dir,recursive=T)}
  ctrl_fil <- paste(ctrl_dir,"/",RUN_CFG$PERIOD,"_loc-",RUN_CFG$LOC,"_",RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,".proc",sep="")
  
  if (!file.exists(ctrl_fil)) {
    #create run setup
    #files that were generated
    setup_rcp <- list()
    setup_rcp$BDIR <- ENV_CFG$BDIR
    setup_rcp$SCRATCH <- ENV_CFG$SCRATCH
    setup_rcp$USE_SCRATCH <- ENV_CFG$USE_SCRATCH
    setup_rcp$CELL <- RUN_CFG$LOC
    setup_rcp$ZONE <- ENV_CFG$CELLS$ZONE[which(ENV_CFG$CELLS$CELL == RUN_CFG$LOC)]
    setup_rcp$METHOD <- NA #not needed for future climate runs
    setup_rcp$CROPNAME <- ENV_CFG$CROP_NAME
    setup_rcp$CAL_DIR <- paste(setup_rcp$BDIR,"/model-runs/",toupper(setup_rcp$CROPNAME),"/runs/",ENV_CFG$RUNS_NAME,"/exp-",RUN_CFG$PARSET,"_outputs/",RUN_CFG$SCE,sep="")
    setup_rcp$PRE_DIR <- paste(setup_rcp$BDIR,"/model-runs/",toupper(setup_rcp$CROPNAME),"/calib/exp-",RUN_CFG$PARSET,"_outputs",sep="")
    setup_rcp$YIELD_FILE <- NA #not needed for future climate runs
    setup_rcp$YGP_FILE <- NA #not needed for future climate runs
    setup_rcp$SOW_FILE_RFD <- paste(setup_rcp$PRE_DIR,"/gridcells/fcal_",setup_rcp$CELL,"/opt_fcal_",setup_rcp$CELL,".txt",sep="")
    setup_rcp$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup_rcp$CELL,"_irr.txt",sep="")
    setup_rcp$WTH_DIR_RFD <- NA #temporary
    setup_rcp$WTH_DIR_IRR <- NA #temporary
    setup_rcp$WTH_ROOT <- "ingc"
    setup_rcp$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",setup_rcp$CELL,".txt",sep="")
    setup_rcp$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",setup_rcp$CELL,".txt",sep="")
    setup_rcp$SIM_NAME <- NA # temporary
    setup_rcp$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
    setup_rcp$OPT_METHOD <- NA #not needed for future climate run
    
    #if using scratch directory instead of nfs
    if (setup_rcp$USE_SCRATCH) {setup_rcp$SCRATCH <- paste(setup_rcp$SCRATCH,"/exp-",RUN_CFG$PARSET,"_",RUN_CFG$SCE,sep="")}
    
    cat("\nprocessing cell",setup_rcp$CELL,"run",RUN_CFG$RUNID,"\n")
    
    #type of input
    inputType <- gsub("rcp_","",RUN_CFG$WTYPE)
    setup_rcp$SIM_NAME <- paste(RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,"_",setup_rcp$CELL,sep="")
    
    #here check if baseline run has been done
    hisType <- gsub("rcp_","his_",RUN_CFG$WTYPE)
    his_dir <- paste(setup_rcp$CAL_DIR,"/",hisType,"_",setup_rcp$CELL,sep="")
    saveFile <- paste(setup_rcp$CAL_DIR,"/",setup_rcp$SIM_NAME,"/output.RData",sep="")
    saveFile_his <- paste(his_dir,"/output.RData",sep="")
    
    if (file.exists(saveFile_his)) {
      if (!file.exists(saveFile)) {
        #load output hist data
        load(saveFile_his)
        cal_ygp <- optimal$YGP
        rm(out_data); rm(setup); rm(optimal); rm(optimised); g=gc(); rm(g) #remove extra stuff
        
        #update parameter set for this run
        params$glam_param.mod_mgt$ISYR <- 2022 #start year
        params$glam_param.mod_mgt$IEYR <- 2049 #end year
        
        ir_vls$YEAR <- params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR
        
        #update parameter set with CO2-related configuration
        params$glam_param.sim_ctr$IC02 <- 1
        params$glam_param.hts_fut$B_TE$Value <- params$glam_param.bmass$TE$Value
        params$glam_param.hts_fut$B_TEN_MAX$Value <- params$glam_param.bmass$TEN_MAX$Value
        params$glam_param.hts_fut$TENFAC$Value <- CO2Exp$TENFAC[which(CO2Exp$EXP_NAME==RUN_CFG$CO2_P)]
        params$glam_param.sim_ctr$SLA_INI <- params$glam_param.sim_ctr$SLA_INI * CO2Exp$SLA_INI[which(CO2Exp$EXP_NAME==RUN_CFG$CO2_P)]
        params$glam_param.bmass$TE$Value <- params$glam_param.bmass$TE$Value * CO2Exp$TE[which(CO2Exp$EXP_NAME==RUN_CFG$CO2_P)]
        params$glam_param.evap$P_TRANS_MAX$Value <- params$glam_param.evap$P_TRANS_MAX$Value * CO2Exp$P_TRANS_MAX[which(CO2Exp$EXP_NAME==RUN_CFG$CO2_P)]
        
        #assign directories according to input type
        if (inputType == "allin") {
          setup_rcp$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45/",RUN_CFG$SCE,"/rfd_",setup_rcp$CELL,sep="")
          setup_rcp$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45/",RUN_CFG$SCE,"/irr_",setup_rcp$CELL,sep="")
        } else if (inputType == "bcrain") {
          setup_rcp$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_bc/",RUN_CFG$SCE,"/rfd_",setup_rcp$CELL,sep="")
          setup_rcp$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_bc/",RUN_CFG$SCE,"/irr_",setup_rcp$CELL,sep="")
        }
        
        #load and update parameter files as per sowing dates
        sowDate_rfd <- read.fortran(setup_rcp$SOW_FILE_RFD,format=c("A8","I6"))
        sowDate_rfd <- as.numeric(sowDate_rfd$V2)
        sowDate_irr <- read.fortran(setup_rcp$SOW_FILE_IRR,format=c("A8","I6"))
        sowDate_irr <- as.numeric(sowDate_irr$V2)
        
        #loop these and produce files for alternate planting dates
        tmpSow <- paste(setup_rcp$SCRATCH,"/sow_tmp",sep="")
        if (!file.exists(paste(tmpSow,"/",RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,sep=""))) {
          dir.create(paste(tmpSow,"/",RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,sep=""),recursive=T)
        }
        
        osow_dir <- paste(tmpSow,"/",RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,"/loc_",setup_rcp$CELL,sep="")
        if (!file.exists(osow_dir)) {dir.create(osow_dir)}
        
        #loop to create the temporary sowing files
        sdCount <- 1
        for (sdv in sdList) {
          ofil_rfd <- paste(osow_dir,"/rfd_r",sdCount,".txt",sep="")
          ofil_irr <- paste(osow_dir,"/irr_r",sdCount,".txt",sep="")
          sdVal_rfd <- sowDate_rfd+sdv
          sdVal_irr <- sowDate_irr+sdv
          dummx_rfd <- data.frame(CELL=setup_rcp$CELL,COL=1,ROW=1,SOW_DATE=sdVal_rfd)
          dummx_irr <- data.frame(CELL=setup_rcp$CELL,COL=1,ROW=1,SOW_DATE=sdVal_irr)
          ofil_rfd <- write_sowdates(dummx_rfd,ofil_rfd,cell=setup_rcp$CELL,fields=list(CELL="CELL",COL="COL",ROW="ROW",SOW_DATE="SOW_DATE"))
          ofil_irr <- write_sowdates(dummx_irr,ofil_irr,cell=setup_rcp$CELL,fields=list(CELL="CELL",COL="COL",ROW="ROW",SOW_DATE="SOW_DATE"))
          sdCount <- sdCount+1
        }
        
        #here run the model. perhaps here loop the sowing dates
        for (sdc in 1:(sdCount-1)) {
          setup_rcp$SOW_FILE_RFD <- paste(osow_dir,"/rfd_r",sdc,".txt",sep="")
          setup_rcp$SOW_FILE_IRR <- paste(osow_dir,"/irr_r",sdc,".txt",sep="")
          
          loc_dir <- GLAM_run_loc(GLAM_params=params,RUN_setup=setup,
                                  iratio=ir_vls,subdir=paste("r",sdc,sep=""))
        }
      }
    } else {
      cat("baseline experiment for this run has not yet been run \n")
    }
    
  }
  
}


#historical wrapper
glam_hist_run_wrapper <- function(RUN_CFG) {
  #check the existence of the environment set up, before running
  if (class(try(get("ENV_CFG"),silent=T)) == "try-error") {
    stop("src.dir needs to be set")
  }
  
  #source functions of interest
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
  source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
  source(paste(src.dir,"/cmip5/07.glam-cmip5_runs-functions.R",sep=""))
  
  #input directories and model
  cropName <- ENV_CFG$CROP_NAME
  cDir <- paste(ENV_CFG$BDIR,"/model-runs/",toupper(ENV_CFG$CROP_NAME),sep="")
  pDir <- paste(cDir,"/params",sep="") #parameter files
  
  #here construct a control file folder
  #out_bdir <- paste(bDir,"/model-runs/",toupper(cropName),"/runs/cmip5_hist",sep="")
  ctrl_dir <- paste(ENV_CFG$OUT_BDIR,"/_process/exp-",RUN_CFG$PARSET,"_",RUN_CFG$SCE,sep="")
  if (!file.exists(ctrl_dir)) {dir.create(ctrl_dir,recursive=T)}
  ctrl_fil <- paste(ctrl_dir,"/",RUN_CFG$PERIOD,"_loc-",RUN_CFG$LOC,"_",RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,".proc",sep="")
  
  if (!file.exists(ctrl_fil)) {
    #create run setup
    #files that were generated
    setup <- list()
    setup$BDIR <- ENV_CFG$BDIR
    setup$SCRATCH <- ENV_CFG$SCRATCH
    setup$USE_SCRATCH <- ENV_CFG$USE_SCRATCH
    setup$CELL <- RUN_CFG$LOC
    setup$ZONE <- ENV_CFG$CELLS$ZONE[which(ENV_CFG$CELLS$CELL == RUN_CFG$LOC)]
    setup$METHOD <- "lin"
    setup$CROPNAME <- ENV_CFG$CROP_NAME
    setup$CAL_DIR <- paste(setup$BDIR,"/model-runs/",toupper(setup$CROPNAME),"/runs/",ENV_CFG$RUNS_NAME,"/exp-",RUN_CFG$PARSET,"_outputs/",RUN_CFG$SCE,sep="")
    setup$PRE_DIR <- paste(setup$BDIR,"/model-runs/",toupper(setup$CROPNAME),"/calib/exp-",RUN_CFG$PARSET,"_outputs",sep="")
    setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
    setup$YGP_FILE <- "nofile"
    setup$SOW_FILE_RFD <- paste(setup$PRE_DIR,"/gridcells/fcal_",setup$CELL,"/opt_fcal_",setup$CELL,".txt",sep="")
    setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
    setup$WTH_DIR_RFD <- NA #temporary
    setup$WTH_DIR_IRR <- NA #temporary
    setup$WTH_ROOT <- "ingc"
    setup$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",setup$CELL,".txt",sep="")
    setup$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
    setup$SIM_NAME <- NA # temporary paste("allin_",setup$CELL,sep="")
    setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
    setup$OPT_METHOD <- ENV_CFG$OPT_METHOD
    
    #if using scratch directory instead of nfs
    if (setup$USE_SCRATCH) {setup$SCRATCH <- paste(setup$SCRATCH,"/exp-",RUN_CFG$PARSET,"_",RUN_CFG$SCE,sep="")}
    
    cat("\nprocessing cell",setup$CELL,"run",RUN_CFG$RUNID,"\n")
    
    #get defaults (parameter set)
    params <- GLAM_get_default(x=ENV_CFG$CELLS,cell=setup$CELL,parDir=pDir)
    params$glam_param.mod_mgt$ISYR <- 1966 #start year
    params$glam_param.mod_mgt$IEYR <- 1993 #end year
    params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
    params$glam_param.sim_ctr$NDSLA <- 1
    
    #extract irrigation rates
    ir_vls <- get_loc_irr(setup$CELL,ENV_CFG$IRR_DATA)
    
    ###############################################
    #load the calib.csv, last iteration, and update parameter set
    cal_data <- read.csv(paste(setup$PRE_DIR,"/optimisation/z",setup$ZONE,"_rfd_irr/calib.csv",sep=""))
    optimal <- cal_data[which(cal_data$iter==ENV_CFG$MAXITER),]
    params <- update_params(optimal,params) #update parameter set
    
    ###############################################
    # final calibration of YGP for the GCM, using all GCM inputs
    ###############################################
    #run the optimiser for YGP, 100 steps
    parname <- "YGP"
    where <- "glam_param.ygp"
    nstep <- 20
    params[[where]][[parname]][,"Min"] <- 0.05
    params[[where]][[parname]][,"Max"] <- 1.00
    
    #######
    inputType <- gsub("his_","",RUN_CFG$WTYPE)
    setup$SIM_NAME <- paste(RUN_CFG$WTYPE,"_",setup$CELL,sep="")
    
    #### choosing the types of inputs
    if (inputType == "allin") {
      setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/rfd_",setup$CELL,sep="")
      setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/irr_",setup$CELL,sep="")
    } else if (inputType == "bcrain") {
      setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_hist_bc/",RUN_CFG$SCE,"/rfd_",setup$CELL,sep="")
      setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_hist_bc/",RUN_CFG$SCE,"/irr_",setup$CELL,sep="")
    } else if (inputType == "norain") {
      #copy the irr_rfd data to a temporary location
      #remove data from location if needed
      wthTmp <- paste(setup$SCRATCH,"/wth_tmp",sep="")
      if (!file.exists(paste(wthTmp,"/",RUN_CFG$WTYPE,sep=""))) {
        dir.create(paste(wthTmp,"/",RUN_CFG$WTYPE,sep=""),recursive=T)
      }
      
      owth_dir <- paste(wthTmp,"/",RUN_CFG$WTYPE,"/rfd_",setup$CELL,sep="")
      if (file.exists(owth_dir)) {system(paste("rm -rf ",owth_dir,sep=""))}
      
      ori_wth <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/rfd_",setup$CELL,sep="")
      system(paste("cp -r ",ori_wth," ",owth_dir,sep=""))
      
      #observed data folders
      obs_wth <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
      
      #loop years
      for (yr in params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR) {
        #grab obs values
        obs_file <- paste(obs_wth,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
        obs_data <- read.fortran(obs_file,format=c("I5","F6","3F7"),skip=4)
        names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
        obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
        obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
        wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir,setup$WTH_ROOT,yr,target_var="RAIN",values=obs_data$RAIN)
      }
      #run model with original data
      setup$WTH_DIR_RFD <- wth_dir_rfd
      setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/irr_",setup$CELL,sep="")
      
    } else if (inputType == "notemp") {
      ####temperature from observations
      wthTmp <- paste(setup$SCRATCH,"/wth_tmp",sep="")
      if (!file.exists(paste(wthTmp,"/",RUN_CFG$WTYPE,sep=""))) {
        dir.create(paste(wthTmp,"/",RUN_CFG$WTYPE,sep=""),recursive=T)
      }
      
      #removing existing data
      owth_dir_rfd <- paste(wthTmp,"/",RUN_CFG$WTYPE,"/rfd_",setup$CELL,sep="")
      if (file.exists(owth_dir_rfd)) {system(paste("rm -rf ",owth_dir_rfd,sep=""))}
      owth_dir_irr <- paste(wthTmp,"/",RUN_CFG$WTYPE,"/irr_",setup$CELL,sep="")
      if (file.exists(owth_dir_irr)) {system(paste("rm -rf ",owth_dir_irr,sep=""))}
      
      #copying needed data
      ori_wth_rfd <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/rfd_",setup$CELL,sep="")
      system(paste("cp -r ",ori_wth_rfd," ",owth_dir_rfd,sep=""))
      ori_wth_irr <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/irr_",setup$CELL,sep="")
      system(paste("cp -r ",ori_wth_irr," ",owth_dir_irr,sep=""))
      
      #observed data folders
      obs_wth_rfd <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
      obs_wth_irr <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
      
      #loop years
      for (yr in params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR) {
        #grab obs values, rainfed
        obs_file <- paste(obs_wth_rfd,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
        obs_data <- read.fortran(obs_file,format=c("I5","F6","3F7"),skip=4)
        names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
        obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
        obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
        wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir_rfd,setup$WTH_ROOT,yr,
                                          target_var="TMIN",values=obs_data$TMIN)
        wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir_rfd,setup$WTH_ROOT,yr,
                                          target_var="TMAX",values=obs_data$TMAX)
        
        #grab obs values, irrigated
        obs_file <- paste(obs_wth_irr,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
        obs_data <- read.fortran(obs_file,format=c("I5","F6","3F7"),skip=4)
        names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
        obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
        obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
        wth_dir_irr <- GLAM_chg_wth_cmip5(owth_dir_irr,setup$WTH_ROOT,yr,
                                          target_var="TMIN",values=obs_data$TMIN)
        wth_dir_irr <- GLAM_chg_wth_cmip5(owth_dir_irr,setup$WTH_ROOT,yr,
                                          target_var="TMAX",values=obs_data$TMAX)
      }
      setup$WTH_DIR_RFD <- wth_dir_rfd
      setup$WTH_DIR_IRR <- wth_dir_irr
    } else if (inputType == "nosrad") {
      ####solar radiation from observations
      wthTmp <- paste(setup$SCRATCH,"/wth_tmp",sep="")
      if (!file.exists(paste(wthTmp,"/",RUN_CFG$WTYPE,sep=""))) {
        dir.create(paste(wthTmp,"/",RUN_CFG$WTYPE,sep=""),recursive=T)
      }
      
      #removing existing data
      owth_dir_rfd <- paste(wthTmp,"/",RUN_CFG$WTYPE,"/rfd_",setup$CELL,sep="")
      if (file.exists(owth_dir_rfd)) {system(paste("rm -rf ",owth_dir_rfd,sep=""))}
      owth_dir_irr <- paste(wthTmp,"/",RUN_CFG$WTYPE,"/irr_",setup$CELL,sep="")
      if (file.exists(owth_dir_irr)) {system(paste("rm -rf ",owth_dir_irr,sep=""))}
      
      #copying needed data
      ori_wth_rfd <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/rfd_",setup$CELL,sep="")
      system(paste("cp -r ",ori_wth_rfd," ",owth_dir_rfd,sep=""))
      ori_wth_irr <- paste(cDir,"/inputs/ascii/wth-cmip5_hist/",RUN_CFG$SCE,"/irr_",setup$CELL,sep="")
      system(paste("cp -r ",ori_wth_irr," ",owth_dir_irr,sep=""))
      
      #observed data folders
      obs_wth_rfd <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
      obs_wth_irr <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
      
      #loop years
      for (yr in params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR) {
        #grab obs values, rainfed
        obs_file <- paste(obs_wth_rfd,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
        obs_data <- read.fortran(obs_file,format=c("I5","F6","3F7"),skip=4)
        names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
        obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
        obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
        wth_dir_rfd <- GLAM_chg_wth_cmip5(owth_dir_rfd,setup$WTH_ROOT,yr,
                                          target_var="SRAD",values=obs_data$SRAD)
        
        #grab obs values, irrigated
        obs_file <- paste(obs_wth_irr,"/",setup$WTH_ROOT,"001001",yr,".wth",sep="")
        obs_data <- read.fortran(obs_file,format=c("I5","F6","3F7"),skip=4)
        names(obs_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
        obs_data$YEAR <- as.numeric(substr(obs_data$DATE,1,2))
        obs_data$JDAY <- as.numeric(substr(obs_data$DATE,3,5))
        wth_dir_irr <- GLAM_chg_wth_cmip5(owth_dir_irr,setup$WTH_ROOT,yr,
                                          target_var="SRAD",values=obs_data$SRAD)
      }
      setup$WTH_DIR_RFD <- owth_dir_rfd
      setup$WTH_DIR_IRR <- owth_dir_irr
    }
    
    ############
    #here the model is being run
    ############
    saveFile <- paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/output.RData",sep="")
    if (!file.exists(saveFile)) {
      #run model with original data
      
      #here run the model
      optimal <- list(); optimised <- list()
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      #update parameter file (YGP value
      params[[where]][[parname]][,"Value"] <- optimal[[parname]]
      
      #here grab the output of relevant runs
      if (length(optimal[[parname]]) > 0) {
        #keep only IRR and RFD for YGP=opt & YGP=1
        run_list <- list.files(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),sep=""),pattern="_run-")
        opt_run <- which(optimised[[parname]]$VALUE == optimal[[parname]])
        suffix <- paste("_run-",opt_run,"_",optimal[[parname]],sep="")
        ers_run <- run_list[grep(suffix,run_list,fixed=T)]
        ers_run <- c(ers_run,run_list[grep(paste("_run-",nstep,"_1",sep=""),run_list,fixed=T)])
        ers_run <- unique(ers_run)
        
        out_data <- list()
        #load glam output data and keep into list
        for (ers in 1:length(ers_run)) {
          ers_dir <- paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),"/",ers_run[ers],sep="")
          setwd(ers_dir)
          
          out_data[[ers]] <- list()
          out_data[[ers]][["RUN_TYPE"]] <- unlist(strsplit(ers_run[ers],"_",fixed=T))[1]
          out_data[[ers]][["RUN_NO"]] <- unlist(strsplit(ers_run[ers],"_",fixed=T))[2]
          out_data[[ers]][["YGP"]] <- unlist(strsplit(ers_run[ers],"_",fixed=T))[3]
          
          if (file.exists("./output/groundnut.out")) {
            out_data[[ers]][["DATA"]] <- read.table("./output/groundnut.out",header=F,sep="\t")
          } else {
            out_data[[ers]][["DATA"]] <- as.data.frame(matrix(NA,nrow=28,ncol=42))
          }
          names(out_data[[ers]][["DATA"]]) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                                                "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                                                "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                                                "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                                                "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
        }
      } else {
        out_data <- list(); out_data[[1]] <- list()
        out_data[[1]][["RUN_TYPE"]] <- NA
        out_data[[1]][["RUN_NO"]] <- NA
        out_data[[1]][["YGP"]] <- NA
        out_data[[1]][["DATA"]] <- as.data.frame(matrix(NA,nrow=28,ncol=42))
        names(out_data[[1]][["DATA"]]) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                                            "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                                            "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                                            "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                                            "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
      }
      #here save the thing
      save(list=c("optimised","optimal","out_data","ir_vls","params","setup"),file=saveFile)
      
      #here remove everything
      setwd(setup$BDIR)
      system(paste("rm -rf ",paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),sep="")))
    }
    
    #delete folders of temporary data, if they exist
    if (exists("wth_dir_rfd")) {if (file.exists(wth_dir_rfd)) {system(paste("rm -rf ",wth_dir_rfd,sep=""))}}
    if (exists("wth_dir_irr")) {if (file.exists(wth_dir_irr)) {system(paste("rm -rf ",wth_dir_irr,sep=""))}}
    
    #write control file
    ff <- file(ctrl_fil,"w")
    cat("Processed on",date(),"\n",file=ff)
    close(ff)
  }
  return(ctrl_fil)
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


#get initial model configuration
get_cfg <- function(i,all_proc) {
  this_run <- all_proc[i,]
  RUN_CFG <- list()
  RUN_CFG$RUNID <- paste(this_run$RUNID)
  RUN_CFG$PERIOD <- unlist(strsplit(RUN_CFG$RUNID,"_",fixed=T))[1]
  RUN_CFG$LOC <- this_run$LOC
  RUN_CFG$SCE <- paste(this_run$GCM)
  RUN_CFG$GCM <- unlist(strsplit(RUN_CFG$SCE,"_ENS_"))[1]
  RUN_CFG$ENS <- unlist(strsplit(RUN_CFG$SCE,"_ENS_"))[2]
  RUN_CFG$PARSET <- this_run$PARSET
  RUN_CFG$WTYPE <- paste(this_run$WTH_TYPE)
  RUN_CFG$CO2_P <- paste(this_run$CO2_P)
  return(RUN_CFG)
}

#update parameter set based on a list of previously tuned optimal parameters
update_params <- function(optimal,params) {
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
  return(params)
}

#get irrigation data
get_ir_vls <- function(cDir,cells,isyr,ieyr) {
  library(raster)
  irDir <- paste(cDir,"/irrigated_ratio",sep="")
  ir_stk <- stack(paste(irDir,"/raw-",isyr:ieyr,".asc",sep=""))
  ir_vls <- extract(ir_stk,cbind(X=cells$X,Y=cells$Y))
  ir_vls <- data.frame(t(ir_vls))
  names(ir_vls) <- paste("LOC.",cells$CELL,sep="")
  ir_vls <- cbind(YEAR=isyr:ieyr,ir_vls)
  row.names(ir_vls) <- 1:nrow(ir_vls)
  return(ir_vls)
}


get_loc_irr <- function(loc,irr_data) {
  ir_vls <- data.frame(YEAR=irr_data$YEAR,IRATIO=irr_data[,paste("LOC.",loc,sep="")])
  ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
  return(ir_vls)
}


############################################################
####### function to optimise a given parameter in GLAM, 
####### except sowing date, RLL, DUL, SAT
############################################################
GLAM_run_loc <- function(GLAM_params,RUN_setup,iratio=0,subdir="r1") {
  simset <- RUN_setup$SIM_NAME
  cell <- RUN_setup$CELL
  cropName <- RUN_setup$CROPNAME
  bDir <- RUN_setup$BDIR
  isyr <- GLAM_params$glam_param.mod_mgt$ISYR
  ieyr <- GLAM_params$glam_param.mod_mgt$IEYR
  
  #input directories and model
  execName <- paste("glam-",tolower(cropName),sep="")
  
  #determine operating system and bin folder
  machine <- as.data.frame(t(Sys.info()))
  machine <- paste(machine$sysname)
  binDir <- paste(bDir,"/model-runs/bin/glam-",tolower(machine),sep="")
  
  if (tolower(machine) == "windows") {
    glam_cmd <- paste(paste(execName,".exe",sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
    execName <- paste(execName,".exe",sep="")
  } else if (tolower(machine) == "linux") {
    glam_cmd <- paste(paste("./",execName,sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
  }
  
  #output directories
  if (RUN_setup$USE_SCRATCH) {
    cal_dir <- RUN_setup$SCRATCH #run directory
    nfs_dir <- paste(RUN_setup$CAL_DIR,"/",simset,sep="")
    if (!file.exists(nfs_dir)) {dir.create(nfs_dir,recursive=T)}
  } else {
    cal_dir <- RUN_setup$CAL_DIR #run directory
  }
  if (!file.exists(cal_dir)) {dir.create(cal_dir,recursive=T)}
  
  cal_dir <- paste(cal_dir,"/",simset,sep="") #run directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #cal_dir <- paste(cal_dir,"/run-",subdir,sep="")
  #if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #files that were generated
  yFile <- "nofile"
  ygpFile <- "nofile"
  sowFile_rfd <- RUN_setup$SOW_FILE_RFD
  sowFile_irr <- RUN_setup$SOW_FILE_IRR
  wthDir_rfd <- RUN_setup$WTH_DIR_RFD
  wthDir_irr <- RUN_setup$WTH_DIR_IRR
  solFile <- RUN_setup$SOL_FILE
  solGrid <- RUN_setup$SOL_GRID
  
  #irrigated/rainfed/mix variable
  if (length(which(iratio$IRATIO == 0)) == nrow(iratio)) {
    run.type <- "RFD"
  } else if (length(which(iratio$IRATIO == 1)) == nrow(iratio)) {
    run.type <- "IRR"
  } else {
    run.type <- "MIX"
  }
  
  #loop through sequence of values
  cat("performing run",run.type,"\n")
  
  ##############here irrigation rate
  if (run.type == "RFD") {
    GLAM_params$glam_param.mod_mgt$SEASON <- "RFD"
    
    #check if the planting date is well configured
    if (sowFile_rfd == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/",run.type,"_run-",subdir,sep=""))
    
    #write the model params
    opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
    opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      
      ######################################################
      #write filenames file
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      wth_row <- paste("inputs/ascii/wth/",RUN_setup$WTH_ROOT,sep="")
      soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(solFile,"/",fixed=T))[length(unlist(strsplit(solFile,"/",fixed=T)))],sep="")
      soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(solGrid,"/",fixed=T))[length(unlist(strsplit(solGrid,"/",fixed=T)))],sep="")
      yield_row <- yFile
      
      if (sowFile_rfd == "nofile") {
        sow_row <- sowFile_rfd
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_rfd,"/",fixed=T))[length(unlist(strsplit(sowFile_rfd,"/",fixed=T)))],sep="")
      }
      if (ygpFile == "nofile") {
        ygp_row <- "nofile"
        if (GLAM_params$glam_param.ygp$YGP$Value < -90) {
          stop("YGP needs to be specified either by value or by a file")
        }
      } else {
        ygp_row <- paste("inputs/",unlist(strsplit(ygpFile,"/",fixed=T))[length(unlist(strsplit(ygpFile,"/",fixed=T)))],sep="")
        GLAM_params$glam_param.ygp$YGP$Value <- -99.0
      }
      
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      
      if (sowFile_rfd != "nofile") {
        x <- file.copy(sowFile_rfd,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_rfd),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_rfd,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file, .inf file and compress the daily files
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #if daily files were produced then compress and remove
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      rfd_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    }
    
  } else if (run.type == "IRR") {
    GLAM_params$glam_param.mod_mgt$SEASON <- "IRR"
    
    #check if the planting date is well configured
    if (sowFile_irr == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/",run.type,"_run-",subdir,sep=""))
    
    #write the model params
    opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
    opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      ######################################################
      #write filenames file
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      wth_row <- paste("inputs/ascii/wth/",RUN_setup$WTH_ROOT,sep="")
      soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(solFile,"/",fixed=T))[length(unlist(strsplit(solFile,"/",fixed=T)))],sep="")
      soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(solGrid,"/",fixed=T))[length(unlist(strsplit(solGrid,"/",fixed=T)))],sep="")
      yield_row <- yFile
      
      if (sowFile_irr == "nofile") {
        sow_row <- sowFile_irr
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_irr,"/",fixed=T))[length(unlist(strsplit(sowFile_irr,"/",fixed=T)))],sep="")
      }
      if (ygpFile == "nofile") {
        ygp_row <- "nofile"
        if (GLAM_params$glam_param.ygp$YGP$Value < -90) {
          stop("YGP needs to be specified either by value or by a file")
        }
      } else {
        ygp_row <- paste("inputs/",unlist(strsplit(ygpFile,"/",fixed=T))[length(unlist(strsplit(ygpFile,"/",fixed=T)))],sep="")
        GLAM_params$glam_param.ygp$YGP$Value <- -99.0
      }
      
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      if (sowFile_irr != "nofile") {
        x <- file.copy(sowFile_irr,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_irr),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_irr,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #compress & remove daily files should they exist
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      irr_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    }
  } else if (run.type == "MIX") {
    GLAM_params$glam_param.mod_mgt$SEASON <- "RFD"
    
    #check if the planting date is well configured
    if (sowFile_rfd == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/RFD_run-",subdir,sep=""))
    
    #write the model params
    opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep="")
    opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      ######################################################
      #write filenames file
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      wth_row <- paste("inputs/ascii/wth/",RUN_setup$WTH_ROOT,sep="")
      soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(solFile,"/",fixed=T))[length(unlist(strsplit(solFile,"/",fixed=T)))],sep="")
      soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(solGrid,"/",fixed=T))[length(unlist(strsplit(solGrid,"/",fixed=T)))],sep="")
      yield_row <- yFile
      
      if (sowFile_rfd == "nofile") {
        sow_row <- sowFile_rfd
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_rfd,"/",fixed=T))[length(unlist(strsplit(sowFile_rfd,"/",fixed=T)))],sep="")
      }
      if (ygpFile == "nofile") {
        ygp_row <- "nofile"
        if (GLAM_params$glam_param.ygp$YGP$Value < -90) {
          stop("YGP needs to be specified either by value or by a file")
        }
      } else {
        ygp_row <- paste("inputs/",unlist(strsplit(ygpFile,"/",fixed=T))[length(unlist(strsplit(ygpFile,"/",fixed=T)))],sep="")
        GLAM_params$glam_param.ygp$YGP$Value <- -99.0
      }
      
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      if (sowFile_rfd != "nofile") {
        x <- file.copy(sowFile_rfd,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_rfd),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_rfd,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #compress & remove daily files
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      rfd_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
    }
    
    ##!
    #Now the irrigated run
    GLAM_params$glam_param.mod_mgt$SEASON <- "IRR"
    
    #check if the planting date is well configured
    if (sowFile_irr == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(optDir,"/IRR_run-",subdir,sep=""))
    
    #write the model params
    opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run-irr.txt",sep="")
    opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      ######################################################
      #write filenames file
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      
      if (sowFile_irr == "nofile") {
        sow_row <- sowFile_irr
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_irr,"/",fixed=T))[length(unlist(strsplit(sowFile_irr,"/",fixed=T)))],sep="")
      }
      
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      if (sowFile_irr != "nofile") {
        x <- file.copy(sowFile_irr,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_irr),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_irr,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #compress & remove daily files, should they exist (IASCII = 2 or 3)
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      irr_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
    }
  }
  
  names(rfd_data) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                       "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
  names(irr_data) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS","T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
  
  out_all <- list(); out_all$DATA <- list()
  out_all$DATA$RFD <- rfd_data
  out_all$DATA$IRR <- irr_data
  
  if (RUN_setup$USE_SCRATCH) {
    system(paste("cp -rf ",cal_dir," ",paste(nfs_dir,"/.",sep=""),sep=""))
    setwd(nfs_dir)
    system(paste("rm -rf ",cal_dir,sep=""))
  }
  return(out_all)
}

