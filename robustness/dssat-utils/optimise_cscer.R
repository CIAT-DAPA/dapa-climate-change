#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014 #borrows from PhD script called "glam-optimise-functions.R" and from "calibrate.R"

##############################################################################################
####### function to calibrate GLAM (for as many grid cells as provided), each one individually
##############################################################################################

#this (second) function should
#1. use number of steps to choose the values to iterate
#2. optimise (i.e. find optimum value for parameter) by running all grid cells simultaneously
#3. return table of parameter values, RMSE value, and crop yield (obs and simulated)

#note: a first function deals with SLPF calibration
#note: this function should be applicable only to the normal optimisation procedure

#example:
#---------------------------------------------------------------
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

wd <- "~/Leeds-work/quest-for-robustness"
runsDir <- paste(wd,"/crop_model_runs",sep="")
calibDir <- paste(runsDir,"/dssat_t1",sep="")
mdataDir <- paste(wd,"/data/model_data",sep="")
metDir <- paste(wd,"/data/meteorology",sep="")
binDir <- paste(wd,"/bin/dssat/csm45_1_23_bin_gfort",sep="")

#load objects
load(paste(mdataDir,"/initial_conditions_major_dssat.RData",sep=""))
load(paste(mdataDir,"/yield_major_dssat.RData",sep=""))

#read in parameter list
param_list <- read.table(paste(mdataDir,"/parameter_list_dssat.txt",sep=""),header=T,sep="\t")
param_list$DESCRIPTION <- NULL

#arguments
opt_data <- list()
opt_data$MODEL <- "MZCER045"
opt_data$BASENAME <- "AFRB" #basename of runs
opt_data$BASE_DIR <- calibDir
opt_data$BIN_DIR <- binDir
opt_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw",sep="") #for reading .wth files
opt_data$WTH_ROOT <- "obs_hist_WFD"
opt_data$LOC <- c(680,681,682)
opt_data$ISYR <- 1980 #1 year before GLAM's because of spin-up year needs in CSM
opt_data$IEYR <- 2001 #1 extra year so as to include in wth file (but this year won't be run)
opt_data$INI_COND <- xy_main
opt_data$YLD_DATA <- xy_main_yield
opt_data$CUL <- data.frame(P1=140,P2=0.3,P5=685,G2=907.9,G3=10.5,PHINT=38.9) #default for missing ones
opt_data$ECO <- data.frame(DSGFT=170,RUE=4.2,KCAN=0.85,TSEN=6.0,CDAY=15.0)
opt_data$SPE <- get_spepar(paste(opt_data$BIN_DIR,"/MZCER045.SPE",sep=""))
opt_data$SIM_NAME <- "optim_01"
opt_data$PARAM <- "PARSR"
opt_data$VALS <- seq(0.4,0.6,length.out=9)
opt_data$IFILE <- "SPE"
opt_data$SECT <- "photo_param"
opt_data$NSTEPS <- 9
opt_data$METHOD <- "RMSE"
opt_data$USE_SCRATCH <- F
opt_data$SCRATCH <- NA

#paroptim <- DSSAT_optimise(opt_data)
#paroptim <- DSSAT_optimise_parallel(opt_data)
#---------------------------------------------------------------

# plot(paroptim$OPTIMISATION$VALUE, paroptim$OPTIMISATION$RMSE, ty='l')

### note:
#simulate year before starting one because if sowing date is late then harvest is in this year
#last year cannot be last year of time series since model runs could fail due to late sowing
#see function DSSAT_calibrate() in calibrate.R for details on how this is done.
### end.

#optimise given parameter
CSCER_optimise <- function(opt_data) {
  param <- toupper(opt_data$PARAM)
  ifile <- toupper(opt_data$IFILE)
  sect <- tolower(opt_data$SECT)
  
  #here is the optimisation method
  #RMSE: is yearly root mean square error (classical)
  #CH07: is the MSE method proposed in Challinor et al. (2007) AGEE, that optimises based on
  #      the differences between mean and standard deviations of the simulated time series
  #CH10: is the MSE method proposed in Challinor et al. (2010) ERL, that optimises based
  #      on the difference between mean yields only. I guess this method is only valid when
  #      an insufficiently large observed yield + weather time series is available.
  if (is.null(opt_data$METHOD)) {
    opt_meth <- "RMSE" #defaulting to RMSE if missing in input list
  } else {
    opt_meth <- toupper(opt_data$METHOD)
  }
  
  if (!opt_meth %in% c("RMSE","CH07","CH10")) {
    opt_meth <- "RMSE" #defaulting the RMSE
  }
  
  #input directories and model
  exec_name <- "DSCSM045.EXE"
  
  #output directories
  if (opt_data$USE_SCRATCH) {
    cal_dir <- opt_data$SCRATCH #optimisation directory
    nfs_dir <- paste(opt_data$BASE_DIR,"/",opt_data$SIM_NAME,sep="")
    if (!file.exists(nfs_dir)) {dir.create(nfs_dir,recursive=T)}
  } else {
    cal_dir <- opt_data$BASE_DIR #calibration directory
  }
  if (!file.exists(cal_dir)) {dir.create(cal_dir,recursive=T)}
  
  cal_dir <- paste(cal_dir,"/",opt_data$SIM_NAME,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #create optimisation folder if it does not exist
  opt_dir <- paste(cal_dir,"/",tolower(param),sep="")
  if (!file.exists(opt_dir)) {dir.create(opt_dir)}
  
  #move model and weather files to scratch
  if (opt_data$USE_SCRATCH) {
    #copy bin
    nbin_dir <- paste(cal_dir,"/glam_bin",sep="")
    if (!file.exists(nbin_dir)) {dir.create(nbin_dir)}
    system(paste("cp -f ",opt_data$BIN_DIR,"/",exec_name," ",nbin_dir,"/.",sep=""))
    opt_data$BIN_DIR <- nbin_dir
    
    #copy weather files
    nwth_dir <- paste(cal_dir,"/weather",sep="")
    if (!file.exists(nwth_dir)) {dir.create(nwth_dir)}
    system(paste("cp -rf ",opt_data$WTH_DIR,"/. ",nwth_dir,sep=""))
    opt_data$WTH_DIR <- nwth_dir
  }
  
  #create sequence of values
  vals <- opt_data$VALS
  
  #file of output
  if (opt_data$USE_SCRATCH) {
    save_file <- paste(nfs_dir,"/opt-",opt_data$PARAM,".RData",sep="")
  } else {
    save_file <- paste(cal_dir,"/opt-",opt_data$PARAM,".RData",sep="")
  }
  
  #do only if calibration file does not exist
  if (!file.exists(save_file)) {
    #loop through sequence of values
    for (i in 1:length(vals)) {
      #i <- 1
      cat("\nperforming run ",i," value = ",vals[i]," (",param,")",sep="","\n")
      
      #assign values to relevant parameter set
      #see below relevant info:
      #opt_data[["SPE"]][["photo_param"]][["PARSR"]]
      #opt_data[["SPE"]][["seed_growth"]][which(gsub(" ","",opt_data[["SPE"]][["seed_growth"]][["PARAM"]]) == "SDSZ"),"VALUE"]
      #opt_data[["ECO"]][["KCAN"]]
      #opt_data[["CUL"]][["P1"]]
      #in_data <- get_xfile_dummy()
      #in_data[["planting"]][["PPOP"]]
      #in_data[["auto_mgmt"]][["PH2OL"]]
      if (ifile == "SPE") {
        if (param %in% c("SDSZ","RSGRT")) {
          opt_data[[ifile]][[sect]][which(gsub(" ","",opt_data[[ifile]][[sect]][["PARAM"]]) == param),"VALUE"] <- vals[i]
        } else {
          opt_data[[ifile]][[sect]][[param]] <- vals[i]
        }
      } else if (ifile == "ECO" | ifile == "CUL") {
        opt_data[[ifile]][[param]] <- vals[i]
      } else if (ifile == "XFILE") {
        opt_data$PARAM_VALUE <- vals[i]
      } else {
        stop("invalid ifile, check opt_data$IFILE")
      }
      
      #calibrate model
      cal_data <- opt_data
      if (opt_data$USE_SCRATCH) {
        cal_data$SCRATCH <- opt_dir
        cal_data$BASE_DIR <- nfs_dir
      } else {
        cal_data$BASE_DIR <- opt_dir
      }
      cal_data$SIM_NAME <- paste("calibration_",cal_data$PARAM,"_run-",i,sep="")
      slpf_calib <- DSSAT_calibrate(cal_data)
      
      #yearly output
      yr_out <- slpf_calib$RAW_DATA
      
      #select optimal ygp of each grid cell
      slpf_all <- data.frame()
      for (loc in unique(yr_out$LOC)) {
        #loc <- unique(yr_out$LOC)[1]
        slpf_opt <- min(slpf_calib$CALIBRATION$RMSE[which(slpf_calib$CALIBRATION$LOC == loc)])
        slpf_opt <- slpf_calib$CALIBRATION$VALUE[which(slpf_calib$CALIBRATION$LOC == loc & slpf_calib$CALIBRATION$RMSE == slpf_opt)]
        if (length(slpf_opt) > 1) {slpf_opt <- max(slpf_opt)}
        slpf_loc <- yr_out[which(yr_out$LOC == loc & yr_out$VALUE == slpf_opt),]
        slpf_all <- rbind(slpf_all, slpf_loc)
      }
      
      #choose optimisation method (RMSE, CH07, CH10)
      if (opt_meth == "RMSE") {
        rmse <- sqrt(sum((slpf_all$OBS_ADJ-slpf_all$PRED_ADJ)^2,na.rm=T) / (length(which(!is.na(slpf_all$OBS_ADJ)))))
      } else if (opt_meth == "CH07") {
        #do for individual cells and add them up
        rmse <- 0
        for (loc in unique(slpf_all$LOC)) {
          rmse_loc <- slpf_all[which(slpf_all$LOC == loc),]
          rmse_loc <- (mean(rmse_loc$OBS_ADJ,na.rm=T)-mean(rmse_loc$PRED_ADJ,na.rm=T))^2 + (sd(rmse_loc$OBS,na.rm=T)-sd(rmse_loc$PRED,na.rm=T))^2
          rmse <- rmse + rmse_loc
        }
      } else if (opt_meth == "CH10") {
        rmse <- 0
        for (loc in unique(slpf_all$LOC)) {
          rmse_loc <- slpf_all[which(slpf_all$LOC == loc),]
          rmse_loc <- (mean(rmse_loc$OBS_ADJ,na.rm=T)-mean(rmse_loc$PRED_ADJ,na.rm=T))^2
          rmse <- rmse + rmse_loc
        }
        rmse <- sqrt(rmse/length(unique(slpf_all$LOC)))
      }
      out_row <- data.frame(VALUE=vals[i],RMSE=rmse,YOBS=mean(slpf_all$OBS,na.rm=T), YPRED=mean(slpf_all$PRED,na.rm=T),
                            YOBS_ADJ=mean(slpf_all$OBS_ADJ,na.rm=T), YPRED_ADJ=mean(slpf_all$PRED_ADJ,na.rm=T))
      names(slpf_all)[4] <- "SLPF"
      slpf_all$VALUE <- vals[i]
      
      #for final object
      names(yr_out)[4] <- "SLPF"; yr_out$VALUE <- vals[i]
      this_cal <- slpf_calib$CALIBRATION; names(this_cal)[2] <- "SLPF"; this_cal$VALUE <- vals[i]
      
      if (i == 1) {
        out_all <- out_row
        out_raw <- slpf_all
        cal_all <- this_cal
        cal_raw <- yr_out
      } else {
        out_all <- rbind(out_all,out_row)
        out_raw <- rbind(out_raw,slpf_all)
        cal_all <- rbind(cal_all,this_cal)
        cal_raw <- rbind(cal_raw,yr_out)
      }
    }
    
    #return object
    r_list <- list(OPTIMISATION=out_all, RAW_OPTIMISATION=out_raw, 
                   CALIBRATION=cal_all, RAW_CALIBRATION=cal_raw)
    
    #save file
    save(list=c("r_list"),file=save_file)
  } else {
    load(file=save_file)
  }
  
  #clean up
  if (opt_data$USE_SCRATCH) {
    system(paste("rm -rf ",cal_dir,sep=""))
  } else {
    system(paste("rm -rf ",opt_dir,sep=""))
  }
  
  return(r_list)
}


#################################################################################
#################################################################################
#optimise given parameter in parallel
CSCER_optimise_parallel <- function(opt_data) {
  param <- toupper(opt_data$PARAM)
  ifile <- toupper(opt_data$IFILE)
  sect <- tolower(opt_data$SECT)
  
  #here is the optimisation method
  #RMSE: is yearly root mean square error (classical)
  #CH07: is the MSE method proposed in Challinor et al. (2007) AGEE, that optimises based on
  #      the differences between mean and standard deviations of the simulated time series
  #CH10: is the MSE method proposed in Challinor et al. (2010) ERL, that optimises based
  #      on the difference between mean yields only. I guess this method is only valid when
  #      an insufficiently large observed yield + weather time series is available.
  if (is.null(opt_data$METHOD)) {
    opt_meth <- "RMSE" #defaulting to RMSE if missing in input list
  } else {
    opt_meth <- toupper(opt_data$METHOD)
  }
  
  if (!opt_meth %in% c("RMSE","CH07","CH10")) {
    opt_meth <- "RMSE" #defaulting the RMSE
  }
  
  #input directories and model
  exec_name <- "DSCSM045.EXE"
  
  #output directories
  if (opt_data$USE_SCRATCH) {
    cal_dir <- opt_data$SCRATCH #optimisation directory
    nfs_dir <- paste(opt_data$BASE_DIR,"/",opt_data$SIM_NAME,sep="")
    if (!file.exists(nfs_dir)) {dir.create(nfs_dir,recursive=T)}
  } else {
    cal_dir <- opt_data$BASE_DIR #calibration directory
  }
  if (!file.exists(cal_dir)) {dir.create(cal_dir,recursive=T)}
  
  cal_dir <- paste(cal_dir,"/",opt_data$SIM_NAME,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #create optimisation folder if it does not exist
  opt_dir <- paste(cal_dir,"/",tolower(param),sep="")
  if (!file.exists(opt_dir)) {dir.create(opt_dir)}
  
  #move model and weather files to scratch
  if (opt_data$USE_SCRATCH) {
    #copy bin
    nbin_dir <- paste(cal_dir,"/glam_bin",sep="")
    if (!file.exists(nbin_dir)) {dir.create(nbin_dir)}
    system(paste("cp -f ",opt_data$BIN_DIR,"/",exec_name," ",nbin_dir,"/.",sep=""))
    opt_data$BIN_DIR <- nbin_dir
    
    #copy weather files
    nwth_dir <- paste(cal_dir,"/weather",sep="")
    if (!file.exists(nwth_dir)) {dir.create(nwth_dir)}
    system(paste("cp -rf ",opt_data$WTH_DIR,"/. ",nwth_dir,sep=""))
    opt_data$WTH_DIR <- nwth_dir
  }
  
  #create sequence of values
  vals <- opt_data$VALS
  
  #file of output
  if (opt_data$USE_SCRATCH) {
    save_file <- paste(nfs_dir,"/opt-",opt_data$PARAM,".RData",sep="")
  } else {
    save_file <- paste(cal_dir,"/opt-",opt_data$PARAM,".RData",sep="")
  }
  
  #do only if calibration file does not exist
  if (!file.exists(save_file)) {
    #calibration value wrapper function
    calib_value <- function(i) {
      #i <- 1
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
      source(paste(src.dir,"/dssat-utils/optimise_cscer.R",sep=""))
      
      #assign values to relevant parameter set
      #see below relevant info:
      #opt_data[["SPE"]][["photo_param"]][["PARSR"]]
      #opt_data[["SPE"]][["seed_growth"]][which(gsub(" ","",opt_data[["SPE"]][["seed_growth"]][["PARAM"]]) == "SDSZ"),"VALUE"]
      #opt_data[["ECO"]][["KCAN"]]
      #opt_data[["CUL"]][["P1"]]
      #in_data <- get_xfile_dummy()
      #in_data[["planting"]][["PPOP"]]
      #in_data[["auto_mgmt"]][["PH2OL"]]
      if (ifile == "SPE") {
        if (param %in% c("SDSZ","RSGRT")) {
          opt_data[[ifile]][[sect]][which(gsub(" ","",opt_data[[ifile]][[sect]][["PARAM"]]) == param),"VALUE"] <- vals[i]
        } else {
          opt_data[[ifile]][[sect]][[param]] <- vals[i]
        }
      } else if (ifile == "ECO" | ifile == "CUL") {
        opt_data[[ifile]][[param]] <- vals[i]
      } else if (ifile == "XFILE") {
        opt_data$PARAM_VALUE <- vals[i]
      } else {
        stop("invalid ifile, check opt_data$IFILE")
      }
      
      #calibrate model
      cal_data <- opt_data
      if (opt_data$USE_SCRATCH) {
        cal_data$SCRATCH <- opt_dir
        cal_data$BASE_DIR <- nfs_dir
      } else {
        cal_data$BASE_DIR <- opt_dir
      }
      cal_data$SIM_NAME <- paste("calibration_",cal_data$PARAM,"_run-",i,sep="")
      slpf_calib <- DSSAT_calibrate(cal_data)
      
      #yearly output
      yr_out <- slpf_calib$RAW_DATA
      
      #select optimal ygp of each grid cell
      slpf_all <- data.frame()
      for (loc in unique(yr_out$LOC)) {
        #loc <- unique(yr_out$LOC)[1]
        slpf_opt <- min(slpf_calib$CALIBRATION$RMSE[which(slpf_calib$CALIBRATION$LOC == loc)])
        slpf_opt <- slpf_calib$CALIBRATION$VALUE[which(slpf_calib$CALIBRATION$LOC == loc & slpf_calib$CALIBRATION$RMSE == slpf_opt)]
        if (length(slpf_opt) > 1) {slpf_opt <- max(slpf_opt)}
        slpf_loc <- yr_out[which(yr_out$LOC == loc & yr_out$VALUE == slpf_opt),]
        slpf_all <- rbind(slpf_all, slpf_loc)
      }
      
      #choose optimisation method (RMSE, CH07, CH10)
      if (opt_meth == "RMSE") {
        rmse <- sqrt(sum((slpf_all$OBS_ADJ-slpf_all$PRED_ADJ)^2,na.rm=T) / (length(which(!is.na(slpf_all$OBS_ADJ)))))
      } else if (opt_meth == "CH07") {
        #do for individual cells and add them up
        rmse <- 0
        for (loc in unique(slpf_all$LOC)) {
          rmse_loc <- slpf_all[which(slpf_all$LOC == loc),]
          rmse_loc <- (mean(rmse_loc$OBS_ADJ,na.rm=T)-mean(rmse_loc$PRED_ADJ,na.rm=T))^2 + (sd(rmse_loc$OBS,na.rm=T)-sd(rmse_loc$PRED,na.rm=T))^2
          rmse <- rmse + rmse_loc
        }
      } else if (opt_meth == "CH10") {
        rmse <- 0
        for (loc in unique(slpf_all$LOC)) {
          rmse_loc <- slpf_all[which(slpf_all$LOC == loc),]
          rmse_loc <- (mean(rmse_loc$OBS_ADJ,na.rm=T)-mean(rmse_loc$PRED_ADJ,na.rm=T))^2
          rmse <- rmse + rmse_loc
        }
        rmse <- sqrt(rmse/length(unique(slpf_all$LOC)))
      }
      out_row <- data.frame(VALUE=vals[i],RMSE=rmse,YOBS=mean(slpf_all$OBS,na.rm=T), YPRED=mean(slpf_all$PRED,na.rm=T),
                            YOBS_ADJ=mean(slpf_all$OBS_ADJ,na.rm=T), YPRED_ADJ=mean(slpf_all$PRED_ADJ,na.rm=T))
      names(slpf_all)[4] <- "SLPF"
      slpf_all$VALUE <- vals[i]
      
      #for final object
      names(yr_out)[4] <- "SLPF"; yr_out$VALUE <- vals[i]
      this_cal <- slpf_calib$CALIBRATION; names(this_cal)[2] <- "SLPF"; this_cal$VALUE <- vals[i]
      
      #return object
      ret_obj <- list(out_row,slpf_all,this_cal,yr_out)
      return(ret_obj)
    }
    
    #initiate workers
    sfInit(parallel=T,cpus=opt_data$NPROC)
    
    #export variables
    sfExport("opt_data")
    sfExport("opt_dir")
    sfExport("nfs_dir")
    sfExport("sect")
    sfExport("ifile")
    sfExport("param")
    sfExport("vals")
    sfExport("opt_meth")
    sfExport("src.dir")
    
    #run the function in parallel
    calib_output <- sfSapply(as.vector(1:length(vals)),calib_value)
    
    #stop the cluster
    sfStop()
    
    #loop to organise output
    for (i in 1:length(vals)) {
      i_s <- i*4-3
      out_row <- calib_output[[i_s]]
      slpf_all <- calib_output[[i_s+1]]
      this_cal <- calib_output[[i_s+2]]
      yr_out <- calib_output[[i_s+3]]
      
      if (i == 1) {
        out_all <- out_row
        out_raw <- slpf_all
        cal_all <- this_cal
        cal_raw <- yr_out
      } else {
        out_all <- rbind(out_all,out_row)
        out_raw <- rbind(out_raw,slpf_all)
        cal_all <- rbind(cal_all,this_cal)
        cal_raw <- rbind(cal_raw,yr_out)
      }
    }
    
    #return object
    r_list <- list(OPTIMISATION=out_all, RAW_OPTIMISATION=out_raw, 
                   CALIBRATION=cal_all, RAW_CALIBRATION=cal_raw)
    
    #save file
    save(list=c("r_list"),file=save_file)
  } else {
    load(file=save_file)
  }
  
  #clean up
  if (opt_data$USE_SCRATCH) {
    system(paste("rm -rf ",cal_dir,sep=""))
  } else {
    system(paste("rm -rf ",opt_dir,sep=""))
  }
  
  return(r_list)
}

