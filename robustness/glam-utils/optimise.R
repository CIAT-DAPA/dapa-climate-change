#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014 #borrows from PhD script called "glam-optimise-functions.R" and from "calibrate.R"

##############################################################################################
####### function to calibrate GLAM (for as many grid cells as provided), each one individually
##############################################################################################

#this (second) function should
#1. use number of steps to choose the values to iterate
#2. optimise (i.e. find optimum value for parameter) by running all grid cells simultaneously
#3. return table of parameter values, RMSE value, and crop yield (obs and simulated)

#note: a first function deals with YGP calibration
#note: this function should is applicable only to the normal optimisation procedure

# #example:
# #---------------------------------------------------------------
# src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
# source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
# source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
# source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
# source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))
# source(paste(src.dir,"/glam-utils/calibrate.R",sep=""))
# 
# wd <- "~/Leeds-work/quest-for-robustness"
# runsDir <- paste(wd,"/crop_model_runs",sep="")
# calibDir <- paste(runsDir,"/ppe_optimisation",sep="")
# mdataDir <- paste(wd,"/data/model_data",sep="")
# metDir <- paste(wd,"/data/meteorology",sep="")
# binDir <- paste(wd,"/bin/glam-maize-osx",sep="")
# 
# #load objects
# load(paste(mdataDir,"/initial_conditions_major.RData",sep=""))
# load(paste(mdataDir,"/yield_major.RData",sep=""))
# 
# #arguments
# opt_data <- list()
# opt_data$CROP <- "maize"
# opt_data$MODEL <- "glam-maiz"
# opt_data$BASE_DIR <- calibDir
# opt_data$BIN_DIR <- binDir
# opt_data$PAR_DIR <- mdataDir
# opt_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw",sep="") #for reading .wth files
# opt_data$WTH_ROOT <- "obs_hist_WFD"
# opt_data$LOC <- c(680,681,682)
# opt_data$ISYR <- 1981
# opt_data$IEYR <- 2000
# opt_data$INI_COND <- xy_main
# opt_data$YLD_DATA <- xy_main_yield
# opt_data$PARAMS <- GLAM_get_default(opt_data$PAR_DIR)
# opt_data$SIM_NAME <- "optim1"
# opt_data$PARAM <- "TE"
# opt_data$SECT <- "glam_param.bmass"
# opt_data$NSTEPS <- 36
# opt_data$RUN_TYPE <- "RFD"
# opt_data$METHOD <- "RMSE"
# opt_data$USE_SCRATCH <- F
# opt_data$SCRATCH <- NA
# 
# #modify parameter value to avoid model failure
# opt_data$PARAMS$glam_param.maize$TLIMJUV$Value <- 280
# 
# paroptim <- GLAM_optimise(opt_data)
# #---------------------------------------------------------------

# plot(paroptim$OPTIMISATION$VALUE, paroptim$OPTIMISATION$RMSE, ty='l')

### note:
#simulate year before starting one because if sowing date is late then harvest is in this year
#last year cannot be last year of time series since model runs could fail due to late sowing
#see function GLAM_calibrate() in calibrate.R for details on how this is done.

#calibrate ygp
GLAM_optimise <- function(opt_data) {
  param <- toupper(opt_data$PARAM)
  sect <- tolower(opt_data$SECT)
  
  #put years into parameter set
  opt_data$PARAMS$glam_param.mod_mgt$ISYR <- opt_data$ISYR
  opt_data$PARAMS$glam_param.mod_mgt$IEYR <- opt_data$IEYR
  
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
  exec_name <- opt_data$MODEL
  
  #running command
  glam_cmd <- paste("./",exec_name,sep="")
  
  #output directories
  if (opt_data$USE_SCRATCH) {
    cal_dir <- opt_data$SCRATCH #calibration directory
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
  
  #create sequence of values
  if (param %in% c("SLA_INI","NDSLA")) {
    vals <- seq(opt_data$MINVAL,opt_data$MAXVAL,length.out=opt_data$NSTEPS)
  } else {
    vals <- seq(opt_data$PARAMS[[sect]][[param]][,"Min"],opt_data$PARAMS[[sect]][[param]][,"Max"],length.out=opt_data$NSTEPS)
  }
  
  #type of run
  opt_data$PARAMS$glam_param.mod_mgt$SEASON <- opt_data$RUN_TYPE
  
  #params config
  opt_data$PARAMS$glam_param.mod_mgt$IASCII <- 1 #output only to season file
  
  #file of output
  cal_outfile <- paste(opt_dir,"/opt-",opt_data$PARAM,".txt",sep="") #summary
  raw_outfile <- paste(opt_dir,"/opt-",opt_data$PARAM,"_raw.txt",sep="") #raw
  
  #do only if calibration file does not exist
  if (!file.exists(cal_outfile)) {
    #loop through sequence of values
    for (i in 1:length(vals)) {
      #i <- 1
      cat("\nperforming run ",opt_data$RUN_TYPE," ",i," value = ",vals[i]," (",param,")",sep="","\n")
      
      #assign values to parameter set
      if (param %in% c("SLA_INI","NDSLA")) {
        opt_data$PARAMS[[sect]][[param]] <- vals[i]
      } else {
        opt_data$PARAMS[[sect]][[param]][,"Value"] <- vals[i]
      }
      
      #calibrate model
      cal_data <- opt_data
      cal_data$BASE_DIR <- opt_dir
      cal_data$SIM_NAME <- paste("calibration_",cal_data$PARAM,"_run-",i,sep="")
      cal_data$NSTEPS <- 67
      ygp_calib <- GLAM_calibrate(cal_data)
      
      #yearly output
      yr_out <- ygp_calib$RAW_DATA
      
      #select optimal ygp of each grid cell
      ygp_all <- data.frame()
      for (loc in unique(yr_out$LOC)) {
        ygp_opt <- min(ygp_calib$CALIBRATION$RMSE[which(ygp_calib$CALIBRATION$LOC == loc)])
        ygp_opt <- ygp_calib$CALIBRATION$VALUE[which(ygp_calib$CALIBRATION$LOC == loc & ygp_calib$CALIBRATION$RMSE == ygp_opt)]
        if (length(ygp_opt) > 1) {ygp_opt <- max(ygp_opt)}
        ygp_loc <- yr_out[which(yr_out$LOC == loc & yr_out$VALUE == ygp_opt),]
        ygp_all <- rbind(ygp_all, ygp_loc)
      }
      
      #choose optimisation method (RMSE, CH07, CH10)
      if (opt_meth == "RMSE") {
        rmse <- sqrt(sum((ygp_all$OBS_ADJ-ygp_all$PRED_ADJ)^2,na.rm=T) / (length(which(!is.na(ygp_all$OBS_ADJ)))))
      } else if (opt_meth == "CH07") {
        #do for individual cells and add them up
        rmse <- 0
        for (loc in unique(ygp_all$LOC)) {
          rmse_loc <- ygp_all[which(ygp_all$LOC == loc),]
          rmse_loc <- (mean(rmse_loc$OBS_ADJ,na.rm=T)-mean(rmse_loc$PRED_ADJ,na.rm=T))^2 + (sd(rmse_loc$OBS,na.rm=T)-sd(rmse_loc$PRED,na.rm=T))^2
          rmse <- rmse + rmse_loc
        }
      } else if (opt_meth == "CH10") {
        rmse <- 0
        for (loc in unique(ygp_all$LOC)) {
          rmse_loc <- ygp_all[which(ygp_all$LOC == loc),]
          rmse_loc <- (mean(rmse_loc$OBS_ADJ,na.rm=T)-mean(rmse_loc$PRED_ADJ,na.rm=T))^2
          rmse <- rmse + rmse_loc
        }
        rmse <- sqrt(rmse/length(unique(ygp_all$LOC)))
      }
      out_row <- data.frame(VALUE=vals[i],RMSE=rmse,YOBS=mean(ygp_all$OBS,na.rm=T), YPRED=mean(ygp_all$PRED,na.rm=T),
                            YOBS_ADJ=mean(ygp_all$OBS_ADJ,na.rm=T), YPRED_ADJ=mean(ygp_all$PRED_ADJ,na.rm=T))
      names(ygp_all)[3] <- "YGP"
      ygp_all$VALUE <- vals[i]
      
      if (i == 1) {
        out_all <- out_row
        out_raw <- ygp_all
      } else {
        out_all <- rbind(out_all,out_row)
        out_raw <- rbind(out_raw,ygp_all)
      }
    }
    #write outputs for this grid cell
    write.table(out_all,sep="\t",quote=F,file=cal_outfile,row.names=F)
    write.table(out_raw,sep="\t",quote=F,file=raw_outfile,row.names=F)
  } else {
    out_all <- read.table(cal_outfile,sep="\t",header=T)
    out_raw <- read.table(raw_outfile,sep="\t",header=T)
  }
  
  #return object
  r_list <- list(OPTIMISATION=out_all, RAW_DATA=out_raw)
  return(r_list)
}
