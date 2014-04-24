#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2014 #borrows from PhD script called "glam-optimise-functions.R"

#############################################################################################
####### function to calibrate GLAM (for as many grid cells as provided)
#############################################################################################

#this (first) function should
#1. use number of steps to choose the values to iterate
#2. calibrate (i.e. find optimum YGP) for each grid cell individually
#3. return table of grid cell * ygp values, RMSE value, and crop yield

#note: a second function will deal with optimisation of parameters

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
# #opt_data$PARAM <- "YGP"
# #opt_data$SECT <- "glam_param.ygp"
# opt_data$NSTEPS <- 100
# opt_data$RUN_TYPE <- "RFD"
# opt_data$METHOD <- "RMSE"
# opt_data$USE_SCRATCH <- F
# opt_data$SCRATCH <- NA
# 
# #modify parameter value to avoid model failure
# opt_data$PARAMS$glam_param.maize$TLIMJUV$Value <- 280
# 
# ygpcalib <- GLAM_calibrate(opt_data)
# #---------------------------------------------------------------


### note:
#simulate year before starting one because if sowing date is late then harvest is in this year
#last year cannot be last year of time series since model runs could fail due to late sowing

#calibrate ygp
GLAM_calibrate <- function(opt_data) {
  param <- "YGP" #toupper(opt_data$PARAM)
  sect <- "glam_param.ygp" #tolower(opt_data$SECT)
  params <- opt_data$PARAMS
  
  #put years into parameter set
  params$glam_param.mod_mgt$ISYR <- opt_data$ISYR
  params$glam_param.mod_mgt$IEYR <- opt_data$IEYR
  
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
  vals <- seq(params[[sect]][[param]][,"Min"],params[[sect]][[param]][,"Max"],length.out=opt_data$NSTEPS)
  
  #type of run
  params$glam_param.mod_mgt$SEASON <- opt_data$RUN_TYPE
  
  #params config
  params$glam_param.mod_mgt$IASCII <- 1 #output only to season file
  
  #loop through desired locations
  for (loc in opt_data$LOC) {
    #loc <- opt_data$LOC[1]
    cat("\n...loc= ",loc,sep="","\n")
    
    #sowing window
    sow_date1 <- opt_data$INI_COND$SOW_DATE1[which(opt_data$INI_COND$LOC == loc)]
    sow_date2 <- opt_data$INI_COND$SOW_DATE2[which(opt_data$INI_COND$LOC == loc)]
    sow_window <- sow_date1 - sow_date2
    params$glam_param.mod_mgt$ISDAY$Value <- sow_window
    
    #prepare input object
    run_data <- list()
    run_data$CROP <- opt_data$CROP
    run_data$MODEL <- opt_data$MODEL
    run_data$BASE_DIR <- opt_dir
    run_data$BIN_DIR <- opt_data$BIN_DIR
    run_data$PAR_DIR <- NA
    run_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw/",opt_data$WTH_ROOT,sep="") #to be specified
    run_data$LOC <- loc
    run_data$LON <- opt_data$INI_COND$x[which(opt_data$INI_COND$LOC == loc)]
    run_data$LAT <- opt_data$INI_COND$y[which(opt_data$INI_COND$LOC == loc)]
    run_data$ME <- opt_data$INI_COND$ME[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$SOW_DATE <- opt_data$INI_COND$SOW_DATE1[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$RLL <- opt_data$INI_COND$RLL[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$DUL <- opt_data$INI_COND$DUL[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$SAT <- opt_data$INI_COND$SAT[which(opt_data$INI_COND$LOC == run_data$LOC)]
    run_data$ISYR <- opt_data$ISYR
    run_data$IEYR <- opt_data$IEYR
    run_data$PARAMS <- params
    
    #file of output
    cal_outfile <- paste(opt_dir,"/cal-",run_data$LOC,".txt",sep="")
    
    if (!file.exists(cal_outfile)) {
      #loop through sequence of values
      for (i in 1:length(vals)) {
        #i <- 1
        cat("performing run ",opt_data$RUN_TYPE," ",i," value = ",vals[i]," (",param,")",sep="","\n")
        
        #run id
        run_data$RUN_ID <- paste("run-",i,"_val-",vals[i],"_loc-",run_data$LOC,sep="")
        
        #assign values to parameter set
        run_data$PARAMS[[sect]][[param]][,"Value"] <- vals[i]
        
        #check whether the *.out already exists
        outfile <- list.files(paste(run_data$BASE_DIR,"/",run_data$RUN_ID,"/output",sep=""),pattern="\\.out")
        if (length(outfile) == 0) {
          #run the model
          run_data <- run_glam(run_data)
        } else {
          run_data$SEAS_FILES <- outfile
          run_data$RUN_DIR <- paste(run_data$BASE_DIR,"/",run_data$RUN_ID,sep="")
        }
        
        #read in the predicted yield
        if (length(run_data$SEAS_FILES) == 1 | length(outfile) == 1) {
          pred <- read.table(paste(run_data$RUN_DIR,"/output/",run_data$SEAS_FILES,sep=""),header=F,sep="\t")
          names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                           "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                           "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                           "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                           "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT",
                           "IPLANT","LETHAL_YIELD","LETHAL_HI","LETHAL_BMASS","LETHAL_BMASS","LETHAL_DAP",
                           "SWFAC_TOT","SWFAC_MEAN","SWFAC_COUNT")
          y_p <- pred$YIELD #predicted yield
          
          #grab observed yield
          y_o <- as.data.frame(t(opt_data$YLD_DATA[which(opt_data$YLD_DATA$x == run_data$LON & opt_data$YLD_DATA$y == run_data$LAT),3:ncol(opt_data$YLD_DATA)]))
          y_o <- cbind(YEAR=1982:2005,y_o)
          names(y_o)[2] <- "YIELD"
          row.names(y_o) <- 1:nrow(y_o)
          y_o <- y_o[which(y_o$YEAR >= opt_data$ISYR & y_o$YEAR <= opt_data$IEYR),]
          y_o <- y_o$YIELD
          
          #calc rmse, depending on which year the crop was actually harvested
          har_date <- mean(pred$PLANTING_DATE + pred$DUR) #get harvest date first
          if (har_date<365) {y_p <- y_p[2:length(y_p)]} else {y_p <- y_p[1:(length(y_p)-1)]}
          odf <- data.frame(YEAR=(opt_data$ISYR+1):opt_data$IEYR,VALUE=vals[i],OBS=y_o,PRED=y_p)
          
          if (opt_meth == "RMSE") {
            rmse <- sqrt(sum((odf$OBS-odf$PRED)^2,na.rm=T) / (length(which(!is.na(odf$OBS)))))
          } else if (opt_meth == "CH07") {
            rmse <- (mean(odf$OBS,na.rm=T)-mean(odf$PRED,na.rm=T))^2 + (sd(odf$OBS,na.rm=T)-sd(odf$PRED,na.rm=T))^2
          } else if (opt_meth == "CH10") {
            rmse <- (mean(odf$OBS,na.rm=T)-mean(odf$PRED,na.rm=T))^2
          }
          
          #remove junk
          system(paste("rm -rf ",run_data$RUN_DIR,sep=""))
        } else {
          rmse <- NA
        }
        out_row <- data.frame(VALUE=vals[i],RMSE=rmse,YOBS=mean(odf$OBS,na.rm=T), YPRED=mean(odf$PRED,na.rm=T))
        
        if (i == 1) {
          out_all <- out_row
          raw_all <- odf
        } else {
          out_all <- rbind(out_all,out_row)
          raw_all <- rbind(raw_all, odf)
        }
      }
      write.table(out_all,sep="\t",quote=F,file=cal_outfile,row.names=F)
    } else {
      out_all <- read.table(cal_outfile,sep="\t",header=T)
    }
    
    #append location data
    out_all <- cbind(LOC=loc,out_all)
    if (loc == opt_data$LOC[1]) {
      cal_all <- out_all
    } else {
      cal_all <- rbind(cal_all, out_all)
    }
  }
  
  #remove junk from scratch
  if (opt_data$USE_SCRATCH) {
    system(paste("cp -rf ",cal_dir," ",paste(nfs_dir,"/.",sep=""),sep=""))
    system(paste("rm -rf ",cal_dir,sep=""))
  }
  
  #return object
  return(cal_all)
}
