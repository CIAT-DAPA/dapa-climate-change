#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2014 #borrows from PhD script called "glam-optimise-functions.R"

##############################################################################################
####### function to calibrate GLAM (for as many grid cells as provided), each one individually
##############################################################################################

#this (first) function should
#1. use number of steps to choose the values to iterate
#2. calibrate (i.e. find optimum YGP) for each grid cell individually
#3. return table of grid cell * ygp values, RMSE value, and crop yield

#note: a second function will deal with optimisation of parameters
#note: this function should be applicable to both the hypercube and the normal 
#      optimisation procedure

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
# cal_data <- list()
# cal_data$CROP <- "maize"
# cal_data$MODEL <- "glam-maiz"
# cal_data$BASE_DIR <- calibDir
# cal_data$BIN_DIR <- binDir
# cal_data$PAR_DIR <- mdataDir
# cal_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw",sep="") #for reading .wth files
# cal_data$WTH_ROOT <- "obs_hist_WFD"
# cal_data$LOC <- c(680,681,682)
# cal_data$ISYR <- 1981
# cal_data$IEYR <- 2000
# cal_data$INI_COND <- xy_main
# cal_data$YLD_DATA <- xy_main_yield
# cal_data$PARAMS <- GLAM_get_default(cal_data$PAR_DIR)
# cal_data$SIM_NAME <- "optim1"
# cal_data$NSTEPS <- 100
# cal_data$RUN_TYPE <- "RFD"
# cal_data$METHOD <- "RMSE"
# cal_data$USE_SCRATCH <- F
# cal_data$SCRATCH <- NA
# 
# #modify parameter value to avoid model failure
# cal_data$PARAMS$glam_param.maize$TLIMJUV$Value <- 280
# 
# ygpcalib <- GLAM_calibrate(cal_data)
# #---------------------------------------------------------------

# #plotting some of the results
# xx <- ygpcalib$RAW_DATA[which(ygpcalib$RAW_DATA$VALUE==0.87 & ygpcalib$RAW_DATA$LOC==680),]
# plot(xx$YEAR,xx$OBS_ADJ,ty="l",ylim=c(0,1200))
# lines(xx$YEAR,xx$PRED_ADJ,col="red")
# 
# yy <- ygpcalib$CALIBRATION[which(ygpcalib$CALIBRATION$LOC==680),]
# plot(yy$VALUE, yy$RMSE/yy$YOBS_ADJ*100, ty='l',ylim=c(0,100))

### note:
#simulate year before starting one because if sowing date is late then harvest is in this year
#last year cannot be last year of time series since model runs could fail due to late sowing

### further notes:
#** due to issues in Sahel, multiple plantings were considered: 10 plantings between dates of Sacks
#** due to issues in Sahel, SAT*1.15 and SAT*1.30 were also simulated (soil storage was too poor)

#calibrate ygp
GLAM_calibrate <- function(cal_data) {
  param <- "YGP" #toupper(cal_data$PARAM)
  sect <- "glam_param.ygp" #tolower(cal_data$SECT)
  params <- cal_data$PARAMS
  
  #put years into parameter set
  params$glam_param.mod_mgt$ISYR <- cal_data$ISYR
  params$glam_param.mod_mgt$IEYR <- cal_data$IEYR
  
  #here is the optimisation method
  #RMSE: is yearly root mean square error (classical)
  #CH07: is the MSE method proposed in Challinor et al. (2007) AGEE, that optimises based on
  #      the differences between mean and standard deviations of the simulated time series
  #CH10: is the MSE method proposed in Challinor et al. (2010) ERL, that optimises based
  #      on the difference between mean yields only. I guess this method is only valid when
  #      an insufficiently large observed yield + weather time series is available.
  if (is.null(cal_data$METHOD)) {
    opt_meth <- "RMSE" #defaulting to RMSE if missing in input list
  } else {
    opt_meth <- toupper(cal_data$METHOD)
  }
  
  if (!opt_meth %in% c("RMSE","CH07","CH10")) {
    opt_meth <- "RMSE" #defaulting the RMSE
  }
  
  #input directories and model
  exec_name <- cal_data$MODEL
  
  #running command
  glam_cmd <- paste("./",exec_name,sep="")
  
  #output directories
  if (cal_data$USE_SCRATCH) {
    cal_dir <- cal_data$SCRATCH #calibration directory
    #nfs_dir <- paste(cal_data$BASE_DIR,"/",cal_data$SIM_NAME,sep="")
    #if (!file.exists(nfs_dir)) {dir.create(nfs_dir,recursive=T)}
  } else {
    cal_dir <- cal_data$BASE_DIR #calibration directory
  }
  if (!file.exists(cal_dir)) {dir.create(cal_dir,recursive=T)}
  
  cal_dir <- paste(cal_dir,"/",cal_data$SIM_NAME,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #create optimisation folder if it does not exist
  opt_dir <- paste(cal_dir,"/",tolower(param),sep="")
  if (!file.exists(opt_dir)) {dir.create(opt_dir)}
  
  #create sequence of values
  vals <- seq(params[[sect]][[param]][,"Min"],params[[sect]][[param]][,"Max"],length.out=cal_data$NSTEPS)
  
  #loop through desired locations
  for (loc in cal_data$LOC) {
    #loc <- cal_data$LOC[1]
    cat("\n...loc= ",loc,sep="","\n")
    
    #sowing window
    sow_date1 <- cal_data$INI_COND$SOW_DATE1[which(cal_data$INI_COND$LOC == loc)]
    sow_date2 <- cal_data$INI_COND$SOW_DATE2[which(cal_data$INI_COND$LOC == loc)]
    #sow_window <- sow_date1 - sow_date2 #no need due to multiple planting
    params$glam_param.mod_mgt$ISDAY$Value <- -30 #min(c(sow_window,-30)) #set to -30 as multiple planting
    
    #data.frame of iterative soil*sowing date trials
    sow_seq <- round(seq(sow_date1, sow_date2, length.out=10), 0)
    sol_seq <- c(1,1.2,1.4,1.6,1.8)
    run_df <- expand.grid(sow=sow_seq, sol=sol_seq)
    
    #prepare input object
    run_data <- list()
    run_data$CROP <- cal_data$CROP
    run_data$MODEL <- cal_data$MODEL
    run_data$BASE_DIR <- opt_dir
    run_data$BIN_DIR <- cal_data$BIN_DIR
    run_data$PAR_DIR <- NA
    run_data$WTH_DIR <- paste(cal_data$WTH_DIR,"/",cal_data$WTH_ROOT,sep="") #to be specified
    run_data$LOC <- loc
    run_data$LON <- cal_data$INI_COND$x[which(cal_data$INI_COND$LOC == loc)]
    run_data$LAT <- cal_data$INI_COND$y[which(cal_data$INI_COND$LOC == loc)]
    run_data$ME <- cal_data$INI_COND$ME[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$SOW_DATE <- cal_data$INI_COND$SOW_DATE1[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$RLL <- cal_data$INI_COND$RLL[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$DUL <- cal_data$INI_COND$DUL[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$SAT <- NA #cal_data$INI_COND$SAT[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$ISYR <- cal_data$ISYR
    run_data$IEYR <- cal_data$IEYR
    run_data$PARAMS <- params
    
    ##file of output
    #cal_outfile <- paste(opt_dir,"/cal-",run_data$LOC,".txt",sep="") #summary
    #raw_outfile <- paste(opt_dir,"/cal-",run_data$LOC,"_raw.txt",sep="") #raw
    
    #if (!file.exists(cal_outfile)) {
      #loop through sequence of values
      for (i in 1:length(vals)) {
        #i <- 1
        cat("performing ygp calibration run ",cal_data$RUN_TYPE," ",i," value = ",vals[i],sep="","\n")
        
        #run id
        run_data$RUN_ID <- paste("run-",i,"_val-",vals[i],"_loc-",run_data$LOC,sep="")
        
        #assign values to parameter set
        run_data$PARAMS[[sect]][[param]][,"Value"] <- vals[i]
        
        #run all sow*sol options for this YGP value and location
        pred_all <- data.frame()
        for (k in 1:nrow(run_df)) {
          #k <- 1
          #get sow date and SAT multiplier
          sow_date <- run_df$sow[k]
          run_data$SAT <- cal_data$INI_COND$SAT[which(cal_data$INI_COND$LOC == run_data$LOC)] * run_df$sol[k]
          
          #run the model from scratch if k == 1, otherwise just go to dir, run and grab
          #check whether the *.out already exists
          outfile <- list.files(paste(run_data$BASE_DIR,"/",run_data$RUN_ID,"/output",sep=""),pattern="\\.out")
          if (length(outfile) == 0) {
            if (k == 1) {
              run_data <- run_glam(run_data)
            } else {
              #if (cal_data$USE_SCRATCH) {}
              solfil <- make_soilcodes(outfile=paste(run_data$BASE_DIR,"/",run_data$RUN_ID,"/inputs/ascii/soil/soilcodes.txt",sep=""))
              solfil <- make_soiltypes(data.frame(CELL=run_data$LOC,RLL=run_data$RLL,DUL=run_data$DUL,SAT=run_data$SAT),
                                       outfile=paste(run_data$BASE_DIR,"/",run_data$RUN_ID,"/inputs/ascii/soil/soiltypes.txt",sep=""))
              thisdir <- getwd(); setwd(paste(run_data$BASE_DIR,"/",run_data$RUN_ID,sep="")); system(paste("./",run_data$MODEL,sep="")); setwd(thisdir)
            }
          } else {
            run_data$SEAS_FILES <- outfile
            run_data$RUN_DIR <- paste(run_data$BASE_DIR,"/",run_data$RUN_ID,sep="")
          }
          
          #read in the simulated yield
          if (length(run_data$SEAS_FILES) == 1 | length(outfile) == 1) {
            pred <- read.table(paste(run_data$RUN_DIR,"/output/",run_data$SEAS_FILES,sep=""),header=F,sep="\t")
            names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                             "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                             "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                             "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                             "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT",
                             "IPLANT","LETHAL_YIELD","LETHAL_HI","LETHAL_BMASS","LETHAL_BMASS","LETHAL_DAP",
                             "SWFAC_TOT","SWFAC_MEAN","SWFAC_COUNT")
            pred <- cbind(SOW=sow_date, SAT_FAC=run_df$sol[k], pred[,c("YEAR","STG","YIELD","PLANTING_DATE","DUR")])
            pred_all <- rbind(pred_all, pred)
            #system(paste("rm -rf ",run_data$RUN_DIR,sep="")) #remove junk
          }
        }
        
        #read in all files and determine best sat multiplier
        if (nrow(pred_all) > 0) { #for existence of output GLAM file
          #average by YEAR and SAT_FAC
          #pred_all <- pred_all[which(pred_all$STG != 9),] #first remove STG=9 (no emergence)
          pred_agg <- aggregate(pred_all[,c("SOW","YIELD","PLANTING_DATE","DUR")], by=list(YEAR=pred_all$YEAR, SAT_FAC=pred_all$SAT_FAC), FUN=function(x) {mean(x,na.rm=T)})
          
          #perform this calculation for each value of SAT_FAC
          odf_all <- data.frame()
          for (sfac in sol_seq) {
            #sfac <- sol_seq[1]
            #grab predicted yield
            pred <- pred_agg[which(pred_agg$SAT_FAC == sfac),]
            y_p <- pred$YIELD
            
            #grab observed yield
            y_o <- as.data.frame(t(cal_data$YLD_DATA[which(cal_data$YLD_DATA$x == run_data$LON & cal_data$YLD_DATA$y == run_data$LAT),3:ncol(cal_data$YLD_DATA)]))
            y_o <- cbind(YEAR=1982:2005,y_o)
            names(y_o)[2] <- "YIELD"
            row.names(y_o) <- 1:nrow(y_o)
            y_o <- y_o[which(y_o$YEAR >= cal_data$ISYR & y_o$YEAR <= cal_data$IEYR),]
            y_o <- y_o$YIELD
            
            #get simulated yield, depending on which year the crop was actually harvested:
            #** if planted and harvested this year then use simulation from this year to end
            #** if planted this and harvested next year then use simulation from year-1 to end-1
            har_date <- mean((pred$PLANTING_DATE + pred$DUR)) #get harvest date first
            if (har_date<365) {y_p <- y_p[2:length(y_p)]} else {y_p <- y_p[1:(length(y_p)-1)]}
            odf <- data.frame(YEAR=(cal_data$ISYR+1):cal_data$IEYR,VALUE=vals[i],OBS=y_o,PRED=y_p)
            
            ## detrending (borrows from detrender-functions.R)
            #detrend observed yield
            fit_loess <- loess(odf$OBS ~ odf$YEAR) #compute lowess fit
            y_loess <- predict(fit_loess, odf$YEAR, se=T) #theoretical prediction
            odf$LOESS_PRED <- y_loess$fit
            rd_loess <- (odf$OBS - odf$LOESS_PRED) / odf$LOESS_PRED #relative difference
            odf$OBS_ADJ <- (rd_loess+1) * odf$OBS[nrow(odf)] #loess
            
            #detrend simulated yield
            fit_loess <- loess(odf$PRED ~ odf$YEAR, degree=1, span=2) #compute lowess fit
            y_loess <- predict(fit_loess, odf$YEAR, se=T) #theoretical prediction
            odf$LOESS_PRED <- y_loess$fit
            rd_loess <- (odf$PRED - odf$LOESS_PRED) / odf$LOESS_PRED #relative difference
            odf$PRED_ADJ <- (rd_loess+1) * odf$PRED[nrow(odf)] #loess
            odf$LOESS_PRED <- NULL
            
            #choose optimisation method (RMSE, CH07, CH10)
            if (opt_meth == "RMSE") {
              rmse <- sqrt(sum((odf$OBS_ADJ-odf$PRED_ADJ)^2,na.rm=T) / (length(which(!is.na(odf$OBS_ADJ)))))
            } else if (opt_meth == "CH07") {
              rmse <- (mean(odf$OBS_ADJ,na.rm=T)-mean(odf$PRED_ADJ,na.rm=T))^2 + (sd(odf$OBS_ADJ,na.rm=T)-sd(odf$PRED_ADJ,na.rm=T))^2
            } else if (opt_meth == "CH10") {
              rmse <- (mean(odf$OBS_ADJ,na.rm=T)-mean(odf$PRED_ADJ,na.rm=T))^2
            }
            odf <- cbind(SAT_FAC=sfac, RMSE=rmse, odf)
            odf_all <- rbind(odf_all, odf)
          }
          
          #select minimum RMSE
          rmse_all <- aggregate(odf_all[,c("RMSE")], by=list(SAT_FAC=odf_all$SAT_FAC), FUN=function(x) {mean(x,na.rm=T)})
          sfac <- rmse_all$SAT_FAC[which(rmse_all$x == min(rmse_all$x))][1]
          rmse <- min(rmse_all$x)
          
          #remove junk
          system(paste("rm -rf ",run_data$RUN_DIR,sep=""))
        } else {
          rmse <- NA
        }
        
        odf <- odf_all[which(odf_all$SAT_FAC == sfac),]; odf$RMSE <- NULL
        out_row <- data.frame(VALUE=vals[i], SAT_FAC=sfac, RMSE=rmse, YOBS=mean(odf$OBS,na.rm=T), YPRED=mean(odf$PRED,na.rm=T), 
                              YOBS_ADJ=mean(odf$OBS_ADJ,na.rm=T), YPRED_ADJ=mean(odf$PRED_ADJ,na.rm=T))
        
        if (i == 1) {
          out_all <- out_row
          raw_all <- odf
        } else {
          out_all <- rbind(out_all,out_row)
          raw_all <- rbind(raw_all, odf)
        }
      }
      ##write outputs for this grid cell
      #write.table(out_all,sep="\t",quote=F,file=cal_outfile,row.names=F)
      #write.table(raw_all,sep="\t",quote=F,file=raw_outfile,row.names=F)
    #} else {
    #  out_all <- read.table(cal_outfile,sep="\t",header=T)
    #  raw_all <- read.table(raw_outfile,sep="\t",header=T)
    #}
    
    #append location data
    out_all <- cbind(LOC=loc,out_all)
    raw_all <- cbind(LOC=loc,raw_all)
    if (loc == cal_data$LOC[1]) {
      cal_all <- out_all
      raw_cal <- raw_all
    } else {
      cal_all <- rbind(cal_all, out_all)
      raw_cal <- rbind(raw_cal, raw_all)
    }
  }
  
  #remove junk from scratch
  if (cal_data$USE_SCRATCH) {
    #system(paste("cp -rf ",cal_dir," ",paste(nfs_dir,"/.",sep=""),sep=""))
    system(paste("rm -rf ",cal_dir,sep=""))
  }
  
  #return object
  r_list <- list(CALIBRATION=cal_all, RAW_DATA=raw_cal)
  return(r_list)
}
