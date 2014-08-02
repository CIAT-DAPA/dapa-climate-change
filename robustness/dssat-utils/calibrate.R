#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2014 #borrows from PhD script called "glam-optimise-functions.R"

##############################################################################################
####### function to calibrate GLAM (for as many grid cells as provided), each one individually
##############################################################################################

#this (first) function should
#1. use number of steps to choose the values to iterate
#2. calibrate (i.e. find optimum SLPF) for each grid cell individually
#3. return table of grid cell * ygp values, RMSE value, and crop yield

#note: a second function will deal with optimisation of parameters
#note: this function should be applicable to both the hypercube and the normal 
#      optimisation procedure

#note: DSSAT requires 1 spin up year. Hence simulations should start in 1980 instead of 1981.
#      the extra year will is removed during the RMSE calculation. As opposed to GLAM, DSSAT
#      is not run for an extra year at the end (see make_xfile.R), but the object cal_data
#      has an extra year so as to get the weather data into the file

# #example:
# #---------------------------------------------------------------
# src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
# source(paste(src.dir,"/dssat-utils/make_soilfile.R",sep=""))
# source(paste(src.dir,"/dssat-utils/make_xfile.R",sep=""))
# source(paste(src.dir,"/dssat-utils/make_wth.R",sep=""))
# source(paste(src.dir,"/dssat-utils/make_parameters.R",sep=""))
# source(paste(src.dir,"/dssat-utils/get_parameters.R",sep=""))
# source(paste(src.dir,"/dssat-utils/get_soils.R",sep=""))
# source(paste(src.dir,"/dssat-utils/get_xfile.R",sep=""))
# source(paste(src.dir,"/dssat-utils/run_dssat.R",sep=""))
# 
# wd <- "~/Leeds-work/quest-for-robustness"
# runsDir <- paste(wd,"/crop_model_runs",sep="")
# calibDir <- paste(runsDir,"/dssat_t1",sep="")
# mdataDir <- paste(wd,"/data/model_data",sep="")
# metDir <- paste(wd,"/data/meteorology",sep="")
# binDir <- paste(wd,"/bin/dssat/csm45_1_23_bin_gfort",sep="")
# 
# #load objects
# load(paste(mdataDir,"/initial_conditions_major_dssat.RData",sep=""))
# load(paste(mdataDir,"/yield_major_dssat.RData",sep=""))
# 
# #arguments
# cal_data <- list()
# cal_data$MODEL <- "MZCER045"
# cal_data$BASENAME <- "AFRB" #basename of runs
# cal_data$BASE_DIR <- calibDir
# cal_data$BIN_DIR <- binDir
# cal_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw",sep="") #for reading .wth files
# cal_data$WTH_ROOT <- "obs_hist_WFD"
# cal_data$LOC <- c(680,681,682)
# cal_data$ISYR <- 1980
# cal_data$IEYR <- 2001
# cal_data$INI_COND <- xy_main
# cal_data$YLD_DATA <- xy_main_yield
# cal_data$CUL <- data.frame(P1=140,P2=0.3,P5=685,G2=907.9,G3=10.5,PHINT=38.9) #default for missing ones
# cal_data$ECO <- data.frame(DSGFT=170,RUE=4.2,KCAN=0.85,TSEN=6.0,CDAY=15.0)
# cal_data$SPE <- get_spepar(paste(cal_data$BIN_DIR,"/MZCER045.SPE",sep=""))
# cal_data$XFILE <- get_xfile_dummy()
# cal_data$SIM_NAME <- "calib_01"
# cal_data$METHOD <- "RMSE"
# cal_data$USE_SCRATCH <- F
# cal_data$SCRATCH <- NA
# 
# slpfcalib <- DSSAT_calibrate(cal_data)
# #---------------------------------------------------------------

#plotting some of the results
#xx <- slpfcalib$CALIBRATION[which(slpfcalib$CALIBRATION$LOC==682),]
#plot(xx$VALUE, xx$RMSE, ty="l")

#yy <- slpfcalib$RAW_DATA
#optval <- xx$VALUE[which(xx$RMSE == min(xx$RMSE))]
#opsfac <- xx$SAT_FAC[which(xx$RMSE == min(xx$RMSE))]
#yy <- yy[which(yy$LOC == 682 & yy$SAT_FAC == opsfac & yy$VALUE == optval),]
#plot(yy$YEAR, yy$PRED_ADJ, col="red", ty="l",ylim=c(200,800))
#lines(yy$YEAR, yy$OBS_ADJ)
#plot(yy$OBS_ADJ, yy$PRED_ADJ,xlim=c(0,1200),ylim=c(0,1200))
#abline(0,1)

### note:
#simulate year before starting one because if sowing date is late then harvest is in this year
#last year cannot be last year of time series since model runs could fail due to late sowing

### further notes:
#** due to issues in Sahel, multiple plantings were considered: 10 plantings between dates of Sacks
#** due to issues in Sahel, SAT*1.15 and SAT*1.30 were also simulated (soil storage was too poor)

#calibrate ygp
DSSAT_calibrate <- function(cal_data) {
  param <- "SLPF"
  ifile <- "SOL"
  sect <- "properties"
  
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
  exec_name <- "DSCSM045.EXE"
  
  #output directories
  if (cal_data$USE_SCRATCH) {
    cal_dir <- cal_data$SCRATCH #calibration directory
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
  #vals <- seq(0,1,length.out=51)[2:51] #0.2, 0.4, ... 1.0 (total of 50)
  vals <- c(0.01,seq(0.05,1,length.out=20)) #(total of 21)
  
  #loop through desired locations
  for (loc in cal_data$LOC) {
    #loc <- cal_data$LOC[1]
    cat("\n...loc= ",loc,sep="","\n")
    
    #sowing window
    sow_date1 <- cal_data$INI_COND$SOW_DATE1[which(cal_data$INI_COND$LOC == loc)]
    sow_date2 <- cal_data$INI_COND$SOW_DATE2[which(cal_data$INI_COND$LOC == loc)]
    
    #data.frame of iterative soil*sowing date trials
    sow_seq <- round(seq(sow_date1, sow_date2, length.out=5), 0)
    sol_seq <- c(1,1.3,1.6)
    run_df <- expand.grid(sow=sow_seq, sol=sol_seq)
    
    #prepare input object
    run_data <- list()
    run_data$MODEL <- cal_data$MODEL
    run_data$BASENAME <- cal_data$BASENAME
    run_data$BASE_DIR <- opt_dir
    run_data$BIN_DIR <- cal_data$BIN_DIR
    run_data$WTH_DIR <- paste(cal_data$WTH_DIR,"/",cal_data$WTH_ROOT,sep="")
    run_data$LOC <- loc
    run_data$LON <- cal_data$INI_COND$x[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$LAT <- cal_data$INI_COND$y[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$ELEV <- cal_data$INI_COND$ELEV[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$ME <- cal_data$INI_COND$ME_NEW[which(cal_data$INI_COND$LOC == run_data$LOC)]
    run_data$ISYR <- cal_data$ISYR
    run_data$IEYR <- cal_data$IEYR
    run_data$SOW_DATE <- sow_date1
    run_data$SOW_WINDOW <- 30 #sow_date2 - sow_date1
    run_data$SOILS <- get_soils(run_data, cal_data$INI_COND)
    run_data$SOILS_ORIG <- run_data$SOILS #original soil data to avoid over-increasing SSAT
    run_data$CUL <- cal_data$CUL
    run_data$ECO <- cal_data$ECO
    run_data$SPE <- cal_data$SPE
    run_data$XFILE <- get_xfile(run_data, cal_data$XFILE)
    #run_data$XFILE$sim_ctrl$VBOSE <- "0" #write only Summary.OUT outputs (as needed)
    
    #loop through sequence of values
    for (i in 1:length(vals)) {
      #i <- 1
      cat("performing slpf calibration run ",i," value = ",vals[i],sep="","\n")
      
      #run id
      run_data$RUN_ID <- paste("run-",i,"_val-",vals[i],"_loc-",run_data$LOC,sep="")
      
      #assign values to soil file set
      run_data$SOILS[[sect]][[param]] <- vals[i]
      
      #run all sow*sol options for this YGP value and location
      pred_all <- data.frame()
      for (k in 1:nrow(run_df)) {
        #k <- 1
        #get sow date and SAT multiplier into relevant files
        sow_date <- run_df$sow[k]
        run_data$SOILS$profile$SSAT <- run_data$SOILS_ORIG$profile$SSAT * run_df$sol[k]
        run_data$XFILE$ini_cond_properties$ICDAT <- paste(substr(as.character(run_data$ISYR),3,4),sprintf("%03d",sow_date),sep="")
        run_data$XFILE$planting$PDATE <- paste(substr(as.character(run_data$ISYR),3,4),sprintf("%03d",sow_date),sep="")
        run_data$XFILE$planting$EDATE <- paste(substr(as.character(run_data$ISYR),3,4),sprintf("%03d",sow_date+8),sep="")
        run_data$XFILE$sim_ctrl$SDATE <- paste(substr(as.character(run_data$ISYR),3,4),sprintf("%03d",sow_date),sep="")
        run_data$XFILE$auto_mgmt$PFRST <- paste(substr(as.character(run_data$ISYR),3,4),sprintf("%03d",sow_date),sep="")
        run_data$XFILE$auto_mgmt$PLAST <- paste(substr(as.character(run_data$ISYR),3,4),sprintf("%03d",sow_date+run_data$SOW_WINDOW),sep="")
        
        #run the model from scratch if k == 1, otherwise just go to dir, run and grab
        #check whether the *.out already exists
        outfile <- list.files(paste(run_data$BASE_DIR,"/",run_data$RUN_ID,sep=""),pattern="\\.OUT")
        if (length(outfile) == 0) {
          if (k == 1) {
            run_data <- run_dssat(run_data)
          } else {
            #if (cal_data$USE_SCRATCH) {}
            soilfil <- make_soilfile(run_data$SOILS, paste(run_data$BASE_DIR,"/",run_data$RUN_ID,"/SOIL.SOL",sep=""), overwrite=T)
            xfil <- make_xfile(run_data$XFILE, paste(run_data$BASE_DIR,"/",run_data$RUN_ID,"/",run_data$BASENAME,substr(paste(run_data$ISYR),3,4),"01.MZX",sep=""),overwrite=T)
            thisdir <- getwd(); setwd(paste(run_data$BASE_DIR,"/",run_data$RUN_ID,sep="")); system(paste("rm -f *.OUT && ./DSCSM045.EXE ",run_data$MODEL," B DSSBatch.v45",sep=""),ignore.stdout=T); setwd(thisdir)
          }
        } else {
          run_data$OUT_FILES <- outfile
          run_data$RUN_DIR <- paste(run_data$BASE_DIR,"/",run_data$RUN_ID,sep="")
        }
        
        #read in the simulated yield
        if (length(run_data$OUT_FILES) > 0 | length(outfile) > 1) {
          #pred <- read.table(paste(run_data$RUN_DIR,"/Summary.OUT",sep=""),skip=4,header=F,sep="")
          pred <- read.fortran(paste(run_data$RUN_DIR,"/Summary.OUT",sep=""),skip=4,
                               format=c("I9","1X","I6","1X","I2","1X","I2","1X","I2","1X","A2","1X",
                                        "A8","1X","A25","1X","A8","1X","A8","1X","A10","1X",
                                        rep(c("I7","1X"),6),"F5","1X",rep(c("F7","1X"),4),"F5","1X",
                                        "F7","1X","F5","1X","F7","1X",rep(c("F5","1X"),28),
                                        rep(c("F6","1X"),4),rep(c("F7","1X"),2),rep(c("F8","1X"),12),
                                        "F5","4F6",rep(c("1X","F6"),3)))
          #HWAM: Harvest Weight At Maturity
          #HIAM: Harvest Index At Maturity
          #LAIX: LAI maXimum
          names(pred) <- c("RUNNO","TRNO","RNO","ONO","CNO","CR","MODEL","TNAME","FNAME","WSTA","SOIL_ID",
                           "SDAT","PDAT","EDAT","ADAT","MDAT","HDAT","DWAP","CWAM","HWAM","HWAH","BWAH",
                           "PWAM","HWUM","HnAM","HnUM","HIAM","LAIX","IRnM","IRCM","PRCM","ETCM","EPCM",
                           "ESCM","ROCM","DRCM","SWXM","NInM","NICM","NFXM","NUCM","NLCM","NIAM","CNAM",
                           "GNAM","PInM","PICM","PUPC","SPAM","KInM","KICM","KUPC","SKAM","RECM","ONTAM",
                           "ONAM","OPTAM","OPAM","OCTAM","OCAM","DMPPM","DMPEM","DMPTM","DMPIM","YPPM",
                           "YPEM","YPTM","YPIM","DPNAM","DPNUM","YPNAM","YPNUM","NDCH","TMAXA","TMINA",
                           "SRADA","DAYLA","CO2A","PRCP","ETCP")
          pred <- cbind(YEAR=pred$SDAT,pred); pred$YEAR <- as.numeric(substr(pred$YEAR,1,4))
          pred <- cbind(SOW=sow_date, SAT_FAC=run_df$sol[k], pred[,c("YEAR","PDAT","MDAT","HWAM")])
          
          #when a run fails PDAT will be NA, need to put both PDAT and MDAT as NA as well
          pred$PDAT[which(pred$PDAT < -90)] <- NA; pred$MDAT[which(is.na(pred$PDAT))] <- NA
          
          #day of year and calculation of duration
          pred$PDAT <- as.numeric(substr(pred$PDAT,5,7)); pred$MDAT <- as.numeric(substr(pred$MDAT,5,7))
          pred$DUR <- NA; pred$DUR[which(pred$MDAT > pred$PDAT)] <- pred$MDAT[which(pred$MDAT > pred$PDAT)]-pred$PDAT[which(pred$MDAT > pred$PDAT)]
          pred$DUR[which(pred$MDAT <= pred$PDAT)] <- (pred$MDAT[which(pred$MDAT > pred$PDAT)]+365)-pred$PDAT[which(pred$MDAT > pred$PDAT)]
          pred_all <- rbind(pred_all, pred)
          system(paste("rm -f ",run_data$RUN_DIR,"/*.OUT",sep="")) #remove junk
        }
      }
      
      #read in all files and determine best sat multiplier
      if (nrow(pred_all) > 0) { #for existence of output GLAM file
        #average by YEAR and SAT_FAC
        pred_all$HWAM[which(pred_all$HWAM == -99)] <- NA #set to NA all STG==0
        pred_agg <- aggregate(pred_all[,c("SOW","HWAM","PDAT","MDAT","DUR")], by=list(YEAR=pred_all$YEAR, SAT_FAC=pred_all$SAT_FAC), FUN=function(x) {mean(x,na.rm=T)})
        
        #perform this calculation for each value of SAT_FAC
        odf_all <- data.frame()
        for (sfac in sol_seq) {
          #sfac <- sol_seq[1]
          #grab predicted yield
          pred <- pred_agg[which(pred_agg$SAT_FAC == sfac),]
          y_p <- pred$HWAM
          y_p[which(is.na(y_p))] <- 0 #set to zero any NAs (product of emergence failure)
          y_p <- y_p[2:length(y_p)] #remove spin up year (1980)
          
          #grab observed yield
          y_o <- as.data.frame(t(cal_data$YLD_DATA[which(cal_data$YLD_DATA$x == run_data$LON & cal_data$YLD_DATA$y == run_data$LAT),3:ncol(cal_data$YLD_DATA)]))
          y_o <- cbind(YEAR=1982:2005,y_o)
          names(y_o)[2] <- "YIELD"
          row.names(y_o) <- 1:nrow(y_o)
          y_o <- y_o[which(y_o$YEAR >= cal_data$ISYR & y_o$YEAR <= (cal_data$IEYR-1)),]
          y_o <- y_o$YIELD
          
          #get simulated yield, depending on which year the crop was actually harvested:
          #** if planted and harvested this year then use simulation from this year to end
          #** if planted this and harvested next year then use simulation from year-1 to end-1
          har_date <- mean((pred$PDAT + pred$DUR)) #get harvest date first
          if (har_date<365) {y_p <- y_p[2:length(y_p)]} else {y_p <- y_p[1:(length(y_p)-1)]}
          odf <- data.frame(YEAR=(cal_data$ISYR+2):(cal_data$IEYR-1),VALUE=vals[i],OBS=y_o,PRED=y_p)
          
          ## detrending (borrows from detrender-functions.R) [based on Heinemann et al. (2011)]
          #detrend observed yield
          fit_loess <- loess(odf$OBS ~ odf$YEAR) #compute lowess fit
          y_loess <- predict(fit_loess, odf$YEAR, se=T) #theoretical prediction
          odf$LOESS_PRED <- y_loess$fit
          rd_loess <- (odf$OBS - odf$LOESS_PRED) / odf$LOESS_PRED #relative difference
          odf$OBS_ADJ <- (rd_loess+1) * mean(odf$OBS, na.rm=T) #odf$OBS[nrow(odf)] #loess
          
          #detrend simulated yield
          if (length(which(odf$PRED == 0)) == length(odf$PRED)) {
            odf$PRED_ADJ <- 0
          } else {
            fit_loess <- loess(odf$PRED ~ odf$YEAR, degree=1, span=2) #compute lowess fit
            y_loess <- predict(fit_loess, odf$YEAR, se=T) #theoretical prediction
            odf$LOESS_PRED <- y_loess$fit
            rd_loess <- (odf$PRED - odf$LOESS_PRED) / odf$LOESS_PRED #relative difference
            odf$PRED_ADJ <- (rd_loess+1) * mean(odf$PRED, na.rm=T) #odf$PRED[nrow(odf)] #loess
          }
          odf$LOESS_PRED <- NULL #remove extra field
          
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
      out_row <- data.frame(VALUE=vals[i], SAT_FAC=sfac, RMSE=rmse, YOBS=mean(odf$OBS,na.rm=T), 
                            YPRED=mean(odf$PRED,na.rm=T), YOBS_ADJ=mean(odf$OBS_ADJ,na.rm=T), 
                            YPRED_ADJ=mean(odf$PRED_ADJ,na.rm=T))
      
      if (i == 1) {
        out_all <- out_row
        raw_all <- odf
      } else {
        out_all <- rbind(out_all,out_row)
        raw_all <- rbind(raw_all, odf)
      }
    }
    
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
    system(paste("rm -rf ",cal_dir,sep=""))
  }
  
  #return object
  r_list <- list(CALIBRATION=cal_all, RAW_DATA=raw_cal)
  return(r_list)
}
