#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014
stop("!")

#source directories
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/dssat-utils/make_soilfile.R",sep=""))
source(paste(src.dir,"/dssat-utils/make_wth.R",sep=""))
source(paste(src.dir,"/dssat-utils/make_parameters.R",sep=""))
source(paste(src.dir,"/dssat-utils/make_xfile.R",sep=""))
source(paste(src.dir,"/dssat-utils/get_parameters.R",sep=""))
source(paste(src.dir,"/dssat-utils/run_dssat.R",sep=""))

wd <- "~/Leeds-work/quest-for-robustness"
runsDir <- paste(wd,"/crop_model_runs",sep="")
calibDir <- paste(runsDir,"/dssat_t1",sep="")
mdataDir <- paste(wd,"/data/model_data",sep="")
metDir <- paste(wd,"/data/meteorology",sep="")
binDir <- paste(wd,"/bin/dssat/csm45_1_23_bin_gfort",sep="")
#binDir <- paste(wd,"/bin/dssat/csm45_1_23_bin_ifort",sep="")

#load objects
load(paste(mdataDir,"/initial_conditions_major_dssat.RData",sep=""))

############################################################################################
############################################################################################
### MZCER045 testing
#arguments
run_data <- list()
run_data$MODEL <- "MZCER045"
run_data$BASENAME <- "AFRB"
run_data$BASE_DIR <- calibDir
run_data$BIN_DIR <- binDir
run_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw/obs_hist_WFD",sep="") #for reading meteo_cell-$LOC$.met
run_data$LOC <- 202
run_data$LON <- xy_main$x[which(xy_main$LOC == run_data$LOC)]
run_data$LAT <- xy_main$y[which(xy_main$LOC == run_data$LOC)]
run_data$ELEV <- xy_main$ELEV[which(xy_main$LOC == run_data$LOC)]
run_data$RUN_ID <- paste("test001_",run_data$LOC,sep="")
run_data$ME <- xy_main$ME_NEW[which(xy_main$LOC == run_data$LOC)]
run_data$ISYR <- 1980
run_data$IEYR <- 2001
run_data$SOW_DATE <- xy_main$SOW_DATE1[which(xy_main$LOC == run_data$LOC)]
run_data$SOW_WINDOW <- xy_main$SOW_DATE2[which(xy_main$LOC == run_data$LOC)] - xy_main$SOW_DATE1[which(xy_main$LOC == run_data$LOC)]
run_data$SOILS <- get_soils(run_data, xy_main)
run_data$CUL <- data.frame(P1=140,P2=0.3,P5=685,G2=907.9,G3=10.5,PHINT=38.9) #default for missing ones
run_data$ECO <- data.frame(DSGFT=170,RUE=4.2,KCAN=0.85,TSEN=6.0,CDAY=15.0)
run_data$SPE <- get_spepar(paste(run_data$BIN_DIR,"/MZCER045.SPE",sep=""))
run_data$XFILE <- get_xfile(run_data)

#note: at this point modifications can be introduced in the parameters being looked at 
#in CUL, ECO, SPE and XFILE

#run DSSAT CSM
ofiles <- run_dssat(run_data)


############################################################################################
############################################################################################
### MZIXM045 testing
#arguments
run_data <- list()
run_data$MODEL <- "MZIXM045"
run_data$BASENAME <- "AFRB"
run_data$BASE_DIR <- calibDir
run_data$BIN_DIR <- binDir
run_data$WTH_DIR <- paste(metDir,"/ascii_extract_raw/obs_hist_WFD",sep="") #for reading meteo_cell-$LOC$.met
run_data$LOC <- 202
run_data$LON <- xy_main$x[which(xy_main$LOC == run_data$LOC)]
run_data$LAT <- xy_main$y[which(xy_main$LOC == run_data$LOC)]
run_data$ELEV <- xy_main$ELEV[which(xy_main$LOC == run_data$LOC)]
run_data$RUN_ID <- paste("test002_",run_data$LOC,sep="")
run_data$ME <- xy_main$ME_NEW[which(xy_main$LOC == run_data$LOC)]
run_data$ISYR <- 1980
run_data$IEYR <- 2001
run_data$SOW_DATE <- xy_main$SOW_DATE1[which(xy_main$LOC == run_data$LOC)]
run_data$SOW_WINDOW <- xy_main$SOW_DATE2[which(xy_main$LOC == run_data$LOC)] - xy_main$SOW_DATE1[which(xy_main$LOC == run_data$LOC)]
run_data$SOILS <- get_soils(run_data, xy_main)
run_data$CUL <- data.frame(P1=140,P2=0.3,P5=685,G2=907.9,G3=10.5,PHINT=38.9,AX=800,LX=800) #default for missing ones
run_data$ECO <- data.frame(DSGFT=170,RUE=4.2,KCAN=0.85,PSTM=0.75,PEAR=0.15,TSEN=6.0,CDAY=15.0)
run_data$SPE <- get_spepar(paste(run_data$BIN_DIR,"/MZIXM045.SPE",sep=""))
run_data$XFILE <- get_xfile(run_data)

#note: at this point modifications can be introduced in the parameters being looked at 
#in CUL, ECO, SPE and XFILE

#run DSSAT CSM
ofiles <- run_dssat(run_data)

