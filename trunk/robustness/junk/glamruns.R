#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014

#source directories
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))

wd <- "~/Leeds-work/quest-for-robustness"
runsDir <- paste(wd,"/crop_model_runs",sep="")
calibDir <- paste(runsDir,"/ppe_optimisation",sep="")
mdataDir <- paste(wd,"/data/model_data",sep="")
metDir <- paste(wd,"/data/meteorology",sep="")
binDir <- paste(wd,"/bin/glam-maize-osx",sep="")

#load objects
load(paste(mdataDir,"/initial_conditions_major.RData",sep=""))

#arguments
run_data <- list()
run_data$CROP <- "maize"
run_data$MODEL <- "glam-maiz"
run_data$BASE_DIR <-calibDir
run_data$BIN_DIR <- binDir
run_data$PAR_DIR <- mdataDir
run_data$WTH_DIR <- paste(metDir,"/extract_temp",sep="") #for reading meteo_cell-$LOC$.met
run_data$LOC <- 1792
run_data$LON <- xy_main$x[which(xy_main$LOC == run_data$LOC)]
run_data$LAT <- xy_main$y[which(xy_main$LOC == run_data$LOC)]
run_data$RUN_ID <- paste("test003_",run_data$LOC,sep="")
run_data$ME <- xy_main$ME[which(xy_main$LOC == run_data$LOC)]
run_data$SOW_DATE <- xy_main$SOW_DATE1[which(xy_main$LOC == run_data$LOC)]
run_data$RLL <- xy_main$RLL[which(xy_main$LOC == run_data$LOC)]
run_data$DUL <- xy_main$DUL[which(xy_main$LOC == run_data$LOC)]
run_data$SAT <- xy_main$SAT[which(xy_main$LOC == run_data$LOC)]
run_data$ISYR <- 1950
run_data$IEYR <- 1950

#run GLAM
ofiles <- run_glam(run_data)



