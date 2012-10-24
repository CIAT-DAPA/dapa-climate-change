#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012

library(raster)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD"

#base and data directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
climDir <- paste(bDir,"/climate-data",sep="")
obsDir <- paste(climDir,"/gridcell-data/IND",sep="")
hisDir <- paste(climDir,"/gridcell-data/IND_CMIP5",sep="")
rcpDir <- paste(climDir,"/gridcell-data/IND_RCP45",sep="")

#source functions of interest
source(paste(src.dir,"/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/000.bias_corr_test-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/cmip5/01.make_wth-functions.R",sep=""))
source(paste(src.dir,"/0007-crop-modelling/scripts/glam/glam-make_wth.R",sep=""))

###variable and gcm
vn <- "rain"
vn_gcm <- "pr"
gcm <- "mohc_hadgem2_cc"
ens <- "r1i1p1"
wleap <- "all30"

# gcm <- "ncar_ccsm4"
# ens <- "r1i1p1"
# wleap <- "no"

#output directories
bcDir_his <- paste(climDir,"/gridcell-data/IND_CMIP5_BC",sep="")
bcDir_rcp <- paste(climDir,"/gridcell-data/IND_RCP45_BC",sep="")
oDir_his <- paste(bcDir_his,"/",gcm,"/",ens,"/",vn_gcm,sep="")
oDir_rcp <- paste(bcDir_rcp,"/",gcm,"/",ens,"/",vn_gcm,sep="")
if (!file.exists(oDir_his)) {dir.create(oDir_his,recursive=T)}
if (!file.exists(oDir_rcp)) {dir.create(oDir_rcp,recursive=T)}

#years initial and final
yi_h <- 1961
yf_h <- 2000
yi_f <- 2020
yf_f <- 2049

#gridcell
loc <- 635


#############################################################################
# bias correcting the GCM output precipitation
#############################################################################
if (!file.exists(paste(oDir_his,"/fit_cell-",loc,".csv",sep=""))) {
  #read in observed and gcm data
  obs_data <- read.csv(paste(obsDir,"/",vn,"/cell-",loc,".csv",sep=""))
  his_data <- read.csv(paste(hisDir,"/",gcm,"/",ens,"/",vn_gcm,"/cell-",loc,".csv",sep=""))
  rcp_data <- read.csv(paste(rcpDir,"/",gcm,"/",ens,"/",vn_gcm,"/cell-",loc,".csv",sep=""))
  
  #first calculate the model wet-day threshold, and climatological mean of wet day
  #rain intensity
  #this is a threshold such that the number of days above that threshold
  #equals the number of days in the observations above a given threshold (1mm/day)
  #this is done per month for the whole series
  
  #separate months into individual series
  obs_list <- mk_mth_list(obs_data,"createDateGrid",dg_args=NA,yi_h,yf_h)
  his_list <- mk_mth_list(his_data,"createDateGridCMIP5",dg_args=wleap,yi_h,yf_h)
  rcp_list <- mk_mth_list(rcp_data,"createDateGridCMIP5",dg_args=wleap,yi_f,yf_f)
  
  #calculate loci metrics
  loci_mets <- loci_cal(obs_list,his_list,wdt_obs=1,iter_step=0.0001)
  # plot(1:12,loci_mets$WDT_MOD,ty="l",xlab="month",ylab="wet-day threshold (mm/day)",lwd=2)
  # points(1:12,loci_mets$WDT_MOD,pch=20,cex=1.5)
  # grid()
  
  #calculate bias corrected data based on historical data
  his_list <- loci_correct(his_list,loci_mets)
  
  #putting all the series together again into a matrix
  his_data_bc <- remake_daily(his_list,his_data,wleap,yi_h,yf_h)
  
  #here bias correct the future climates and plot the pdfs together
  rcp_list <- loci_correct(rcp_list,loci_mets)
  rcp_data_bc <- remake_daily(rcp_list,rcp_data,wleap,yi_f,yf_f)
  
  #here write the data
  write.csv(his_data_bc,paste(oDir_his,"/cell-",loc,".csv",sep=""),quote=T,row.names=F)
  write.csv(rcp_data_bc,paste(oDir_rcp,"/cell-",loc,".csv",sep=""),quote=T,row.names=F)
  write.csv(loci_mets,paste(oDir_his,"/fit_cell-",loc,".csv",sep=""),quote=T,row.names=F)
}

###########################################################################
###########################################################################
#here calculate the monthly fields and compare the PDFs of, for JJA season
#bias corrected and uncorrected with observations. Also compare current and
#future corrected and uncorrected. Variables to calculate:
# 1. total rainfall
# 2. number of rain days

# #all this has been commented out for processing reasons
# obs_calcs <- calc_metrics(obs_data,"createDateGrid",dg_args=NA,yi_h,yf_h)
# his_calcs <- calc_metrics(his_data,"createDateGridCMIP5",dg_args=wleap,yi_h,yf_h)
# rcp_calcs <- calc_metrics(rcp_data,"createDateGridCMIP5",dg_args=wleap,yi_f,yf_f)
# his_calcs_bc <- calc_metrics(his_data_bc,"createDateGridCMIP5",dg_args=wleap,yi_h,yf_h)
# rcp_calcs_bc <- calc_metrics(rcp_data_bc,"createDateGridCMIP5",dg_args=wleap,yi_f,yf_f)
# 
# plot_vn <- "RD"
# plot_mth <- "JJA"
# 
# pdf_obs <- density(obs_calcs[which(obs_calcs$MTH == plot_mth),plot_vn])
# pdf_his <- density(his_calcs[which(his_calcs$MTH == plot_mth),plot_vn])
# pdf_rcp <- density(rcp_calcs[which(rcp_calcs$MTH == plot_mth),plot_vn])
# pdf_his_bc <- density(his_calcs_bc[which(his_calcs_bc$MTH == plot_mth),plot_vn])
# pdf_rcp_bc <- density(rcp_calcs_bc[which(rcp_calcs_bc$MTH == plot_mth),plot_vn])
# 
# xlims <- c(min(c(pdf_obs$x,pdf_his$x,pdf_rcp$x,pdf_his_bc$x,pdf_rcp_bc$x)),max(c(pdf_obs$x,pdf_his$x,pdf_rcp$x,pdf_his_bc$x,pdf_rcp_bc$x)))
# ylims <- c(0,max(c(pdf_obs$y,pdf_his$y,pdf_rcp$y,pdf_his_bc$y,pdf_rcp_bc$y)))
# 
# plot(pdf_obs,ylim=ylims,xlim=xlims,main=NA,xlab="number of rain days",lwd=2)
# lines(pdf_his,col="blue",lwd=2)
# lines(pdf_rcp,col="red",lwd=2)
# lines(pdf_his_bc,col="blue",lty=2,lwd=2)
# lines(pdf_rcp_bc,col="red",lty=2,lwd=2)
# grid()



###########################################################################
###########################################################################
#here do some crop model simulations
#1. generate the wth files
#2. calibrate glam using exp33, with the climate model bias-corrected and uncorrected data
#3. compare the two simulations (bc and non-bc) with the simulation driven by observations

######
#1. generate the wth files
#some details
crop_short <- "gnut"
ver <- "v6"
sce <- paste(gcm,"_ENS_",ens,sep="")

glam_dir <- paste(bDir,"/GLAM",sep="")
crop_dir <- paste(glam_dir,"/model-runs/",toupper(crop_short),sep="")
input_dir <- paste(crop_dir,"/inputs",sep="")
asc_dir <- paste(input_dir,"/ascii",sep="")
runs_dir <- paste(crop_dir,"/runs",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")

#output directories
wth_dir <- paste(asc_dir,"/wth-cmip5_hist_bc",sep="")
if (!file.exists(wth_dir)) {dir.create(wth_dir)}

#data that is needed by the process
cells <- read.csv(paste(input_dir,"/calib-cells-selection-",ver,".csv",sep=""))
rabi_sow <- raster(paste(crop_dir,"/",tolower(crop_short),"-zones/plant_rabi.asc",sep=""))

#directories where the data is
cmip_wth <- paste(bcDir_his,"/",gcm,"/",ens,sep="") #folder with gridded data

#copy all other data from the uncorrected output, and then remove it
#copy temperature data
for (cvn in c("rsds","tasmax","tasmin")) {
  codir <- paste(bcDir_his,"/",gcm,"/",ens,"/",cvn,sep="")
  if (!file.exists(codir)) {dir.create(codir)}
  ff <- file.copy(from=paste(cmip_wth,"/tasmax/cell-",loc,".csv",sep=""),to=codir)
}

#create the daily data files
outfol <- write_cmip5_loc(all_locs=cells,gridcell=loc,scen=sce,
                          year_i=1966,year_f=1993,wleap=wleap,out_wth_dir=wth_dir,
                          fut_wth_dir=cmip_wth,sow_date_dir=sow_dir)


######
#2. calibrate glam using exp33, with the climate model bias-corrected and uncorrected data
gcmDir <- paste(crop_dir,"/inputs/ascii/wth-cmip5_hist",sep="")

#source all needed functions
src.dir.mod <- paste(src.dir,"/0007-crop-modelling/scripts",sep="")
source(paste(src.dir.mod,"/glam/glam-parFile-functions.R",sep=""))
source(paste(src.dir.mod,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir.mod,"/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir.mod,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir.mod,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir.mod,"/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir.mod,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir.mod,"/cmip5/03.glam-cmip5_hist-functions.R",sep=""))

### some details
expID <- 33
use_scratch <- T
maxiter <- 15 #to grab last optim values

#input directories and model
cropName <- "gnut"
scratch <- "/scratch/eejarv/biascorr"
cDir <- paste(glam_dir,"/model-runs/",toupper(cropName),sep="")
pDir <- paste(cDir,"/params",sep="") #parameter files

#load cell details
cells <- read.csv(paste(cDir,"/inputs/calib-cells-selection-",ver,".csv",sep=""))

#list of tests to run
run_tests <- data.frame(SIM_NAME=c("bcin","bcout","bcin_ch07","bcout_ch07","bcin_ch10","bcout_ch10"),
                        WTH_DIR=rep(c("wth-cmip5_hist_bc","wth-cmip5_hist"),3),
                        OPT_METH=c("RMSE","RMSE","CH07","CH07","CH10","CH10"))


#here construct a control file folder
out_bdir <- paste(glam_dir,"/model-runs/",toupper(cropName),"/runs/testing/biascorr",sep="")
ctrl_dir <- paste(out_bdir,"/_process",sep="")
if (!file.exists(ctrl_dir)) {dir.create(ctrl_dir,recursive=T)}
ctrl_fil <- paste(ctrl_dir,"/exp-",expID,"_",gcm,".proc",sep="")


######
###### bias corrected inputs
######
#get run setup
setup <- list()
setup$BDIR <- glam_dir
setup$SCRATCH <- scratch
setup$USE_SCRATCH <- use_scratch
setup$CELL <- loc
setup$ZONE <- cells$ZONE[which(cells$CELL == loc)]
setup$METHOD <- "lin"
setup$CROPNAME <- crop_short
setup$CAL_DIR <- paste(setup$BDIR,"/model-runs/",toupper(setup$CROPNAME),"/runs/testing/biascorr/exp-",expID,"_outputs/",sce,sep="")
setup$PRE_DIR <- paste(setup$BDIR,"/model-runs/",toupper(setup$CROPNAME),"/calib/exp-",expID,"_outputs",sep="")
setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
setup$YGP_FILE <- "nofile"
setup$SOW_FILE_RFD <- paste(setup$PRE_DIR,"/gridcells/fcal_",setup$CELL,"/opt_fcal_",setup$CELL,".txt",sep="")
setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
setup$WTH_ROOT <- "ingc"
setup$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",setup$CELL,".txt",sep="")
setup$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default

#if using scratch directory instead of nfs
if (use_scratch) {setup$SCRATCH <- paste(setup$SCRATCH,"/exp-",expID,"_",sce,sep="")}

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
nstep <- 20
params[[where]][[parname]][,"Min"] <- 0.05
params[[where]][[parname]][,"Max"] <- 1.00


#looping through test experiments
all_simy <- data.frame()
all_mets <- data.frame()
for (i in 1:nrow(run_tests)) {
  #get the tests details
  setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/",run_tests$WTH_DIR[i],"/",sce,"/rfd_",setup$CELL,sep="")
  setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/",run_tests$WTH_DIR[i],"/",sce,"/irr_",setup$CELL,sep="")
  setup$SIM_NAME <- paste(run_tests$SIM_NAME[i],"_",setup$CELL,sep="")
  setup$OPT_METHOD <- run_tests$OPT_METH[i]
  
  #if the output control file does not exist
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
    ers_run <- ers_run[-grep(paste("_run-",nstep,"_1",sep=""),ers_run,fixed=T)]
    for (ers in 1:length(ers_run)) {
      system(paste("rm -rf ",paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),"/",ers_run[ers],sep=""),sep=""))
    }
    
    save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
  }
  
  #here you have to load the hindcasts and then compare the time series with observed
  #calculate RMSE, CCOEF. Make a boxplot where one is obs, other is control, other
  #is raw GCM other is bias corrected GCM
  load(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
  opt_run <- which(optimised[[parname]]$VALUE == optimal[[parname]])
  rundir <- paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/",tolower(parname),sep="")
  rfd_fil <- paste(rundir,"/RFD_run-",opt_run,"_",optimal[[parname]],"/output/groundnut.out",sep="")
  irr_fil <- paste(rundir,"/IRR_run-",opt_run,"_",optimal[[parname]],"/output/groundnut.out",sep="")
  
  if (file.exists(rfd_fil) & file.exists(irr_fil)) {
    #rainfed data
    pred <- read.table(rfd_fil)
    names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                     "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                     "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                     "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                     "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
    y_p <- pred$YIELD
    y_o <- read.fortran(setup$YIELD_FILE,format=c("3I4","F8"),n=28)
    y_o <- y_o[which(y_o$V1 >= 1966 & y_o$V1 <= 1993),]
    y_o <- y_o$V4
    
    odf <- data.frame(YEAR=params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR,
                      IRATIO=ir_vls$IRATIO,OBS=y_o,RFD=y_p)
    
    #irrigated data
    pred <- read.table(irr_fil)
    names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                     "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                     "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                     "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                     "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
    y_p <- pred$YIELD
    odf$IRR <- y_p
    
    odf$PRED <- odf$IRR*odf$IRATIO + odf$RFD*(1-odf$IRATIO)
    odf <- cbind(TEST=i,SIM_NAME=setup$SIM_NAME,odf)
    
    rmse <- sqrt(sum((odf$OBS-odf$PRED)^2,na.rm=T) / (length(which(!is.na(odf$OBS)))))
    ccoef <- cor(odf$OBS,odf$PRED)
    mnyi <- mean(odf$PRED)
    sdyi <- sd(odf$PRED)
    omets <- data.frame(TEST=i,SIM_NAME=setup$SIM_NAME,YGP=optimal[[parname]],RMSE=rmse,CCOEF=ccoef,MEAN=mnyi,SD=sdyi)
    
    all_simy <- rbind(all_simy,odf)
    all_mets <- rbind(all_mets,omets)
  } #here complete for those gridcells that are either fully rainfed or fully irrigated
}


### loading control run data
crun_dir <- paste(setup$PRE_DIR,"/gridcells/fcal_",loc,sep="")
rfd_fil <- paste(crun_dir,"/groundnut_RFD.out",sep="")
irr_fil <- paste(crun_dir,"/groundnut_IRR.out",sep="")

load(paste(crun_dir,"/ygp.RData",sep=""))

if (file.exists(rfd_fil) & file.exists(irr_fil)) {
  #rainfed data
  pred <- read.table(rfd_fil)
  names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                   "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                   "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                   "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                   "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
  y_p <- pred$YIELD
  y_o <- read.fortran(setup$YIELD_FILE,format=c("3I4","F8"),n=28)
  y_o <- y_o[which(y_o$V1 >= 1966 & y_o$V1 <= 1993),]
  y_o <- y_o$V4
  
  odf <- data.frame(YEAR=params$glam_param.mod_mgt$ISYR:params$glam_param.mod_mgt$IEYR,
                    IRATIO=ir_vls$IRATIO,OBS=y_o,RFD=y_p)
  
  #irrigated data
  pred <- read.table(irr_fil)
  names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                   "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                   "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                   "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                   "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
  y_p <- pred$YIELD
  odf$IRR <- y_p
  
  odf$PRED <- odf$IRR*odf$IRATIO + odf$RFD*(1-odf$IRATIO)
  odf <- cbind(TEST=NA,SIM_NAME="CTRL",odf)
  
  rmse <- sqrt(sum((odf$OBS-odf$PRED)^2,na.rm=T) / (length(which(!is.na(odf$OBS)))))
  ccoef <- cor(odf$OBS,odf$PRED)
  mnyi <- mean(odf$PRED)
  sdyi <- sd(odf$PRED)
  omets <- data.frame(TEST=NA,SIM_NAME="CTRL",YGP=optimal$YGP,RMSE=rmse,CCOEF=ccoef,MEAN=mnyi,SD=sdyi)
  
  all_simy <- rbind(all_simy,odf)
  all_mets <- rbind(all_mets,omets)
}

#add obs to that thing
odf$RFD <- odf$OBS*(1-odf$IRATIO); odf$IRR <- odf$OBS*odf$IRATIO
odf$PRED <- odf$OBS
odf$TEST <- NA; odf$SIM_NAME <- "OBS"
all_simy <- rbind(all_simy,odf)

all_mets$MEAN_NORM <- all_mets$MEAN/mean(odf$OBS)
all_mets$SD_NORM <- all_mets$SD/sd(odf$OBS)

windows()
par(mar=c(8,5,1,1),las=2)
boxplot(all_simy$PRED~all_simy$SIM_NAME,las=2,col="grey 80",pch=20,ylab="yield (kg/ha)")
barplot(all_mets$RMSE,names.arg=all_mets$SIM_NAME,axes=T,ylab="RMSE (kg/ha)")
barplot(all_mets$CCOEF,names.arg=all_mets$SIM_NAME,axes=T,ylab="Correlation coefficient")
barplot(all_mets$MEAN_NORM,names.arg=all_mets$SIM_NAME,axes=T,ylab="Mean yield (normalised by obs)")
barplot(all_mets$SD_NORM,names.arg=all_mets$SIM_NAME,axes=T,ylab="S.d. yield (normalised by obs)")






