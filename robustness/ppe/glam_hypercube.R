#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")

#outline
#1. load list of parameters
#2. create sample of parameter sets
#3. store the matrix of combinations as a data.frame
#4. put a given line into a parameter set
#5. use this parameter set for a model run

#load packages
library(lhs)

#source functions and stuff
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))
source(paste(src.dir,"/glam-utils/calibrate.R",sep=""))
source(paste(src.dir,"/meteo/extract_weather.R",sep=""))

#input directories
wd <- "~/Leeds-work/quest-for-robustness"
#wd <- "/nfs/a101/earjr/quest-for-robustness"
runs_dir <- paste(wd,"/crop_model_runs",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
hyp_dir <- paste(runs_dir,"/glam_hypercube",sep="")
if (!file.exists(hyp_dir)) {dir.create(hyp_dir)}

#load objects (initial conditions and yields)
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

#select ME and its corresponding initial data
me_list <- unique(xy_main$ME_NEW)
me_sel <- me_list[1]
xy_me <- xy_main[which(xy_main$ME_NEW == me_sel),]

#me-specific dir
me_hdir <- paste(hyp_dir,"/run_me-",me_sel,sep="")
if (!file.exists(me_hdir)) {dir.create(me_hdir)}

#ensure SAT is not below DUL
corr_loc <- xy_me$LOC[which(round(xy_me$SAT,2) <= round(xy_me$DUL,2))]
corr_fac <- mean((xy_me$SAT[-which(round(xy_me$SAT,2) <= round(xy_me$DUL,2))]-xy_me$DUL[-which(round(xy_me$SAT,2) <= round(xy_me$DUL,2))]),na.rm=T)
xy_me$SAT[which(xy_me$LOC %in% corr_loc)] <- xy_me$DUL[which(xy_me$LOC %in% corr_loc)] + corr_fac
xy_main$SAT[which(xy_main$LOC %in% corr_loc)] <- xy_me$SAT[which(xy_me$LOC %in% corr_loc)]

###
#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_list <- read.csv(paste(mdata_dir,"/parameter_list.txt",sep=""),sep="\t",header=T)

#2. create sample of parameter sets
#n=number of points (i.e. replicas) --how many values in each dimension?
#k=number of dimensions (i.e. parameters) --how many dimensions (i.e. parameters)?
nrep <- 10000
set.seed(2303)
if (!file.exists(paste(mdata_dir,"/glam_lhs.RData",sep=""))) {
  lhyp1 <- maximinLHS(n=nrep,k=nrow(param_list),dup=1)
  save(list=c("lhyp1"),file=paste(mdata_dir,"/glam_lhs.RData",sep=""))
} else {
  load(file=paste(mdata_dir,"/glam_lhs.RData",sep=""))
}

#3. generate the values in the actual parameter range and put into data.frame
out_df <- as.data.frame(matrix(NA, nrow=nrep, ncol=nrow(param_list)))
names(out_df) <- param_list$PARAM
for (i in 1:nrow(param_list)) {
  #i <- 1
  pvals <- qunif(lhyp1[1,],min=param_list$MIN[i],max=param_list$MAX[i])
  out_df[,i] <- pvals
}

###
#Note: from here onwards the process can be parallelised
#4. get a given line into a parameter set
hrun <- 1
base_params <- GLAM_get_default(mdata_dir)
hyp_params <- put_param_hyp(out_df[hrun,], params=base_params, p_list=names(out_df), all_param=param_list)

#5. use this parameter set to calibrate the model
#location
loc <- xy_me$LOC[1]

#arguments
cal_data <- list()
cal_data$CROP <- "maize"
cal_data$MODEL <- "glam-maiz"
cal_data$BASE_DIR <- me_hdir
cal_data$BIN_DIR <- bin_dir
cal_data$PAR_DIR <- mdata_dir
cal_data$WTH_DIR <- paste(met_dir,"/ascii_extract_raw",sep="") #for reading .wth files
cal_data$WTH_ROOT <- "obs_hist_WFD"
cal_data$LOC <- loc
cal_data$LON <- xy_me$x[which(xy_me$LOC == cal_data$LOC)]
cal_data$LAT <- xy_me$y[which(xy_me$LOC == cal_data$LOC)]
cal_data$ISYR <- 1981
cal_data$IEYR <- 2000
cal_data$INI_COND <- xy_main
cal_data$YLD_DATA <- xy_main_yield
cal_data$SIM_NAME <- paste("calib-",hrun,"_loc-",loc,sep="")
cal_data$RUN_TYPE <- "RFD"
cal_data$METHOD <- "RMSE"
cal_data$USE_SCRATCH <- T
#cal_data$SCRATCH <- "/scratch/earjr"
cal_data$SCRATCH <- paste(wd,"/scratch/me-",me_sel,sep="")
cal_data$NSTEPS <- 67
cal_data$PARAMS <- hyp_params

#create meteorology for selected grid cells
wval <- extract_weather(cellid=cal_data$LOC, lon=cal_data$LON, lat=cal_data$LAT, met_dir=met_dir, 
                        data_type="obs", dataset="WFD", sce="hist", years=1950:2001)

#run calibration
cal_data$PARAMS$glam_param.mod_mgt$IASCII <- 1 #output only to season file
ygp_calib <- GLAM_calibrate(cal_data)



