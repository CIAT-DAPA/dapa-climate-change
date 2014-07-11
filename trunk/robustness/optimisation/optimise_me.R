#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")

#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
#2. load objects (initial conditions and yields)
#3. create a list of 100 seeds
#4. select ME and grid cells to optimise on
#5. create meteorology for selected grid cells
#6. iteratively optimise over the list of parameters (with a defined number of iterations)

src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))
source(paste(src.dir,"/glam-utils/calibrate.R",sep=""))
source(paste(src.dir,"/glam-utils/optimise.R",sep=""))
source(paste(src.dir,"/meteo/extract_weather.R",sep=""))

#input directories
wd <- "~/Leeds-work/quest-for-robustness"
runs_dir <- paste(wd,"/crop_model_runs",sep="")
calib_dir <- paste(runs_dir,"/ppe_optimisation_t1",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
bin_dir <- paste(wd,"/bin/glam-maize-c",sep="")

###
#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_list <- read.csv(paste(mdata_dir,"/parameter_list.txt",sep=""),sep="\t",header=T)

###
#2. load objects (initial conditions and yields)
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

###
#3. create a list of 100 seeds
set.seed(2302) #fixed seed to make it replicable
seed_list <- round(runif(100, 1000, 9999),0)
seed <- seed_list[1]

#randomise list of parameters
set.seed(seed)
reord <- sample(1:nrow(param_list),replace=F)
param_list <- param_list[reord,]
row.names(param_list) <- 1:nrow(param_list)

###
#4. select ME and grid cells to optimise on
me_list <- unique(xy_main$ME_NEW)
me_sel <- me_list[1]

xy_me <- xy_main[which(xy_main$ME_NEW == me_sel),]
set.seed(2059) #randomly choose grid cells (use fixed seed to make it replicable)
xy_sel <- xy_me[sample(1:nrow(xy_me), 10, replace=F),]
row.names(xy_me) <- 1:nrow(xy_me); row.names(xy_sel) <- 1:nrow(xy_sel)

#ensure SAT is not below DUL
corr_loc <- xy_sel$LOC[which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]
corr_fac <- mean((xy_sel$SAT[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]-xy_sel$DUL[-which(round(xy_sel$SAT,2) <= round(xy_sel$DUL,2))]),na.rm=T)
xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)] <- xy_sel$DUL[which(xy_sel$LOC %in% corr_loc)] + corr_fac
xy_main$SAT[which(xy_main$LOC %in% corr_loc)] <- xy_sel$SAT[which(xy_sel$LOC %in% corr_loc)]

#plot(xy_me$x, xy_me$y)
#points(xy_sel$x, xy_sel$y, pch=20, col="red")

###
#5. create meteorology for selected grid cells
for (i in 1:nrow(xy_sel)) {
  loc <- xy_sel$LOC[i]; x <- xy_sel$x[i]; y <- xy_sel$y[i]
  wval <- extract_weather(cellid=loc, lon=x, lat=y, met_dir=met_dir, data_type="obs", dataset="WFD", 
                          sce="hist", years=1950:2001)
}

###
#6. iteratively optimise over the list of parameters (with a defined number of iterations)
this_params <- GLAM_get_default(mdata_dir)
iter <- 1

#arguments
opt_data <- list()
opt_data$CROP <- "maize"
opt_data$MODEL <- "glam-maiz"
opt_data$BASE_DIR <- calib_dir
opt_data$BIN_DIR <- bin_dir
opt_data$PAR_DIR <- mdata_dir
opt_data$WTH_DIR <- paste(met_dir,"/ascii_extract_raw",sep="") #for reading .wth files
opt_data$WTH_ROOT <- "obs_hist_WFD"
opt_data$LOC <- xy_sel$LOC
opt_data$ISYR <- 1981
opt_data$IEYR <- 2000
opt_data$INI_COND <- xy_main
opt_data$YLD_DATA <- xy_main_yield
opt_data$SIM_NAME <- paste("optim_me-",me_sel,"_seed-",seed,"_iter-",iter,sep="")
opt_data$RUN_TYPE <- "RFD"
opt_data$METHOD <- "RMSE"
opt_data$USE_SCRATCH <- F
opt_data$SCRATCH <- NA

for (i in 1:nrow(param_list)) {
  #i <- 2
  #previous parameters
  prev_params <- this_params
  
  #parameter and loc within
  param <- paste(param_list$PARAM[i]); sect <- paste(param_list$WHERE[i])
  nsteps <- param_list$NSTEPS[i]
  
  #update arguments
  opt_data$PARAMS <- this_params
  opt_data$PARAM <- param
  opt_data$SECT <- sect
  opt_data$NSTEPS <- nsteps
  
  if (param %in% c("SLA_INI","NDSLA")) {
    opt_data$MINVAL <- param_list$MIN[i]
    opt_data$MAXVAL <- param_list$MAX[i]
  }
  
  #run optim function
  par_optim <- GLAM_optimise(opt_data)
  #plot(par_optim$OPTIMISATION$VALUE, par_optim$OPTIMISATION$RMSE, ty="l") #plot RMSE curve
  
  #update parameter value with optimal
  opt_val <- par_optim$OPTIMISATION$VALUE[which(par_optim$OPTIMISATION$RMSE == min(par_optim$OPTIMISATION$RMSE))]
  if (length(opt_val > 1)) {opt_val <- opt_val[ceiling(length(opt_val)/2)]}
  if (param %in% c("SLA_INI","NDSLA")) {
    this_params[[sect]][[param]] <- opt_val
  } else {
    this_params[[sect]][[param]]$Value <- opt_val
  }
}


