#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
#stop("!")

#outline
#1. load list of parameters
#2. create sample of parameter sets
#3. store the matrix of combinations as a data.frame
#4. put a given line into a parameter set
#5. use this parameter set for a model run

#src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
#source(paste(src.dir,"/ppe/dssat_hypercube.R",sep=""))

#load packages
library(lhs)

#source functions and stuff
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
source(paste(src.dir,"/dssat-utils/optimise.R",sep=""))
source(paste(src.dir,"/ppe/put_param_hyp.R",sep=""))

#choose model
csmodel <- "MZCER045"

#input directories
wd <- "~/Leeds-work/quest-for-robustness"
#wd <- "/nfs/a101/earjr/quest-for-robustness"
runs_dir <- paste(wd,"/crop_model_runs",sep="")
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
bin_dir <- paste(wd,"/bin/dssat/csm45_1_23_bin_gfort",sep="")
#bin_dir <- paste(wd,"/bin/dssat/csm45_1_23_bin_ifort",sep="")
hyp_dir <- paste(runs_dir,"/dssat_",csmodel,"_hypercube",sep="")
if (!file.exists(hyp_dir)) {dir.create(hyp_dir)}

#load objects (initial conditions and yields)
load(paste(mdata_dir,"/initial_conditions_major_dssat.RData",sep=""))
load(paste(mdata_dir,"/yield_major_dssat.RData",sep=""))

#select ME and its corresponding initial data
me_list <- unique(xy_main$ME_NEW)
me_sel <- me_list[1]
xy_me <- xy_main[which(xy_main$ME_NEW == me_sel),]

#me-specific dir
me_hdir <- paste(hyp_dir,"/run_me-",me_sel,sep="")
if (!file.exists(me_hdir)) {dir.create(me_hdir)}

#ensure SAT is not below DUL for all 8 soil layers
for (sl in 1:8) {
  #sl <- 1
  corr_loc <- xy_me$LOC[which(round(xy_me[paste("SSAT_",sl,sep="")],3) <= round(xy_me[paste("SDUL_",sl,sep="")],3))]
  corr_fac <- mean((xy_me[-which(round(xy_me[paste("SSAT_",sl,sep="")],3) <= round(xy_me[paste("SDUL_",sl,sep="")],3)),paste("SSAT_",sl,sep="")]-xy_me[-which(round(xy_me[paste("SSAT_",sl,sep="")],3) <= round(xy_me[paste("SDUL_",sl,sep="")],3)),paste("SDUL_",sl,sep="")]),na.rm=T)
  
  xy_me[which(xy_me$LOC %in% corr_loc),paste("SSAT_",sl,sep="")] <- xy_me[which(xy_me$LOC %in% corr_loc),paste("SDUL_",sl,sep="")] + corr_fac
  xy_main[which(xy_main$LOC %in% corr_loc),paste("SSAT_",sl,sep="")] <- xy_me[which(xy_me$LOC %in% corr_loc),paste("SSAT_",sl,sep="")]
}

###
#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_list <- read.csv(paste(mdata_dir,"/parameter_list_dssat.txt",sep=""),sep="\t",header=T)
param_list <- param_list[which(param_list$MODEL == csmodel),] #select those relevant to model

#2. create sample of parameter sets
#n=number of points (i.e. replicas) --how many values in each dimension?
#k=number of dimensions (i.e. parameters) --how many dimensions (i.e. parameters)?
nrep <- 10000
set.seed(2303)
if (!file.exists(paste(mdata_dir,"/dssat_",csmodel,"_lhs.RData",sep=""))) {
  lhyp1 <- maximinLHS(n=nrep,k=nrow(param_list),dup=1)
  save(list=c("lhyp1"),file=paste(mdata_dir,"/dssat_",csmodel,"_lhs.RData",sep=""))
} else {
  load(file=paste(mdata_dir,"/dssat_",csmodel,"_lhs.RData",sep=""))
}

#3. generate the values in the actual parameter range and put into data.frame
out_df <- as.data.frame(matrix(NA, nrow=nrep, ncol=nrow(param_list)))
names(out_df) <- param_list$PARAM
for (i in 1:nrow(param_list)) {
  #i <- 1
  pvals <- qunif(lhyp1[,i],min=param_list$MIN[i],max=param_list$MAX[i])
  out_df[,i] <- pvals
}

#because snow has a limit in number of workers, i decided to drive using two different machines
driver <- Sys.info()[["nodename"]]
if (driver == "eljefe") {
  socket_list <- c(rep("localhost",30),rep("foe-linux-01",20),rep("foe-linux-02",20))
  hrun_list <- 1:(nrow(out_df)/2)
} else if (driver == "lajefa") {
  socket_list <- c(rep("localhost",30),rep("foe-linux-03",20),rep("foe-linux-04",20))
  hrun_list <- (nrow(out_df)/2+1):nrow(out_df)
}

#full list of locations
loc_list <- xy_me$LOC

###
#Note: from here onwards the process can be parallelised
#4. get a given line into a parameter set
for (hrun in hrun_list) {
  #hrun <- 1
  cat("\n------------------------------------\n")
  cat("\n...processing hypercube run=",hrun,"\n")
  cat("\n------------------------------------\n")
  
  #get parameter set
  cul_params <- put_culpar(plist_in=out_df[hrun,], model=csmodel)
  eco_params <- put_ecopar(plist_in=out_df[hrun,], model=csmodel)
  spe_params <- put_spepar(params=get_spepar(paste(bin_dir,"/MZCER045.SPE",sep="")),
                           plist_in=out_df[hrun,], param_list)
  xfi_params <- put_xfile(params=get_xfile_dummy(), plist_in=out_df[hrun,], param_list)
  xfi_params$sim_ctrl$VBOSE <- "0" #write only Summary.OUT outputs (as needed)
  
  #create hrun dir
  out_hdir <- paste(me_hdir,"/calib_",hrun,sep="")
  if (!file.exists(out_hdir)) {dir.create(out_hdir)}
  
  #check what has been done already
  loc_sel <- loc_list
  for (tloc in loc_list) {
    #tloc <- loc_list[1]
    save_file <- paste(out_hdir,"/calib-",hrun,"_loc-",tloc,".RData",sep="")
    if (file.exists(save_file)) {loc_sel <- loc_sel[-which(loc_sel == tloc)]}
  }
  
  #create meteorology for selected grid cells (DSSAT)
  for (tloc in loc_sel) {
    #tloc <- loc_sel[1]
    #filename
    yri <- substr(paste(1980),3,4); nyrs <- length(1980:2001)
    wthfile <- paste(met_dir,"/ascii_extract_raw/obs_hist_WFD/loc-",tloc,"/AFRB",yri,nyrs,"_loc-",tloc,".WTH",sep="")
    
    #write file
    if (!file.exists(wthfile)) {
      cat("...making weather for loc=",tloc,"\n")
      x <- xy_me$x[which(xy_me$LOC == tloc)]
      y <- xy_me$y[which(xy_me$LOC == tloc)]
      elev <- xy_me$ELEV[which(xy_me$LOC == tloc)]
      fildir <- make_wth(x=data.frame(CELL=tloc,X=x,Y=y,ELEV=elev),
                         wthDir_in=paste(met_dir,"/ascii_extract_raw/obs_hist_WFD",sep=""),
                         wthDir_out=paste(met_dir,"/ascii_extract_raw/obs_hist_WFD/loc-",tloc,sep=""),
                         years=1980:2001,
                         fields=list(CELL="CELL",X="X",Y="Y",ELEV="ELEV"),
                         out_file=NA)
    }
  }
  
  #5. use this parameter set to calibrate the model for all locations in parallel
  run_hyp_loc <- function(iloc) {
    if (driver %in% c("eljefe","lajefa")) {system("renice 19 -u earjr",ignore.stdout=T)}
    
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
    source(paste(src.dir,"/dssat-utils/optimise.R",sep=""))
    source(paste(src.dir,"/ppe/put_param_hyp.R",sep=""))
    
    #select location
    loc <- loc_sel[iloc]
    
    #create object for optimisation
    cal_data <- list()
    cal_data$MODEL <- csmodel
    cal_data$BASENAME <- "AFRB" #basename of runs
    cal_data$BASE_DIR <- out_hdir
    cal_data$BIN_DIR <- bin_dir
    cal_data$WTH_DIR <- paste(met_dir,"/ascii_extract_raw",sep="") #for reading .wth files
    cal_data$WTH_ROOT <- "obs_hist_WFD"
    cal_data$LOC <- loc
    cal_data$ISYR <- 1980 #1 year before GLAM's because of spin-up year needs in CSM
    cal_data$IEYR <- 2001 #1 extra year so as to include in wth file (but this year won't be run)
    cal_data$INI_COND <- xy_main
    cal_data$YLD_DATA <- xy_main_yield
    cal_data$CUL <- cul_params
    cal_data$ECO <- eco_params
    cal_data$SPE <- spe_params
    cal_data$XFILE <- xfi_params
    cal_data$SIM_NAME <- paste("calib-",hrun,"_loc-",loc,sep="")
    cal_data$METHOD <- "RMSE"
    cal_data$USE_SCRATCH <- T
    
    #scratch in /scratch or in /dev/shm
    if (driver %in% c("eljefe","lajefa")) {
      if (loc%%2 == 0) {
        cal_data$SCRATCH <- "/scratch/earjr"
      } else {
        cal_data$SCRATCH <- "/dev/shm/earjr"
      }
    } else {
      cal_data$SCRATCH <- "~/workspace/scratch/earjr"
    }
    
    if (!file.exists(paste(cal_data$BASE_DIR,"/",cal_data$SIM_NAME,".RData",sep=""))) {
      #run calibration
      slpf_calib <- DSSAT_calibrate(cal_data)
      
      #save output object
      save(slpf_calib, file=paste(cal_data$BASE_DIR,"/",cal_data$SIM_NAME,".RData",sep=""))
    }
    
    #remove junk / scratch as needed
    if (cal_data$USE_SCRATCH) {system(paste("rm -rf ",cal_data$SCRATCH,"/",cal_data$SIM_NAME,sep=""))}
    
    #return object
    rdata_fil <- paste(cal_data$BASE_DIR,"/",cal_data$SIM_NAME,".RData",sep="")
    return(rdata_fil)
  }
  
  
  #if there are param*seeds still not fully done then run the opt function in parallel
  if (length(loc_sel) > 0) {
    require(snowfall)
    
    #parallelisation
    sfInit(parallel=T,cpus=70,socketHosts=socket_list,type="SOCK")
    sfExport(list=c("loc_sel","bin_dir","mdata_dir","met_dir","xy_me","xy_main","xy_main_yield"))
    sfExport(list=c("csmodel","me_sel","param_list","src.dir","me_hdir","out_hdir","wd","cul_params"))
    sfExport(list=c("eco_params","spe_params","xfi_params","run_hyp_loc","driver","hrun"))
    run_steps <- sfSapply(as.vector(1:length(loc_sel)), run_hyp_loc)
    sfStop()
  }
}

