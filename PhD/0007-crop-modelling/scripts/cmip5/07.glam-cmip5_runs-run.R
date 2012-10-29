#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012

library(raster)

#source directories
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#source functions of interest
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir,"/cmip5/07.glam-cmip5_runs-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
maxiter <- 15 #to grab last optim values
plot_all <- F #do i want to plot (not for eljefe)
scratch <- "/scratch/eejarv"
use_scratch <- T

#base and data directories
#bDir <- "W:/eejarv/PhD-work/crop-modelling"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
ascDir <- paste(glamInDir,"/ascii",sep="")
sowDir <- paste(ascDir,"/sow",sep="")

#weather data directories
wthDir_his_rw <- paste(glamInDir,"/ascii/wth-cmip5_hist",sep="")
wthDir_rcp_rw <- paste(glamInDir,"/ascii/wth-cmip5_rcp45",sep="")
wthDir_his_bc <- paste(ascDir,"/wth-cmip5_hist_bc",sep="")
wthDir_rcp_bc <- paste(ascDir,"/wth-cmip5_rcp45_bc",sep="")

############################################
#here you need to put the permutation factors
#Factor 1: Gridcells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))
irrData <- get_ir_vls(cropDir,cells,1966,1993)

#Factor 2: GCMs
gcmList_his <- list.files(wthDir_his_rw,pattern="_ENS_")
gcmList_rcp <- list.files(wthDir_rcp_rw,pattern="_ENS_")
gcmList <- gcmList_his[gcmList_his %in% gcmList_rcp]

#Factor 3: parameter sets
parsetList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
parsetList <- parsetList[which(parsetList$ISSEL==1),]
parsetList <- parsetList$EXPID

#Factor 4: experiments
#conditionals may be needed inside the final function
expList_his <- c("his_allin","his_norain","his_notemp","his_nosrad","his_bcrain")
expList_rcp <- c("rcp_allin","rcp_bcrain")

#Factor 5: planting date (30 days around baseline value)
#only relevant for rcp experiments
sdList <- c(-5:5)

#Factor 6: values of parameters for increased CO2
#following Challinor and Wheeler (2008a,b), changes in the following parameters
#are introduced: TTMAX, TE, SLA and TENFAC. This means
#
#under doubled CO2:
#a. TTMAX needs to be always 17% below the calibrated value
#b. TE can be 24% or 40% aboive calibrated value
#c. SLA can be 0% or 10% below calibrated value
#d. TENFAC can be 0.0 or 0.4

#under 2035 RCP4.5 climate, factors were estimated using
#linear interpolation with CO2 concentrations as predictors
#mean 2021-2035 CO2 concentrations derived from Meinshausen et al. (2012)
#http://edoc.gfz-potsdam.de/pik/get/5095/0/0ce498a63b150282a29b729de9615698/5095.pdf

#These calculations assume an increase in CO2 from 330ppm (baseline) to 451 ppm
#in 2035

#a. TTMAX reduction -6.23%
#b. TE increase to 8.80% or 14.67%
#c. SLA reduction -3.67%
#d. TENFAC 0.0 or 0.4

#experiments under 2035 RCP4.5 climate are thus
#TENFAC=0.0, SLA=SLA_B*0.9633, TE=TE_B*1.0880, TTMAX=TTMAX_B*.9377
#TENFAC=0.0, SLA=SLA_B*0.9633, TE=TE_B*1.1467, TTMAX=TTMAX_B*.9377
#TENFAC=0.4, SLA=SLA_B*0.9633, TE=TE_B*1.0880, TTMAX=TTMAX_B*.9377
#TENFAC=0.4, SLA=SLA_B*0.9633, TE=TE_B*1.1467, TTMAX=TTMAX_B*.9377

CO2Exp <- data.frame(EXP_NAME=c("CO2_p1","CO2_p2","CO2_p3","CO2_p4"),
                     TENFAC=c(0,0,0.4,0.4),SLA_INI=c(0.9633,0.9633,0.9633,0.9633),
                     TE=c(1.0880,1.1467,1.0880,1.1467),
                     P_TRANS_MAX=c(0.9377,0.9377,0.9377,0.9377))
CO2ExpList <- CO2Exp$EXP_NAME

#in increased CO2 experiments, parameters to be changed:
#IC02 = 1
#B_TE = baseline TE
#B_TEN_MAX = baseline TEN_MAX
#TEN_MAX = baseline value, this will be re-calculated in the simulation
#those in the table

####
#baseline experiments
all_proc_his <- expand.grid(LOC=cells$CELL,GCM=gcmList,PARSET=parsetList,WTH_TYPE=expList_his)
all_proc_his$CO2_P <- "1xCO2"
all_proc_his <- cbind(RUNID=1:nrow(all_proc_his),all_proc_his)
all_proc_his$RUNID <- paste("HIS_",all_proc_his$RUNID+1e8,sep="")

#rcp experiments
all_proc_rcp <- expand.grid(LOC=cells$CELL,GCM=gcmList,PARSET=parsetList,WTH_TYPE=expList_rcp,
                            CO2_P=CO2ExpList)
all_proc_rcp <- cbind(RUNID=1:nrow(all_proc_rcp),all_proc_rcp)
all_proc_rcp$RUNID <- paste("RCP_",all_proc_rcp$RUNID+1e8,sep="")

#all runs together
all_proc <- rbind(all_proc_his,all_proc_rcp)

#variable ENV_CFG
ENV_CFG <- list()
ENV_CFG$SRC.DIR <- src.dir
ENV_CFG$BDIR <- glamDir
ENV_CFG$CROP_NAME <- cropName
ENV_CFG$VER <- ver
ENV_CFG$MAXITER <- maxiter
ENV_CFG$PLOT_ALL <- plot_all
ENV_CFG$RUNS_NAME <- runs_name
ENV_CFG$SCRATCH <- paste(scratch,"/",ENV_CFG$RUNS_NAME,sep="")
ENV_CFG$USE_SCRATCH <- use_scratch
ENV_CFG$CELLS <- cells
ENV_CFG$IRR_DATA <- irrData
ENV_CFG$OPT_METHOD <- "CH07"
ENV_CFG$OUT_BDIR <- paste(ENV_CFG$BDIR,"/model-runs/",toupper(ENV_CFG$CROP_NAME),"/runs/",ENV_CFG$RUNS_NAME,sep="")

##########################################################
# testing and running
#i <- 503881 # 1 125971 251941 377911 503881 #for hist
#i <- 881791 # 629851 755821 881791 #for rcp

#do some type of grouping. This is done by gridcell, parameter set and GCM
groupingList <- expand.grid(LOC=cells$CELL,PARSET=parsetList,GCM=gcmList)
#this is 125970 processes

#with 31 sowing dates
#time 1 run = ~695 sec
#total 87549150 sec = 1459153 min = 24319.22 hr = 1013.30 days
#in 32 processors, total = 31 days

#with 15 sowing dates
#time 1 run = ~460 sec
#total 57946200 sec = 965770 min = 16096.17 hr = 670.67 days
#in 32 processors, total = 21 days

#with 11 sowing dates
#time 1 run = ~408 sec
#total 51395760 sec = 856596 min = 14276.6 hr = 594.86 days
#in 32 processors, total = 18 days

tll1 <- run_group_his_rcp(1)





