#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012

################################################################################
#get the command line arguments
args=(commandArgs(TRUE))

#evaluate the arguments
for(i in 1:length(args)) {
  eval(parse(text=args[[i]]))
}
#should have read in something like
# g_ini=1000
# g_end=1100

cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("Processing ",g_ini,"to",g_end,"XXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")

library(raster)

#source directories
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
scratch <- "~/PhD-work/scratch"
use_scratch <- T

#base and data directories
bDir <- "~/PhD-work/crop-modelling"
glamDir <- bDir #paste(bDir,"/GLAM",sep="")
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
if (!file.exists(paste(cropDir,"/irrigated_ratio/irrData.RData",sep=""))) {
  irrData <- get_ir_vls(cropDir,cells,1966,1993)
  save(list=c("irrData"),file=paste(cropDir,"/irrigated_ratio/irrData.RData",sep=""))
} else {
  load(paste(cropDir,"/irrigated_ratio/irrData.RData",sep=""))
}

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
sdList <- c(-7:7)

#Factor 6: values of parameters for increased CO2
#following Challinor and Wheeler (2008a,b), 
CO2Exp <- data.frame(EXP_NAME=c("CO2_p1","CO2_p2","CO2_p3","CO2_p4"),
                     TENFAC=c(0,0,0.4,0.4),SLA_INI=c(0.9633,0.9633,0.9633,0.9633),
                     TE=c(1.0880,1.1467,1.0880,1.1467),
                     P_TRANS_MAX=c(0.9377,0.9377,0.9377,0.9377))
CO2ExpList <- CO2Exp$EXP_NAME

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

#load analysis objects
load(paste(ENV_CFG$OUT_BDIR,"/_config/config.RData",sep=""))

##########################################################
#model runs
##########################################################
#eljefe02 is running from 51871 to 71370
#first test with 71371 to 71375 -> succeeded
#second test with 71376 to 71377 -> succeeded
#first test of qsub with 71378 71380 -> succeeded in 39min (this is 3)

#send jobs of 90 groups
#j1: qsub run.sh 71371 71470
#j2: qsub run.sh 71471 71560
#j3: qsub run.sh 71561 71650
#j4: qsub run.sh 71651 71740
#j5: qsub run.sh
#j6: qsub run.sh

#send jobs of 1 processor and 24 hours

for (k in g_ini:g_end) {
 tll1 <- run_group_his_rcp(k)
}

#cd /home/ufaserv1_m/eejarv/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts/cmip5
#./07.glam-cmip5_runs-run_ARC1.sh 71371 71375


