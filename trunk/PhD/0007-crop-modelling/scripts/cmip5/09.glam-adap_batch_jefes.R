#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

################################################################################
#get the command line arguments
args=(commandArgs(TRUE))

#evaluate the arguments
for(i in 1:length(args)) {
  eval(parse(text=args[[i]]))
}

#should have read lim_a lim_b and gcm_id

#load library
require(raster)

#source functions
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/cmip5/09.glam-adap_test-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
adap_name <- "cmip5_adapt"
maxiter <- 15 #to grab last optim values
scratch <- "/scratch/eejarv"
use_scratch <- T

#base and data directories
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir <- paste(cropDir,"/runs/",runs_name,sep="")
adapDir <- paste(cropDir,"/adapt",sep="")

#load grid cells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#experimental set up
inList <- c("allin","bcrain","sh","del")
CO2ExpList <- c("CO2_p1","CO2_p2","CO2_p3","CO2_p4")

#load list of parameter sets
expList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expSel <- expList$EXPID[which(expList$ISSEL == 1)]

#list of GCMs
gcmList <- list.files(paste(runsDir,"/exp-33_outputs",sep=""),pattern="_ENS_")
gcmList <- gcmList[gcm_id]

##########################
#all processes
all_proc <- expand.grid(LOC=cells$CELL,GCM=gcmList,PARSET=expSel,WTH_TYPE=inList,
                        CO2_P=CO2ExpList)
all_proc <- cbind(RUNID=1:nrow(all_proc),all_proc)
all_proc$RUNID <- paste("RCP_",all_proc$RUNID+1e8,sep="")

#load experiments setup
adap_runs <- read.table(paste(adapDir,"/data/adapt.tab",sep=""),sep="\t",header=T)

###### configuration
#variable ENV_CFG
ENV_CFG <- list()
ENV_CFG$SRC.DIR <- src.dir
ENV_CFG$BDIR <- glamDir
ENV_CFG$CROP_NAME <- cropName
ENV_CFG$VER <- ver
ENV_CFG$MAXITER <- maxiter
ENV_CFG$RUNS_NAME <- runs_name
ENV_CFG$ADAP_NAME <- adap_name
ENV_CFG$SCRATCH <- paste(scratch,"/",ENV_CFG$ADAP_NAME,sep="")
ENV_CFG$USE_SCRATCH <- use_scratch
ENV_CFG$CELLS <- cells
ENV_CFG$IRR_DATA <- NA
ENV_CFG$OPT_METHOD <- NA
ENV_CFG$OUT_BDIR <- paste(ENV_CFG$BDIR,"/model-runs/",toupper(ENV_CFG$CROP_NAME),"/adapt/",ENV_CFG$ADAP_NAME,sep="") #eljefe
ENV_CFG$ADAP_RUNS <- adap_runs

#grouping list
groupingList <- expand.grid(LOC=cells$CELL,PARSET=expSel,GCM=gcmList)

cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("Processing ",lim_a,"to",lim_b,"XXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("GCM:",paste(gcm),"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")


cat("\n",paste(as.vector(groupingList[lim_a,])),"\n")
cat(paste(as.vector(groupingList[lim_b,])),"\n")

# for (k in lim_a:lim_b) {
#   tima <- run_group_adap(k)
#   tima <- sum(tima)/60
#   cat("XXXXXXXXXXXXXXXXXXXXXX\n")
#   cat("TIME ELAPSED:",tima,"\n")
#   cat("XXXXXXXXXXXXXXXXXXXXXX\n")
# }
#currently L2

