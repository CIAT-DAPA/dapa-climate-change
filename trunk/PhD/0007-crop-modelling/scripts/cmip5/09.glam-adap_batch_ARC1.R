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

#should have read lim_a gcm_id and exp_id

#load library
require(raster)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/cmip5/09.glam-adap_test-functions_v2_ARC1.R",sep=""))
#source(paste(src.dir,"/cmip5/09.glam-adap_test-functions.R",sep="")) #original!

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
adap_name <- "cmip5_adapt"
maxiter <- 15 #to grab last optim values
scratch <- "/scratch/eejarv"
use_scratch <- T
if (use_scratch & !file.exists(scratch)) {dir.create(scratch,recursive=T)}

#base and data directories
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir <- paste(cropDir,"/runs/",runs_name,sep="")
adapDir <- paste(cropDir,"/adapt",sep="")
localcopy <- paste("~/workspace/localcopy/copy_arc1_",gcm_id,"_",exp_id,"_",lim_a,sep="")

#load required base information
load(file=paste(localcopy,"/arc1_data.RData",sep=""))

#experimental set up
inList <- c("allin","bcrain","sh","del")
CO2ExpList <- c("CO2_p1","CO2_p2","CO2_p3","CO2_p4")

#parameter set
expSel <- exp_id

#GCM
gcmList <- gcm_id

##########################
#all processes
all_proc <- expand.grid(LOC=cells$CELL,GCM=gcmList,PARSET=expSel,WTH_TYPE=inList,
                        CO2_P=CO2ExpList)
all_proc <- cbind(RUNID=1:nrow(all_proc),all_proc)
all_proc$RUNID <- paste("RCP_",all_proc$RUNID+1e8,sep="")


###### configuration
#variable ENV_CFG
ENV_CFG <- list()
ENV_CFG$ARCONE.DIR <- getwd()
ENV_CFG$LOCALCOPY <- localcopy
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
cat("Processing ",lim_a,"XXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("GCM:",paste(gcmList),"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")

k <- which(groupingList$LOC == lim_a)
cat("\n",paste(as.vector(groupingList[k,])),"\n")

#loop the runs
tima <- run_group_adap(k)
tima <- sum(tima)/60
cat("XXXXXXXXXXXXXXXXXXXXXX\n")
cat("TIME ELAPSED:",tima,"\n")
cat("XXXXXXXXXXXXXXXXXXXXXX\n")
#end



