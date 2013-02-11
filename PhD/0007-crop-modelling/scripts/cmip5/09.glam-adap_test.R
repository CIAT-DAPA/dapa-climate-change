#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

library(raster)

#source directories
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

source(paste(src.dir,"/cmip5/09.glam-adap_test-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
adap_name <- "cmip5_adapt"
maxiter <- 15 #to grab last optim values
scratch <- "/scratch/eejarv"
#scratch <- "~/Workspace"

use_scratch <- T

#base and data directories
#bDir <- "W:/eejarv/PhD-work/crop-modelling"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
#bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
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
gcmList <- gcmList[1]

##########################
#all processes
all_proc <- expand.grid(LOC=cells$CELL,GCM=gcmList,PARSET=expSel,WTH_TYPE=inList,
                        CO2_P=CO2ExpList)
all_proc <- cbind(RUNID=1:nrow(all_proc),all_proc)
all_proc$RUNID <- paste("RCP_",all_proc$RUNID+1e8,sep="")

#############################################################################
#############################################################################
#############################################################################
## experiments of adaptation

#parameters i have to look at are:

#1. TE and TEN_MAX. TEN_MAX should not exceed 7 g/kg
#2. DHDT, without exceeding a maximum EOS HI of 0.66
#3. SLA (in theory), but 300 cm2/g is already in the upper bound of range, so maybe unrealistic
#4. thermal time from planting to flowering
#   a. increases should be beneficial for well-watered
#   b. decreases should be beneficial for water-stressed (if TT of pod fill is increased)
#5. increase thermal time from start of pod filling to maturity (GCPFLM+GCLMPHA)
#6. increase thermal time from flowering to LMAX (GCFLPF+GCPFLM) to get better LAI
#7. HTS


##
#durations beyond 150 days not to be considered when increases in thermal time were done
#runs with final HI values above >0.66 not to be considered

#some discussion:
#escape to terminal drought stress can be achieved through shortening crop duration
#so the question is whether or not avoiding TDS will be more benefficial than the
#resulting decrease in net assimilate production caused by reduction in cropping
#cycle


#other strategies: Reddy et al. (2003) report rainfall as the main constraint for gnut
#production. Possible analyses.

#1. fully irrigated run
#2. targeted irrigation. Basu and Ghosh (1996) at 
#   http://agropedialabs.iitk.ac.in/openaccess/sites/default/files/RA%2000287_0.pdf#page=39
#   state that:
#   a. protective irrigation (targeted to flowering and pod filling) can increase yields by 33-63%
#   b. pre-monsoon (15-30 days advance) sowing with 1-2 times initial irrigation can 
#      increase yields by 20%
#

#for discussion Narayanamoorthy (http://nrlp.iwmi.org/PDocs/DReports/Phase_01/12.%20Water%20Savings%20Technologies%20-%20Narayanmoorthy.pdf)
#indicate the potential of drip and sprinkler irrigation in India

#it may be worth trying:
# a. fully irrigated run
# b. irrigation during flowering+podfill stage (modify GLAM so that 
#    POT=T when after flowering)


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
#ENV_CFG$OUT_BDIR <- paste("~/PhD-work/crop-modelling/model-runs/",toupper(ENV_CFG$CROP_NAME),"/adapt/",ENV_CFG$ADAP_NAME,sep="") #local L2
ENV_CFG$OUT_BDIR <- paste(ENV_CFG$BDIR,"/model-runs/",toupper(ENV_CFG$CROP_NAME),"/adapt/",ENV_CFG$ADAP_NAME,sep="") #eljefe
ENV_CFG$ADAP_RUNS <- adap_runs
#ENV_CFG$ALT_BIN <- "~/PhD-work/crop-modelling/model-runs/bin" #local L2

#grouping list
#do some type of grouping. This is done by gridcell, parameter set and GCM
groupingList <- expand.grid(LOC=cells$CELL,PARSET=expSel,GCM=gcmList)
#this is 125970 processes

###########
### details of testing runs
#run_config <- get_cfg_adap(170,all_proc)
#run testing
#system.time(glam_adap_run_wrapper(run_config))
#timall <- run_group_adap(976)

# preliminary runs while other stuff finishes up
# lim_a <- 1951
# lim_b <- 2145
# 
# for (k in lim_a:lim_b) {
#   tima <- run_group_adap(k)
#   tima <- sum(tima)/60
#   cat("XXXXXXXXXXXXXXXXXXXXXX\n")
#   cat("TIME ELAPSED:",tima,"\n")
#   cat("XXXXXXXXXXXXXXXXXXXXXX\n")
# }
#currently eljefe1

#############################################################################
## parallel processing ######################################################
#############################################################################
#number of cpus to use
if (nrow(groupingList) > 15) {ncpus <- 15} else {ncpus <- nrow(groupingList)}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("cropName")
sfExport("ver")
sfExport("runs_name")
sfExport("adap_name")
sfExport("maxiter")
sfExport("scratch")
sfExport("use_scratch")
sfExport("bDir")
sfExport("glamDir")
sfExport("cropDir")
sfExport("glamInDir")
sfExport("cells")
sfExport("gcmList")
sfExport("expSel")
sfExport("inList")
sfExport("CO2ExpList")
sfExport("all_proc")
sfExport("ENV_CFG")
sfExport("groupingList")
sfExport("adap_runs")

#run the bias correction function in parallel
system.time(sfSapply(as.vector(1:19500),run_group_adap))

#stop the cluster
sfStop()


