#JRV May 2013
#process Andean occurrence data
stop("dont run")

###
#Calculate a geographically null model sensu Hijmans 2012 Ecology
###

##############################
#Procedure to follow
##############################
#
#1. species name
#2. load species data and pseudo absences
#3. bootstrap sample selection
#4. fit null model
#5. use ssb correction to assess model
##############################
##############################

#load relevant package(s)
library(raster); library(dismo); library(biomod2)

#base dir
bDir <- "/nfs/a102/eejarv/DNP-biodiversity"
#bDir <- "/mnt/a102/eejarv/DNP-biodiversity"
setwd(bDir)

#source functions
source(paste(bDir,"/scripts/modelfit-fun.R",sep=""))

#seeds to draw presences and pseudo absences (bootstraps)
seedList <- c(3379,5728,3781,3590,3266,9121,3441,11667,4484,9559)

#list of number of PA selections
npaList <- c(100, 250, 500, 750, 1000, 2000, 4000, 6000, 8000, 10000)

#experimental matrix
all_runs <- expand.grid(NPA=npaList,SEED=seedList)

#species name and configuration of run
this_sppName <- "bixa_final" #species name

#### 
#some testing runs
for (run_i in 1:100) {
  this_seed <- as.numeric(paste(all_runs$SEED[run_i])) #seed for the cross validation
  this_npa <- as.numeric(paste(all_runs$NPA[run_i])) #number of pseudo absences (from list)
  odir <- run_null_model(bDir,sppName=this_sppName,seed=this_seed,npa=this_npa)
}
###





