#JRV May 2013
#process Andean occurrence data
stop("dont run")

##############################
#Procedure to follow
##############################

#1. use the all_runs data.frame to select a given run
#2. load model object
#3. load data frame (*_bg_2_5min.RData) in pseudo-absences folder
#4. extract all data that is in the model
#5. project the model (beware of interactions)
#6. assess the model in the same way that ecocrop was assessed
#7. load the imd_cru_climatology_1dd
#8. project the model (beware of interactions)
#9. assess the model in the same way ecocrop was assessed

##############################
##############################

#load relevant package(s)
library(biomod2); library(raster); library(rgdal);
library(maptools); library(dismo); library(usdm)

#source dir
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#source functions
source(paste(src.dir,"/scripts/niche_based/nb-09-sdm_modelproj-fun.R",sep=""))

#i/o directories
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/niche-based"
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")
bioDir <- paste(clmDir,"/bio_ind_30s",sep="")
solDir <- paste(envDir,"/soil",sep="")
modDir <- paste(bDir,"/models",sep="")
sdmDir <- paste(modDir,"/SDM",sep="")

#set working directory to base directory
setwd(bDir)

#lists of variables
varList <- data.frame(SET_ID=1:4,CLIM=c(rep("full",times=2),rep("subset",times=2)),
                      SOIL=c(F,T,F,T))

#seeds to draw presences and pseudo absences (bootstraps)
seedList <- c(3379,5728,3781,3590,3266)

#list of number of PA selections (seeds to make the selections)
npaList <- c(3893,2748,9121,2031,9559)

#list of models
modList <- c('GLM','GAM','GBM','ANN','MAXENT')

#experimental matrix
all_runs <- expand.grid(ALG=modList,NPA=npaList,SEED=seedList,VSET=varList$SET_ID)

#species name and configuration of run
this_sppName <- "gnut" #species name

#some testing runs
for (run_i in 1:nrow(all_runs)) {
  #run_i <- 1 #23
  this_seed <- as.numeric(paste(all_runs$SEED[run_i])) #seed for the cross validation
  this_npa <- as.numeric(paste(all_runs$NPA[run_i])) #number of pseudo absences (from list)
  this_alg <- paste(all_runs$ALG[run_i]) #modelling algorithm
  this_vset <- as.numeric(paste(all_runs$VSET[run_i])) #set of variables to use
  
  #run model
  odir <- proj_model(base_dir=sdmDir,env_dir=envDir,spp_name=this_sppName,
                    seed=this_seed,npa=this_npa,alg=this_alg,vset=this_vset)
  
}


