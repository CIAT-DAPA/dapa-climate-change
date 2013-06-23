#JRV May 2013
#process Andean occurrence data
stop("dont run")

##############################
#Procedure to follow
##############################

#1. select pseudo-absences with likelihood depending on percent area cultivated. 
#2. number of pseudo-absences is a less interesting question than regularisation. use 10 times more pseudo-absences than presences * 1.25 so as to include the testing percentage.
#3. use: maxent, gbm, glm, gam(?), ann(?)
#4. repeat 5 times for different background selections.
#5. repeat 5 times for different 75/25 train/test selections
#6. create additional predictors using interactions: prec*temp, sindex*temp, soil*rain, 
#   soil*sindex
#   6.a for maxent: use only linear, quadratic, and hinge features
#7. variables should be minimum collinearity and full set (each with and without soil)

###
#This is: 5 (pa) * 5 (folds) * 4 (variables) * 3 (or 5 techniques) = 300 (or 500) models
#Remove models: AUCc is very low (<0.7)
#Do all analyses using remaining models. Suit vs. yield relationships use an ensemble of 
#models (for each technique), weighted by AUC ranking and not weighted.
###

#
#1. species name
#2. load species data
#3. load background data.frame (create from shapefile if doesn't exist)
#4. using seed select training species data AND training pseudo absences
#   number of PA = a given number ()
#5. select particular combination of variables
#   a. subset climate
#   b. subset climate + soils
#   c. subset climate + topography
#   d. subset climate + soils + topography
#   e. full climate
#   f. full climate + soils
#   g. full climate + topography
#   h. full climate + soils + topography

#loop cross-val 1:10 (with specified seed)
#5. run without cross-validation (but do the cross validation independently so as
#   to be able to use Hijmans 2012 Ecology ssb AUC correction)
#6. run a particular algorithm, evaluate, store eval output
#   configuration in Maxent: probably 10k PA)
#end loop cross-val

##############################
##############################

#load relevant package(s)
library(biomod2); library(raster); library(rgdal);
library(maptools); library(dismo); library(usdm)

#source dir
src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#source functions
source(paste(src.dir,"/scripts/niche_based/nb-08-sdm_modelfit-fun.R",sep=""))

#i/o directories
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
#bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
occDir <- paste(bDir,"/occurrences",sep="")
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")
bioDir <- paste(clmDir,"/bio_ind_30s",sep="")
intDir <- paste(envDir,"/crop_intensity",sep="")
solDir <- paste(envDir,"/soil",sep="")
mskDir <- paste(envDir,"/mask",sep="")
modDir <- paste(bDir,"/models",sep="")
sdmDir <- paste(modDir,"/SDM",sep="")

vifDir <- paste(sdmDir,"/input-samples",sep="")
if (!file.exists(vifDir)) {dir.create(vifDir,recursive=T)}

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
occFile <- paste(occDir,"/",this_sppName,"-india_final.csv",sep="")

#some testing runs
for (run_i in 1:5) {
  #run_i <- 1 #23
  this_seed <- as.numeric(paste(all_runs$SEED[run_i])) #seed for the cross validation
  this_npa <- as.numeric(paste(all_runs$NPA[run_i])) #number of pseudo absences (from list)
  this_alg <- paste(all_runs$ALG[run_i]) #modelling algorithm
  this_vset <- as.numeric(paste(all_runs$VSET[run_i])) #set of variables to use
  
  #create variable sets
  samples <- vif_analysis(spp_name=this_sppName,occ_file=occFile,bio_dir=bioDir,
                          vif_dir=vifDir,sol_dir=solDir)
  
  #run model
  odir <- run_model(base_dir=sdmDir,env_dir=envDir,spp_name=this_sppName,
                    seed=this_seed,npa=this_npa,alg=this_alg,vset=this_vset)
}

#changes introduced:
#------------------
#threshold in pwdSampling() to .33 #from Hijmans 2012 Ecology #done
#sp_mOpt@GLM$control$maxit <- 100 #because sometimes it shows warning of not-convergence #done
#sp_mOpt@GBM$n.trees <- 2000 #Braunisch et al. 2013 #done

#pending changes:
#----------------
#add a background size of 13333 so that training background is 10000 (Maxent's default) #pending
#70/30 instead of 75/25 for training/testing #pending

