#JRV Jul 2013
#process Andean occurrence data
stop("dont run")

##############################
#Procedure to follow
##############################
#
#1. 

#loop cross-val 1:10 (with specified seed)
#5. run without cross-validation (but do the cross validation independently so as
#   to be able to use Hijmans 2012 Ecology ssb AUC correction)
#6. run a particular algorithm, evaluate, store eval output
#   configuration in Maxent: probably 10k PA)
#end loop cross-val

##############################
##############################

#load relevant package(s)
library(biomod2); library(raster); library(rgdal); library(maptools); library(dismo)

#base dir
bDir <- "/nfs/a102/eejarv/DNP-biodiversity"
#bDir <- "/mnt/a102/eejarv/DNP-biodiversity"
setwd(bDir)

#source functions
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/dnp-sdm"
#src.dir <- "~/Repositories/dapa-climate-change/trunk/dnp-sdm"
source(paste(src.dir,"/scripts/modelproj-fun.R",sep=""))

#lists of variables
varList <- data.frame(SET_ID=1:8,CLIM=c(rep("full",times=4),rep("subset",times=4)),
                      SOIL=c(F,T,F,T,F,T,F,T),TOPO=c(F,F,T,T,F,F,T,T))

#seeds to draw presences and pseudo absences (bootstraps)
seedList <- c(3379, 5728, 3781, 3590, 3266, 9121, 3441, 11667, 4484, 9559)

#list of number of PA selections
npaList <- c(3829, 1922, 1945, 5484, 2125, 8746, 2187, 1521, 9623, 1561)

#list of models
modList <- c('GLM','GAM','GBM','RF','ANN','MAXENT')

#experimental matrix
all_runs <- expand.grid(ALG=modList,NPA=npaList,VSET=varList$SET_ID,SEED=seedList)
all_runs <- cbind(ID=paste("SDM_",1:nrow(all_runs),sep=""),all_runs)

#projection scenarios
sceList <- c("baseline","sres_a1b","sres_a2","sres_b1")
mList <- data.frame()
for (i in 1:length(sceList)) {
  if (i == 1) {
    m1 <- expand.grid(SCE=sceList[i],PERIOD="1950_2000",
                      MODEL=list.files(paste(bDir,"/env-data/proj_COL/",sceList[i],"/Global_2_5min",sep="")))
  } else {
    m1 <- expand.grid(SCE=sceList[i],PERIOD=c("2020_2049","2040_2069","2070_2099"),
                      MODEL=list.files(paste(bDir,"/env-data/proj_COL/",sceList[i],"/Global_2_5min",sep="")))
  }
  mList <- rbind(mList,m1)
  rm(mq)
}
mList <- cbind(ID=paste("CLM_",1:nrow(mList),sep=""),mList)

#species name and configuration of run
this_sppName <- "Jaca_cauc" #species name

mod <- "GLM"
truns <- all_runs$ID[which(all_runs$ALG == mod)]

#actual model runs
for (run_i in truns) {
  #run_i <- truns[1] #23
  this_seed <- as.numeric(paste(all_runs$SEED[which(all_runs$ID == run_i)])) #seed for the cross validation
  this_npa <- as.numeric(paste(all_runs$NPA[which(all_runs$ID == run_i)])) #number of pseudo absences (from list)
  this_alg <- paste(all_runs$ALG[which(all_runs$ID == run_i)]) #modelling algorithm
  this_vset <- as.numeric(paste(all_runs$VSET[which(all_runs$ID == run_i)])) #set of variables to use
  
  for (prj in 1:nrow(mList)) {
    #prj <- 1
    tsce <- paste(mList$SCE[prj])
    tper <- paste(mList$PERIOD[prj])
    tmodel <- paste(mList$MODEL[prj])
    
    odir <- proj_model(bDir,sppName=this_sppName,seed=this_seed,npa=this_npa,alg=this_alg,
                       vset=this_vset,scenario=tsce,period=tper,model=tmodel)
  }
}








