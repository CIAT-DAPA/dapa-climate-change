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
source(paste(src.dir,"/scripts/samplingbias-fun.R",sep=""))

#list of number of PA selections
npaList <- c(3829, 1922, 1945, 5484, 2125, 8746, 2187, 1521, 9623, 1561)

#list of models
modList <- c('GLM','GAM','GBM','RF','MAXENT')

#experimental matrix
all_runs <- expand.grid(ALG=modList,NPA=npaList)

#species name and configuration of run
this_sppName <- "Bixa_orel" #species name

#this is the loop for the runs
for (run_i in 1:nrow(all_runs)) {
  #run_i <- 4 #23
  this_npa <- as.numeric(paste(all_runs$NPA[run_i])) #number of pseudo absences (from list)
  this_alg <- paste(all_runs$ALG[run_i]) #modelling algorithm
  odir <- run_bias_model(bDir,sppName=this_sppName,npa=this_npa,alg=this_alg)
}


#projection of models
for (run_i in 1:nrow(all_runs)) {
  #run_i <- 1 #23
  this_npa <- as.numeric(paste(all_runs$NPA[run_i])) #number of pseudo absences (from list)
  this_alg <- paste(all_runs$ALG[run_i]) #modelling algorithm
  odir <- proj_model(bDir,sppName=this_sppName,npa=this_npa,alg=this_alg) 
}

set.seed(71)
iris2 <- iris
iris2$Petal.Width[which(iris$Petal.Width <= 1)] <- 1
iris2$Petal.Width[which(iris$Petal.Width > 1)] <- 0
#iris2$Petal.Width <- as.factor(iris2$Petal.Width)
iris.rf <- randomForest(Petal.Width ~ ., data=iris2, importance=TRUE,
                        proximity=TRUE,norm.votes = TRUE,ntree=100)
print(iris.rf)
prd <- as.numeric(predict(iris.rf,iris))


