#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#summarise and make charts of randomized optimizer runs

#local
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
maxiter <- 10
version <- "c"
selection <- "v3"
base_exp <- 10 #change if you have done any other experiment

#run <- 1
#expID <- "10"

#eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
# maxiter <- 10
# version <- "d"
# selection <- "v4"
# base_exp <- 31 #change if you have done any other experiment


####list of seeds to randomise parameter list
set.seed(512)
seeds <- c(NA,sample(1:9999,20))
#seeds <- c(NA)

expIDs <- c(base_exp:((base_exp-1)+length(seeds)))
expIDs[which(expIDs<10)] <- paste("0",expIDs,sep="")
expIDs <- paste(expIDs)

#list of runs to be performed
runs_ref <- data.frame(SID=1:length(seeds),SEED=seeds,EXPID=expIDs)
runs_ref2 <- expand.grid(SID=runs_ref$SID,RUN=1:5)
runs_ref <- merge(runs_ref2,runs_ref,by="SID",all=T,sort=F)


#for a given zone
#load parameter response from output.RData in last iteration
#load parameter response of all other seeds
#plot each parameter response in a different graph









