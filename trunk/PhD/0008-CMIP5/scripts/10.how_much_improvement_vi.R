#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2013

library(ggplot2)

#CMIP5 skill analyses
#7. Summarise the results of mean climate skill analyses

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"


src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
mdDir <- "/mnt/a102/eejarv/CMIP5"

#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir2,"/scripts/10.how_much_improvement-functions.R",sep=""))

#directories
cmip5Dir <- paste(mdDir,"/assessment/output-data/_summary_revised2",sep="") #ERL revision
cmip3Dir <- paste(mdDir,"/assessment/output-data-cmip3/_summary",sep="") #ERL revision

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
vnList <- c("pr","tas","dtr","rd")
procList <- expand.grid(GCM=gcmList,VAR=vnList)

#list regions
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
isoList <- regions$ISO

#output directory
outputDir_c5 <- paste(mdDir,"/assessment/output-data",sep="")
outputDir_c3 <- paste(mdDir,"/assessment/output-data-cmip3",sep="")


#get the data
if (!file.exists(paste(cmip5Dir,"/vi-summary_final.RData",sep=""))) {
  all_mets <- lapply(1:nrow(procList),summarise_proc_c5)
  all_mets <- do.call("rbind", all_mets)
  save(list=c("all_mets"),file=paste(cmip5Dir,"/vi-summary_final.RData",sep=""))
} else {
  load(file=paste(cmip5Dir,"/vi-summary_final.RData",sep=""))
}







