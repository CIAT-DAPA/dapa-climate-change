#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

library(raster)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

source(paste(src.dir,"/cmip5/08.glam-summarise_cmip5_runs-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
maxiter <- 15 #to grab last optim values

#base and data directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir <- paste(cropDir,"/runs/",runs_name,sep="")

#load grid cells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#experimental set up
expList_his <- c("his_allin","his_norain","his_notemp","his_nosrad","his_bcrain")
expList_rcp <- c("rcp_allin","rcp_bcrain")
CO2ExpList <- c("CO2_p1","CO2_p2","CO2_p3","CO2_p4")
sdList <- c(-7:7)

#load list of parameter sets
expList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expSel <- expList$EXPID[which(expList$ISSEL == 1)]

#list of GCMs
gcmList <- list.files(paste(runsDir,"/exp-33_outputs",sep=""),pattern="_ENS_")

#list of variables of interest
varNames <- c("YGP","STG","DUR","TRADABS","TP_UP","T_TRANS","TP_TRANS","TOTPP",
              "TOTPP_HIT","TOTPP_WAT","LAI","HI","BMASS","YIELD")

i <- 1

#options
##########################################################################
######### BASELINE DATA
for (n in 1:length(expList_his)) {
  stat <- collate_his(cells,runsDir,gcm=gcmList[i],intype=expList_his[n],varNames,expSel)
}


##########################################################################
######### PROJECTION DATA
for (n in 1:length(expList_rcp)) {
  for (z in 1:length(CO2ExpList)) {
    stat <- collate_rcp(cells,runsDir,gcm=gcmList[i],intype=expList_rcp[n],co2=CO2ExpList[z],
                        varNames,expSel,sdList)
  }
}



