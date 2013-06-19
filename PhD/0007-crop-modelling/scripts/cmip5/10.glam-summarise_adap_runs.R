#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Nov 2012

library(raster)

#source directories
#src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

source(paste(src.dir,"/cmip5/10.glam-summarise_adap_runs-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_adapt"

#base and data directories
#bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#bDir2 <- "/mnt/a102/eejarv/PhD-work"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
bDir2 <- "/nfs/a102/eejarv/PhD-work"

glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir_p1 <- paste(cropDir,"/adapt/",runs_name,sep="")
runsDir_p2 <- paste(bDir2,"/adapt/",runs_name,sep="")

#load grid cells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#experimental set up
expList_his <- c("his_allin","his_norain","his_notemp","his_nosrad","his_bcrain","his_sh")
expList_rcp <- c("rcp_allin","rcp_bcrain","rcp_del","rcp_sh")
CO2ExpList <- c("CO2_p1","CO2_p2","CO2_p3","CO2_p4")

#load list of parameter sets
expList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expSel <- expList$EXPID[which(expList$ISSEL == 1)]

#list of GCMs
gcmList_p1 <- list.files(paste(runsDir_p1,"/exp-33_outputs",sep=""),pattern="_ENS_")
gcmList_p2 <- list.files(paste(runsDir_p2,"/exp-33_outputs",sep=""),pattern="_ENS_")
gcmList <- data.frame(GCM=c(gcmList_p1,gcmList_p2),RDIR=c(rep(runsDir_p1,times=length(gcmList_p1)),rep(runsDir_p2,times=length(gcmList_p2))))

#list of variables of interest
#varNames <- c("YGP","STG","DUR","TRADABS","TP_UP","T_TRANS","TP_TRANS","TOTPP",
#              "TOTPP_HIT","TOTPP_WAT","LAI","HI","BMASS","YIELD","T_RAIN","TBARTOT","VPDTOT")

varNames <- c("YGP","STG","DUR","TRADABS","TP_UP","TOTPP_HIT","TOTPP_WAT",
              "LAI","HI","BMASS","YIELD","T_RAIN","TBARTOT","VPDTOT")

#load experiments setup
adap_runs <- read.table(paste(cropDir,"/adapt/data/adapt.tab",sep=""),sep="\t",header=T)


#for (gcm_i in 1:nrow(gcmList)) {
collate_this <- function(gcm_i) {
  #gcm_i <- 1
  for (n in 1:length(expList_rcp)) {
    #n <- 1
    for (z in 1:length(CO2ExpList)) {
      #z <- 1
      stat <- collate_adap(cells,runsDir=paste(gcmList$RDIR[gcm_i]),gcm=paste(gcmList$GCM[gcm_i]),
                           intype=expList_rcp[n],co2=CO2ExpList[z],varNames,expSel,sdList,
                           boutDir=runsDir_p2)
    }
  }
}


collate_this(7) #1 to 14

# xt = x / (x+z)
# z <- 50
# x <- seq(0.5,1000,by=0.1)
# xt <- x / (x+z)
# yt <- (x-min(x)) / (max(x)-min(x))
# zt <- (max(x)-x) / (max(x)-min(x))
# plot(x,xt,ty="l",ylim=c(0,1))
# plot(x,yt,ty="l",ylim=c(0,1))
# plot(x,zt,ty="l",ylim=c(0,1))


