#Julian Ramirez-Villegas
#July 2013
#UoL / CCAFS / CIAT
stop("!")

#load packages
library(rgdal); library(raster)

#i/o directories
cropName <- "gnut"
bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
nbDir <- paste(bDir,"/niche-based",sep="")
modDir <- paste(nbDir,"/models",sep="")
ecoDir <- paste(modDir,"/EcoCrop",sep="")
prjDir <- paste(ecoDir,"/proj",sep="")

#selected model runs
skill <- read.csv(paste(ecoDir,"/data/runs_discard.csv",sep=""))
ecoRuns <- skill$RUN[which(skill$SEL)]

#details for calculations
inList <- c("del","loci","raw")
gcmList <- list.files(paste(prjDir,"/rcp_del",sep=""))
all_runs <- expand.grid(RUN=ecoRuns,GCM=gcmList)

#calculate future change in suitability for each future-baseline combination
chgList <- list()

for (inty in inList) {
  #inty <- inList[1]
  chgList[[inty]] <- list()
  for (i in 1:nrow(all_runs)) {
    #i <- 1
    gcm <- paste(all_runs$GCM[i])
    ecorun <- all_runs$RUN[i]
    cat("...processing",inty," / ",gcm," / run=",ecorun,"\n")
    
    #configure output list
    if (is.null(chgList[[inty]][[gcm]])) {chgList[[inty]][[gcm]] <- list()}
    
    #load baseline
    if (inty == "del") {
      rs_pd <- raster(paste(prjDir,"/baseline/clm_1966_1993/run_",ecorun,"/",tolower(cropName),"_suitability.tif",sep=""))
    } else {
      rs_pd <- raster(paste(prjDir,"/baseline_",inty,"/",gcm,"/run_",ecorun,"/",tolower(cropName),"_suitability.tif",sep=""))
    }
    
    #load future
    rs_fc <- raster(paste(prjDir,"/rcp_",inty,"/",gcm,"/run_",ecorun,"/",tolower(cropName),"_suitability.tif",sep=""))
    
    #calculate relative change
    rs_chg <- calc(stack(rs_pd,rs_fc),fun=function(x) {a <- x[1]; b <- x[2]; if (is.na(a) | is.na(b)) return(NA); if (a == 0) return(NA); chg <- b-a; return(chg)})
    
    #put into list
    chgList[[inty]][[gcm]][[paste("RUN.",ecorun,sep="")]] <- rs_chg
  }
}


#calculate means and quantiles


#calculate probability of suit change above and below certain thresholds





