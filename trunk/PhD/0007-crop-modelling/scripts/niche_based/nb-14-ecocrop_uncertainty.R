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


#calculate future suitability for each future-baseline combination
if (!file.exists(paste(prjDir,"/future_rasters.RData",sep=""))) {
  futList <- list()
  for (inty in inList) {
    #inty <- inList[1]
    futList[[inty]] <- list()
    for (i in 1:nrow(all_runs)) {
      #i <- 1
      gcm <- paste(all_runs$GCM[i])
      ecorun <- all_runs$RUN[i]
      cat("...processing",inty," / ",gcm," / run=",ecorun,"\n")
      
      #configure output list
      if (is.null(futList[[inty]][[gcm]])) {futList[[inty]][[gcm]] <- list()}
      
      #load future
      rs_fc <- raster(paste(prjDir,"/rcp_",inty,"/",gcm,"/run_",ecorun,"/",tolower(cropName),"_suitability.tif",sep=""))
      
      #put into list
      futList[[inty]][[gcm]][[paste("RUN.",ecorun,sep="")]] <- rs_fc
    }
  }
  save(list=c("futList"),file=paste(prjDir,"/future_rasters.RData",sep=""))
} else {
  load(file=paste(prjDir,"/future_rasters.RData",sep=""))
}


#modelList
modList <- data.frame(GCM_ENS=gcmList)
modList$GCM <- sapply(modList$GCM_ENS,FUN=function(x) {unlist(strsplit(paste(x),"_ENS_",fixed=T))[1]})
modList$ENS <- sapply(modList$GCM_ENS,FUN=function(x) {unlist(strsplit(paste(x),"_ENS_",fixed=T))[2]})

#a. first average individual ensemble members
futList_gcm <- list()
for (inty in inList) {
  #inty <- inList[1]
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("XXXX processing",inty,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  #a. first average individual ensemble members
  futList_gcm[[inty]] <- list()
  #loop GCMs in modList
  for (i in 1:length(unique(modList$GCM))) {
    #i <- 1
    gcm <- paste(unique(modList$GCM)[i])
    cat("\n...averaging",gcm,"\n")
    
    #create list
    if (is.null(futList_gcm[[inty]][[gcm]])) {futList_gcm[[inty]][[gcm]] <- list()}
    
    #list of ensemble members inside this
    wList <- grep(gcm,names(futList[[inty]]))
    if (length(wList) == 1) {
      #if only one ensemble member then put directly
      cat("...ens=1, so no need for averaging\n")
      futList_gcm[[inty]][[gcm]] <- futList[[inty]][[wList]]
    } else {
      #if more than 1 ensemble member then needs to average
      cat("...ens=",wList,", so averaging\n")
      for (rj in ecoRuns) {
        #rj <- ecoRuns[1]
        tstk <- c()
        for (wj in wList) {tstk <- c(tstk,futList[[inty]][[wj]][[paste("RUN.",rj,sep="")]])}
        tstk <- stack(tstk)
        tstk <- calc(tstk,fun=function(x) {mean(x,na.rm=T)})
        futList_gcm[[inty]][[gcm]][[paste("RUN.",rj,sep="")]] <- tstk
      }
    }
  }
}

#b. calculate means per each of the things and then calculate s.d., 
#   and then fractional uncertainty



