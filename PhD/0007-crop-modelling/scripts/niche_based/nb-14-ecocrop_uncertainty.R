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

#*crop model parameters
#*gcms

#*crop model parameters ## futList_gcm$del$bcc_csm1_1$RUN.1
unc_crop <- list(raw=list(),del=list(),loci=list())
for (rj in ecoRuns) {
  #rj <- ecoRuns[1]
  cat("...run=",rj,"\n")
  
  #calculate for raw
  unc_crop$raw[[paste("RUN.",rj,sep="")]] <- list()
  for (gcm in names(futList_gcm$raw)) {
    #gcm <- names(futList_gcm$raw)[1]
    unc_crop$raw[[paste("RUN.",rj,sep="")]][[gcm]] <- futList_gcm$raw[[gcm]][[paste("RUN.",rj,sep="")]]
  }
  unc_crop$raw[[paste("RUN.",rj,sep="")]] <- stack(unc_crop$raw[[paste("RUN.",rj,sep="")]])
  unc_crop$raw[[paste("RUN.",rj,sep="")]] <- calc(unc_crop$raw[[paste("RUN.",rj,sep="")]],fun=function(x) {mean(x,na.rm=T)})
  
  #calculate for delta bias corrected
  unc_crop$del[[paste("RUN.",rj,sep="")]] <- list()
  for (gcm in names(futList_gcm$del)) {
    #gcm <- names(futList_gcm$del)[1]
    unc_crop$del[[paste("RUN.",rj,sep="")]][[gcm]] <- futList_gcm$del[[gcm]][[paste("RUN.",rj,sep="")]]
  }
  unc_crop$del[[paste("RUN.",rj,sep="")]] <- stack(unc_crop$del[[paste("RUN.",rj,sep="")]])
  unc_crop$del[[paste("RUN.",rj,sep="")]] <- calc(unc_crop$del[[paste("RUN.",rj,sep="")]],fun=function(x) {mean(x,na.rm=T)})
  
  #calculate for loci bias corrected
  unc_crop$loci[[paste("RUN.",rj,sep="")]] <- list()
  for (gcm in names(futList_gcm$loci)) {
    #gcm <- names(futList_gcm$loci)[1]
    unc_crop$loci[[paste("RUN.",rj,sep="")]][[gcm]] <- futList_gcm$loci[[gcm]][[paste("RUN.",rj,sep="")]]
  }
  unc_crop$loci[[paste("RUN.",rj,sep="")]] <- stack(unc_crop$loci[[paste("RUN.",rj,sep="")]])
  unc_crop$loci[[paste("RUN.",rj,sep="")]] <- calc(unc_crop$loci[[paste("RUN.",rj,sep="")]],fun=function(x) {mean(x,na.rm=T)})
}

#calculate std of thing
unc_crop_f <- list()
for (inty in inList) {
  cat("...type=",inty,"\n")
  unc_crop_f[[inty]] <- stack(unc_crop[[inty]])
  unc_crop_f[[inty]] <- calc(unc_crop_f[[inty]],fun=function(x) {sd(x,na.rm=T)})
}


#*gcm ## futList_gcm$del$bcc_csm1_1$RUN.1
unc_gcm <- list(raw=list(),del=list(),loci=list())
for (gcm in unique(modList$GCM)) {
  #gcm <- unique(modList$GCM)[1]
  cat("...gcm=",gcm,"\n")
  
  #calculate for raw
  unc_gcm$raw[[gcm]] <- list()
  for (rj in ecoRuns) {
    #rj <- ecoRuns[1]
    unc_gcm$raw[[gcm]][[paste("RUN.",rj,sep="")]] <- futList_gcm$raw[[gcm]][[paste("RUN.",rj,sep="")]]
  }
  unc_gcm$raw[[gcm]] <- stack(unc_gcm$raw[[gcm]])
  unc_gcm$raw[[gcm]] <- calc(unc_gcm$raw[[gcm]],fun=function(x) {mean(x,na.rm=T)})
  
  #calculate for delta bias corrected
  unc_gcm$del[[gcm]] <- list()
  for (rj in ecoRuns) {
    #rj <- ecoRuns[1]
    unc_gcm$del[[gcm]][[paste("RUN.",rj,sep="")]] <- futList_gcm$del[[gcm]][[paste("RUN.",rj,sep="")]]
  }
  unc_gcm$del[[gcm]] <- stack(unc_gcm$del[[gcm]])
  unc_gcm$del[[gcm]] <- calc(unc_gcm$del[[gcm]],fun=function(x) {mean(x,na.rm=T)})
  
  #calculate for loci bias corrected
  unc_gcm$loci[[gcm]] <- list()
  for (rj in ecoRuns) {
    #rj <- ecoRuns[1]
    unc_gcm$loci[[gcm]][[paste("RUN.",rj,sep="")]] <- futList_gcm$loci[[gcm]][[paste("RUN.",rj,sep="")]]
  }
  unc_gcm$loci[[gcm]] <- stack(unc_gcm$loci[[gcm]])
  unc_gcm$loci[[gcm]] <- calc(unc_gcm$loci[[gcm]],fun=function(x) {mean(x,na.rm=T)})
}

#calculate std of thing
unc_gcm_f <- list()
for (inty in inList) {
  unc_gcm_f[[inty]] <- stack(unc_gcm[[inty]])
  unc_gcm_f[[inty]] <- calc(unc_gcm_f[[inty]],fun=function(x) {sd(x,na.rm=T)})
}


#calculate fractional uncertainty
frac_unc <- list()
for (inty in inList) {
  frac_unc[[inty]] <- list()
  frac_unc[[inty]]$gcm <- unc_gcm_f[[inty]] / (unc_gcm_f[[inty]] + unc_crop_f[[inty]])
  frac_unc[[inty]]$crop <- unc_crop_f[[inty]] / (unc_gcm_f[[inty]] + unc_crop_f[[inty]])
}

#save all objects
save(list=c("futList_gcm","unc_crop","unc_crop_f","unc_gcm","unc_gcm_f","frac_unc"),file=paste(prjDir,"/uncertainty.RData",sep=""))


