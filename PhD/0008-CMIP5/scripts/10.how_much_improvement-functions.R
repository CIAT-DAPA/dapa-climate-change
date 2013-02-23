#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2013

#function to do lapply
summarise_proc_c5 <- function(this_proc) {
  #details of what I need
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  vn <- paste(procList$VAR[this_proc])
  
  cat("GCM:",gcm,"/ ENS:",ens,"/ VAR:",vn,"\n")
  
  #load all datasets, countries and seasons mean climate skill
  all_iso_vi <- data.frame()
  for (iso in regions$ISO) {
    #iso <- regions$ISO[1]
    reg <- regions$REGION[which(regions$ISO == iso)]
    
    ###
    #read file, interannual vi (CRU)
    infil <- paste(outputDir_c5,"/",reg,"/",iso,"/vi_rev-CRU/",vn,"_",gcm,"_",ens,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",REG=reg,ISO=iso,indat)
    indat$LON <- NULL; indat$LAT <- NULL; indat$CELL <- NULL
    
    #read file, interannual vi (WST)
    infil <- paste(outputDir_c5,"/",reg,"/",iso,"/vi_rev-WST/",vn,"_",gcm,"_",ens,".csv",sep="")
    if (file.exists(infil)) {
      tmpdat <- read.csv(infil)
      tmpdat$LON <- NULL; tmpdat$LAT <- NULL; tmpdat$CELL <- NULL
      tmpdat <- cbind(DSET="WST",REG=reg,ISO=iso,tmpdat)
      indat <- rbind(indat,tmpdat)
    }
    
    #binding data, interannual vi
    all_iso_vi <- rbind(all_iso_vi,indat)
  }
  
  all_iso_vi <- cbind(GCM=gcm,ENS=ens,VAR=vn,all_iso_vi)
  return(all_iso_vi)
}


### CMIP3
#function to do lapply
summarise_proc_c3 <- function(this_proc) {
  #details of what I need
  gcm <- paste(procList$GCM[this_proc])
  vn <- paste(procList$VAR[this_proc])
  
  cat("GCM:",gcm,"/ VAR:",vn,"\n")
  
  #load all datasets, countries and seasons mean climate skill
  all_iso_vi <- data.frame()
  for (iso in regions$ISO) {
    #iso <- regions$ISO[1]
    reg <- regions$REGION[which(regions$ISO == iso)]
    
    ###
    #read file, interannual vi (CRU)
    infil <- paste(outputDir_c3,"/",reg,"/",iso,"/vi-CRU/",vn,"_",gcm,".csv",sep="")
    indat <- read.csv(infil)
    indat <- cbind(DSET="CRU",REG=reg,ISO=iso,indat)
    indat$LON <- NULL; indat$LAT <- NULL; indat$CELL <- NULL
    
    #read file, interannual vi (WST)
    infil <- paste(outputDir_c3,"/",reg,"/",iso,"/vi-WST/",vn,"_",gcm,".csv",sep="")
    if (file.exists(infil)) {
      tmpdat <- read.csv(infil)
      tmpdat$LON <- NULL; tmpdat$LAT <- NULL; tmpdat$CELL <- NULL
      tmpdat <- cbind(DSET="WST",REG=reg,ISO=iso,tmpdat)
      indat <- rbind(indat,tmpdat)
    }
    
    #binding data, interannual vi
    all_iso_vi <- rbind(all_iso_vi,indat)
  }
  
  all_iso_vi <- cbind(GCM=gcm,VAR=vn,all_iso_vi)
  return(all_iso_vi)
}


