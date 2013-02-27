#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012


### overall details
seas <- "ANN"
for (seas in c("DJF","MAM","JJA","SON")) {
  vn <- "rd"
  
  #local
  #src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
  #src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
  #cmipDir <- "V:/eejarv/CMIP5"
  
  src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
  src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0008-CMIP5"
  cmip5Dir <- "/mnt/a102/eejarv/CMIP5"
  cmip3Dir <- "/mnt/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
  
  # #eljefe
  # src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
  # src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
  # cmipDir <- "/nfs/a102/eejarv/CMIP5"
  # cmip3Dir <- "/nfs/a17/eejarv/IPCC_CMIP3/20C3M/original-data"
  
  
  ######################################################################3
  ############ CMIP5
  
  #list of gcms
  gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
  gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
  gcmList <- c(gcmList,paste("multi_model_mean_ENS_r1i1p1"))
  
  #list of variables and datasets
  dsetList <- c("vi_rev-CRU","vi_rev-WST")
  
  #list of gcms and regions
  regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")
  isoList <- regions$ISO
  
  #process list
  procList <- expand.grid(GCM=gcmList)
  
  #data directory
  outputDD <- paste(cmip5Dir,"/assessment/output-data",sep="")
  out_sum <- paste(outputDD,"/_summary_revised2",sep="")
  
  #output data directory
  oDir <- paste(out_sum,"/pdfs",sep="")
  if (!file.exists(oDir)) {dir.create(oDir)}
  
  rdata_dir <- paste(oDir,"/rdata_files",sep="")
  if (!file.exists(rdata_dir)) {dir.create(rdata_dir)}
  
  fig_dir <- paste(oDir,"/figures",sep="")
  if (!file.exists(fig_dir)) {dir.create(fig_dir)}
  
  #this_proc <- 1
  
  if (!file.exists(paste(rdata_dir,"/vi_",seas,"_",vn,"_CMIP5.RData",sep=""))) {
    #loop GCMs
    for (this_proc in 1:nrow(procList)) {
      #details of what I need
      #this_proc <- 1
      gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
      ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
      
      cat(gcm,"/",ens,"\n")
      
      #load all datasets, countries and seasons mean climate skill
      all_iso <- data.frame()
      for (iso in regions$ISO) {
        #iso <- regions$ISO[1]
        reg <- regions$REGION[which(regions$ISO == iso)]
        
        for (dset in dsetList) {
          #dset <- dsetList[1]
          #read file, vi skill
          infil <- paste(outputDD,"/",reg,"/",iso,"/",dset,"/",vn,"_",gcm,"_",ens,".csv",sep="")
          if (file.exists(infil)) {
            indat <- read.csv(infil)
            #### here you go!
            indat <- indat$VI[which(indat$SEASON == seas)]
            indat <- data.frame(REGION=paste(reg),ISO=paste(iso),OBS=dset,VALUE=indat)
            #names(indat)[ncol(indat)] <- paste(gcm,"_ENS_",ens,sep="")
          } else {
            indat <- data.frame(REGION=paste(reg),ISO=paste(iso),OBS=dset,VALUE=NA)
          }
          all_iso <- rbind(all_iso,indat)
        }
      }
      
      #binding data as columns
      if (this_proc == 1) {
        gcm_vals <- list()
        gcm_vals[[this_proc]] <- all_iso
      } else {
        gcm_vals[[this_proc]] <- all_iso #cbind(gcm_vals,VALUE=all_iso[,ncol(all_iso)])
      }
      #names(gcm_vals)[ncol(gcm_vals)] <- paste(gcm,"_ENS_",ens,sep="")
    }
    c5_vals <- gcm_vals
    
    #determine breaks for histogram
    #alldata <- as.numeric(unlist(c(as.vector(c5_vals[,4:ncol(c5_vals)]))))
    alldata <- as.numeric(unlist(lapply(c5_vals,FUN=function(x) {return(x$VALUE)})))
    alldata <- alldata[which(!is.infinite(alldata))]
    alldata <- alldata[which(!is.na(alldata))]
    brks <- c(seq(0,5,by=0.5),seq(5,10,by=1),seq(10,50,by=10))
    brks <- unique(brks)
    if (max(brks) < max(alldata,na.rm=T)) {brks <- c(brks,max(alldata,na.rm=T))}
    
    #compute histograms for all parameter sets
    for (this_proc in 1:nrow(procList)) {
      #this_proc <- 1
      gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
      ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
      
      this_vals <- c5_vals[[this_proc]]$VALUE
      this_vals <- this_vals[which(!is.na(this_vals))]
      this_vals <- this_vals[which(!is.infinite(this_vals))]
      
      if (length(this_vals) != 0) {
        his_gcm <- hist(this_vals,breaks=brks,plot=F)
        hgcm <- his_gcm$counts/sum(his_gcm$counts)*100
        hgcm <- data.frame(XVAL=his_gcm$mids,VAL=hgcm)
      } else {
        hgcm <- data.frame(XVAL=hc5_all$XVAL,VAL=NA)
      }
      names(hgcm)[2] <- paste(gcm,"_ENS_",ens,sep="")
      
      if (this_proc == 1) {
        hc5_all <- hgcm
      } else {
        hc5_all <- merge(hc5_all,hgcm,by="XVAL",sort=F)
      }
    }
    
    save(list=c("c5_vals","hc5_all"),file=paste(rdata_dir,"/vi_",seas,"_",vn,"_CMIP5.RData",sep=""))
  } else {
    load(paste(rdata_dir,"/vi_",seas,"_",vn,"_CMIP5.RData",sep=""))
  }
  
  #####################################################################
  #### with out_all3 of CMIP5 and that of CMIP3, scattergrams need 
  #    to be constructed that compare them
  
  ######################################################################3
  ############ CMIP3
  if (vn != "rd") {
    #list of gcms
    gcmList <- list.files(cmip3Dir)
    gcmList <- gcmList[gcmList != "multi_model_mean"]
    gcmList <- c(gcmList,"multi_model_mean")
    
    #list of variables and datasets
    dsetList <- c("vi-CRU","vi-WST","vi-WCL")
    
    #process list
    if (vn == "pr") vn <- "prec"
    if (vn == "tas") vn <- "tmean"
    procList <- expand.grid(GCM=gcmList)
    
    #data directory
    outputDD <- paste(cmip5Dir,"/assessment/output-data-cmip3",sep="")
    out_sum <- paste(outputDD,"/_summary",sep="")
    
    #this_proc <- 1
    if (!file.exists(paste(rdata_dir,"/vi_",seas,"_",vn,"_CMIP3.RData",sep=""))) {
      #loop through GCMs
      for (this_proc in 1:nrow(procList)) {
        #details of what I need
        #this_proc <- 1
        gcm <- paste(procList$GCM[this_proc])
        
        cat(gcm,"\n")
        
        #load all datasets, countries and seasons mean climate skill
        all_iso <- data.frame()
        for (iso in regions$ISO) {
          #iso <- regions$ISO[1]
          reg <- regions$REGION[which(regions$ISO == iso)]
          
          for (dset in dsetList) {
            #dset <- dsetList[1]
            infil <- paste(outputDD,"/",reg,"/",iso,"/",dset,"/",vn,"_",gcm,".csv",sep="")
            if (file.exists(infil)) {
              indat <- read.csv(infil)
              indat <- indat$VI[which(indat$SEASON == seas)]
              indat <- data.frame(REGION=paste(reg),ISO=paste(iso),OBS=dset,VALUE=indat)
            } else {
              indat <- data.frame(REGION=paste(reg),ISO=paste(iso),OBS=dset,VALUE=NA)
            }
            all_iso <- rbind(all_iso,indat)
          }
        }
        
        #binding data as columns
        if (this_proc == 1) {
          gcm_vals <- list()
          gcm_vals[[this_proc]] <- all_iso
        } else {
          gcm_vals[[this_proc]] <- all_iso
        }
        #names(gcm_vals)[ncol(gcm_vals)] <- gcm
      }
      c3_vals <- gcm_vals
      
      alldata <- as.numeric(unlist(lapply(c3_vals,FUN=function(x) {return(x$VALUE)})))
      alldata <- alldata[which(!is.infinite(alldata))]
      alldata <- alldata[which(!is.na(alldata))]
      if (max(brks)<max(alldata,na.rm=T)) {brks <- c(brks,max(alldata,na.rm=T))}
      
      #compute histograms for all parameter sets
      for (this_proc in 1:nrow(procList)) {
        #this_proc <- 1
        gcm <- paste(procList$GCM[this_proc])
        
        this_vals <- c3_vals[[this_proc]]$VALUE
        this_vals <- this_vals[which(!is.na(this_vals))]
        this_vals <- this_vals[which(!is.infinite(this_vals))]
        
        if (length(this_vals) != 0) {
          his_gcm <- hist(this_vals,breaks=brks,plot=F)
          hgcm <- his_gcm$counts/sum(his_gcm$counts)*100
          hgcm <- data.frame(XVAL=his_gcm$mids,VAL=hgcm)
        } else {
          hgcm <- data.frame(XVAL=hc3_all$XVAL,VAL=NA)
        }
        names(hgcm)[2] <- gcm
        
        
        if (this_proc == 1) {
          hc3_all <- hgcm
        } else {
          hc3_all <- merge(hc3_all,hgcm,by="XVAL",sort=F)
        }
      }
      save(list=c("c3_vals","hc3_all"),file=paste(rdata_dir,"/vi_",seas,"_",vn,"_CMIP3.RData",sep=""))
    } else {
      load(paste(rdata_dir,"/vi_",seas,"_",vn,"_CMIP3.RData",sep=""))
    }
  }
  
  ###################################################
  ####### plot
  xval <- hc5_all$XVAL
  if (vn != "rd") {
    if (nrow(hc5_all) < nrow(hc3_all)) {
      xval <- hc3_all$XVAL
    }
  }
  
  hc5_all$XVAL <- NULL
  hc5_m <- apply(hc5_all[,1:(ncol(hc5_all)-1)],1,mean,na.rm=T)
  hc5_sd <- apply(hc5_all[,1:(ncol(hc5_all)-1)],1,sd,na.rm=T)
  
  if (vn != "rd") {
    hc3_all$XVAL <- NULL
    hc3_m <- apply(hc3_all[,1:(ncol(hc3_all)-1)],1,mean,na.rm=T)
    hc3_sd <- apply(hc3_all[,1:(ncol(hc3_all)-1)],1,sd,na.rm=T)
  } else {
    hc3_all <- hc5_all
    hc3_m <- rep(NA,times=length(hc5_m))
    hc3_sd <- hc3_m
  }
  
  
  if (nrow(hc5_all) < nrow(hc3_all)) {
    hsum <- data.frame(XVAL=xval,C5.MEAN=c(hc5_m,0),C5.SD=c(hc5_sd,0),C3.MEAN=hc3_m,C3.SD=hc3_sd)
  } else {
    hsum <- data.frame(XVAL=xval,C5.MEAN=hc5_m,C5.SD=hc5_sd,C3.MEAN=hc3_m,C3.SD=hc3_sd)
  }
  
  #produce the plot
  tiff(paste(fig_dir,"/",seas,"_shaded_vi_",vn,".tif",sep=""),res=300,height=1500,width=2048,
       compression="lzw",pointsize=10)
  par(mar=c(5,5,1,1))
  
  #options of plots
  plot(hsum$XVAL,hsum$C5.MEAN,ty="l",main=NA,xlab="Variability index",
       ylab="pdf (%)",xlim=c(0,5),ylim=c(0,70))
  
  
  ##########
  polup <- hsum$C5.MEAN+hsum$C5.SD
  poldw <- hsum$C5.MEAN-hsum$C5.SD
  poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
  
  polygon(x=c(hsum$XVAL,rev(hsum$XVAL)),y=c(polup,rev(poldw)),col="#FF000040",border=NA)
  lines(hsum$XVAL,hsum$C5.MEAN,col="red")
  if (nrow(hc5_all) < nrow(hc3_all)) {
    lines(hsum$XVAL,c(hc5_all$multi_model_mean_ENS_r1i1p1,0),col="red",lty=2,lwd=0.5)
  } else {
    lines(hsum$XVAL,hc5_all$multi_model_mean_ENS_r1i1p1,col="red",lty=2,lwd=0.5)
  }
  
  #polygon 2
  if (vn != "rd") {
    polup <- hsum$C3.MEAN+hsum$C3.SD
    poldw <- hsum$C3.MEAN-hsum$C3.SD
    poldw <- sapply(poldw,FUN=function(x) {max(c(0,x))})
    
    polygon(x=c(hsum$XVAL,rev(hsum$XVAL)),y=c(polup,rev(poldw)),col="#0000FF32",border=NA)
    lines(hsum$XVAL,hsum$C3.MEAN,col="blue")
    lines(hsum$XVAL,hc3_all$multi_model_mean,col="blue",lty=2,lwd=0.5)
  }
  grid()
  abline(v=0.5,col="black",lty=2)
  dev.off()
  
  
  
  ##########################################################################
  ##########################################################################
}

