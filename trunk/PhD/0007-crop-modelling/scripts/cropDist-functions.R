#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

library(raster); library(maptools); data(wrld_simpl)

#Function to create appropriate crop distribution surfaces in each crop working folder
createCropDist <- function(bd,cropIDs,cropName,ow=F) {
  cropRow <- which(cropIDs$ABRV==paste(cropName)) 
  cDir <- paste(bd,"/EcoCrop/models/EcoCrop-",toupper(cropName),sep="")
  if (!file.exists(paste(cDir,"/cropdist/mirca2000.tif",sep="")) | ow) {
  
    #find acronym in cropID table
    if (!is.na(cropIDs$MIRCA_ID[cropRow])) {
      cat("MIRCA dataset exists, loading... \n")
      mirca.exists <- T #for final writing
      cropID <- cropIDs$MIRCA_ID[cropRow] #get crop id
      gName <- paste("ANNUAL_AREA_HARVESTED_IRC_CROP",cropID,"_HA.ASC",sep="") #make raster name
      mirca <- raster(paste(bd,"/crop-distribution-data/MIRCA2000/rasters/",gName,sep="")) #load crop dist raster
      mirca[which(mirca[]==0)] <- NA #set all zeros to NAs
    } else {
      cat("MIRCA does not exist \n")
      mirca.exists <- F
    }
    
    #now read monfreda
    cat("Loading Monfreda et al. dataset \n")
    mCrop <- cropIDs$Monfreda_NAME[cropRow]
    monf <- raster(paste(bd,"/crop-distribution-data/Monfreda/rasters_fixed/",mCrop,"_harea.asc",sep=""))
    monf[which(monf[]==0)] <- NA
    
    #now read SPAM
    sCrop <- cropIDs$SPAM_NAME[cropRow]
    sDir <- paste(bd,"/crop-distribution-data/SPAM/rasters/spam2000v3r5_harvested-area/",sep="")
    if (!is.na(sCrop)) {
      cat("SPAM dataset exists, loading \n")
      spam.exists <- T #for final writing
      #load rainfed area high input
      rhi <- paste(paste(sDir,"/spam2000v3r5_harvested-area_rainfed-high_",sCrop,".asc",sep=""))
      spam_rhi <- raster(rhi)
      spam_rhi[which(is.na(spam_rhi[]))] <- 0
      #load rainfed area low input
      rlo <- paste(paste(sDir,"/spam2000v3r5_harvested-area_rainfed-low_",sCrop,".asc",sep=""))
      spam_rlo <- raster(rlo)
      spam_rlo[which(is.na(spam_rlo[]))] <- 0
      #calculating total rainfed area
      spam <- spam_rhi + spam_rlo
      spam[which(spam[]==0)] <- NA
    } else {
      cat("SPAM dataset does not exist \n")
      spam.exists <- F
    }
    
    #now write everything in the crop folder
    cat("Writing data \n")
    monf <- writeRaster(monf,paste(cDir,"/cropdist/monfreda.asc",sep=""),format='ascii',overwrite=T)
    if (spam.exists) spam <- writeRaster(spam,paste(cDir,"/cropdist/spam.asc",sep=""),format='ascii',overwrite=T)
    if (mirca.exists) mirca <- writeRaster(mirca,paste(cDir,"/cropdist/mirca2000.asc",sep=""),format='ascii',overwrite=T)
    
    #create a plot of each surface cropped to the mask
    cat("Creating plots in tiff format \n")
    msk <- raster(paste(bd,"/EcoCrop/analysis-mask/mask.asc",sep=""))
    mo <- crop(monf,msk)
    qs <- unique(quantile(mo[],na.rm=T,probs=seq(0,1,by=0.05)))
    tiff(paste(cDir,"/cropdist/monfreda.tif",sep=""),res=150,height=1400,width=1500,compression="lzw")
    plot(mo,breaks=qs,lab.breaks=round(qs,2),
         col=colorRampPalette(c("green","yellow","red"))(100),
         legend=T,horizontal=T,legend.shrink=0.9)
    plot(wrld_simpl,add=T)
    grid()
    dev.off()
    
    if (spam.exists) {
      sp <- crop(spam,msk)
      qs <- unique(quantile(sp[],na.rm=T,probs=seq(0,1,by=0.05)))
      tiff(paste(cDir,"/cropdist/spam.tif",sep=""),res=150,height=1400,width=1500,compression="lzw")
      plot(sp,breaks=qs,lab.breaks=round(qs,2),
           col=colorRampPalette(c("green","yellow","red"))(100),
           legend=T,horizontal=T,legend.shrink=0.9)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    
    if (mirca.exists) {
      mi <- crop(mirca,msk)
      qs <- unique(quantile(mi[],na.rm=T,probs=seq(0,1,by=0.05)))
      tiff(paste(cDir,"/cropdist/mirca2000.tif",sep=""),res=150,height=1400,width=1500,compression="lzw")
      plot(mi,breaks=qs,lab.breaks=round(qs,2),
           col=colorRampPalette(c("green","yellow","red"))(100),
           legend=T,horizontal=T,legend.shrink=0.9)
      plot(wrld_simpl,add=T)
      grid()
      dev.off()
    }
    return("Done!")
  } else {
    cat("Files exist or overwrite is set to FALSE \n")
  }
}

#####################################################
#function to convert the Monfreda data from unreadable ascii grids to correctly
#formatted ascii grids
convertMon <- function(inDir,outDir,gName,ow=T) {
  fn <- paste(inDir,"/",gName,sep="")
  ofn <- paste(outDir,"/",gName,sep="")
  
  if (!file.exists(ofn) | ow) {
    fz <- file(fn,"r")
    
    #get raster characteristics
    header <- readLines(fz,6)
    ncols <- as.numeric(gsub("ncols","",header[1]))
    nrows <- as.numeric(gsub("nrows","",header[2]))
    xllco <- as.numeric(gsub("xllcorner","",header[3]))
    yllco <- as.numeric(gsub("yllcorner","",header[4]))
    cllsz <- as.numeric(gsub("cellsize","",header[5]))
    ndata <- as.numeric(gsub("NODATA_value","",header[6]))
    
    #construct the raster
    library(raster)
    xn <- xllco; yn <- yllco
    rs <- raster(ncol=ncols,nrow=nrows,xmn=xllco,ymn=yllco)
    
    #get the raster values
    rdata <- readLines(fz,n=-1)
    close(fz)
    rk <- rdata
    rk <- as.numeric(unlist(strsplit(rdata," ")))
    rk <- rk[which(!is.na(rk))]
    
    #put the raster values into the raster
    rs[] <- rk
    rs[which(rs[]==-9999)] <- NA
    
    #write output raster
    rs <- writeRaster(rs,paste(outDir,"/",gName,sep=""),format='ascii')
  } else {
    cat("File already exists \n")
  }
}
