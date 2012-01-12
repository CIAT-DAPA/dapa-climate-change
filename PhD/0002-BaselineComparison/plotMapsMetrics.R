#This would plot the GCM-timeseries skill assessment in a nice way (map), for fancy plotting
#on the paper

rm(list=ls()); g=gc(); rm(g)
require(rgdal); require(raster); require(maptools)
data(wrld_simpl)

work.dir <- "C:/CIAT_work/CCAFS/climate-data-assessment/baselineComparison/timeseries/results.v2"

#Dataset, variable, period definition and folder
prd <- "TTL"

#Desired extent 40°30'N to 47°30'S in latitude, and 18°W to 98°E in longitude), 
in.xt <- extent(c(-20,100,-15,40.5))

plotMetrics(wd=work.dir,dataset="ghcn",period="TTL",in.xt,RMSE=T,S=T,R2=T)
plotMetrics(wd=work.dir,dataset="gsod",period="TTL",in.xt,RMSE=T,S=T,R2=T)

plotMetrics <- function(wd="",dataset="ghcn",period="TTL",xt,RMSE=T,S=T,R2=T) {
  #Input folder (dataset)
  dstDir <- paste(wd,"/",dataset,"-vs-gcm-ts",sep="")
  
  #########RAINFALL
  variable <- "rain"
  varDir <- paste(dstDir,"/",variable,"-gridded",sep="")
  
  #Listing models
  gcmList <- list.files(varDir)
  
  for (gcm in gcmList) {
    cat("Processing GCM",gcm,variable,"\n")
    
    #Working directory
    gcmDir <- paste(varDir,"/",gcm,sep="")
    setwd(gcmDir)
    
    
    #########################################################
    ### R-SQUARE
    #########################################################
    metric <- "R2.FORCED"
    #Loading raster
    r2 <- raster(paste(metric,".",period,".CELLMEAN.asc",sep=""))
    r2 <- crop(r2,xt)
    
    #Classification
    brks <- seq(0,1,by=0.1)
    nb <- length(brks)-1
    
    #Aspect (y/x ratio)
    aspect <- (xt@ymax-xt@ymin)*1.15/(xt@xmax-xt@xmin)
    
    #Plotting 
    tiff(paste(metric,".",period,".CELLMEAN.tif",sep=""),
         res=300,pointsize=5.5,width=1500,height=1500*aspect,units="px",compression="lzw")
    par(mar=c(2.5,2.5,1,1),cex=0.8)
    plot(r2,
         col=colorRampPalette(c("grey 90","grey 20"))(nb),
         breaks=brks,
         lab.breaks=brks,
         axes=T,
         xlim=c(xt@xmin,xt@xmax),ylim=c(xt@ymin,xt@ymax),
         zlim=c(0,1),
         horizontal=T,
         legend.width=1.5,
         legend.shrink=0.99,
         nlevel=nb*100)
    grid(col="black")
    plot(wrld_simpl,lwd=1,add=T)
    dev.off()
    
    
    #########################################################
    ### ERROR (RMSE)
    #########################################################
    metric <- "ERROR"
    #Loading raster
    error <- raster(paste(metric,".",period,".CELLMEAN.asc",sep=""))
    error <- crop(error,xt)
    
    #Aspect (y/x ratio)
    aspect <- (xt@ymax-xt@ymin)*1.15/(xt@xmax-xt@xmin)
    
    #Classification
    brks <- c(0,50,100,250,500,1000,round(max(error[],na.rm=T),0))
    lab.brks <- c(0,50,100,250,500,1000,NA)
    nb <- length(brks)-1
    
    #Plotting 
    tiff(paste(metric,".",period,".CELLMEAN.tif",sep=""),
         res=300,pointsize=5.5,width=1500,height=1500*aspect,units="px",compression="lzw")
    par(mar=c(2.5,2.5,1,1),cex=0.8)
    plot(error,
         col=colorRampPalette(c("grey 90","grey 10"))(nb),
         breaks=brks,
         axes=T,
         xlim=c(xt@xmin,xt@xmax),ylim=c(xt@ymin,xt@ymax),
         lab.breaks=lab.brks,
         zlim=c(0,max(brks)),
         horizontal=T,
         legend.width=1.5,
         legend.shrink=0.99,
         nlevel=nb*100)
    grid(col="black")
    plot(wrld_simpl,lwd=1,add=T)
    dev.off()
    
    #########################################################
    ### SLOPE (S)
    #########################################################
    metric <- "SLOPE.FORCED"
    #Loading raster
    slope <- raster(paste(metric,".",period,".CELLMEAN.asc",sep=""))
    slope <- crop(slope,xt)
    
    #Aspect (y/x ratio)
    aspect <- (xt@ymax-xt@ymin)*1.15/(xt@xmax-xt@xmin)
    
    #Classification
    brks <- c(0,.5,1,round(max(slope[],na.rm=T),0))
    lab.brks <- c(0,.5,1,NA)
    nb <- length(brks)-1
    
    #Plotting 
    tiff(paste(metric,".",period,".CELLMEAN.tif",sep=""),
         res=300,pointsize=5.5,width=1500,height=1500*aspect,units="px",compression="lzw")
    par(mar=c(2.5,2.5,1,1),cex=0.8)
    plot(slope,
         col=colorRampPalette(c("grey 90","grey 40"))(nb),
         breaks=brks,
         axes=T,
         xlim=c(xt@xmin,xt@xmax),ylim=c(xt@ymin,xt@ymax),
         lab.breaks=lab.brks,
         zlim=c(0,max(brks,na.rm=T)),
         horizontal=T,
         legend.width=1.5,
         legend.shrink=0.99,
         nlevel=nb*100)
    grid(col="black")
    plot(wrld_simpl,lwd=1,add=T)
    dev.off()
  }

  #########MEAN TEMPERATURE
  variable <- "tmean"
  varDir <- paste(dstDir,"/",variable,"-gridded",sep="")
  
  #Listing models
  gcmList <- list.files(varDir)
  
  for (gcm in gcmList) {
    cat("Processing GCM",gcm,variable,"\n")
    
    #Working directory
    gcmDir <- paste(varDir,"/",gcm,sep="")
    setwd(gcmDir)
    
    #########################################################
    ### R-SQUARE
    #########################################################
    metric <- "R2.FORCED"
    #Loading raster
    r2 <- raster(paste(metric,".",period,".CELLMEAN.asc",sep=""))
    r2 <- crop(r2,xt)
    
    #Classification
    brks <- seq(0,1,by=0.1)
    nb <- length(brks)-1
    
    #Aspect (y/x ratio)
    aspect <- (xt@ymax-xt@ymin)*1.15/(xt@xmax-xt@xmin)
    
    #Plotting 
    tiff(paste(metric,".",period,".CELLMEAN.tif",sep=""),
         res=300,pointsize=5.5,width=1500,height=1500*aspect,units="px",compression="lzw")
    par(mar=c(2.5,2.5,1,1),cex=0.8)
    plot(r2,
         col=colorRampPalette(c("grey 90","grey 20"))(nb),
         breaks=brks,
         lab.breaks=brks,
         axes=T,
         xlim=c(xt@xmin,xt@xmax),ylim=c(xt@ymin,xt@ymax),
         zlim=c(0,1),
         horizontal=T,
         legend.width=1.5,
         legend.shrink=0.99,
         nlevel=nb*100)
    grid(col="black")
    plot(wrld_simpl,lwd=1,add=T)
    dev.off()
    
    
    #########################################################
    ### ERROR (RMSE)
    #########################################################
    metric <- "ERROR"
    #Loading raster
    error <- raster(paste(metric,".",period,".CELLMEAN.asc",sep=""))
    error <- crop(error,xt)
    
    #Aspect (y/x ratio)
    aspect <- (xt@ymax-xt@ymin)*1.15/(xt@xmax-xt@xmin)
    
    #Classification
    brks <- c(0,0.5,1,1.5,2,2.5,5,round(max(error[],na.rm=T),0))
    lab.brks <- c(0,0.5,1,1.5,2,2.5,5,NA)
    nb <- length(brks)-1
    
    #Plotting 
    tiff(paste(metric,".",period,".CELLMEAN.tif",sep=""),
         res=300,pointsize=5.5,width=1500,height=1500*aspect,units="px",compression="lzw")
    par(mar=c(2.5,2.5,1,1),cex=0.8)
    plot(error,
         col=colorRampPalette(c("grey 90","grey 10"))(nb),
         breaks=brks,
         axes=T,
         xlim=c(xt@xmin,xt@xmax),ylim=c(xt@ymin,xt@ymax),
         lab.breaks=lab.brks,
         zlim=c(0,max(brks)),
         horizontal=T,
         legend.width=1.5,
         legend.shrink=0.99,
         nlevel=nb*100)
    grid(col="black")
    plot(wrld_simpl,lwd=1,add=T)
    dev.off()
    
    #########################################################
    ### SLOPE (S)
    #########################################################
    metric <- "SLOPE.FORCED"
    #Loading raster
    slope <- raster(paste(metric,".",period,".CELLMEAN.asc",sep=""))
    slope <- crop(slope,xt)
    
    #Aspect (y/x ratio)
    aspect <- (xt@ymax-xt@ymin)*1.15/(xt@xmax-xt@xmin)
    
    #Classification
    brks <- c(0,.5,1,round(max(slope[],na.rm=T),0))
    lab.brks <- c(0,.5,1,NA)
    nb <- length(brks)-1
    
    #Plotting 
    tiff(paste(metric,".",period,".CELLMEAN.tif",sep=""),
         res=300,pointsize=5.5,width=1500,height=1500*aspect,units="px",compression="lzw")
    par(mar=c(2.5,2.5,1,1),cex=0.8)
    plot(slope,
         col=colorRampPalette(c("grey 90","grey 40"))(nb),
         breaks=brks,
         axes=T,
         xlim=c(xt@xmin,xt@xmax),ylim=c(xt@ymin,xt@ymax),
         lab.breaks=lab.brks,
         zlim=c(0,max(brks,na.rm=T)),
         horizontal=T,
         legend.width=1.5,
         legend.shrink=0.99,
         nlevel=nb*100)
    grid(col="black")
    plot(wrld_simpl,lwd=1,add=T)
    dev.off()
    
    
  }
  
  return("Done!")
}

