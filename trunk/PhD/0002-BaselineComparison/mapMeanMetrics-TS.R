#Julian Ramirez-Villegas
#University of Leeds / CCAFS / CIAT
#August, 2011

#Create an average station and fit it with each cell for each GCM
#take the average R2, RMSE, SLOPE for each GCM cell, and GCM, for each period (JJA, DJF, TOTAL)
#produce a map and plot it nicely

#a. Loop through countries for a given GCM
#b. Load metrics-[ISO].csv, select desired period and mashup all this (metrics-mashup), end loop
#c. Load the station locations and keep only ID,LONG,LAT (ghcn_dtr_1961_1990_mean.csv)
#d. Load dummy GCM global mask and assign IDs
#e. Get cellIDs using locations and mask
#f. Average desired parameter for each cell (accounts to country overlaps) and match with 
#   the grid (mask) (hint: na.rm=T)
#g. Write the raster in a given location 
#   (possibly ./results/dataset-vs-gcm-ts/[rain|tmean]-gridded/[GCM]/[R2|SLOPE|RMSE].asc)

require(raster)
rm(list=ls());g=gc();rm(g)
gcm.chars <- read.csv("gcm_chars.csv")

mapMetricMean <- function(wd="",dataset="ghcn",gcm="",variable="rain",period="TTL",metric="R2.FORCED") {
  
  #Input folder stuff
  dstDir <- paste(wd,"/results/",dataset,"-vs-gcm-ts",sep="")
  metDir <- paste(dstDir,"/",variable,"-metrics",sep="")
  inpDir <- paste(wd,"/input-data/",dataset,"-weather-stations/organized-data",sep="")
  
  #Output folder
  outDir <- paste(dstDir,"/",variable,"-gridded",sep=""); if (!file.exists(outDir)) {dir.create(outDir)}
  outDirGCM <- paste(outDir, "/", gcm, sep=""); if (!file.exists(outDirGCM)) {dir.create(outDirGCM)}
  
  filesDir <- paste(metDir,"/",gcm,sep="")
  fileList <- list.files(filesDir,pattern="stmean")
  
  #Looping through countries to mash-up metrics files
  for (isoFile in fileList) {
    #Input metrics file definition
    metFile <- paste(filesDir,"/",isoFile,sep="")
    
    #Loading metrics file
    metrics <- read.csv(metFile)
    metrics <- metrics[which(metrics$MONTH == period),]
    
    #Add column with ISO (filename metrics-stmean-BFA.csv)
    iso <- substr(isoFile,16,18)
    metrics <- cbind(ISO=iso,metrics)
    
    if (isoFile == fileList[1]) {
      metMash <- metrics
    } else {
      metMash <- rbind(metMash,metrics)
    }
  }
  
  #Load dummy global mask of GCM cells and assign cell IDs as cell numbers
  gcm.res <- getGCMRes(gcm, gcm.chars)
  #GCMDir <- paste(wd, "/input-data/gcm-data/20C3M/1961_1990", sep="")
  #modDir <- paste(GCMDir,"/",gcm,sep="")
  xt <- extent(c(-180,180,-90,90))
  nc <- 360/gcm.res
  nr <- 180/gcm.res
  msk <- raster(xt,ncol=nc,nrow=nr) #msk <- raster(paste(modDir,"/prec_01.asc",sep=""))
  msk[] <- 1:ncell(msk)
  
  #Extract stations locations from mask
  metMash$CELL <- extract(msk,data.frame(x=metMash$LONG,y=metMash$LAT))
  
  #Average desired parameter for each cell and match with the grid (mask) (hint: na.rm=T)
  ucell <- unique(metMash$CELL)
  metCol <- which(names(metMash) == metric)
  for (cn in ucell) {
    cellData <- metMash[which(metMash$CELL == cn),metCol]
    meanCell <- mean(cellData,na.rm=T)
    cellv <- data.frame(CELL=cn,MEAN=meanCell)
    
    if (cn == ucell[1]) {
      outData <- cellv
    } else {
      outData <- rbind(outData,cellv)
    }
  }
  
  #Mashing table with grid
  rs <- msk; rs[] <- NA
  rs[outData$CELL] <- outData$MEAN
  
  #Write the raster in a given location 
  outRsName <- paste(outDirGCM,"/",metric,".",period,".CELLMEAN.asc",sep="")
  rs <- writeRaster(rs, outRsName, format='ascii',overwrite=T)
  
  return(rs)
}

#GET GCM Resolution
getGCMRes <- function(gcm, gcm.params) {
  return(gcm.params$dxNW[which(gcm.params$model == gcm)])
}
