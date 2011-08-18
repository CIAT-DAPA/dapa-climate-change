#Julian Ramirez-Villegas
#University of Leeds / CCAFS / CIAT
#August, 2011

#Create an average station and fit it with each cell for each GCM
#take the average R2, RMSE, SLOPE for each GCM cell, and GCM, for each period (JJA, DJF, TOTAL)
#produce a map and plot it nicely

#./rain-per-station/ID.csv structure is:
#ID  YEAR  REPLICATED	LAT	LONG	ALT	JAN	FEB	MAR	APR	MAY	JUN	JUL	AUG	SEP	OCT	NOV	DEC

#./ghcn_dtr_1961_1990_mean.csv structure is:
#ID  REPLICATED	LAT	LONG	ALT	JAN	FEB	MAR	APR	MAY	JUN	JUL	AUG	SEP	OCT	NOV	DEC

# Take the average R2, RMSE, SLOPE for each GCM cell, and GCM, for each period (JJA, DJF, TOTAL)
# produce a map and plot it nicely.

#a. Loop through countries for a given GCM
#b. Load metrics-[ISO].csv, select desired period and mashup all this (metrics-mashup), end loop
#c. Load the station locations and keep only ID,LONG,LAT (ghcn_dtr_1961_1990_mean.csv)
#d. Load dummy GCM global mask and assign IDs
#e. Get station locations from _mean.csv into metrics-mashup (hint: use merge)
#f. Get cellIDs using station locations and mask
#g. Average desired parameter for each cell and match with the grid (mask) (hint: na.rm=T)
#h. Write the raster in a given location 
#   (possibly ./results/dataset-vs-gcm-ts/[rain|tmean]-gridded/[GCM]/[R2|SLOPE|RMSE].asc)

require(raster)
rm(list=ls());g=gc();rm(g)

mapMetric <- function(wd="",dataset="gsod",gcm="",variable="rain",period="TTL",metric="R2.FORCED") {
  #Input folder stuff
  dstDir <- paste(wd,"/results/",dataset,"-vs-gcm-ts",sep="")
  metDir <- paste(dstDir,"/",variable,"-metrics",sep="")
  inpDir <- paste(wd,"/input-data/",dataset,"-weather-stations/all-years",sep="")
  
  #Output folder
  outDir <- paste(dstDir,"/",variable,"-gridded",sep=""); if (!file.exists(outDir)) {dir.create(outDir)}
  outDirGCM <- paste(outDir, "/", gcm, sep=""); if (!file.exists(outDirGCM)) {dir.create(outDirGCM)}
  
  filesDir <- paste(metDir,"/",gcm,sep="")
  fileList <- list.files(filesDir)
  
  #Looping through countries to mash-up metrics files
  for (isoFile in fileList) {
    #Input metrics file definition
    metFile <- paste(filesDir,"/",isoFile,sep="")
    
    #Loading metrics file
    metrics <- read.csv(metFile)
    metrics <- metrics[which(metrics$MONTH == period),]
    
    #Add column with ISO
    iso <- substr(isoFile,9,11)
    metrics <- cbind(ISO=iso,metrics)
    
    if (isoFile == fileList[1]) {
      metMash <- metrics
    } else {
      metMash <- rbind(metMash,metrics)
    }
  }
  
  metMash$ID <- paste(metMash$ID,metMash$WBAN,sep="")
  
  #Now load the stations locations (gsod-rain-1961_1990-mean.csv)
  wsLoc <- read.csv(paste(inpDir,"/",dataset,"-",variable,"-1961_1990-mean.csv",sep=""))
  wsLoc <- data.frame(ID=paste(wsLoc$ID,wsLoc$WBAN,sep=""),LAT=wsLoc$LAT,LONG=wsLoc$LONG)
  
  #Load dummy global mask of GCM cells and assign cell IDs as cell numbers
  modDir <- paste(GCMDir,"/",gcm,sep="")
  msk <- raster(paste(modDir,"/prec_01.asc",sep=""))
  msk[] <- 1:ncell(msk)
  
  #Get stations locations into metrics-mashup (hint: use merge)
  metMash <- merge(metMash,wsLoc,all.x=T,all.y=F)
  
  #Extract stations locations from mask
  metMash$CELL <- extract(msk,data.frame(x=metMash$LONG,y=metMash$LAT))
  
  #Average desired parameter for each cell and match with the grid (mask) (hint: na.rm=T)
  ucell <- unique(metMash$CELL)
  metCol <- which(names(metMash) == metric)
  for (cn in ucell) {
    cellData <- metMash[which(metMash$CELL == cn),metCol]
    meanCell <- mean(cellData,na.rm=T)
    nwstCell <- length(cellData[which(!is.na(cellData))])
    cellv <- data.frame(CELL=cn,MEAN=meanCell,COUNT=nwstCell)
    
    if (cn == ucell[1]) {
      outData <- cellv
    } else {
      outData <- rbind(outData,cellv)
    }
  }
  
  #Mashing table with grid
  rs <- msk; rs[] <- NA
  rs[outData$CELL] <- outData$MEAN
  
  rs.count <- msk; rs.count[] <- NA
  rs.count[outData$CELL] <- outData$COUNT
  
  #Write the raster in a given location 
  outRsName <- paste(outDirGCM,"/",metric,".",period,".asc",sep="")
  rs <- writeRaster(rs, outRsName, format='ascii',overwrite=T)
  
  outRscName <- paste(outDirGCM,"/",metric,".",period,".COUNT.asc",sep="")
  rs.count <- writeRaster(rs.count, outRscName, format='ascii',overwrite=T)
  return(rs)
}

#############################################################################
#############################################################################
#############################################################################