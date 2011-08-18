#Julian Ramirez-Villegas
#August, 2011
#University of Leeds / CCAFS / CIAT

require(raster); require(maptools)
rm(list=ls());g=gc();rm(g)
source("compareGSODRaster-TS.v2.R")

#THis function would take already produced results from the linear comparison between
#GHCN and GCM time series and then perform the same comparison but using the average of all
#cells

compareMeanCellGSOD <- function(wd="",gcm="",variable="rain",divide=F,iso="ETH",isoDir="") {
  #General stuff
  GCMDir <- paste(wd, "/input-data/gcm-data/20C3M/1961_1990", sep="")
  
  #Default dataset for this function
  dataset <- "gsod"
  
  #a. Load the country and generate a mask in the same way it was generated before
  shp <- paste(isoDir,"/",iso,"_adm/",iso,"0.shp",sep="")
  shp <- readShapePoly(shp)
  
  #Comparisons and timeseries folder
  comDir <- paste(wd,"/results/",dataset,"-vs-gcm-ts/",variable,"-extracted/",iso,"/",gcm,sep="")
  tseDir<- paste(wd,"/results/",dataset,"-vs-gcm-ts/",variable,"-timeseries/",iso,"/",gcm,sep="")
  metDir <- paste(wd,"/results/",dataset,"-vs-gcm-ts/",variable,"-metrics/",gcm,sep="")
  
  outMetFile <- paste(metDir,"/metrics-stmean-",iso,".csv",sep="")
  
  if (!file.exists(outMetFile)) {
    #Load GCM configuration via a any raster in there (prec_01.asc)
    rs <- raster(paste(GCMDir,"/",gcm,"/prec_01.asc",sep=""))
    rsol <- getGCMRes(gcm, gcm.chars) #rsol <- xres(rs);rm(rs)
    
    #Create a mask from this shapefile
    msk <- createMask(shp,rsol)
    
    #b. Load the list of stations and keep only ID,LONG,LAT (ghcn_dtr_1961_1990_mean.csv)
    inpDir <- paste(wd,"/input-data/",dataset,"-weather-stations/all-years",sep="")
    in.data <- read.csv(paste(inpDir,"/",dataset,"-",variable,"-1961_1990-mean.csv",sep=""))
    locations <- data.frame(ID=paste(in.data$ID,in.data$WBAN,sep=""),LAT=in.data$LAT,LONG=in.data$LONG)
    
    #c. Do numbering to ISO mask and get locations
    rs <- msk
    rs[] <- 1:ncell(rs); rs[which(is.na(msk[]))] <- NA
    locations$CELL <- extract(rs,data.frame(x=locations$LONG,y=locations$LAT))
    locations <- locations[which(!is.na(locations$CELL)),]
    
    #d. Average at each gridcell all the available WST, for each month, and year (hint: use RowMeans)
    #Loop through cells
    
    ucell <- unique(locations$CELL)
    cat("Processing",length(ucell),"cells \n")
    for (uc in ucell) {
      cat("Cell",uc,"\n")
      #List stations in that cell
      cellLocs <- locations[which(locations$CELL == uc),]
      stList <- unique(cellLocs$ID)
      
      #Loop through them and load the comparison data so that you can get the timeseries
      counter <- 1
      for (st in stList) {
        #cat ("Processing station",paste(st),"\n")
        
        #Read the data in (ID-comparison.csv)
        comFile <- paste(comDir, "/", st,"-comparison.csv",sep="")
        if (file.exists(comFile)) {
          comData <- read.csv(comFile)
          gcmCols <- grep(".GCM",names(comData))
          comData <- comData[,-gcmCols]
          
          #Changing the struture of the data from months in columns to all months in a row
          for (m in 1:12) {
            #Get month name
            mName <- substr(names(comData)[m+1],1,3)
            mthData <- data.frame(YEAR=comData$YEAR,MONTH=mName,VALUE=comData[,m+1])
            if (m == 1) {
              mAll <- mthData
            } else {
              mAll <- rbind(mAll,mthData)
            }
          }
          
          names(mAll) <- c("YEAR","MONTH",paste(st))
          if (counter == 1) {
            stOut <- mAll
          } else {
            stOut <- merge(stOut,mAll,all.x=T,all.y=F)
          }
          
          counter <- counter+1
        }
        
      }
      #Now average all this for that cell (rowMeans)
      tmp <- stOut[,3:ncol(stOut)]
      
      if (length(stList) > 1) {
        rowLen <- apply(tmp,1,function(x) {k<-length(x[which(!is.na(x))]);return(k)})
        stMean <- data.frame(YEAR=stOut$YEAR,MONTH=stOut$MONTH,MEAN=rowMeans(tmp,na.rm=T))
      } else {
        stMean <- data.frame(YEAR=stOut$YEAR,MONTH=stOut$MONTH,MEAN=tmp)
        rowLen <- rep(1,times=nrow(stMean))
      }
      
      #Creating a data frame from the counts
      counts <- data.frame(YEAR=stOut$YEAR,MONTH=stOut$MONTH,rowLen)
      
      #e. Get GCM series from cellID
      GCMFile <- paste(tseDir,"/TS.",uc,".csv",sep="")
      GCMSer <- read.csv(GCMFile)
      
      #Change formatting of this file
      for (m in 1:12) {
        #Get month name
        mName <- substr(names(GCMSer)[m+1],1,3)
        
        gcmM <- GCMSer[,c(1,m+1)]
        stM <- stMean[which(stMean$MONTH == mName),c(1,3)]
        
        outM <- merge(gcmM,stM,all.x=T,all.y=F)
        names(outM) <-c("YEAR",paste(mName,".GCM",sep=""),paste(mName,".WST",sep=""))
        
        #Organising counts
        countData <- counts[which(counts$MONTH == mName),c(1,3)]
        names(countData) <- c("YEAR",paste(mName,".COUNT",sep=""))
        
        #Match both series,
        if (m == 1) {
          outAll <- outM
          outCount <- countData
        } else {
          outAll <- merge(outAll,outM)
          outCount <- merge(outCount,countData)
        }
      }
      
      #Calculate TTL, DJF, JJA
      outAll$TTL.GCM <- outAll$JAN.GCM+outAll$FEB.GCM+outAll$MAR.GCM+outAll$APR.GCM+
                            outAll$MAY.GCM+outAll$JUN.GCM+outAll$JUL.GCM+outAll$AUG.GCM+
                            outAll$SEP.GCM+outAll$OCT.GCM+outAll$NOV.GCM+outAll$DEC.GCM
      outAll$TTL.WST <- outAll$JAN.WST+outAll$FEB.WST+outAll$MAR.WST+outAll$APR.WST+
                            outAll$MAY.WST+outAll$JUN.WST+outAll$JUL.WST+outAll$AUG.WST+
                            outAll$SEP.WST+outAll$OCT.WST+outAll$NOV.WST+outAll$DEC.WST
      outAll$DJF.GCM <- outAll$DEC.GCM+outAll$JAN.GCM+outAll$FEB.GCM
      outAll$DJF.WST <- outAll$DEC.WST+outAll$JAN.WST+outAll$FEB.WST
      outAll$JJA.GCM <- outAll$JUN.GCM+outAll$JUL.GCM+outAll$AUG.GCM
      outAll$JJA.WST <- outAll$JUN.WST+outAll$JUL.WST+outAll$AUG.WST
      
      if (divide) {
        outAll$TTL.GCM <- outAll$TTL.GCM/12
        outAll$TTL.WST <- outAll$TTL.WST/12
        outAll$DJF.GCM <- outAll$DJF.GCM/3
        outAll$DJF.WST <- outAll$DJF.WST/3
        outAll$JJA.GCM <- outAll$JJA.GCM/3
        outAll$JJA.WST <- outAll$JJA.WST/3
      }
      
      #f. Fit the regressions and get parameters and store the results at the same
      #   location where all other stations are, but name it as "cell[ID]-comparison.csv", where
      #   ID is the cell ID
      compRes <- compareTS(outAll,plotit=F,plotName="null")
      
      #Write the actual data to dset folder.
      #Get xy from cell ID
      xy <- xyFromCell(msk,uc)
      outAll <- cbind(CELL=uc,LONG=xy[1],LAT=xy[2],outAll)
      outCount <- cbind(CELL=uc,LONG=xy[1],LAT=xy[2],outCount)
      
      compRes <- cbind(CELL=uc,LONG=xy[1],LAT=xy[2],compRes)
      row.names(compRes) <- 1:nrow(compRes)
      
      outAllFile <- paste(comDir,"/cell",uc,"-comparison.csv",sep="")
      write.csv(outAll,outAllFile,row.names=F,quote=F)
      
      outCountFile <- paste(comDir, "/cell",uc,"-count.csv",sep="")
      write.csv(outCount,outCountFile,row.names=F,quote=F)
      
      if (uc == ucell[1]) {
        allMetrics <- compRes
      } else {
        allMetrics <- rbind(allMetrics,compRes)
      }
      
    }
    write.csv(allMetrics,outMetFile,row.names=F,quote=F)
  }
  return("Done!")
}
