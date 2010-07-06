# Julian Ramirez, dawnpatrolmustaine@gmail.com
#Reads dbf files and creates csv files with updated corresponding values (monthly) for each variable

require(foreign)
require(rgdal)
require(sp)
require(raster)

wcDataUpdate <- function(scenario='SRES_A1B', model='bccr_bcm2_0') {
  
  if (!toupper(scenario) %in% c("SRES_A1B","SRES_A2","SRES_B1")) {
    stop('Scenario', scenario, ' is not supported')
  }
  
  # Folder with worldclim data
  
  wcFolder <- "X://climate_change//_scripts"
  
  # Folder with future anomalies
  
  rootFolder <- "X://climate_change//IPCC_CMIP3"
  
  scenDir <- paste(rootFolder, "//", scenario, sep="")
  modelDir <- paste(scenDir, "//anomalies", sep="")
  
  # Output directory
  
  outDir <- paste(scenDir, "//updated", sep="")
  if (!file.exists(outDir)) {
    dir.create(outDir)
  }
  
  # Listing the models
  
  #modList <- list.files(modelDir, pattern="bccr_*")
  modName <- model
  
  #for (modName in modList) {
    cat("\n", "Processing model ", modName, "\n")
    
    modPath <- paste(modelDir, "//", modName, sep="")
    if (!file.exists(modPath)) {
      stop('Invalid model name, ', modName, 'please correct')
    }
    
    outModPath <- paste(outDir, "//", modName, sep="")
    if (!file.exists(outModPath)) {
      dir.create(outModPath)
    }
    
    # Listing timeslices
    
    periodList <- list.files(modPath, pattern="*_*")
    periodList <- periodList[which(periodList != "done.txt")]
    
    for (period in periodList) {
      
      cat("Processing period ", period, "\n")
      
      outModPerPath <- paste(outModPath, "//", period, sep="")
      if (!file.exists(outModPerPath)) {
        dir.create(outModPerPath)
      }
      
      # Listing and looping the variables
      
      varList <- c("tmean", "tmin", "tmax", "prec")
      
      for (varName in varList) {
        
        outFile <- paste(outModPerPath, "//", varName, ".csv", sep="")
        
        if (!file.exists(outFile)) {
          
          cat("Processing variable ", varName, "\n")
          
          # Loading WorldClim data
          
          wcData <- read.dbf(paste(wcFolder, "//", "wc_", varName, "_stations.dbf", sep=""))
          
          # Create a matrix of 13 columns and nrow equal to the number of rows in the dbf (number of stations)
          # ID, Lon, Lat, Alt, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
          
          newData <- matrix(ncol=16, nrow=nrow(wcData))
          
          # Filling the matrix with the data
          
          newData[,1] <- wcData$ID
          newData[,2] <- wcData$LONG
          newData[,3] <- wcData$LAT
          newData[,4] <- wcData$ALT
          newData[,5] <- wcData$JAN
          newData[,6] <- wcData$FEB
          newData[,7] <- wcData$MAR
          newData[,8] <- wcData$APR
          newData[,9] <- wcData$MAY
          newData[,10] <- wcData$JUN
          newData[,11] <- wcData$JUL
          newData[,12] <- wcData$AUG
          newData[,13] <- wcData$SEP
          newData[,14] <- wcData$OCT
          newData[,15] <- wcData$NOV
          newData[,16] <- wcData$DEC
          
          resNmx <- matrix(nrow=nrow(newData), ncol=ncol(newData))
          resNmx[,1] <- paste(wcData$ID)
          resNmx[,2] <- wcData$LONG
          resNmx[,3] <- wcData$LAT
          resNmx[,4] <- wcData$ALT
          
          # Looping the models
          
          for (m in 1:12) {
            
            cat("Processing month ", m, "\n")
            
            # Loading the raster with anomalies
            
            rName <- paste(modPath, "//", period, "//", varName, "_", m, ".asc", sep="")
            rs <- raster(rName)
            
            # A matrix with the xy coordinates of the input raster with anomalies
            
            xyVals <- xyFromCell(rs, 1:ncell(rs))
    
            # Getting the values (the update values) from the raster
    
            cVals <- cellValues(rs, 1:ncell(rs))
    
            # Creating an output matrix where all the distances, the xy coordinates, and the values are to be stored
            # CellNumber, Lon, Lat, Value, Distance
            
            outMx <- matrix(nrow=nrow(xyVals), ncol=5)
            outMx[,1] <- 1:ncell(rs) #Cell ID
            outMx[,2] <- xyVals[,1] #Lon
            outMx[,3] <- xyVals[,2] #Lat
            outMx[,4] <- cVals  #Values
            
            # The update function
            
            updateStation <- function(stationData) {
              
              # Station coordinate and distances to all cells
              
              thePoint <- c(stationData[2], stationData[3])
              pd <- pointDistance(thePoint, xyVals, type='Euclidean')
              outMx[,5] <- pd # Distance
              rm(pd)
              
              # Creating a data frame only with cells within a 4 degree radius
              
              selCells <- data.frame(CELL=outMx[which(outMx[,5] <= 4),1], LON=outMx[which(outMx[,5] <= 4),2], LAT=outMx[which(outMx[,5] <= 4),3], VALUE=outMx[which(outMx[,5] <= 4),4], DISTANCE=outMx[which(outMx[,5] <= 4),5])
              
              # Creating a matrix with the selected cells to calculate inverse distances and weight each value
              
              nwValues <- matrix(nrow=nrow(selCells), ncol=4)
              nwValues[,1] <- selCells$DISTANCE
              nwValues[,2] <- selCells$VALUE
              nwValues[,3] <- 1 / selCells$DISTANCE
              nwValues[,4] <- nwValues[,3] * selCells$VALUE
              
              # Calculating total inverse distance (weighting factor) and total of values
              
              sumInvDist <- sum(nwValues[,3])
              sumUpdated <- sum(nwValues[,4])
              
              rm(nwValues)
              
              # Calculating new values
              
              toUpdate <- sumUpdated / sumInvDist
              
              # Putting the data in the update matrix
              
              if (varName == "prec") {
                relChg <- abs(1 + (toUpdate/ (stationData[(4+m)] + 1)))
                newValue <- stationData[(4+m)] * relChg
              } else {
                newValue <- toUpdate + stationData[(4+m)]
              }
              result <- newValue
              
              return(result)
            }
          
          resMx <- apply(newData, 1, updateStation)
          resNmx[,(4+m)] <- resMx
          rm(resMx)
          
          }
        
        resDF <- as.data.frame(resNmx)
        names(resDF) <- c("ID","Lon","Lat","Alt","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
        out <- write.csv(resDF, file=outFile, row.names=F, quote=F)
        
        } else {
          cat("File ", outFile, " already exists", "\n")
        }
        
      }
      
    }
  #}
  
  return("Done!")
}