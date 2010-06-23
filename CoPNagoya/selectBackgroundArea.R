#Select the background file based on where the species occurrences to be continental areas. Continents used are previously prepared ascii files at any resolution (an intermediate one such as 0.1 or 0.2 should be ok) for Asia, Africa, Australia+New Zealand, Europe (incl. Russia), Latin America (incl. Mexico) and North America (incl. Greenland). These files, along with the background files themselves should be in the same folder (backFilesDir).

#The input file (occFile) needs to have lon and lat in the 2nd (lon) and 3rd (lat) columns. The output (outBackName) file will contain the same number of columns and rows, but will have cells from all the continents in which the species has at least one sample. 

require(rgdal)
require(raster)
  
selectBack <- function(occFile, outBackName, backFilesDir) {
  
  if (file.exists(occFile)) {
    cat("Selecting...\n")
    spData <- read.csv(occFile)
    
    globZonesFile <- paste(backFilesDir, "//backselection.asc", sep="")
    globZones <- raster(globZonesFile)
    
    occZones <- xyValues(globZones, spData[,2:3])
    occZones <- occZones[which(!is.na(occZones[]))]
    uniqueOccZones <- unique(occZones)
    
    if (length(uniqueOccZones) == 1) {
      zone <- uniqueOccZones
      backFile <- paste(backFilesDir, "//backsamples_z", zone, ".csv", sep="")
      backPts <- read.csv(backFile)
      finalBackPts <- backPts
      out <- write.csv(finalBackPts, outBackName, quote=F, row.names=F)
      
      rm(uniqueOccZones)
      rm(occZones)
      rm(globZones)
      rm(spData)
      rm(backPts)
    } else {
      
      zCounter <- 1
      
      for (zone in uniqueOccZones) {
        backFile <- paste(backFilesDir, "//backsamples_z", zone, ".csv", sep="")
        backPts <- read.csv(backFile)
        if (zCounter == 1) {
          backPoints <- backPts
          rm(backPts)
          zCounter <- zCounter + 1
        } else {
          backPoints <- rbind(backPoints, backPts)
          rm(backPts)
          zCounter <- zCounter + 1
        }
      }
      selPts <- sample(1:nrow(backPoints), 10000)
      finalBackPts <- backPoints[selPts,]
      
      out <- write.csv(finalBackPts, outBackName, quote=F, row.names=F)
    }
  }
  return(finalBackPts)
}
