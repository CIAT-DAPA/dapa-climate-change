#Script to calculate average climates over a set of years by loading the data on different years

require(raster)
require(rgdal)

cat("Available avoid scenarios are: \n")
cat("A1B_A16r2h \n")
cat("A1B_A16r4l \n")
cat("A1B_A16r5l \n")
cat("A1B_A30r2h \n")
cat("A1B_A30r5l \n")

cat("\n")
cat("averageClimates(nyears, centralYear, inputAVOIDScenDIR, outptAVOIDScenDIR, AVOIDScenario) \n")
cat("\n")

averageClimates <- function(nYears, centralYear, inputAVOIDScen, outptAVOIDScen, scen) {

#AVOIDList <- c("A1B_A16r2h", "A1B_A16r4l", "A1B_A16r5l", "A1B_A30r2h", "A1B_A30r5l")

#inputAVOIDScen <- "G://Tyndall_data"
#outptAVOIDScen <- "K://WallaceInitiative//ClimateData"

#nYears <- 30
#centralYear <- 2025
selDec <- paste((centralYear - 5), "s", sep="")

iniYear <- centralYear - nYears / 2
finYear <- centralYear + nYears / 2 - 1

#for (scen in AVOIDList) {
  inFolder <- paste(inputAVOIDScen, "//", scen, "//yearly_asciis", sep="")
  cat("Processing scenario", scen, "\n")
  
  outAVOID <- paste(outptAVOIDScen, "//", scen, sep="")
  
  if (!file.exists(outAVOID)) {
      dir.create(outAVOID) 
    }
   
    if (!file.exists(paste(outAVOID, "//", selDec, sep=""))) {
    	dir.create(paste(outAVOID, "//", selDec, sep=""))
    }
  
  gcmList <- list.files(inFolder, pattern="_")
  
  for (GCM in gcmList) {
    
    cat("Model", GCM, "\n")
    
    inGCMFolder <- paste(inFolder, "//", GCM, sep="")
    outFolder <- paste(outptAVOIDScen, "//", scen, "//", selDec, "//", GCM, sep="")
    
    if (!file.exists(outFolder)) {
      dir.create(outFolder) 
    }
    
    varList <- c("dtr", "pre", "tmp")
    
    for (m in 1:12) {
      
      cat("Month", m, "\n")
      
      for (procVar in varList) {
    
        cat("Variable", procVar, "\n")
        
        if (!file.exists(paste(outFolder, "//", procVar, "_", m, ".asc", sep=""))) {
					
					for (i in iniYear:finYear) {
						
						cat("\n Year", i, "\n")
						
						cat("...Reading input file \n")
						
						fileName <- paste(inGCMFolder, "//", i, "//", procVar, "_all.asc", sep="")
						inData <- read.csv(fileName)
						names(inData) <- c("IDP", "CRUcell", "x", "y", "year", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
						
						spp <- SpatialPoints(inData[,3:4])
						spx <- SpatialPixels(spp)
						rs <- raster(spx)
						
						rm(spp)
						rm(spx)
						
						cat("...xyFromCell \n")
						
						xyMatrix <- xyFromCell(rs, 1:ncell(rs))
						xyMatrix <- cbind(cell=(1:ncell(rs)), xyMatrix)
						
						cat("...Merge the data \n")
						
						outMerge <- merge(xyMatrix, inData, all=T)
						valOrd <- order(outMerge$cell, decreasing=F)
						outMergeOrd <- outMerge[valOrd,]
						
						rm(outMerge)
						rm(valOrd)
						rm(xyMatrix)
						rm(inData)
						
						cat("...Sum the raster \n")
						
						if (i == iniYear) {
							totRaster <- raster(rs)
							totRaster[] <- outMergeOrd[,(m+6)]
						} else {
							tmpRaster <- raster(rs)
							tmpRaster[] <- outMergeOrd[,(m+6)]
							totRaster <- totRaster + tmpRaster
							rm(tmpRaster)
						}
					}
					
					cat("Final calculation \n")
					
					totRaster <- totRaster / nYears
					outName <- paste(outFolder, "//", procVar, "_", m, ".asc", sep="")
					rs <- writeRaster(totRaster, outName, format='ascii')
				}
      }
    } 
  }
#}
}