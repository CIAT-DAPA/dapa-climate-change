#To extract environmental data, just specify the occurrence file (spFile), which should have lon in column 2 and lat in column 3, and your environmental layers directory. You need to specify for which layers you want to extract the data.

extractClimates <- function(spFile, spOutFile, envLyrDir, layers=c(1,12,3,15)) {
  spData <- read.csv(spFile)
  
  spMatrix <- spData[,2:3]
  climMatrix <- matrix(ncol=(ncol(spData)+length(layers)), nrow=nrow(spData))
  climMatrix[,1] <- spData[,1]
  climMatrix[,2] <- spData[,2]
  climMatrix[,3] <- spData[,3]
  
  pb <- pbCreate(length(layers), type='text', style=3)
  for (i in 1:length(layers)) {
    lyr <- layers[i]
    bioVar <- raster(paste(envLyrDir, "//bio_", lyr, ".asc", sep=""), values=T)
    climMatrix[,(i+3)] <- xyValues(bioVar, spMatrix)
    pbStep(pb, i)
  }
  pbClose(pb)
  
  newMatrix <- climMatrix[which(!is.na(climMatrix[,4])),]
  newMatrix <- as.data.frame(newMatrix)
  newMatrix[,1] <- spData[(1:nrow(newMatrix)),1]
  names(newMatrix) <- c("taxon", "lon", "lat", paste("bio_", layers, sep=""))
  
  outCsv <- write.csv(newMatrix, spOutFile, quote=F, row.names=F)
  rm(spMatrix)
  rm(climMatrix)
  rm(bioVar)
  rm(spData)
  return(newMatrix)
}