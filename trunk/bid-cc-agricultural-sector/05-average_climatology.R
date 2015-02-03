#Set directories
iDir <- "W:/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat"
var <- "rsds"

gcmList <- list.dirs(iDir, recursive = FALSE, full.names = FALSE)

mthLs <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
mthListMod <- c(1:12)
ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
mthMat <- as.data.frame(cbind(mthLs, mthListMod, ndays))
names(mthMat) <- c("Mth", "MthMod", "Ndays")

for (gcm in gcmList[1:20]){
  
  gcm <- basename(gcm)
    
  if(file.exists(paste(iDir, "/", gcm, "/2020_2049/", "bc_", var, "_2020_2049.Rdat", sep=''))){
    
    cat(paste("Averaging Hist", gcm, "\n"))
    
    load(paste(iDir, "/", gcm, "/1971_2000/bc_", var, "_1950_2000.Rdat", sep=""))
    
    months <- format(as.Date(as.Date(names(dataMatrix)[4:length(dataMatrix)])), "%m")
    colnames(dataMatrix) <- c("Cell", "Lon", "Lat", months)
    
    databymth <- dataMatrix[1:3]
    
    for(i in 1:12){
      
      selmon <- dataMatrix[, which(colnames(dataMatrix) == paste(mthMat[i,1]))]
      if (var == "prec"){
        mean_mth <- rowMeans(selmon, na.rm = T) * as.numeric(paste(mthMat[i,3]))
      } else {
        mean_mth <- rowMeans(selmon, na.rm = T) 
      }
      
      
      databymth <- cbind(databymth, mean_mth)
      
    }
    
    colnames(databymth) <- c("Cell", "Lon", "Lat", 1:12)
    
    #   Writting csv file 
    write.csv(databymth, paste(iDir, "/", gcm, "/1971_2000/", "bc_", var, "_1950_2000_climatology.csv", sep=""), row.names=F)
    
    
    load(paste(iDir, "/", gcm, "/2020_2049/bc_", var, "_2020_2049.Rdat", sep=""))
    
    months <- format(as.Date(as.Date(names(dataMatrixFut)[4:length(dataMatrixFut)])), "%m")
    colnames(dataMatrixFut) <- c("Cell", "Lon", "Lat", months)
    
    cat(paste("Averaging Fut", gcm, "\n"))
    
    databymth <- dataMatrixFut[1:3]
    
    for(mth in mthLs){
     
      selmon <- dataMatrixFut[, which(colnames(dataMatrixFut) == paste(mthMat[i,1]))]
      
      if (var == "prec"){
        mean_mth <- rowMeans(selmon, na.rm = T) * as.numeric(paste(mthMat[i,3]))
      } else {
        mean_mth <- rowMeans(selmon, na.rm = T)  
      }
      
      databymth <- cbind(databymth, mean_mth)
      
    }
    
    colnames(databymth) <- c("Cell", "Lon", "Lat", 1:12)
    
    #   Writting csv file 
    write.csv(databymth, paste(iDir, "/", gcm, "/2020_2049/", "bc_", var, "_2020_2049_climatology.csv", sep=""), row.names=F)
    
  }
}

    