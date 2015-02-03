#Set directories
wfdDir <- "//dapadfs/data_cluster_4/observed/gridded_products/wfd"
gcmHistDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/"
gcmFutDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/"
outDir <- "W:/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat"
var <- "rsds"

gcmList <- list.dirs(gcmHistDir, recursive = FALSE, full.names = FALSE)

for (gcm in gcmList[1:20]){
  
  gcm <- basename(gcm)
  
  if(!file.exists(paste(outDir, "/", gcm, "/2020_2049/", "bc_", var, "_2020_2049.Rdat", sep=''))){
    
    cellID <- read.table(paste(outDir, "/cells_index.txt", sep=""), header=T) ##Tener en cuenta el cellID
    histDates <- read.table(paste(outDir, "/historic_dates_index.txt", sep=""), header=T)
    futDates <- read.table(paste(outDir, "/future_dates_index.txt", sep=""), header=T)
    
    gcmHistBCLs <- list.files(paste(outDir, "/", gcm, "/1971_2000", sep=""), pattern=paste("bc_", var, "_1950_2000_", "*", sep=""), full.names=T)
    gcmFutBCLs <- list.files(paste(outDir, "/", gcm, "/2020_2049", sep=""), pattern=paste("bc_", var, "_2020_2049_", "*", sep=""), full.names=T)
    
    if(length(gcmHistBCLs) == 8 && length(gcmFutBCLs) == 8){
      
      
      dataMatrix <- c()
      
      for (i in 1:length(gcmHistBCLs)){
        
        print(paste(basename(gcmHistBCLs[i]), gcm))
        load(gcmHistBCLs[i])
        dataMatrix <- rbind(dataMatrix, gcmHistBC[1:1000,])
        
      }
      
      dataMatrix <- cbind(cellID[1:8000,], dataMatrix)
      colnames(dataMatrix) <- c("CellID","Lon", "Lat", as.vector(as.matrix(histDates)))
      a <- save(dataMatrix, file=paste(outDir, "/", gcm, "/1971_2000/", "bc_", var, "_1950_2000.Rdat", sep=''))
      
      
      
      dataMatrixFut <- c()
      
      for (i in 1:length(gcmFutBCLs)){
        
        print(paste(basename(gcmFutBCLs[i]), gcm))
        load(gcmFutBCLs[i])
        dataMatrixFut <- rbind(dataMatrixFut, gcmFutBC[1:1000,])
        
      }
      
      dataMatrixFut <- cbind(cellID[1:8000,], dataMatrixFut)
      colnames(dataMatrixFut) <- c("CellID", "Lon", "Lat", as.vector(as.matrix(futDates)))
      b <- save(dataMatrixFut, file=paste(outDir, "/", gcm, "/2020_2049/", "bc_", var, "_2020_2049.Rdat", sep=''))
      
    }
    
    
  }
  
  
}



#Set directories
iDir <- "W:/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat"
var <- "rsds"

gcmList <- list.dirs(gcmHistDir, recursive = FALSE, full.names = FALSE)

for (gcm in gcmList[1:20]){
  
  gcm <- basename(gcm)
  
  
  if(!file.exists(paste(outDir, "/", gcm, "/2020_2049/", "bc_", var, "_2020_2049.Rdat", sep=''))){
    
    load(paste(outDir, "/", gcm, "/1971_2000/bc_", var, "_1950_2000.Rdat", sep=""))
    dataMatrix <- c()
    
    for(i in month){
      
      months <- format(as.Date(as.Date(names(dataMatrix)[4:length(dataMatrix)])), "%m")
      colnames(dataMatrix) <- c("Cell", "Lon", "Lat", months)
      selmon <- dataMatrix[, which(colnames(dataMatrix) == "02")]
      mean_mth <- rowMeans(selmon)
    }
    
    
    dataMatrixFut <- c()
    
    load(paste(outDir, "/", gcm, "/2020_2049/bc_", var, "_2020_2049.Rdat", sep=""))
    
    for(i in month){
      months <- format(as.Date(as.Date(names(dataMatrixFut)[4:length(dataMatrixFut)])), "%m")
      colnames(dataMatrixFut) <- c("Cell", "Lon", "Lat",months)
      selmon <- dataMatrixFut[, which(colnames(dataMatrixFut) == "02")]
      mean_mth <- rowMeans(selmon)
    }
    
    dataMatrixFut_t <- cbind(colnames(dataMatrixFut), t(dataMatrixFut))
    stdHist <- aggregate(dataMatrixFut[,4:length(selYrHist)], by=list(selYrHist$GCM, selYrHist$Month), FUN=fun)
    
    
    
    
    
    
    