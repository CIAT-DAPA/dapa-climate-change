library(reshape2)
library(raster)
library(ncdf)

#Set directories
wfdDir <- "//dapadfs/data_cluster_4/observed/gridded_products/wfd"
bcDir <- "W:/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat"
rawHisDir <- "D:/cenavarro/bid/gcm_0_5deg_lat"
rawFutDir <- "W:/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat"
oDir <- "W:/bid-cc-agricultural-sector/01-climate-data/bc_extracts/plots/correlation_matrices"
stFile <- "W:/bid-cc-agricultural-sector/01-climate-data/sample_type_climates.csv"
stFile <- read.csv(stFile)

#List of variables
varList <- c("tmax", "tmin", "prec", "rsds")

# Cells and dates 
cellID <- read.table(paste(bcDir, "/cells_index.txt", sep=""), header=T) ##Tener en cuenta el cellID
histDates <- read.table(paste(bcDir, "/historic_dates_index.txt", sep=""), header=T)
futDates <- read.table(paste(bcDir, "/future_dates_index.txt", sep=""), header=T)

years.hist = 1971:2000  #goes from 1950 to 2001, should use this period eventually (needs to be consistent with GCM.hist)
dates.hist = seq(as.Date('1971-01-01'),as.Date('2000-12-31'),by=1)
months = c(paste(0,1:9,sep=''),10:12)

years.fut = 2020:2049  #goes from 1950 to 2001, should use this period eventually (needs to be consistent with GCM.hist)
dates.fut = seq(as.Date('2020-01-01'),as.Date('2049-12-31'),by=1)

# Mask Observations
maskWFDLat <- raster(paste(wfdDir,"/raw/mask_wfd_lat.nc",sep=''))
ncell <- 8000

#List of GCMs
gcmList <- list.dirs(bcDir, recursive = FALSE, full.names = FALSE)

## Function correlation matrix
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 2)
}

#Loop GCMs BC Hist
for (gcm in gcmList){

  print(gcm)
  
  ## Load rsds
  rsds <- load(paste(bcDir, "/", gcm, "/1971_2000/bc_rsds_1950_2000_daily.Rdat", sep=''))
  rsds <- cbind(dataMatrix[1], dataMatrix[7674:length(dataMatrix)])
  rsds <- melt(rsds, id=c("CellID"))

  ## Load prec
  prec <- load(paste(bcDir, "/", gcm, "/1971_2000/bc_prec_1950_2000_daily.Rdat", sep=''))
  prec <- cbind(dataMatrix[1], dataMatrix[7674:length(dataMatrix)])
  prec <- melt(prec, id=c("CellID"))

  ## Load tmax 
  var = "tmax"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(bcDir, '/',gcm, '/1971_2000/by_month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  tmax <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load tmin 
  var = "tmin"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(bcDir, '/',gcm, '/1971_2000/by_month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  tmin <- melt(hist.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(prec[,1], prec[,3], rsds[,3], tmax[,3], tmin[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID","Prec", "Rsds", "Tmax", "Tmin")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/bc_hist_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)

      mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
      pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("BC Historical      Type:",stFile$Type[i], "     GCM:", gcm))
    
    dev.off()
    
  }
          
}

#Loop GCMs BC Fut
for (gcm in gcmList){
  
  print(gcm)
  
  ## Load rsds
  rsds <- load(paste(bcDir, "/", gcm, "/2020_2049/bc_rsds_2020_2049_daily.Rdat", sep=''))
  rsds <- cbind(dataMatrixFut[1], dataMatrixFut)
  rsds <- melt(rsds, id=c("CellID"))
  
  ## Load prec
  prec <- load(paste(bcDir, "/", gcm, "/2020_2049/bc_prec_2020_2049_daily.Rdat", sep=''))
  prec <- cbind(dataMatrixFut[1], dataMatrixFut)
  prec <- melt(prec, id=c("CellID"))
  
  ## Load tmax 
  var = "tmax"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(bcDir, '/',gcm, '/2020_2049/by_month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  tmax <- melt(fut.dat.all, id=c("CellID"))
  
  ## Load tmin 
  var = "tmin"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(bcDir, '/',gcm, '/2020_2049/by_month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  tmin <- melt(fut.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(prec[,1], prec[,3], rsds[,3], tmax[,3], tmin[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID","Prec", "Rsds", "Tmax", "Tmin")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/bc_fut_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("BC Future      Type:",stFile$Type[i], "     GCM:", gcm))
    
    dev.off()
    
  }
  
}

#Loop GCMs Raw Hist
for (gcm in gcmList){
  
  print(gcm)
  
  ## Load rsds
  var = "rsds"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  rsds <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load prec
  var = "prec"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  prec <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load tmax 
  var = "tmax"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  tmax <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load tmin 
  var = "tmin"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  tmin <- melt(hist.dat.all, id=c("CellID"))
  
  
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(prec[,1], prec[,3], rsds[,3], tmax[,3], tmin[,3])
  mergeVar <- na.omit(mergeVar)  
  colnames(mergeVar) <- c("CellID","Prec", "Rsds", "Tmax", "Tmin")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/test_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("Raw Historical      Type:",stFile$Type[i], "     GCM:", gcm))
    
    dev.off()
    
  }
  
}

#Loop GCMs Raw Fut
for (gcm in gcmList){
  
  print(gcm)
  
  ## Load rsds
  var = "rsds"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  rsds <- melt(fut.dat.all, id=c("CellID"))
  
  ## Load prec
  var = "prec"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  prec <- melt(fut.dat.all, id=c("CellID"))
  
  ## Load tmax 
  var = "tmax"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  tmax <- melt(fut.dat.all, id=c("CellID"))
  
  ## Load tmin 
  var = "tmin"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  tmin <- melt(fut.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(prec[,1], prec[,3], rsds[,3], tmax[,3], tmin[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID","Prec", "Rsds", "Tmax", "Tmin")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/raw_fut_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("BC Future      Type:",stFile$Type[i], "     GCM:", gcm))
    
    dev.off()
    
  }
  
}

#Loop GCMs Raw Fut
for (gcm in gcmList){
  
  print(gcm)
  
  ## Load rsds
  var = "rsds"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  rsds <- melt(fut.dat.all, id=c("CellID"))
  
  ## Load prec
  var = "prec"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  prec <- melt(fut.dat.all, id=c("CellID"))
  
  ## Load tmax 
  var = "tmax"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  tmax <- melt(fut.dat.all, id=c("CellID"))
  
  ## Load tmin 
  var = "tmin"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  tmin <- melt(fut.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(prec[,1], prec[,3], rsds[,3], tmax[,3], tmin[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID","Prec", "Rsds", "Tmax", "Tmin")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/raw_fut_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("BC Future      Type:",stFile$Type[i], "     GCM:", gcm))
    
    dev.off()
    
  }
  
}


#Loop WFD Obs

## Load prec
var = "prec"
if(var == "prec"){varmod <- "Rainf"}
if(var == "rsds"){varmod <- "SWdown"}
hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
for (j in 1:length(years.hist)){
  for (k in 1:12){
    WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD_GPCC/lat_', varmod, '_daily_WFD_GPCC_',years.hist[j],months[k],'.nc',sep=''))        
    xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
    xmax(WFD) = xmax(WFD) - 360
    WFD = mask(WFD, maskWFDLat)  #cut to Latin America
    hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
    if (j==1 & k==1)  {
      cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
    }
    n = dim(hist.dat)[2] - 2  #calculate number of days in month
    dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
    ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
    hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]*86400  #convert to mm before putting in big matrix
    print(paste(j,k))  #print month & year of loop
  }
}
hist.dat.all <- cbind(1:8000, hist.dat.all)
hist.dat.all <- as.data.frame(hist.dat.all)
names(hist.dat.all) <- c("CellID", paste(dates.hist))
prec <- melt(hist.dat.all, id=c("CellID"))



## Load prec
var = "rsds"
if(var == "prec"){varmod <- "Rainf"}
if(var == "rsds"){varmod <- "SWdown"}
hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
for (j in 1:length(years.hist)){
  for (k in 1:12){
    WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD/lat_', varmod, '_daily_WFD_',years.hist[j],months[k],'.nc',sep=''))        
    xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
    xmax(WFD) = xmax(WFD) - 360
    WFD = mask(WFD, maskWFDLat)  #cut to Latin America
    hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
    if (j==1 & k==1)  {
      cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
    }
    n = dim(hist.dat)[2] - 2  #calculate number of days in month
    dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
    ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
    hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]  #convert to mm before putting in big matrix
    print(paste(j,k))  #print month & year of loop
  }
}
hist.dat.all <- cbind(1:8000, hist.dat.all)
hist.dat.all <- as.data.frame(hist.dat.all)
names(hist.dat.all) <- c("CellID", paste(dates.hist))
rsds <- melt(hist.dat.all, id=c("CellID"))

## Load tmax
var = "tmax"
if(var == "prec"){varmod <- "Rainf"}
if(var == "rsds"){varmod <- "SWdown"}
if(var == "tmax"){varmod <- "Tmax"}
hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
for (j in 1:length(years.hist)){
  for (k in 1:12){
    WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD/lat_', varmod, '_daily_WFD_',years.hist[j],months[k],'.nc',sep=''))        
    xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
    xmax(WFD) = xmax(WFD) - 360
    WFD = mask(WFD, maskWFDLat)  #cut to Latin America
    hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
    if (j==1 & k==1)  {
      cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
    }
    n = dim(hist.dat)[2] - 2  #calculate number of days in month
    dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
    ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
    hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]  #convert to mm before putting in big matrix
    print(paste(j,k))  #print month & year of loop
  }
}
hist.dat.all <- cbind(1:8000, hist.dat.all)
hist.dat.all <- as.data.frame(hist.dat.all)
names(hist.dat.all) <- c("CellID", paste(dates.hist))
tmax <- melt(hist.dat.all, id=c("CellID"))

## Load tmin
var = "tmin"
if(var == "prec"){varmod <- "Rainf"}
if(var == "rsds"){varmod <- "SWdown"}
if(var == "tmin"){varmod <- "Tmin"}
hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
for (j in 1:length(years.hist)){
  for (k in 1:12){
    WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD/lat_', varmod, '_daily_WFD_',years.hist[j],months[k],'.nc',sep=''))        
    xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
    xmax(WFD) = xmax(WFD) - 360
    WFD = mask(WFD, maskWFDLat)  #cut to Latin America
    hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
    if (j==1 & k==1)  {
      cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
    }
    n = dim(hist.dat)[2] - 2  #calculate number of days in month
    dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
    ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
    hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]  #convert to mm before putting in big matrix
    print(paste(j,k))  #print month & year of loop
  }
}
hist.dat.all <- cbind(1:8000, hist.dat.all)
hist.dat.all <- as.data.frame(hist.dat.all)
names(hist.dat.all) <- c("CellID", paste(dates.hist))
tmin <- melt(hist.dat.all, id=c("CellID"))


## Combine all variables in a single matrix
mergeVar <- cbind(prec[,1], prec[,3], rsds[,3], tmax[,3], tmin[,3])
mergeVar <- na.omit(mergeVar)
colnames(mergeVar) <- c("CellID","Prec", "Rsds", "Tmax", "Tmin")

mergeVar <- data.frame(mergeVar)

for(i in c(1, 2, 5, 12, 14)){
  
  id <- stFile$Cell_ID_prec_rsds[i]
  
  tiff(paste(oDir, "/wfd_obs_", stFile$Type[i], ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
  
  mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
  pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("Wfd Obs      Type:",stFile$Type[i]))
  
  dev.off()
  
}





#Loop prec 
for (gcm in gcmList){
  
  print(gcm)
  
  ## Load prec
  var = "prec"
  if(var == "prec"){varmod <- "Rainf"}
  if(var == "rsds"){varmod <- "SWdown"}
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12){
      WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD_GPCC/lat_', varmod, '_daily_WFD_GPCC_',years.hist[j],months[k],'.nc',sep=''))        
      xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
      xmax(WFD) = xmax(WFD) - 360
      WFD = mask(WFD, maskWFDLat)  #cut to Latin America
      hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
      if (j==1 & k==1)  {
        cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
      }
      n = dim(hist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]*86400  #convert to mm before putting in big matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  wfd_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load prec
  prec <- load(paste(bcDir, "/", gcm, "/1971_2000/bc_prec_1950_2000_daily.Rdat", sep=''))
  prec <- cbind(dataMatrix[1], dataMatrix[7674:length(dataMatrix)])
  bc_his <- melt(prec, id=c("CellID"))

  
  ## Load prec
  prec <- load(paste(bcDir, "/", gcm, "/2020_2049/bc_prec_2020_2049_daily.Rdat", sep=''))
  prec <- cbind(dataMatrixFut[1], dataMatrixFut)
  bc_fut <- melt(prec, id=c("CellID"))
  
  ## Load prec
  var = "prec"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  raw_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load prec
  var = "prec"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  raw_fut <- melt(fut.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(bc_his[,1], wfd_his[,3], bc_his[,3], bc_fut[,3], raw_his[,3], raw_fut[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID", "WFD Obs","BC His", "BC Fut", "Raw His", "Raw Fut")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/", var, "_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("Type:",stFile$Type[i], "  GCM:", gcm, "  Var:", var))
    
    dev.off()
    
  }
}

#Loop rsds 
for (gcm in gcmList){
  
  print(gcm)
  
  ## Load prec
  var = "rsds"
  if(var == "prec"){varmod <- "Rainf"}
  if(var == "rsds"){varmod <- "SWdown"}
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12){
      WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD/lat_', varmod, '_daily_WFD_',years.hist[j],months[k],'.nc',sep=''))        
      xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
      xmax(WFD) = xmax(WFD) - 360
      WFD = mask(WFD, maskWFDLat)  #cut to Latin America
      hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
      if (j==1 & k==1)  {
        cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
      }
      n = dim(hist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]  #convert to mm before putting in big matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  wfd_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load prec
  rsds <- load(paste(bcDir, "/", gcm, "/1971_2000/bc_rsds_1950_2000_daily.Rdat", sep=''))
  rsds <- cbind(dataMatrix[1], dataMatrix[7674:length(dataMatrix)])
  bc_his <- melt(rsds, id=c("CellID"))
  
  
  ## Load prec
  rsds <- load(paste(bcDir, "/", gcm, "/2020_2049/bc_rsds_2020_2049_daily.Rdat", sep=''))
  rsds <- cbind(dataMatrixFut[1], dataMatrixFut)
  bc_fut <- melt(rsds, id=c("CellID"))
  
  ## Load prec
  var = "rsds"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  raw_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load prec
  var = "rsds"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  raw_fut <- melt(fut.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(bc_his[,1], wfd_his[,3], bc_his[,3], bc_fut[,3], raw_his[,3], raw_fut[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID", "WFD Obs", "BC His", "BC Fut", "Raw His", "Raw Fut")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/", var, "_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("Type:",stFile$Type[i], "  GCM:", gcm, "  Var:", var))
    
    dev.off()
    
  }
  
}

#Loop tmax 
for (gcm in gcmList){
  
  print(gcm)
  
  ## Load tmax
  var = "tmax"
  if(var == "prec"){varmod <- "Rainf"}
  if(var == "rsds"){varmod <- "SWdown"}
  if(var == "tmax"){varmod <- "Tmax"}
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12){
      WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD/lat_', varmod, '_daily_WFD_',years.hist[j],months[k],'.nc',sep=''))        
      xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
      xmax(WFD) = xmax(WFD) - 360
      WFD = mask(WFD, maskWFDLat)  #cut to Latin America
      hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
      if (j==1 & k==1)  {
        cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
      }
      n = dim(hist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]  #convert to mm before putting in big matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  wfd_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load tmax 
  var = "tmax"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(bcDir, '/',gcm, '/1971_2000/by_month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  bc_his <- melt(hist.dat.all, id=c("CellID"))
  
  
  ## Load tmax 
  var = "tmax"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(bcDir, '/',gcm, '/2020_2049/by_month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  bc_fut <- melt(fut.dat.all, id=c("CellID"))
  
  
  ## Load prec
  var = "tmax"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  raw_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load prec
  var = "tmax"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  raw_fut <- melt(fut.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(bc_his[,1], wfd_his[,3], bc_his[,3], bc_fut[,3], raw_his[,3], raw_fut[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID", "WFD Obs", "BC His", "BC Fut", "Raw His", "Raw Fut")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/", var, "_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("Type:",stFile$Type[i], "  GCM:", gcm, "  Var:", var))
    
    dev.off()
    
  }
  
}

#Loop tmin 
for (gcm in gcmList[c(1:6,8:20)]){
  
  print(gcm)
  
  ## Load tmin
  var = "tmin"
  if(var == "prec"){varmod <- "Rainf"}
  if(var == "rsds"){varmod <- "SWdown"}
  if(var == "tmin"){varmod <- "Tmin"}
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12){
      WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD/lat_', varmod, '_daily_WFD_',years.hist[j],months[k],'.nc',sep=''))        
      xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
      xmax(WFD) = xmax(WFD) - 360
      WFD = mask(WFD, maskWFDLat)  #cut to Latin America
      hist.dat = rasterToPoints(WFD)[1:8000,]  #extract values (gridcells by days)
      if (j==1 & k==1)  {
        cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
      }
      n = dim(hist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]  #convert to mm before putting in big matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  wfd_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load tmin 
  var = "tmin"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(bcDir, '/',gcm, '/1971_2000/by_month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  bc_his <- melt(hist.dat.all, id=c("CellID"))
  
  
  ## Load tmin 
  var = "tmin"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(bcDir, '/',gcm, '/2020_2049/by_month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  bc_fut <- melt(fut.dat.all, id=c("CellID"))
  
  
  ## Load tmin
  var = "tmin"
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x days x months x years
  for (j in 1:length(years.hist)){
    for (k in 1:12)  {
      gcmHist = stack(paste(rawHisDir, '/',gcm, '/1950_2000/by-month/',var,'_',years.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = rasterToPoints(gcmHist)[1:8000,]
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
      hist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  hist.dat.all <- cbind(1:8000, hist.dat.all)
  hist.dat.all <- as.data.frame(hist.dat.all)
  names(hist.dat.all) <- c("CellID", paste(dates.hist))
  raw_his <- melt(hist.dat.all, id=c("CellID"))
  
  ## Load tmin
  var = "tmin"
  fut.dat.all = array(NA,dim=c(ncell,length(dates.fut)))  #gridcells x days x months x years
  for (j in 1:length(years.fut)){
    for (k in 1:12)  {
      gcmfut = stack(paste(rawFutDir, '/',gcm, '/2020_2049/by-month/',var,'_',years.fut[j],'_',months[k],'.nc',sep=''))  
      gcmfut.dat = rasterToPoints(gcmfut)[1:8000,]
      n = dim(gcmfut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.fut)  #find indices of this month in big dates vector
      fut.dat.all[,ind.mo] = gcmfut.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  fut.dat.all <- cbind(1:8000, fut.dat.all)
  fut.dat.all <- as.data.frame(fut.dat.all)
  names(fut.dat.all) <- c("CellID", paste(dates.fut))
  raw_fut <- melt(fut.dat.all, id=c("CellID"))
  
  ## Combine all variables in a single matrix
  mergeVar <- cbind(bc_his[,1], wfd_his[,3], bc_his[,3], bc_fut[,3], raw_his[,3], raw_fut[,3])
  mergeVar <- na.omit(mergeVar)
  colnames(mergeVar) <- c("CellID", "WFD Obs", "BC His", "BC Fut", "Raw His", "Raw Fut")
  
  mergeVar <- data.frame(mergeVar)
  
  for(i in c(1, 2, 5, 12, 14)){
    
    id <- stFile$Cell_ID_prec_rsds[i]
    
    tiff(paste(oDir, "/", var, "_", stFile$Type[i], "_", gcm, ".tif", sep =""), width=800, height=600, pointsize=8, compression='lzw',res=150)
    
    mergeVarSel <- mergeVar[which(mergeVar$CellID == id),]  
    pairs(mergeVarSel[,2:length(mergeVarSel)],lower.panel=panel.cor, pch=19, col="black", cex = 0.5, main=paste("Type:",stFile$Type[i], "  GCM:", gcm, "  Var:", var))
    
    dev.off()
    
  }
  
}



