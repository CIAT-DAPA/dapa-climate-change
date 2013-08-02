# Carlos Navarro 
# CIAT - CCAFS
# 28-05-2013

# Boxplots for article
require("gplots")

baseDir <- "T:/gcm/cmip5/raw/monthly"
ens <- "r1i1p1"

rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
for (rcp in rcpList){
  
  # List of variables and months
  mthList <- c(1:12)
  
  periodList <- c("2020", "2040", "2060")
  for (period in periodList) {
    
    # Define start and end year
    staYear <- as.integer(period)
    endYear <- as.integer(period) + 29
    
    ensCsv1975s <- read.csv(paste(baseDir, "/ensemble_anomalies", "/1975s/", rcp, "/anomalies-", rcp, "-", ens, "-", staYear, "_", endYear, ".csv", sep=""))
    ensCsv1985s <- read.csv(paste(baseDir, "/ensemble_anomalies", "/1985s/", rcp, "/anomalies-", rcp, "-", ens, "-", staYear, "_", endYear, ".csv", sep=""))
    
    
    setwd(paste(baseDir, "/ensemble_anomalies", "/diff/", rcp, sep=""))
    
    tiff(paste('prec-differences-all-gcm-all-months-', staYear, "_", endYear, ".tif", sep=""), width=1024, height=768, pointsize=8, compression='lzw',res=150)
    plot(c(0,0), type="n", xlab="Month", ylab="value", ylim=c(0,8), xlim=c(1,12), cex=1, cex.axis=0.9) 
    for (i in 1:nrow(ensCsv1975s)){
      lines(mthList,as.numeric(ensCsv1975s[i,2:13]), type="l", col="gray80")
      lines(mthList,as.numeric(ensCsv1985s[i,2:13]), type="l", col="lightpink2")
    }
    
    lines(mthList,as.numeric(colMeans(ensCsv1975s[,2:13])), type="l", col="black", lwd=3)
    lines(mthList,as.numeric(colMeans(ensCsv1985s[,2:13])), type="l", col="red", lwd=3)
    dev.off()
    
    
    tiff(paste('tmax-differences-all-gcm-all-months-', staYear, "_", endYear, ".tif", sep=""), width=1024, height=768, pointsize=8, compression='lzw',res=150)
    plot(c(0,0), type="n", xlab="Month", ylab="value", ylim=c(0,5), xlim=c(1,12), cex=1, cex.axis=0.9) 
    for (i in 1:nrow(ensCsv1975s)){
      lines(mthList,as.numeric(ensCsv1975s[i,14:25]), type="l", col="gray80")
      lines(mthList,as.numeric(ensCsv1985s[i,14:25]), type="l", col="lightpink2")
    }
    
    lines(mthList,as.numeric(colMeans(ensCsv1975s[,14:25])), type="l", col="black", lwd=3)
    lines(mthList,as.numeric(colMeans(ensCsv1985s[,14:25])), type="l", col="red", lwd=3)
    dev.off()
    
    
    tiff(paste('tmin-differences-all-gcm-all-months-', staYear, "_", endYear, ".tif", sep=""), width=1024, height=768, pointsize=8, compression='lzw',res=150)
    plot(c(0,0), type="n", xlab="Month", ylab="value", ylim=c(0,5), xlim=c(1,12), cex=1, cex.axis=0.9) 
    for (i in 1:nrow(ensCsv1975s)){
      lines(mthList,as.numeric(ensCsv1975s[i,26:37]), type="l", col="gray80")
      lines(mthList,as.numeric(ensCsv1985s[i,26:37]), type="l", col="lightpink2")
    }
    
    lines(mthList,as.numeric(colMeans(ensCsv1975s[,26:37])), type="l", col="black", lwd=3)
    lines(mthList,as.numeric(colMeans(ensCsv1985s[,26:37])), type="l", col="red", lwd=3)
    dev.off()
    
    
  }
}
    