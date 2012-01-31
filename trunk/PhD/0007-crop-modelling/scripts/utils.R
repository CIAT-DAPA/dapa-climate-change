#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

################################################
#Function to aggregate based on i/o directories
aggregateAll <- function(iDir,oDir,ow=F,ext=".asc",agg.factor=4) {
  if (!file.exists(oDir)) {dir.create(oDir)}
  
  #Processing monthly files
  for (mth in 1:12) {
    #Mean temperature
    if (!file.exists(paste(oDir,"/tmean_",mth,".asc",sep="")) | ow) {
      cat("\nProcess started for tmean",mth,"\n")
      rs <- raster(paste(iDir,"/tmean_",mth,ext,sep=""))
      rs <- readAll(rs)
      cat("Aggregating raster \n")
      rs <- aggregate(rs,fact=agg.factor)
      cat("Writing raster \n")
      rs <- writeRaster(rs,paste(oDir,"/tmean_",mth,".asc",sep=""),overwrite=ow,format='ascii')
      rm(rs); g<-gc()
    }
    
    #Minimum temperature
    if (!file.exists(paste(oDir,"/tmin_",mth,".asc",sep="")) | ow) {
      cat("\nProcess started for tmin",mth,"\n")
      rs <- raster(paste(iDir,"/tmin_",mth,ext,sep=""))
      rs <- readAll(rs)
      cat("Aggregating raster \n")
      rs <- aggregate(rs,fact=agg.factor)
      cat("Writing raster \n")
      rs <- writeRaster(rs,paste(oDir,"/tmin_",mth,".asc",sep=""),overwrite=ow,format='ascii')
      rm(rs); g<-gc()
    }
    
    #Precipitation temperature
    if (!file.exists(paste(oDir,"/prec_",mth,".asc",sep="")) | ow) {
      cat("\nProcess started for prec",mth,"\n")
      rs <- raster(paste(iDir,"/prec_",mth,ext,sep=""))
      rs <- readAll(rs)
      cat("Aggregating raster \n")
      rs <- aggregate(rs,fact=agg.factor)
      cat("Writing raster \n")
      rs <- writeRaster(rs,paste(oDir,"/prec_",mth,".asc",sep=""),overwrite=ow,format='ascii')
      rm(rs); g<-gc()
    }
  }
  return(oDir)
}


################################################
#Function to decompress based on i/o directories
decompressAll <- function(zDir,oDir) {
  if (!file.exists(oDir)) {dir.create(oDir)}
  aDir <- paste(oDir, "/_tmp", sep="")
  if (!file.exists(aDir)) {dir.create(aDir)}
  
  vList <- c("prec", "tmin", "tmean")
  for (v in vList) {
		f <- paste(zDir, "/", v, "_asc.zip", sep="")
		fd <- paste(aDir, "/", v, "_asc.zip", sep="")
		cat("Copy..."); file.copy(f, fd)
		cat("Unzip..."); unzip(fd, files=NULL, exdir=aDir)
		cat("Remove dup... \n"); file.remove(fd)
	}
  return(aDir)
}

