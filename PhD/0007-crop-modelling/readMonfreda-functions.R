#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL

#function to convert the data
convertMon <- function(inDir,outDir,gName,ow=T) {
  fn <- paste(inDir,"/",gName,sep="")
  ofn <- paste(outDir,"/",gName,sep="")
  
  if (!file.exists(ofn) | ow) {
    fz <- file(fn,"r")
    
    #get raster characteristics
    header <- readLines(fz,6)
    ncols <- as.numeric(gsub("ncols","",header[1]))
    nrows <- as.numeric(gsub("nrows","",header[2]))
    xllco <- as.numeric(gsub("xllcorner","",header[3]))
    yllco <- as.numeric(gsub("yllcorner","",header[4]))
    cllsz <- as.numeric(gsub("cellsize","",header[5]))
    ndata <- as.numeric(gsub("NODATA_value","",header[6]))
    
    #construct the raster
    library(raster)
    xn <- xllco; yn <- yllco
    rs <- raster(ncol=ncols,nrow=nrows,xmn=xllco,ymn=yllco)
    
    #get the raster values
    rdata <- readLines(fz,n=-1)
    close(fz)
    rk <- rdata
    rk <- as.numeric(unlist(strsplit(rdata," ")))
    rk <- rk[which(!is.na(rk))]
    
    #put the raster values into the raster
    rs[] <- rk
    rs[which(rs[]==-9999)] <- NA
    
    #write output raster
    rs <- writeRaster(rs,paste(outDir,"/",gName,sep=""),format='ascii')
  } else {
    cat("File already exists \n")
  }
}
