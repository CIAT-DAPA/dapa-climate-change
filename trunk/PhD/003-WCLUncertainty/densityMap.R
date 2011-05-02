#Julian Ramirez
#CIAT / University of Leeds

#Calculates a station density map based on a set of stations at 30arc-second spatial resolution

require(raster)
require(foreign)
rm(list=ls());gc(T);gc()

#sd <- "F:/PhD-work/climate-data-assessment/wcl-uncertainties/input-data"
#rd <- "F:/PhD-work/climate-data-assessment/wcl-uncertainties/mask-srtm/ETH"
#od <- "F:/PhD-work/climate-data-assessment/wcl-uncertainties/outputs/density-map"
#varname <- "rain"

densityMap <- function(stDir, rDir, oDir, vn="rain", nclosest=10) {
  
  #Defining pixel-based function to calculate distances of nearest stations
  pDist <- function(grxy,stxy) {
    #Point distance between each point and the set of stations
    pd <- pointDistance(c(grxy[1],grxy[2]), stxy, longlat=T)
    
    #Metrics (minimum and mean distances)
    min.d <- min(pd)
    mean.2n <- mean(sort(pd, decreasing=F)[1:2])
    mean.5n <- mean(sort(pd, decreasing=F)[1:5])
    mean.10n <- mean(sort(pd, decreasing=F)[1:10])
    mean.20n <- mean(sort(pd, decreasing=F)[1:20])
    
    #Metrics (mode)
    dd <- density(sort(pd, decreasing=F)[1:5])
    mode.5n <- dd$x[which.max(dd$y)]
    
    dd <- density(sort(pd, decreasing=F)[1:10])
    mode.10n <- dd$x[which.max(dd$y)]
    
    dd <- density(sort(pd, decreasing=F)[1:20])
    mode.20n <- dd$x[which.max(dd$y)]
    
    #Output vector)
    out <- c(min.d,mean.2n,mean.5n,mean.10n,mean.20n,mode.5n,mode.10n,mode.20n)
    return(out)
  }
  
  #Function to calculate chull over area that the nearest n-stations do cover
  chullDensity <- function(grxy, stxy, nclosest=10, rs) {
    #Calculating distance from points and
    pd <- pointDistance(c(grxy[1], grxy[2]), stxy, longlat=T)
    pd.order <- order(pd,decreasing=F)
    close.stat <- stxy[pd.order[1:10],]
    
    #Create the convex hull
    ch <- chull(close.stat[,1], close.stat[,2])
    ch <- close.stat[ch,]
    chClosed <- rbind(ch, ch[1,])
    
    xt <- extent(min(chClosed[,1])-1,max(chClosed[,1])+1,min(chClosed[,2])-1,max(chClosed[,2])+1)
    msk <- crop(rs,xt)
    
    #Transforming to polygons
    pol <- SpatialPolygons(list(Polygons(list(Polygon(chClosed)), 1)))
    pa <- rasterize(pol, msk, silent=T)
    pa.area <- area(pa)
    ar.val <- nclosest / (sum(pa.area[which(!is.na(pa[]))]) / (1000^2))
    return(ar.val)
  }
  
  #Creating output directory
  roDir <- paste(oDir, "/", vn, sep="")
  if (!file.exists(roDir)) {dir.create(roDir)}
  
  #Reading set of stations
  st <- read.dbf(paste(stDir, "/wc_", vn, "_stations.dbf", sep=""))
  
  #Loading mask raster and selecting stations within 
  rs <- raster(paste(rDir, "/altitude.asc", sep=""))
  st <- st[which(st$LONG >= rs@extent@xmin & st$LONG <= rs@extent@xmax & st$LAT >= rs@extent@ymin & st$LAT <= rs@extent@ymax),]
  xy.stat <- cbind(st$LONG,st$LAT)
  
  #Extract xy from raster
  xy <- xyFromCell(rs, which(!is.na(rs[])))
  desiredCells <- which(!is.na(rs[]))
  
  #Applying functions to raster positions
  cat("Applying distance function \n")
  ot <- apply(xy, 1, pDist, xy.stat)
  
  #Output raster writing
  min.d.rs <- rs; min.d.rs[desiredCells] <- ot[1,]
  min.d.rs <- writeRaster(min.d.rs, paste(roDir, "/min-distance.asc", sep=""), format='ascii', overwrite=T)
  
  mean.2n.rs <- rs; mean.2n.rs[desiredCells] <- ot[2,]
  mean.2n.rs <- writeRaster(mean.2n.rs, paste(roDir, "/mean-distance-2p.asc", sep=""), format='ascii', overwrite=T)
  
  mean.5n.rs <- rs; mean.5n.rs[desiredCells] <- ot[3,]
  mean.5n.rs <- writeRaster(mean.5n.rs, paste(roDir, "/mean-distance-5p.asc", sep=""), format='ascii', overwrite=T)
  
  mean.10n.rs <- rs; mean.10n.rs[desiredCells] <- ot[4,]
  mean.10n.rs <- writeRaster(mean.10n.rs, paste(roDir, "/mean-distance-10p.asc", sep=""), format='ascii', overwrite=T)
  
  mean.20n.rs <- rs; mean.20n.rs[desiredCells] <- ot[5,]
  mean.20n.rs <- writeRaster(mean.20n.rs, paste(roDir, "/mean-distance-20p.asc", sep=""), format='ascii', overwrite=T)
  
  mode.5n.rs <- rs; mode.5n.rs[desiredCells] <- ot[6,]
  mode.5n.rs <- writeRaster(mode.5n.rs, paste(roDir, "/mode-distance-5p.asc", sep=""), format='ascii', overwrite=T)
  
  mode.10n.rs <- rs; mode.10n.rs[desiredCells] <- ot[7,]
  mode.10n.rs <- writeRaster(mode.10n.rs, paste(roDir, "/mode-distance-10p.asc", sep=""), format='ascii', overwrite=T)
  
  mode.20n.rs <- rs; mode.20n.rs[desiredCells] <- ot[8,]
  mode.20n.rs <- writeRaster(mode.20n.rs, paste(roDir, "/mode-distance-20p.asc", sep=""), format='ascii', overwrite=T)
  
  cat("Applying density function \n")
  dens <- apply(xy, 1, chullDensity, xy.stat, nclosest=10, rs)
  
  ch.dens.rs <- rs; ch.dens.rs[desiredCells] <- dens
  ch.dens.rs <- writeRaster(ch.dens.rs, paste(roDir, "/chull-density-", nclosest, ".asc", sep=""), format='ascii', overwrite=T)
}




