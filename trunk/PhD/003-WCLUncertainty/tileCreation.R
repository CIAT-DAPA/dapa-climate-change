#Julian Ramirez
#CIAT / University of Leeds

require(raster); require(foreign); require(maptools)

#Directories
#rDir <- "C:/CIAT_work/_tools/dapa-climate-change/trunk/PhD/003-WCLUncertainty"
#oDir <- "C:/CIAT_work/_tools/dapa-climate-change/trunk/PhD/003-WCLUncertainty/tiles"
#stDir <- "C:/CIAT_work/climate_change/IPCC_CMIP3_data/WC"
#overlap <- 1500
#ntiles <- 3
#vn <- "rain"

createTiles <- function(rDir, stDir, oDir, vn="rain", ntiles=3, overlap=1500) {
  #Rasters loading
  alt <- raster(paste(rDir, "/altitude.asc", sep=""))
  lat <- raster(paste(rDir, "/latitude.asc", sep=""))
  lon <- raster(paste(rDir, "/longitude.asc", sep=""))
  
  #Extent
  xt <- extent(alt)
  xt@xmin <- xt@xmin - 10
  xt@xmax <- xt@xmax + 10
  xt@ymin <- xt@ymin - 10
  xt@ymax <- xt@ymax + 10
  
  #Loading stations
  st <- read.dbf(paste(stDir, "/wc_", vn, "_stations.dbf", sep=""))
  st <- st[which(st$LONG >= xt@xmin & st$LONG <= xt@xmax),]
  st <- st[which(st$LAT >= xt@ymin & st$LAT <= xt@ymax),]
  
  #Tiling configuration
  nst <- nrow(st)
  lat.sorted <- order(st$LAT, decreasing=T)
  nst.per.tile <- round(nst / ntiles)
  
  for (i in 1:ntiles) {
    tDir <- paste(oDir, "/tile-", i, sep="")
    if (!file.exists(tDir)) {dir.create(tDir)}
    
    if (i == 1) {
      inirow <- 1 + nst.per.tile * (i - 1)
      finrow <- nst.per.tile * i + overlap
    } else if (i == ntiles) {
      inirow <- 1 + nst.per.tile * (i - 1) - overlap
      finrow <- nrow(st)
    } else {
      inirow <- 1 + nst.per.tile * (i - 1) - overlap/2
      finrow <- nst.per.tile * i + overlap/2
    }
    
    lim.rows <- lat.sorted[inirow:finrow]
    lim.st <- st[lim.rows,]
    
    lat.min <- min(lim.st$LAT)
    lat.max <- max(lim.st$LAT)
    cat("Tile", i, "between", lat.min, "and", lat.max, "\n")
    cat("Number of stations:", nrow(lim.st), "\n")
    
    xt.tile <- extent(xt@xmin, xt@xmax, lat.min, lat.max)
    
    #Cropping rasters
    alt.tile <- crop(alt, xt.tile)
    alt.tile <- writeRaster(alt.tile, paste(tDir, "/altitude.asc", sep=""), format='ascii', overwrite=T)
    lat.tile <- crop(lat, xt.tile)
    lat.tile <- writeRaster(lat.tile, paste(tDir, "/latitude.asc", sep=""), format='ascii', overwrite=T)
    lon.tile <- crop(lon, xt.tile)
    lon.tile <- writeRaster(lon.tile, paste(tDir, "/longitude.asc", sep=""), format='ascii', overwrite=T)
  }
}

