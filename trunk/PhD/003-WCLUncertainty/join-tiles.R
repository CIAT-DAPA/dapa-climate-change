#Julian Ramirez
#March 2011

require(raster)
require(sp)

#Initial setup
bDir <- "F:/PhD-work/climate-data-assessment/wcl-uncertainties"
rDir <- paste(bDir, "/outputs/rain/jan/interpolations", sep="")
ntiles <- 5
fold <- 1

#Output setup
oDir <- paste(bDir, "/outputs/rain/jan/merged-tiles",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#dummy plotting part
plot(rs,col='grey')

for (i in 1:ntiles) {
  cat("Cutting tile", i, "\n")
  #Loading tile to cut
  cTile <- raster(paste(rDir, "/tile-", i, "-fold-", fold, ".asc", sep=""))
  yres <- (cTile@extent@ymax - cTile@extent@ymin) / cTile@nrows
  #Processing depending on position
  if (i == 1) {
    #Loading overlapping tiles
    bTile <- raster(paste(rDir, "/tile-", (i+1), "-fold-", fold, ".asc", sep=""))
    #Capturing extents
    lower.lim <- cTile@extent@ymin
    upper.lim <- bTile@extent@ymax
    #Calculating overlap and cut extent
    overlap <- upper.lim - lower.lim
    cutpoint <- overlap / 2
    cutlat <- lower.lim + cutpoint - yres*1
    cExtent <- extent(cTile@extent@xmin,cTile@extent@xmax,cutlat,cTile@extent@ymax)
  } else if (i == ntiles) {
    #Loading overlapping tiles
    tTile <- raster(paste(rDir, "/tile-", (i-1), "-fold-", fold, ".asc", sep=""))
    #Capturing extents
    lower.lim <- tTile@extent@ymin
    upper.lim <- cTile@extent@ymax
    #Calculating overlap and cut extent
    overlap <- upper.lim - lower.lim
    cutpoint <- overlap / 2
    cutlat <- upper.lim - cutpoint + yres*1
    cExtent <- extent(cTile@extent@xmin,cTile@extent@xmax,cTile@extent@ymin,cutlat)
  } else {
    #Loading overlapping tiles
    bTile <- raster(paste(rDir, "/tile-", (i+1), "-fold-", fold, ".asc", sep=""))
    tTile <- raster(paste(rDir, "/tile-", (i-1), "-fold-", fold, ".asc", sep=""))
    #Capturing extents (bottom)
    lower.lim <- cTile@extent@ymin
    upper.lim <- bTile@extent@ymax
    #Calculating overlap
    overlap <- upper.lim - lower.lim
    cutpoint <- overlap / 2
    cutlat <- lower.lim + cutpoint - yres*1
    cExtent <- extent(cTile@extent@xmin,cTile@extent@xmax,cutlat,cTile@extent@ymax)
    cTile <- crop(cTile, cExtent)
    #Capturing extents (top)
    lower.lim <- tTile@extent@ymin
    upper.lim <- cTile@extent@ymax
    #Calculating overlap and cut extent
    overlap <- upper.lim - lower.lim
    cutpoint <- overlap / 2
    cutlat <- upper.lim - cutpoint + yres*1
    cExtent <- extent(cTile@extent@xmin,cTile@extent@xmax,cTile@extent@ymin,cutlat)
  }
  #Cropping raster
  assign(paste("tile",i,sep=""),crop(cTile, cExtent))
  plot(get(paste("tile",i,sep=""))@extent,add=T)
  #Listing
  #if (i == 1) {mList <- c(get(paste("tile",i,sep="")))} else {mList <- c(mList,c(get(paste("tile",i,sep=""))))}
}
#m <- merge(mList)
m <- merge(tile1,tile2,tile3,tile4,tile5)
m <- writeRaster(m,"F:/PhD-work/climate-data-assessment/wcl-uncertainties/outputs/rain/jan/summary/merge.asc",format='ascii')