#Julian Ramirez
#University of Leeds / CCAFS / CIAT
#March 2011

#Merge tiles coming from interpolations

rm(list=ls()); g=gc(); rm(g)
require(raster); require(sp); require(rgdal)

#Initial setup
cd <- getwd()
# bd <- "/mnt/GIS-HD720/CCAFS/climate-data-assessment/wcl-uncertaities/outputs/cross-validation"
# td <- "/data/jramirez/tmp"
# 
# vr <- "rain"
# pt <- 4
# nti <- 5
# fld <- 1
# mth <- 1

batch.mergeTiles <- function(bd, td, vr="rain", folds=1:10, parts=1:10, months=1:12, nti=5) {
  for (fld in folds) {
    for (prt in parts) {
      for (mth in months) {
        cat("Merging fold", fld, "from part", prt, "month", mth, "for", vr, "\n")
        mt <- mergeTiles(bDir=bd, tmpDir=td, variable=vr, part=prt, fold=fld, month=mth, ntiles=nti)
      }
    }
  }
}

mergeTiles <- function(bDir, tmpDir, variable="rain", part=1, fold=1, month=1, ntiles=5) {
  if (!file.exists(tmpDir)) {dir.create(tmpDir)}
  procDir <- paste(tmpDir, "/", variable, "_", month, "_", part, "_", fold, sep="")
  if (!file.exists(procDir)) {dir.create(procDir, recursive=T)}
  setwd(procDir)
  
  #Define input (network path) dir
  rDir <- paste(bDir, "/", variable, "/part-", part, "/fold-", fold, sep="")
  
  #Output setup
  oDir <- paste(rDir, "/merged", sep="")
  if (!file.exists(oDir)) {dir.create(oDir)}
  
  if (!file.exists(paste(oDir, "/status.m.",month,".merge", sep=""))) {
    #Cleaning working dir if necessary
    cat("Cleaning if necessary \n")
    fl <- list.files(procDir); for (f in fl) {file.remove(f)}
    fl <- list.files(oDir); for (f in fl) {file.remove(paste(oDir, "/", f, sep=""))}
    
    #Copying required inputs to the processing dir
    for (i in 1:ntiles) {
      grd <- paste(rDir, "/tile-", i, "/", variable, "_", month, ".zip", sep="")
      file.copy(from=grd, to=paste(variable, "_", month, "_", i, ".zip", sep=""))
    }
    
    #Iterating 
    for (i in 1:ntiles) {
      cat("Cutting tile", i, "\n")
      
      #Loading tile to cut
      cTileNew <- paste(variable, "_", month, "_", i, ".asc", sep="")
      if (!file.exists(cTileNew)) {
        cTile <- paste(variable, "_", month, "_", i, ".zip", sep="")
        cTile <- unzip(cTile, files=NULL, exdir=".")
        fr <- file.rename(cTile, cTileNew)
      }
      rcTile <- raster(cTileNew)
      
      yres <- (rcTile@extent@ymax - rcTile@extent@ymin) / rcTile@nrows
      
      #Processing depending on position
      if (i == 1) {
        #Loading overlapping tiles
        bTileNew <- paste(variable, "_", month, "_", (i+1), ".asc", sep="")
        if (!file.exists(bTileNew)) {
          bTile <- paste(variable, "_", month, "_", (i+1), ".zip", sep="")
          bTile <- unzip(bTile, files=NULL, exdir=".")
          fr <- file.rename(bTile, bTileNew)
        }
        rbTile <- raster(bTileNew)
        
        #Capturing extents
        lower.lim <- rcTile@extent@ymin
        upper.lim <- rbTile@extent@ymax
        
        #Calculating overlap and cut extent
        overlap <- upper.lim - lower.lim
        cutpoint <- overlap / 2
        cutlat <- lower.lim + cutpoint - yres*1
        cExtent <- extent(rcTile@extent@xmin,rcTile@extent@xmax,cutlat,rcTile@extent@ymax)
        
      } else if (i == ntiles) {
        #Loading overlapping tiles
        tTileNew <- paste(variable, "_", month, "_", (i-1), ".asc", sep="")
        if (!file.exists(tTileNew)) {
          tTile <- paste(variable, "_", month, "_", (i-1), ".zip", sep="")
          tTile <- unzip(tTile, files=NULL, exdir=".")
          fr <- file.rename(tTile, tTileNew)
        }
        rtTile <- raster(tTileNew)
        
        #Capturing extents
        lower.lim <- rtTile@extent@ymin
        upper.lim <- rcTile@extent@ymax
        
        #Calculating overlap and cut extent
        overlap <- upper.lim - lower.lim
        cutpoint <- overlap / 2
        cutlat <- upper.lim - cutpoint + yres*1
        cExtent <- extent(rcTile@extent@xmin,rcTile@extent@xmax,rcTile@extent@ymin,cutlat)
        
      } else {
        #Loading overlapping tiles
        bTileNew <- paste(variable, "_", month, "_", (i+1), ".asc", sep="")
        if (!file.exists(bTileNew)) {
          bTile <- paste(variable, "_", month, "_", (i+1), ".zip", sep="")
          bTile <- unzip(bTile, files=NULL, exdir=".")
          fr <- file.rename(bTile, bTileNew)
        }
        rbTile <- raster(bTileNew)
        
        tTileNew <- paste(variable, "_", month, "_", (i-1), ".asc", sep="")
        if (!file.exists(tTileNew)) {
          tTile <- paste(variable, "_", month, "_", (i-1), ".zip", sep="")
          tTile <- unzip(tTile, files=NULL, exdir=".")
          fr <- file.rename(tTile, tTileNew)
        }
        rtTile <- raster(tTileNew)
        
        #Capturing extents (bottom)
        lower.lim <- rcTile@extent@ymin
        upper.lim <- rbTile@extent@ymax
        
        #Calculating overlap
        overlap <- upper.lim - lower.lim
        cutpoint <- overlap / 2
        cutlat <- lower.lim + cutpoint - yres*1
        cExtent <- extent(rcTile@extent@xmin,rcTile@extent@xmax,cutlat,rcTile@extent@ymax)
        rcTile <- crop(rcTile, cExtent)
        
        #Capturing extents (top)
        lower.lim <- rtTile@extent@ymin
        upper.lim <- rcTile@extent@ymax
        
        #Calculating overlap and cut extent
        overlap <- upper.lim - lower.lim
        cutpoint <- overlap / 2
        cutlat <- upper.lim - cutpoint + yres*1
        cExtent <- extent(rcTile@extent@xmin,rcTile@extent@xmax,rcTile@extent@ymin,cutlat)
      }
      #Cropping raster
      assign(paste("tile",i,sep=""),crop(rcTile, cExtent))
      #plot(get(paste("tile",i,sep=""))@extent,add=T)
    }
  
  cat("Merging \n")
  m <- merge(tile1,tile2,tile3,tile4,tile5)
  oFile <- paste(variable, "_", month, ".asc", sep="")
  cat("Writing raster \n")
  m <- writeRaster(m, oFile ,format='ascii')
  
  cat("Compressing \n")
  ozFile <- paste(variable, "_", month, ".zip", sep="")
  system(paste("zip", ozFile, oFile))
  fs <- file.remove(oFile)
  
  #Copy to output oDir
  cat("Copying \n")
  fs <- file.copy(ozFile, paste(oDir, "/", ozFile, sep=""))
  
  #Remove working dir (temporal) and contents
  lf <- list.files("."); for(f in lf) {file.remove(paste(f,sep=""))}
  setwd(cd)
  fs <- file.remove(procDir)
  
  #Create log file
  ml <- makeLog(oDir,month)
  } else {
    cat("Month, part and fold aready processed \n")
    ml <- paste(oDir, "/status.",month,".merge", sep="")
  }
  return(ml)
}

#Function to create a log file
makeLog <- function(path, m) {
  zz <- file(paste(path, "/status.m.",m,".merge", sep=""), "w")
  cat("Process finished on", date(), "\n", file=zz)
  close(zz)
  return(paste(path, "/status.m.",m,".merge", sep=""))
}
