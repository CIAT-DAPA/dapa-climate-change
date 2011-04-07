#Julian Ramirez-Villegas
#CIAT / University of Leeds / CCAFS

# Code to load data from a netcdf and get data into an ascii file with specified raster dimensions

require(raster)
require(ncdf)
require(rgdal)

#Cleaning memory
rm(list=ls()); gc(); gc(T)

#Main function to process all GCMs
processNCDF <- function(gcmdir, which="ALL") {
  gcmList <- list.files(gcmdir)
  if (length(which) == 1) {
    if (which != "ALL") {gcmList <- gcmList[which]}
  } else {
    gcmList <- gcmList[which]
  }
  
  for (gcm in gcmList) {
    cat("\n")
    cat("Processing GCM:", gcm, "\n")
    yd <- paste(gcmdir, "/", gcm, "/yearly_files", sep="")
    pg <- processGCM(yd, gcm=gcm)
  }
  return(gcmdir)
}

#Third order function to be applied over a model for all years
processGCM <- function(yrdir, gcm=NULL) {
  yearList <- list.files(yrdir)
  for (year in yearList) {
    cat("Processing", year, "\n")
    fd <- paste(yrdir, "/", year, sep="")
    py <- processYear(fd, gcm=gcm)
  }
  return(yrdir)
}

#Second order function to be applied over a year but all variables and months
processYear <- function(fdir, gcm=NULL) {
  vars <- c("prec","tmean","tmin","tmax")
  for (v in vars) {
    cat("Processing", v, "\n")
    for (m in 1:12) {
      if (m < 10) {mth <- paste("0", m, sep="")} else {mth <- paste(m)}
      in.file.name <- paste(fdir, "/", v, "_", mth, ".nc", sep="")
      ot.file.name <- paste(fdir, "/", v, "_", mth, ".asc", sep="")
      
      if (file.exists(in.file.name) & !file.exists(ot.file.name)) {
        rs <- ncToAscii(in.file.name, vn=v, month=mth, gcm=gcm)
        rs <- writeRaster(rs, ot.file.name, format='ascii')
      }
    }
  }
  return(fdir)
}

#Basic function to transform one .nc file and get a rasterLayer
ncToAscii <- function(fname, vn="prec", month="01", gcm=NULL) {
  #Number of days per month matrix
  ndaymtx <- data.frame(Month=c(paste(0,c(1:9),sep=""),paste(c(10:12))),Ndays=c(31,28,31,30,31,30,31,31,30,31,30,31))
  
  #Defining NetCDF variable name
  if (vn == "prec") {
    nc.varname <- "pr"
  } else if (vn == "tmean") {
    nc.varname <- "tas"
  } else if (vn == "tmin") {
    nc.varname <- "tasmin"
  } else if (vn == "tmax") {
    nc.varname <- "tasmax"
  }
  
  #GCM characteristics loading
  gcm.char <- read.csv("gcm_chars.csv")
  orCols <- gcm.char$ncolsOR[which(gcm.char$model == gcm)]; orRows <- gcm.char$nrowsOR[which(gcm.char$model == gcm)]
  nwCols <- gcm.char$ncolsNW[which(gcm.char$model == gcm)]; nwRows <- gcm.char$nrowsNW[which(gcm.char$model == gcm)]
  
  #NetCDF file loading (if the raster package fails then try from scratch with the ncdf package)
  if (class(try(raster(fname,varname=nc.varname), silent=T)) == "try-error") {
    nc <- open.ncdf(fname)
    vals <- get.var.ncdf(nc)
    close.ncdf(nc)
    rs <- raster(nrow=orRows, ncol=orCols, xmn=0, xmx=360, ymn=-90, ymx=90)
    rs[] <- as.vector(vals)
    rs <- flip(rs, direction="y")
  } else {
    rs <- raster(fname,varname=nc.varname)
  }
  
  #Querying number of days in the month
  nd <- ndaymtx$Ndays[which(ndaymtx$Month == month)]
  
  #If xres and yres are different then match them using a basic resampling technique
  if (orCols == nwCols & orRows == nwRows) {
    rs2 <- raster(nrow=orRows, ncol=orCols, xmn=0, xmx=360, ymn=-90, ymx=90)
    rs2[] <- rs[]
    rsk <- rotate(rs2)
  } else {
    rs2 <- raster(nrow=orRows, ncol=orCols, xmn=0, xmx=360, ymn=-90, ymx=90)
    rs3 <- raster(nrow=nwRows, ncol=nwCols, xmn=0, xmx=360, ymn=-90, ymx=90)
    rs2[] <- rs[]
    rs3 <- resample(rs2,rs3, method="ngb")
    rsk <- rotate(rs3)
  }
  
  #Unit conversion (pp flux to mm, or Kelvin to Celsius)
  if (vn == "prec") {
    rsk <- rsk*86400*(nd)
  } else {
    rsk <- rsk - 272.15
  }
  
  #Returning resulting object
  return(rsk)
}

