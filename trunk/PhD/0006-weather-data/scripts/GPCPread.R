#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")
#Read and write ascii grids of GPCP data using convsh1.91.exe and R raster/ncdf package

#load libraries and sources
require(raster); require(ncdf); require(rgdal)
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/GPCPRead-functions.R",sep=""))

#configuration
bDir <- "F:/PhD-work/crop-modelling/climate-data"
gpcpDir <- paste(bDir,"/GPCP-1dd-v1.1-daily",sep="")
gDir <- paste(gpcpDir,"/GrADS-files",sep="")
nDir <- paste(gpcpDir,"/NetCDF-files",sep="")
aDir <- paste(gpcpDir,"/ASCII-files",sep="")
sDir <- paste(gpcpDir,"/1dd_v1.1.software",sep="")

#loop through files
gzList <- list.files(gDir,pattern=".gz")
for (gz in gzList) {
  cat("Processing",gz,"\n")
  setwd(bDir)
  basename <- substr(gz,1,(nchar(gz)-3))
  if (!file.exists(paste(nDir,"/",basename,".nc",sep=""))) {
    x <- gradsToNC(gz,gDir,nDir,sDir)
  }
  x <- createASCII(gz,aDir,nDir)
}

