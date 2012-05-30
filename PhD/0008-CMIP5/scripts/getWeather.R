#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP5 weather data
library(raster)

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"
#yi <- 1961
#yf <- 2002
#i <- 1 #gcm to process

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "W:/eejarv/PhD-work/crop-modelling"
#mdDir <- "V:/eejarv/CMIP5/baseline"
#yi <- 1961
#yf <- 2002
#i <- 1 #gcm to process


#sourcing needed functions
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#output gridcell data dir
cDataDir <- paste(bDir,"/climate-data/gridcell-data",sep="")
outDir <- paste(cDataDir,"/IND_CMIP5",sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

#load GCM characteristics
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)

#load cell details
cropName <- "gnut"
all_cells <- read.csv(paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))

#get the indian extent
drs <- raster(paste(src.dir2,"/data/mask.tif",sep=""))
drs[which(!is.na(drs[]))] <- 1

cll <- all_cells$CELL[1]

#extract the data for a given GCM
od <- CMIP5_extract_wrapper(cells=all_cells,cell=cll,dum_rs=drs,cChars=gcmChars,i=1,oDir=outDir)

#here now what i need to do is make the extraction as efficient as possible
#that probably means loading all rasters in a rasterstack 








