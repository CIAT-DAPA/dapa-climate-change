#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#get a given domain (from the cells-process.csv), and make a *grid.txt file
#for reference. This file will not be used by GLAM, but it will be needed if you want
#to map the results

#load packages
library(raster)

#load functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))

bDir <- "F:/PhD-work/crop-modelling/GLAM"
cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

#these are the cells that have both yield and rainfall data
cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))

#get longitude and latitude (row and column)
rs <- raster(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/0_base_grids/igp_dummy.tif",sep=""))
cells$COL <- colFromX(rs,cells$X)
cells$ROW <- rowFromY(rs,cells$Y)

#output folders
dataDir <- paste(cropDir,"/inputs",sep="")
if (!file.exists(dataDir)) {dir.create(dataDir)}
gridDir <- paste(dataDir,"/grid",sep="")
if (!file.exists(gridDir)) {dir.create(gridDir)}
ascDir <- paste(dataDir,"/ascii",sep="")
if (!file.exists(ascDir)) {dir.create(ascDir)}
gsoilDir <- paste(ascDir,"/soil",sep="")
if (!file.exists(gsoilDir)) {dir.create(gsoilDir)}
gsowDir <- paste(ascDir,"/sow",sep="")
if (!file.exists(gsowDir)) {dir.create(gsowDir)}


#write the file
ofil <- paste(gridDir,"/IITMgrid.txt",sep="")
if (!file.exists(ofil)) {
  ofil <- write_gridFile(x=cells,outfile=ofil,fields=list(CELL="CELL",X="X",Y="Y",COL="COL",ROW="ROW"))
}

#soil types file
#make a soil type file where the soil code is actually each gridcell
#and then a soils grid file for any selected gridcell
#these soil data have been derived from the HARMOIZED SOIL DATABASE of FAO
#see the script gridSoilData.R for details on how the data was obtained
soilDir <- paste(bDir,"/soil-data/HWSD",sep="")
solData <- read.csv(paste(soilDir,"/cellValues.csv",sep=""))
oSolFile <- paste(gsoilDir,"/soiltypes_all.txt",sep="")
oSolFile <- write_soil_types(x=solData,outfile=oSolFile,fields=list(CELL="CELL",SAND="SAND",CLAY="CLAY",AREA_FRAC="AREA_FRAC"))

oSolFile <- paste(gsoilDir,"/soiltypes_636.txt",sep="")
selSolData <- solData[which(solData$CELL == cell),]
oSolFile <- write_soil_types(x=selSolData,outfile=oSolFile,fields=list(CELL="CELL",SAND="SAND",CLAY="CLAY",AREA_FRAC="AREA_FRAC"))


#now need to create the soil codes file
cell <- 636
oSoilGrid <- paste(gsoilDir,"/soilcodes_636.txt",sep="")
oSoilGrid <- write_soilcodes(x=cells,outfile=oSoilGrid,cell=c(636),fields=list(CELL="CELL",COL="COL",ROW="ROW"))

######################################################
# planting dates file
#get the planting date from Sacks et al. (2010)

osowFile <- ""


