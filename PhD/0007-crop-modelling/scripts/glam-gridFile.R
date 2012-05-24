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
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))

cmDir <- "F:/PhD-work/crop-modelling"
bDir <- paste(cmDir,"/GLAM",sep="")
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
yieldDir <- paste(ascDir,"/obs",sep="")
if (!file.exists(yieldDir)) {dir.create(yieldDir)}
wthDir <- paste(ascDir,"/wth",sep="")
if (!file.exists(wthDir)) {dir.create(wthDir)}


######################################################
#write the file
ofil <- paste(gridDir,"/IITMgrid.txt",sep="")
if (!file.exists(ofil)) {
  ofil <- write_gridFile(x=cells,outfile=ofil,fields=list(CELL="CELL",X="X",Y="Y",COL="COL",ROW="ROW"))
}


######################################################
############# SELECTED GRIDCELL(S) ###################
######################################################
cell <- 636
######################################################
######################################################

######################################################
#soil types file
#make a soil type file where the soil code is actually each gridcell
#and then a soils grid file for any selected gridcell
#these soil data have been derived from the HARMOIZED SOIL DATABASE of FAO
#see the script gridSoilData.R for details on how the data was obtained
soilDir <- paste(bDir,"/soil-data/HWSD",sep="")
solData <- read.csv(paste(soilDir,"/cellValues.csv",sep=""))
oSolFile <- paste(gsoilDir,"/soiltypes_all.txt",sep="")
if (!file.exists(oSolFile)) {
  oSolFile <- write_soil_types(x=solData,outfile=oSolFile,fields=list(CELL="CELL",SAND="SAND",CLAY="CLAY",AREA_FRAC="AREA_FRAC"))
}


oSolFile <- paste(gsoilDir,"/soiltypes_",cell,".txt",sep="")
if (!file.exists(oSolFile)) {
  selSolData <- solData[which(solData$CELL == cell),]
  oSolFile <- write_soil_types(x=selSolData,outfile=oSolFile,fields=list(CELL="CELL",SAND="SAND",CLAY="CLAY",AREA_FRAC="AREA_FRAC"))
}

#now need to create the soil codes file
oSoilGrid <- paste(gsoilDir,"/soilcodes_",cell,".txt",sep="")
if (!file.exists(oSoilGrid)) {
  oSoilGrid <- write_soilcodes(x=cells,outfile=oSoilGrid,cell=c(cell),fields=list(CELL="CELL",COL="COL",ROW="ROW"))
}


######################################################
# planting dates file (rainfed)
# get the planting date from Sacks et al. (2010)
sow_rs <- raster(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/calendar/",tolower(cropName),"/plant_start_lr.tif",sep=""))
cells$SOW_DATE <- extract(sow_rs,cbind(X=cells$X,Y=cells$Y))

osowFile <- paste(gsowDir,"/sowing_",cell,"_start.txt",sep="")
if (!file.exists(osowFile)) {
  osowFile <- write_sowdates(x=cells,outfile=osowFile,cell=c(cell),fields=list(CELL="CELL",COL="COL",ROW="ROW",SOW_DATE="SOW_DATE"))
}


######################################################
# yield file
method <- "lin"

yields <- stack(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/raster/gridded/",tolower(method),"/",tolower(method),"-",66:94,".asc",sep=""))
yFile <- paste(yieldDir,"/yield_",cell,"_",method,".txt",sep="")
if (!file.exists(yFile)) {
  yFile <- write_yield(x=cells,outfile=yFile,yld_stk=yields,yri=66,yrf=94,cell=cell,fields=list(CELL="CELL",X="X",Y="Y"))
}



######################################################
#write weather (irr and rainfed)
wthDataDir <- paste(cmDir,"/climate-data/gridcell-data/IND",sep="") #folder with gridded data
owthDir <- make_wth(x=cells,cell,wthDir=paste(wthDir,"/rfd",sep=""),wthDataDir,
                   fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE"))

#Study on groundnuts says that irrigated gnuts in Gujarat are sown between Jan-Feb and harvested
#between April and May
icells <- cells; icells$SOW_DATE <- 32
owthDir <- make_wth(x=icells,cell,wthDir=paste(wthDir,"/irr",sep=""),wthDataDir,
                   fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE"))


######################################################
#write planting dates once weather figured out (irrigated)
#bear in mind this is a modified weather
icells$SOW_DATE <- owthDir$SOW_DATE
osowFile <- paste(gsowDir,"/sowing_",cell,"_irr.txt",sep="")
if (!file.exists(osowFile)) {
  osowFile <- write_sowdates(x=icells,outfile=osowFile,cell=c(cell),fields=list(CELL="CELL",COL="COL",ROW="ROW",SOW_DATE="SOW_DATE"))
}





