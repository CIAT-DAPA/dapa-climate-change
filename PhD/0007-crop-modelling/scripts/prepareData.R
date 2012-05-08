#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#March 2012
stop("Do not runt the whole thing")

#extract daily data for all cells in a domain to create 

#local
# src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
# src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#eljefe
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#sourcing needed functions
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/watbal.R",sep=""))
source(paste(src.dir2,"/climateSignals-functions.R",sep=""))
source(paste(src.dir2,"/prepareData-functions.R",sep=""))

library(raster)

#Climate signals on yield for Indian sorghum
#local
# bDir <- "F:/PhD-work/crop-modelling"

#eljefe
#bDir <- "~/PhD-work/crop-modelling"

sradDir <- paste(bDir,"/climate-data/CRU_CL_v1-1_data",sep="")
tempDir <- paste(bDir,"/climate-data/CRU_TS_v3-1_data",sep="")
era40Dir <- paste(bDir,"/climate-data/ERA-40",sep="")

y_iyr <- 1961
y_eyr <- 2002

ncFile <- paste(bDir,"/climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
mthRainAsc <- paste(bDir,"/climate-data/IND-TropMet",sep="")

ydDir <- paste(bDir,"/GLAM/climate-signals-yield/SORG-RABI/raster/gridded",sep="")
oGridDir <- paste(bDir,"/climate-data/gridcell-data",sep="")
if (!file.exists(oGridDir)) {dir.create(oGridDir)}

oDir <- paste(oGridDir,"/IND",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#create mask
metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
msk <- maskCreate(metFile,yldFile)

#cells to know where to extract the data from
if (!file.exists(paste(oDir,"/cells-process.csv",sep=""))) {
  pCells <- data.frame(CELL=1:ncell(msk))
  pCells$X <- xFromCell(msk,pCells$CELL); pCells$Y <- yFromCell(msk,pCells$CELL)
  pCells$Z <- extract(msk,cbind(X=pCells$X,Y=pCells$Y))
  pCells <- pCells[which(!is.na(pCells$Z)),]
  write.csv(pCells,paste(oDir,"/cells-process.csv",sep=""),quote=F,row.names=F)
} else {
  pCells <- read.csv(paste(oDir,"/cells-process.csv",sep=""))
}


#cell <- 427 #test cell

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=13) #initiate cluster

#export functions and data
sfExport("pCells")
sfExport("bDir")
sfExport("oGridDir")
sfExport("oDir")
sfExport("ncFile")
sfExport("mthRainAsc")
sfExport("sradDir")
sfExport("tempDir")
sfExport("era40Dir")
sfExport("y_iyr")
sfExport("y_eyr")
sfExport("src.dir")
sfExport("src.dir2")

#remove all those cells in the list that do not exist
cellList <- NULL
for (cell in pCells$CELL) {
  if (!file.exists(paste(oDir,"/cru_srad/cell-",cell,".csv",sep=""))) {
    cellList <- c(cellList,cell)
  }
}

#run the control function
system.time(sfSapply(as.vector(cellList), prepareCellData))

#stop the cluster
sfStop()



