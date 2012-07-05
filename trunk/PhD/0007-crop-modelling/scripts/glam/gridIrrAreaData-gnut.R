#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#libraries
library(maptools); library(rgdal); library(raster)
data(wrld_simpl)

#src.dir<-"D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "/home/jramirez/dapa-climate-change/PhD/0007-crop-modelling/scripts"
#src.dir<-"~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/gridding-functions.R",sep=""))

#set the working folder
#bDir <- "Z:/PhD-work/crop-modelling/GLAM"
#bDir <- "/andromeda_data1/jramirez/crop-modelling/GLAM"
#bDir <- "~/PhD-work/crop-modelling/GLAM"
cropName <- "gnut"
cd <- paste(bDir,"/climate-signals-yield/",toupper(cropName),sep="")

##   2.2. Create a 1x1 min resolution raster with the 1x1 degree cells
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

dumm <- raster(paste(cd,"/0_base_grids/igp_dummy.tif",sep=""))
dumm[] <- 1:ncell(dumm)

msk <- raster(paste(cd,"/0_base_grids/india-1min-msk.tif",sep=""))
xy <- xyFromCell(msk,which(!is.na(msk[])))
cells <- unique(cellFromXY(dumm,xy))
dumm[] <- NA; dumm[cells] <- 1
dumm[which(!is.na(dumm[]))] <- 1:length(which(!is.na(dumm[])))

#create high resolution raster with values of coarse cells
if (!file.exists(paste(cd,"/0_base_grids/india-1min-1d_cells.tif",sep=""))) {
  rs_c <- raster(msk)
  rs_c <- resample(dumm,rs_c,method="ngb")
  rs_c <- writeRaster(rs_c,paste(cd,"/0_base_grids/india-1min-1d_cells.tif",sep=""),format='ascii')
} else {
  rs_c <- raster(paste(cd,"/0_base_grids/india-1min-1d_cells.tif",sep=""))
}

#   2.3. Calculate areas per pixel
if (!file.exists(paste(cd,"/0_base_grids/india-1min-1d_cells_area.tif",sep=""))) {
  rs_a <- area(rs_c)
  rs_a <- writeRaster(rs_a,paste(cd,"/0_base_grids/india-1min-1d_cells_area.tif",sep=""),
                         format='ascii',overwrite=T)
} else {
  rs_a <- raster(paste(cd,"/0_base_grids/india-1min-1d_cells_area.tif",sep=""))
}

rs_dis <- raster(paste(cd,"/0_base_grids/india-1min-disid.tif",sep=""))
rs_dis <- readAll(rs_dis)

#series of years
iyr <- 66; fyr <- 94
if (fyr < iyr) {
  tser <- (1900+iyr):(2000+fyr)
} else {
  tser <- 1900+(iyr:fyr)
}


##############################################################################
######## IRRIGATED AREAS
##############################################################################

#parallelise the run
method <- "raw"
yldDir <- paste(cDir,"/irrigated_area/raster/yearly",sep="")

outDir <- paste(cDir,"/irrigated_area/raster/gridded/",sep="")
if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}

xy <- xyFromCell(dumm,which(!is.na(dumm[])))
xy <- data.frame(CELL=which(!is.na(dumm[])),xy)

#   2.4. parallelise years and grid the data
#o
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

#export functions
sfExport("weightValues")

#export variables
sfExport("cDir")
sfExport("yldDir")
sfExport("outDir")
sfExport("method")
sfExport("rs_dis")
sfExport("rs_c")
sfExport("rs_a")
sfExport("dumm")
sfExport("xy")

#run the control function
system.time(sfSapply(as.vector(tser), controlGridding))

#stop the cluster
sfStop()



##############################################################################
######## HARVESTED AREAS
##############################################################################

#parallelise the run
method <- "raw"
yldDir <- paste(cDir,"/harvested_area/raster/yearly",sep="")

outDir <- paste(cDir,"/harvested_area/raster/gridded/",sep="")
if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}

xy <- xyFromCell(dumm,which(!is.na(dumm[])))
xy <- data.frame(CELL=which(!is.na(dumm[])),xy)

#   2.4. parallelise years and grid the data
#o
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

#export functions
sfExport("weightValues")

#export variables
sfExport("cDir")
sfExport("yldDir")
sfExport("outDir")
sfExport("method")
sfExport("rs_dis")
sfExport("rs_c")
sfExport("rs_a")
sfExport("dumm")
sfExport("xy")

#run the control function
system.time(sfSapply(as.vector(tser), controlGridding))

#stop the cluster
sfStop()


