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
#bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield"
#bDir <- "/andromeda_data1/jramirez/crop-modelling/GLAM/climate-signals-yield"
#bDir <- "~/PhD-work/crop-modelling/GLAM/climate-signals-yield"
cropName <- "rice"
cd <- paste(bDir,"/",toupper(cropName),sep="")

##   2.2. Create a 1x1 min resolution raster with the 1x1 degree cells
cDir <- cd

dumm <- raster(paste(cd,"/0_base_grids/igp_dummy.asc",sep=""))
dumm[] <- 1:ncell(dumm)

msk <- raster(paste(cd,"/0_base_grids/india-1min-msk.asc",sep=""))
xy <- xyFromCell(msk,which(!is.na(msk[])))
cells <- unique(cellFromXY(dumm,xy))
dumm[] <- NA; dumm[cells] <- 1
dumm[which(!is.na(dumm[]))] <- 1:length(which(!is.na(dumm[])))

#create high resolution raster with values of coarse cells
if (!file.exists(paste(cd,"/0_base_grids/india-1min-1d_cells.asc",sep=""))) {
  rs_c <- raster(msk)
  rs_c <- resample(dumm,rs_c,method="ngb")
  rs_c <- writeRaster(rs_c,paste(cd,"/0_base_grids/india-1min-1d_cells.asc",sep=""),format='ascii')
} else {
  rs_c <- raster(paste(cd,"/0_base_grids/india-1min-1d_cells.asc",sep=""))
}

#   2.3. Calculate areas per pixel
if (!file.exists(paste(cd,"/0_base_grids/india-1min-1d_cells_area.asc",sep=""))) {
  rs_a <- area(rs_c)
  rs_a <- writeRaster(rs_a,paste(cd,"/0_base_grids/india-1min-1d_cells_area.asc",sep=""),
                         format='ascii',overwrite=T)
} else {
  rs_a <- raster(paste(cd,"/0_base_grids/india-1min-1d_cells_area.asc",sep=""))
}

rs_dis <- raster(paste(cd,"/0_base_grids/india-1min-disid.asc",sep=""))
rs_dis <- readAll(rs_dis)

#series of years
iyr <- 66; fyr <- 04
if (fyr < iyr) {
  tser <- (1900+iyr):(2000+fyr)
} else {
  tser <- 1900+(iyr:fyr)
}


#loop through the different detrending methods
for (method in c("raw","lin","qua","loe","fou")) {
  outDir <- paste(cDir,"/raster/gridded/",method,sep="")
  if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}
  
  xy <- xyFromCell(dumm,which(!is.na(dumm[])))
  xy <- data.frame(CELL=which(!is.na(dumm[])),xy)
  
  #   2.4. parallelise years and grid the data
  #o
  library(snowfall)
  sfInit(parallel=T,cpus=10) #initiate cluster
  
  #export functions
  sfExport("weightValues")
  
  #export variables
  sfExport("cDir")
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
}


