#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#packages
library(raster)

#base working directory and other details
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
cmipDir <- "V:/eejarv/CMIP5"
cropName <- "gnut"
method <- "lin"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

#source some functions
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))

#load cell list
cells <- read.csv(paste(cDir,"/inputs/calib-cells-selection-v2.csv",sep=""))
cells <- cells[which(cells$AHRATIO>=.2),] ##select only  gridcells with more than .2 in AHRATIO
cells$ISSEL <- NULL; cells$ISSEL_F <- NULL

#load zones raster
#zrs <- raster(paste(cDir,"/gnut-zones/zones_lr.asc",sep=""))

#plot points in the selected cells
#windows()
#plot(zrs,col=rev(terrain.colors(5)))
#points(cells$X,cells$Y,pch=20,cex=0.75)
#plot(wrld_simpl,add=T)

#create output directories
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
if (!file.exists(glam_dir)) {dir.create(glam_dir)}

input_dir <- paste(glam_dir,"/inputs",sep="")
if (!file.exists(input_dir)) {dir.create(input_dir)}

runs_dir <- paste(glam_dir,"/model-runs",sep="")
if (!file.exists(runs_dir)) {dir.create(runs_dir)}

grid_dir <- paste(input_dir,"/grid",sep="")
if (!file.exists(grid_dir)) {dir.create(grid_dir,recursive=T)}

param_dir <- paste(input_dir,"/params",sep="")
if (!file.exists(param_dir)) {dir.create(param_dir,recursive=T)}

soil_dir <- paste(input_dir,"/ascii/soil",sep="")
if (!file.exists(soil_dir)) {dir.create(soil_dir,recursive=T)}

sow_dir <- paste(input_dir,"/ascii/sow",sep="")
if (!file.exists(sow_dir)) {dir.create(sow_dir,recursive=T)}

fwth_dir <- paste(input_dir,"/ascii/wth_fut",sep="")
if (!file.exists(fwth_dir)) {dir.create(fwth_dir,recursive=T)}

wth_dir <- paste(input_dir,"/ascii/wth",sep="")
if (!file.exists(wth_dir)) {dir.create(wth_dir,recursive=T)}

#write output gridcell selection
write.csv(cells,paste(input_dir,"/calib-cells-selection.csv",sep=""),quote=T,row.names=F)








