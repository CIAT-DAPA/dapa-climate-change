#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#get a given domain (from the cells-process.csv), and make a *grid.txt file
#for reference. This file will not be used by GLAM, but it will be needed if you want
#to map the results

#load packages
library(raster); library(maptools)

#load functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))

cmDir <- "W:/eejarv/PhD-work/crop-modelling"
bDir <- paste(cmDir,"/GLAM",sep="")
cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

#input folder
cmipDir <- "V:/eejarv/CMIP5"
input_dir <- paste(glam_dir,"/inputs",sep="")

#these are the cells that have both yield and rainfall data
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

#get longitude and latitude (row and column)
rs <- raster(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/0_base_grids/igp_dummy.tif",sep=""))
cells$COL <- colFromX(rs,cells$X)
cells$ROW <- rowFromY(rs,cells$Y)

#output folders
gridDir <- paste(input_dir,"/grid",sep="")
ascDir <- paste(input_dir,"/ascii",sep="")
gsoilDir <- paste(ascDir,"/soil",sep="")

######################################################
#write the grid file
ofil <- paste(gridDir,"/CMIP5_grid.txt",sep="")
if (!file.exists(ofil)) {
  ofil <- write_gridFile(x=cells,outfile=ofil,fields=list(CELL="CELL",X="X",Y="Y",COL="COL",ROW="ROW"))
}


#get the soil shapefile from Andy's data and transform to raster so to get the data
soil_shp <- readShapePoly(paste(bDir,"/soil-data/C2004_IND/soil_IND.shp",sep=""))
rs[] <- NA
rs[cells$CELL] <- 1

rs_soil <- raster:::.polygonsToRaster(soil_shp, rs, field=soil_shp$Soil) 
rs_soil[which(is.na(rs[]))] <- NA

#all missing values need to be assigned appropriately. This was done via visual inspection
#also looking to the grid file "fe_soil_grid2.dat" of Andy
rs_soil[c(529,530)] <- 4
rs_soil[c(324,328,358:360,392:394,429,430)] <- 3
rs_soil[c(401,402,436,437)] <- 3
rs_soil[c(578,579,613:615)] <- 3
rs_soil[c(653)] <- 6
rs_soil[c(721)] <- 1
rs_soil[c(823,885,920,891,926,961)] <- 3

#plot(rs_soil,col=rev(rainbow(6)))
#text(x=cells$X,y=cells$Y,labels=cells$CELL,cex=0.5)
#points(cells$X,cells$Y,pch=20,cex=1)
#plot(soil_shp,add=T)

#assign values of gridcells
cells$SOILCODE <- extract(rs_soil,cbind(x=cells$X,y=cells$Y))

#note that the soiltypes file was copied from:
#\nfs\see-fs-01\earac\glam\inaleeds\inputs\soiltypes2.txt

######################################################
################## LOOP GRIDCELLS ####################
######################################################
for (cell in cells$CELL) {
  ######################################################
  cat("creating soil file for gricell",cell,"\n")
  ######################################################
  #soil types file
  #make a soil type file where the soil code is actually each gridcell
  #and then a soils grid file for any selected gridcell
  #these soil data have been derived from the HARMOIZED SOIL DATABASE of FAO
  #see the script gridSoilData.R for details on how the data was obtained
  
  #now need to create the soil codes file
  #function to write_soilcodes_oldstyle was created to overcome issue of incompatibility
  oSoilGrid <- paste(gsoilDir,"/soilcodes_",cell,".txt",sep="")
  if (!file.exists(oSoilGrid)) {
    oSoilGrid <- write_soilcodes_oldstyle(x=cells,outfile=oSoilGrid,cell=c(cell),fields=list(CELL="CELL",COL="COL",ROW="ROW",SOILCODE="SOILCODE"))
  }
}


