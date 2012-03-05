#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

library(maptools); library(raster); data(wrld_simpl)

wd <- "F:/PhD-work/crop-modelling/EcoCrop"
setwd(wd)

#load shapefile
shp <- readShapePoly("./analysis-mask/adm_data_shp/adm1_update1.shp")
shData <- shp@data

#here loop through the number of units, create mask grid and get total harv area from total grid


#calculate total harvested area
#if Total_h > a given value for both Monfreda and SPAM then present, else absent
#write back to table
#write shapefile with zeros and ones
#plot as an example
