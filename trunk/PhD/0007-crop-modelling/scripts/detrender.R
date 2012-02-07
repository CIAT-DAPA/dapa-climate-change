#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL

#libraries
library(maptools); library(rgdal); library(raster)

#set the working folder
bDir <- "D:/CIAT_work/crop-modelling/GLAM/climate-signals-yield"
cropName <- "gnut"
cDir <- paste(bDir,"/",toupper(cropName),sep="")

#load shapefile and define characteristics
shp <- paste(cDir,"/shp/IND2-",tolower(cropName),".shp",sep="")
shp <- readShapePoly(shp)
relField <- "DISID"
adm1Field <- "NAME_2"

#list features
nPol <- length(shp@polygons)
for (p in 1:nPol) {
  	cat("Pol", p, "\n")
		cname <- shp@data$COUNTRY[p]
		pol <- shp@polygons[p] #extract single polygon
		sh <- SpatialPolygons(pol) #create SP object from extracted feature
		rs <- createMask(sh, res) #create a raster from the SP object
}

