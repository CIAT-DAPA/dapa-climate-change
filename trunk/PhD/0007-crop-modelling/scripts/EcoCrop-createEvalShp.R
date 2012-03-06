#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

library(maptools); library(raster); data(wrld_simpl)

#Loading extra functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop/src"
source(paste(src.dir,"/createMask.R",sep="")) #loading the function

#working directory
wd <- "F:/PhD-work/crop-modelling/EcoCrop"
setwd(wd)

#load shapefile
shp <- readShapePoly("./analysis-mask/adm_data_shp/adm1_update1.shp")
shData <- shp@data

#load raster
cropName <- "cass"
mon <- raster(paste("./models/EcoCrop-",toupper(cropName),"/cropdist/monfreda.asc",sep=""))
spa <- raster(paste("./models/EcoCrop-",toupper(cropName),"/cropdist/spam.asc",sep=""))

#here loop through the number of units, create mask grid and get total harv area from total grid
nPol <- length(shp@polygons)

for (p in 1:nPol) {
  cname <- shData$COUNTRY_NA[p]
  dname <- shData$NAME1_[p]
  
  cat("District", paste(dname, ", ", cname, " (", p, " of ", nPol, ")",sep=""),"\n")
  pol <- shp@polygons[p] #extract single polygon
  sh <- SpatialPolygons(pol) #create SP object from extracted feature
  rs <- createMask(sh, xres(mon)) #create a raster from the SP object
  ar <- area(rs)
  xy <- as.data.frame(xyFromCell(rs,which(!is.na(rs[]))))
  
  xy$area <- extract(ar,cbind(xy$x,xy$y))
  xy$mon_raw <- extract(mon,cbind(xy$x,xy$y))
  xy$mon <- xy$mon_raw*xy$area
  xy$spa <- extract(spa,cbind(xy$x,xy$y))
  
  tot_mon <- sum(xy$mon,na.rm=T)
  tot_spa <- sum(xy$spa,na.rm=T)
  
  shp@data$HAREA_MON[p] <- tot_mon
  shp@data$HAREA_SPM[p] <- tot_spa
  
  if (tot_mon > 0.5 & tot_spa > 0.5) {
    shp@data$ISPRESENT[p] <- 1
  } else {
    shp@data$ISPRESENT[p] <- 0
  }
}

#write shapefile with zeros and ones
od <- paste("./models/EcoCrop-",toupper(cropName),"/agricultural-data",sep="")
if (!file.exists(od)) {dir.create(od)}
writePolyShape(shp,paste(od,"/monfreda_spam_distribution",sep=""))


