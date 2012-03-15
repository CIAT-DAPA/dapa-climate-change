#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#libraries
library(maptools); library(rgdal); library(raster)
data(wrld_simpl)

#sourcing important functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop/src"
#src.dir <- "/home/jramirez/dapa-climate-change/EcoCrop/src"
source(paste(src.dir,"/createMask.R",sep=""))

src.dir2<-"D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "/home/jramirez/dapa-climate-change/PhD/0007-crop-modelling/scripts"
source(paste(src.dir2,"/detrender-functions.R",sep=""))

#set the working folder
bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield"
#bDir <- "/andromeda_data1/jramirez/crop-modelling/GLAM/climate-signals-yield"
cropName <- "wheat"
cd <- paste(bDir,"/",toupper(cropName),sep="")

#set DOS emulator folder
eDir <- "C:/Program Files (x86)/DOSBox-0.74"
dDir <- "F:/PhD-work/GLAM/detrending/smoothr"

#load shapefile and define characteristics
shp <- paste(cd,"/shp/IND2-",cropName,".shp",sep="")
shp <- readShapePoly(shp)
relField <- "DISID" #relational field
adm2Field <- "NAME_1" #field of adm1 names
adm2Field <- "NAME_2" #field of adm2 names

#read historic yield data
yieldData <- read.table(paste(cd,"/data/IND2-",cropName,".tab",sep=""),sep="\t",header=T)

#1. Detrend each of the districts data using lowess, linear or polynomial regression and do summaries
iyr <- 66; fyr <- 98

#important fields
yfds <- paste("Y",iyr:fyr,sep="") #yield
hfds <- paste("H",iyr:fyr,sep="") #area harvested
pfds <- paste("T",iyr:fyr,sep="") #total production

#detrend all districts
x <- detrendAll(yieldData,"DISID",yfds,iyr,fyr,cd,cropName)




# ###############################
# #list features
# nPol <- length(shp@polygons)
# for (p in 1:nPol) {
#   	cat("Pol", p, "\n")
# 		cname <- shp@data$COUNTRY[p]
# 		pol <- shp@polygons[p] #extract single polygon
# 		sh <- SpatialPolygons(pol) #create SP object from extracted feature
# 		rs <- createMask(sh, res) #create a raster from the SP object
# }

