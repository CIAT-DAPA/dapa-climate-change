#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#libraries
library(maptools); library(rgdal); library(raster)

#sourcing important functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop/src"
source(paste(src.dir,"/createMask.R",sep=""))

src.dir2<-"D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir2,"/detrender-functions.R",sep=""))

#set the working folder
bDir <- "F:/PhD-work/crop-modelling/GLAM/climate-signals-yield"
cropName <- "gnut"
cd <- paste(bDir,"/",toupper(cropName),sep="")

#set DOS emulator folder
eDir <- "C:/Program Files (x86)/DOSBox-0.74"
dDir <- "F:/PhD-work/GLAM/detrending/smoothr"

#load shapefile and define characteristics
shp <- paste(cd,"/shp/IND2-",tolower(cropName),".shp",sep="")
shp <- readShapePoly(shp)
relField <- "DISID" #relational field
adm2Field <- "NAME_1" #field of adm1 names
adm2Field <- "NAME_2" #field of adm2 names

#read historic yield data
yieldData <- read.table(paste(cDir,"/data/IND2-gnut.tab",sep=""),sep="\t",header=T)

#1. Detrend each of the districts data using lowess, linear or polynomial regression and do summaries
iyr <- 66; fyr <- 95

#important fields
yfds <- paste("Y",iyr:fyr,sep="") #yield
hfds <- paste("H",iyr:fyr,sep="") #area harvested
pfds <- paste("T",iyr:fyr,sep="") #total production

#detrend all districts
x <- detrendAll(yieldData,"DISID",yfds,1900+(iyr:fyr),cd)

#load data to not re-process everything again
raw <- read.csv(paste(cd,"/data/detrended-IND2-gnut-raw.csv",sep=""))
loe <- read.csv(paste(cd,"/data/detrended-IND2-gnut-loess.csv",sep=""))
lin <- read.csv(paste(cd,"/data/detrended-IND2-gnut-linear.csv",sep=""))
qua <- read.csv(paste(cd,"/data/detrended-IND2-gnut-quadratic.csv",sep=""))
fou <- read.csv(paste(cd,"/data/detrended-IND2-gnut-fourier.csv",sep=""))

# plot(rawdat$YEAR,rawdat$YIELD,pch=20,cex=0.8,ylim=c(0,1500)); lines(rawdat$YEAR,rawdat$YIELD)
# points(loedat$YEAR,loedat$YIELD,pch=20,cex=0.8,col="red"); lines(loedat$YEAR,loedat$YIELD,col="red")
# points(lindat$YEAR,lindat$YIELD,pch=20,cex=0.8,col="blue"); lines(lindat$YEAR,lindat$YIELD,col="blue")
# points(quadat$YEAR,quadat$YIELD,pch=20,cex=0.8,col="orange"); lines(quadat$YEAR,quadat$YIELD,col="orange")
# points(foudat$YEAR,foudat$YIELD,pch=20,cex=0.8,col="dark green"); lines(foudat$YEAR,foudat$YIELD,col="dark green")

#Calculate or load the summary data
if (!file.exists(paste(cd,"/data/detrended-IND2-gnut-summary.csv",sep=""))) {
  sData <- calcSummary(yieldData,"DISID",yfds,hfds,pfds,1900+(iyr:fyr),cd)
} else {
  sData <- read.csv(paste(cDir,"/data/detrended-IND2-gnut-summary.csv",sep=""))
}

##################################################################################
##################################################################################
#2. For each of the years create a 1x1 degree grid with the detrended series by
#   2.1. Create a 1x1 min resolution raster with the districts
#   2.2. Create a 1x1 min resolution raster with the 1x1 degree cells
#   2.3. Calculate areas per pixel
#   2.4. Loop through cells and calculate the proportion of each district (per year)
#   2.5. Calculate the average by weighting the values of yield times 
#        the area divided by sum of areas (per year)

#convert shape to raster (if not exists) for assigning values and then plot
resol <- 1/60

#clean mask raster
if (!file.exists(paste(cDir,"/raster/india-1min-msk.asc",sep=""))) {
  rs <- createMask(shp,resol)
  rs <- writeRaster(rs,paste(cDir,"/raster/india-1min-msk.asc",sep=""),format='ascii')
} else {
  rs <- raster(paste(cDir,"/raster/india-1min-msk.asc",sep=""))
}

#raster with district IDs
if (!file.exists(paste(cd,"/raster/india-1min-disid.asc",sep=""))) {
  rk <- rasterize(shp,rs,field="DISID")
  rk <- writeRaster(rk,paste(cd,"/raster/india-1min-disid.asc",sep=""),format='ascii')
} else {
  rk <- raster(paste(cd,"/raster/india-1min-disid.asc",sep=""))
}

x <- createSummaryRasters(rs,rk,sData,"DISID",cd)


#Function to put the district data of a year into a raster and return it
outYearDir <- paste(cd,"/raster/yearly",sep="")
if (!file.exists(outYearDir)) {dir.create(outYearDir)}

dataType <- "raw"
outDataDir <- paste(outYearDir,"/",dataType,"-",sep="")
if (!file.exists(outDataDir)) {dir.create(outDataDir)}

#loop through years
for (year in 66:95) {
  cat("Processing year",(1900+year),"\n")
  yr_rs <- createYearRaster(raw,rk,year,"Y","DISID")
  outName <- paste(outDataDir,"/",type,"-",year,".asc",sep="")
  yr_rs <- writeRaster(yr_rs,outName,format="ascii")
  rm(yr_rs); g=gc(); rm(g)
}










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

