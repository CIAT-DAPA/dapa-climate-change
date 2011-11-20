#Julian Ramirez-Villegas
#University of Leeds / CCAFS / CIAT
#2011

#Select occurrences that are within a given extent

require(raster); require(rgdal)
require(maptools)

gridDir <- "F:/EcoCrop-development/climate/afasia_2_5min"
rs <- raster(paste(gridDir,"/prec_1.asc",sep=""))

dataDir <- "D:/CCAFS/EcoCrop-data/groundnut/from-gbif-ciat-db"
dta <- read.csv(paste(dataDir,"/arachis.csv",sep=""))

dtaSel <- dta[which(dta$longitude >= rs@extent@xmin & dta$longitude <= rs@extent@xmax
                    & dta$latitude >= rs@extent@ymin & dta$latitude <= rs@extent@ymax),]

#select only hypogaea
dtaSelHyp <- dtaSel[which(!is.na(dtaSel$specie_name)),]

#load and plot harvested area grids
harvDir <- "D:/CCAFS/EcoCrop-data/groundnut/cropdist"
mirca <- raster(paste(harvDir,"/mirca2000-rfd.asc",sep=""))
monfr <- raster(paste(harvDir,"/monfreda.asc",sep=""))
ispam <- raster(paste(harvDir,"/spam.asc",sep=""))

#crop all resutls
mirca <- crop(mirca,extent(rs))
monfr <- crop(monfr,extent(rs))
ispam <- crop(ispam,extent(rs))

mirca[which(mirca[] == 0)] <- NA
monfr[which(monfr[] == 0)] <- NA
ispam[which(ispam[] == 0)] <- NA

#plot all stuff
par(mar=c(1,1,1,1))
data(wrld_simpl)
plot(dtaSel$longitude,dtaSel$latitude,pch="+",col="red",cex=0.5)
points(dtaSelHyp$longitude,dtaSelHyp$latitude,pch="+",col="blue",cex=0.5)
plot(wrld_simpl,add=T)

plot(mirca,col=colorRampPalette(c("grey80","grey10"))(100))
plot(wrld_simpl,add=T)
points(dtaSel$longitude,dtaSel$latitude,pch="+",col="red",cex=0.5)
points(dtaSelHyp$longitude,dtaSelHyp$latitude,pch="+",col="blue",cex=0.5)

plot(monfr,col=colorRampPalette(c("grey80","grey10"))(100))
plot(wrld_simpl,add=T)
points(dtaSel$longitude,dtaSel$latitude,pch="+",col="red",cex=0.5)
points(dtaSelHyp$longitude,dtaSelHyp$latitude,pch="+",col="blue",cex=0.5)

plot(ispam,col=colorRampPalette(c("grey80","grey10"))(100))
plot(wrld_simpl,add=T)
points(dtaSel$longitude,dtaSel$latitude,pch="+",col="red",cex=0.5)
points(dtaSelHyp$longitude,dtaSelHyp$latitude,pch="+",col="blue",cex=0.5)


