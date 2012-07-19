#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#extract mean yield, yield variability, harvested area, and area irrigated for all gridcells
#of all groundnut-growing areas of India

#packages
library(raster)

#base working directory and other details
#bDir <- "F:/PhD-work/crop-modelling/GLAM"
bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
cropName <- "gnut"
method <- "lin"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

#load cell list
cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))

#load yield data and calculate mean, std and cv
yields <- stack(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/raster/gridded/",tolower(method),"/",tolower(method),"-",66:93,".asc",sep=""))
ymn <- calc(yields,fun = function(x) {mean(x[which(x!=0)],na.rm=T)})
ysd <- calc(yields,fun = function(x) {sd(x[which(x!=0)],na.rm=T)})
ycv <- ysd/ymn*100
yna <- calc(yields,fun = function(x) {length(which(x==0))})

#load area harvested and calculate mean, std, etc
aha <- stack(paste(cDir,"/harvested_area/raster/gridded/raw-",1966:1993,".asc",sep=""))
mha <- calc(aha,fun = function(x) {mean(x[which(x!=0)],na.rm=T)})
car <- area(mha)
car <- car*(1000^2)/(100^2)
ahr <- mha*1000/car; ahr <- ahr*100

#load irrigation ratio
ir <- stack(paste(cDir,"/irrigated_ratio/raw-",1966:1993,".asc",sep=""))
mir <- calc(ir,fun = function(x) {mean(x,na.rm=T)}) #mean irrigation ratio

#load zones raster
zrs <- raster(paste(cDir,"/gnut-zones/zones_lr.asc",sep=""))


#output data.frame
ocells <- data.frame(CELL=cells$CELL,X=cells$X,Y=cells$Y)
ocells$ZONE <- extract(zrs,cbind(x=cells$X,y=cells$Y))
ocells$YIELD_MN <- extract(ymn,cbind(x=cells$X,y=cells$Y))
ocells$YIELD_SD <- extract(ysd,cbind(x=cells$X,y=cells$Y))
ocells$YIELD_CV <- extract(ycv,cbind(x=cells$X,y=cells$Y))
ocells$YIELD_NA <- extract(yna,cbind(x=cells$X,y=cells$Y))
ocells$AHARV <- extract(mha,cbind(x=cells$X,y=cells$Y))
ocells$AHRATIO <- extract(ahr,cbind(x=cells$X,y=cells$Y))
ocells$IRATIO <- extract(mir,cbind(x=cells$X,y=cells$Y))


#selecting the gridcells i will work with in the multi cell calib
ocells <- ocells[which(!is.na(ocells$YIELD_CV)),]
ocells$ISSEL <- 0

#version 3
ocells$ISSEL[which(ocells$AHRATIO>0.2 & ocells$YIELD_NA == 0)] <- 1 #v3

#version 6
#remove gridcells which are not to be run anyway
ocells <- ocells[which(ocells$AHRATIO>=.2),]
ocells$ISSEL[which(ocells$AHRATIO>0.2 & ocells$YIELD_NA == 0)] <- 1 #v3


#######################
#The below plots are for visual inspection of maximum yield and reasonable harvested area
windows()
library(maptools); data(wrld_simpl)
par(mfrow=c(1,2))
plot(zrs,col=rev(terrain.colors(5)))
plot(wrld_simpl,add=T)
text(x=ocells$X[which(ocells$ISSEL==1)],
     y=ocells$Y[which(ocells$ISSEL==1)],
     labels=round(ocells$AHRATIO[which(ocells$ISSEL==1)]*10,0),cex=0.4)


plot(zrs,col=rev(terrain.colors(5)))
plot(wrld_simpl,add=T)
text(x=ocells$X[which(ocells$ISSEL==1)],
     y=ocells$Y[which(ocells$ISSEL==1)],
     labels=round(ocells$YIELD_MN[which(ocells$ISSEL==1)]*0.1,0),cex=0.4)
points(x=ocells$X[ocells$CELL==644],y=ocells$Y[ocells$CELL==644])


###############
#after this visual inspection the gridcells that I decided to select were:
#zone 1: 435
#zone 2: 565
#zone 3: 644
#zone 4: 720
#zone 5: 960
#

out_cells <- ocells
out_cells$ISSEL_F <- 0

out_cells$ISSEL_F[which(out_cells$CELL == 435)] <- 1 #zone 1
out_cells$ISSEL_F[which(out_cells$CELL == 565)] <- 1 #zone 2
out_cells$ISSEL_F[which(out_cells$CELL == 644)] <- 1 #zone 3
out_cells$ISSEL_F[which(out_cells$CELL == 720)] <- 1 #zone 4
out_cells$ISSEL_F[which(out_cells$CELL == 960)] <- 1 #zone 5

#write output data.frame
write.csv(out_cells,paste(cDir,"/inputs/calib-cells-selection-v6.csv",sep=""),row.names=F,quote=T)


#plot points in the selected cells
#windows()
plot(zrs,col=rev(terrain.colors(5)))
points(out_cells$X[which(out_cells$ISSEL_F == 1)],out_cells$Y[which(out_cells$ISSEL_F == 1)],pch=20,cex=0.75)
#points(ocells$X[which(ocells$ISSEL == 1)],ocells$Y[which(ocells$ISSEL == 1)],pch=20,cex=0.75)
#points(out_cells$X[which(out_cells$CELL == 636)],out_cells$Y[which(out_cells$CELL == 636)],pch="+",cex=0.75)
#plot(wrld_simpl,add=T)


