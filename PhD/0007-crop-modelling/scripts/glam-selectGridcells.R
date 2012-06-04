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
ymn <- calc(yields,fun = function(x) {mean(x,na.rm=T)})
ysd <- calc(yields,fun = function(x) {sd(x,na.rm=T)})
ycv <- ysd/ymn*100

#load area harvested and calculate mean, std, etc
aha <- stack(paste(cDir,"/harvested_area/raster/gridded/raw-",1966:1993,".asc",sep=""))
mha <- calc(aha,fun = function(x) {mean(x,na.rm=T)})
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
ocells$AHARV <- extract(mha,cbind(x=cells$X,y=cells$Y))
ocells$AHRATIO <- extract(ahr,cbind(x=cells$X,y=cells$Y))
ocells$IRATIO <- extract(mir,cbind(x=cells$X,y=cells$Y))


#selecting the gridcells i will work with in the multi cell calib
ocells <- ocells[which(!is.na(ocells$YIELD_CV)),]
ocells$ISSEL <- 0
#ocells$ISSEL[which(ocells$YIELD_CV>15 & ocells$AHRATIO > 0.1 & ocells$IRATIO < .25)] <- 1 #v1
ocells$ISSEL[which(ocells$YIELD_CV>30 & ocells$AHRATIO > 0.2)] <- 1 #v2

#now select the gridcells that you would optimise based on the following rules
#get how many gridcells are ISSEL == 1
#from those that are ISSEL == 1, subselect randomly the number that
#  corresponds to 30% of total
#create a new column with truly selected gridcells
for (z in unique(ocells$ZONE)) {
  zcells <- ocells[which(ocells$ZONE == z),]
  nsel <- 10 #round(nrow(zcells)*0.3+0.5,0)
  
  scells <- zcells[which(zcells$ISSEL == 1),]
  row.names(scells) <- 1:nrow(scells)
  
  zcells$ISSEL_F <- 0
  scells$ISSEL_F <- 0
  if (nrow(scells) > nsel) {
    scells$ISSEL_F[sample(1:nrow(scells),size=nsel)] <- 1
    fcells <- scells$CELL[which(scells$ISSEL_F == 1)]
    zcells$ISSEL_F[which(zcells$CELL %in% fcells)] <- 1
  } else {
    if (nrow(scells) > 0) {
      scells$ISSEL_F <- 1
      fcells <- scells$CELL[which(scells$ISSEL_F == 1)]
      zcells$ISSEL_F[which(zcells$CELL %in% fcells)] <- 1
    }
  }
  
  if (z == unique(ocells$ZONE)[1]) {
    out_cells <- zcells
  } else {
    out_cells <- rbind(out_cells,zcells)
  }
  
}

#write output data.frame
write.csv(out_cells,paste(cDir,"/inputs/calib-cells-selection-v2.csv",sep=""),row.names=F,quote=T)


#plot points in the selected cells
#windows()
plot(zrs,col=rev(terrain.colors(5)))
points(out_cells$X[which(out_cells$ISSEL_F == 1)],out_cells$Y[which(out_cells$ISSEL_F == 1)],pch=20,cex=0.75)




