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

#remove gridcells which are not to be run anyway
ocells <- ocells[which(ocells$AHRATIO>=.2),]

#calculate "initial" ygp
###method 1 (fraction of maximum yield)
ocells_ygp <- data.frame()
for (z in unique(ocells$ZONE)) {
  #z <- unique(ocells$ZONE)[1]
  zCells <- ocells[which(ocells$ZONE == z),]
  zCells$YGP <- round(zCells$YIELD_MN/max(zCells$YIELD_MN),2)
  ocells_ygp <- rbind(ocells_ygp,zCells)
}

###method 2 (from first successful simulation)
ygp_rs <- raster(paste(cDir,"/calib/exp-10_outputs/general/calib_results_spat/ygp.asc",sep=""))
ocells_ygp2 <- ocells
ocells_ygp2$YGP <- extract(ygp_rs,cbind(x=ocells_ygp2$X,y=ocells_ygp2$Y))

#version 1
#ocells$ISSEL[which(ocells$YIELD_CV>15 & ocells$AHRATIO > 0.1 & ocells$IRATIO < .25)] <- 1 #v1

#version 2
ocells$ISSEL[which(ocells$YIELD_CV>25 & ocells$YIELD_MN>300 & ocells$AHRATIO>0.2 & ocells$YIELD_NA == 0)] <- 1 #v2
ocells$ISSEL[which(ocells$ZONE == 4 & ocells$YIELD_NA == 0 & ocells$YIELD_CV>20 & ocells$YIELD_MN>300 & ocells$AHRATIO>0.2 )] <- 1 #v2
ocells$ISSEL[which(ocells$CELL == 636)] <- 1 #v2

#version 4 (3 is single "best" gridcells, see script 'glam-selecGridcells_loc_best.R')
ocells_ygp$ISSEL <- 0
ocells_ygp$ISSEL[which(ocells_ygp$YIELD_CV>25 & ocells_ygp$YIELD_MN>300 & ocells_ygp$AHRATIO>0.2 & ocells_ygp$YIELD_NA == 0)] <- 1 #v2
ocells_ygp$ISSEL[which(ocells_ygp$ZONE == 4 & ocells_ygp$YIELD_NA == 0 & ocells_ygp$YIELD_CV>20 & ocells_ygp$YIELD_MN>300 & ocells_ygp$AHRATIO>0.2 )] <- 1 #v2
ocells_ygp$ISSEL[which(ocells_ygp$CELL == 636)] <- 1 #v2

#version 5
ocells_ygp2$ISSEL <- 0
ocells_ygp2$ISSEL[which(ocells_ygp2$YIELD_CV>25 & ocells_ygp2$YIELD_MN>300 & ocells_ygp2$AHRATIO>0.2 & ocells_ygp2$YIELD_NA == 0)] <- 1 #v2
ocells_ygp2$ISSEL[which(ocells_ygp2$ZONE == 4 & ocells_ygp2$YIELD_NA == 0 & ocells_ygp2$YIELD_CV>20 & ocells_ygp2$YIELD_MN>300 & ocells_ygp2$AHRATIO>0.2 )] <- 1 #v2
ocells_ygp2$ISSEL[which(ocells_ygp2$CELL == 636)] <- 1 #v2

#for v5 simply load version 4 and put the selected ones into the v5 data.frame
ocells_v4 <- read.csv(paste(cDir,"/inputs/calib-cells-selection-v4.csv",sep=""))
ocells_v4 <- data.frame(CELL=ocells_v4$CELL,ISSEL_F=ocells_v4$ISSEL_F)
ocells_ygp2 <- merge(ocells_ygp2,ocells_v4,by="CELL",sort=F)
write.csv(ocells_ygp2,paste(cDir,"/inputs/calib-cells-selection-v5.csv",sep=""),row.names=F,quote=T)

#verify number of gridcells
# length(ocells_ygp2$YGP[which(ocells_ygp2$ZONE==1 & ocells_ygp2$ISSEL_F == 1)])
# length(ocells_ygp2$YGP[which(ocells_ygp2$ZONE==2 & ocells_ygp2$ISSEL_F == 1)])
# length(ocells_ygp2$YGP[which(ocells_ygp2$ZONE==3 & ocells_ygp2$ISSEL_F == 1)])
# length(ocells_ygp2$YGP[which(ocells_ygp2$ZONE==4 & ocells_ygp2$ISSEL_F == 1)])
# length(ocells_ygp2$YGP[which(ocells_ygp2$ZONE==5 & ocells_ygp2$ISSEL_F == 1)])


#some plotting
#windows()
plot(zrs,col=rev(terrain.colors(5)))
points(ocells_ygp$X[which(ocells_ygp$ISSEL == 1)],ocells_ygp$Y[which(ocells_ygp$ISSEL == 1)],pch=20,cex=0.75)

plot(ocells_ygp$YIELD_MN,ocells_ygp$YIELD_SD,pch=20)
points(ocells_ygp$YIELD_MN[which(ocells_ygp$ISSEL==1)],
       ocells_ygp$YIELD_SD[which(ocells_ygp$ISSEL==1)],pch=20,col="red")


#final gridcell selection
###get back to original table
ocells <- ocells_ygp

############################################################################
### selection of various gridcells for optimisation
#now select the gridcells that you would optimise based on the following rules
#get how many gridcells are ISSEL == 1
#from those that are ISSEL == 1, subselect randomly the number that
#  corresponds to 30% of total
#create a new column with truly selected gridcells
for (z in unique(ocells$ZONE)) {
  zcells <- ocells[which(ocells$ZONE == z),]
  nsel <- 10 #round(nrow(zcells)*0.3+0.5,0)
  
  scells <- zcells[which(zcells$ISSEL == 1),]
  if (nrow(scells) < 10) {stop(z,": less than 10 gridcells")}
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
write.csv(out_cells,paste(cDir,"/inputs/calib-cells-selection-v4.csv",sep=""),row.names=F,quote=T)


#plot points in the selected cells
#windows()
plot(zrs,col=rev(terrain.colors(5)))
points(out_cells$X[which(out_cells$ISSEL_F == 1)],out_cells$Y[which(out_cells$ISSEL_F == 1)],pch=20,cex=0.75)
#points(ocells$X[which(ocells$ISSEL == 1)],ocells$Y[which(ocells$ISSEL == 1)],pch=20,cex=0.75)
#points(out_cells$X[which(out_cells$CELL == 636)],out_cells$Y[which(out_cells$CELL == 636)],pch="+",cex=0.75)
#plot(wrld_simpl,add=T)

plot(out_cells$YIELD_MN,out_cells$YIELD_SD,pch=20)
points(out_cells$YIELD_MN[which(out_cells$ISSEL_F==1)],
       out_cells$YIELD_SD[which(out_cells$ISSEL_F==1)],pch=20,col="red")


