#### LIBRARIES: raster, maptools, rgdal, sp
####
stop("Error: do not source this whole thing!")

library(maptools); data(wrld_simpl)

#Getting unique coordinates
src.dir <- "D:/_tools/dapa-climate-change/trunk/wheat-igp"
source(paste(src.dir,"/EcoCrop-wspr.R",sep="")) #loading the functions

#directories
bDir <- "D:/CIAT_work/crop-modelling/when_wheat_igp"
crop <- "wspr"
rDir <- paste(bDir,"/runs",sep="")
if (!file.exists(rDir)) {dir.create(rDir)}

#sowing dates
sd <- paste(bDir,"/sow_dates/plant_lr.tif",sep="")
hd <- paste(bDir,"/sow_dates/harvest_lr.tif",sep="")

#outfolder
outf <- paste(rDir,'/run2', sep="")

###
#Running the model
eco <- suitCalc(climPath=paste(bDir,"/cru_data/clm_1980",sep=""), 
                sowDat=sd,
                harDat=hd,
                Gmin=NA,Gmax=NA,Tkmp=0,Tmin=10,Topmin=220,
                Topmax=340,Tmax=400,Rmin=NA,Ropmin=NA,
                Ropmax=NA,Rmax=NA, 
                outfolder=outf,
                cropname=crop,ext=".asc",cropClimate=T)

jpeg(paste(outf,"/output.jpg",sep=""), quality=100, height=1000,width=1000,units="px",pointsize=22)
par(mar=c(3,3,1,2))
plot(eco[[2]],col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
dev.off()




