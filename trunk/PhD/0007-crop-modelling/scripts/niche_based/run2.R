#### LIBRARIES: raster, maptools, rgdal, sp
####
stop("Error: do not source this whole thing!")

### notes and changes
#May 2013: modified to use vrieling2012_filled instead of vrieling2012 (JRV)
#Jun 2013: modified to include LA and to use global Thornton data (JRV)

library(maptools); data(wrld_simpl); library(rasterVis)

#Getting unique coordinates
src.dir <- "/mnt/a101/earak/GeneticRes_Beans_Africa/scripts"
source(paste(src.dir,"/EcoCrop-model.R",sep="")) #loading the functions

#directories
bDir <- "/mnt/a101/earak/GeneticRes_Beans_Africa"
crop <- "bean"
rDir <- paste(bDir,"/runs",sep="")
if (!file.exists(rDir)) {dir.create(rDir)}

#############
### Africa
#############

#sowing dates
#vrieling
sd_vri <- paste(bDir,"/planting_harvest_dates/vrieling2012_filled/gs_start_doy1.tif",sep="")
hd_vri <- paste(bDir,"/planting_harvest_dates/vrieling2012_filled/gs_end_doy1.tif",sep="")

#Jones and Thornton
sd_jot <- paste(bDir,"/planting_harvest_dates/jtglobal/gs_start_doy_af.tif",sep="")
hd_jot <- paste(bDir,"/planting_harvest_dates/jtglobal/gs_end_doy_af.tif",sep="")

#outfolder
drun <- "run01_afr_vri" ### unique run number, please change in every run
outf <- paste(rDir,"/",drun, sep="")

###
#Some parameters
## Beebe et al. (2011)
#Tkill=0, Tmin=13.6, Topmin=17.5, Topmax=23.1, Tmax, 25.6
#Rmin=200, Ropmin=363, Ropmax=450, Rmax=710

#Vara-Prasad et al. (2002)
#Tmax=35 (or maybe 30)

###
#Running the model (with normal worldclim)
eco <- suitCalc(climPath=paste(bDir,"/climate_data/worldclim/africa_5min",sep=""), 
                sowDat=sd_vri,
                harDat=hd_vri,
                Gmin=NA,Gmax=NA,Tkmp=0,Tmin=80,Topmin=175,
                Topmax=250,Tmax=300,Rmin=200,Ropmin=350,
                Ropmax=700,Rmax=1600, 
                outfolder=outf,
                cropname=crop,ext=".tif",cropClimate=F)

png(paste(outf,"/out_psuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[1]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_tsuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[2]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

rm(eco); gc(T)

###
#African run for JTO LGP data
###

#outfolder
drun <- "run01_afr_jot" ### unique run number, please change in every run
outf <- paste(rDir,"/",drun, sep="")

###
#Running the model (with normal worldclim)
eco <- suitCalc(climPath=paste(bDir,"/climate_data/worldclim/africa_5min",sep=""), 
                sowDat=sd_jot,
                harDat=hd_jot,
                Gmin=NA,Gmax=NA,Tkmp=0,Tmin=80,Topmin=175,
                Topmax=250,Tmax=300,Rmin=200,Ropmin=350,
                Ropmax=700,Rmax=1600, 
                outfolder=outf,
                cropname=crop,ext=".tif",cropClimate=F)

png(paste(outf,"/out_psuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[1]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_tsuit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[2]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

rm(eco); gc(T)

#############
### Americas
#############

#sowing dates
#vrieling
#sd_vri <- paste(bDir,"/planting_harvest_dates/vrieling2012_filled/gs_start_doy1.tif",sep="")
#hd_vri <- paste(bDir,"/planting_harvest_dates/vrieling2012_filled/gs_end_doy1.tif",sep="")

#Jones and Thornton
sd_jot <- paste(bDir,"/planting_harvest_dates/jtglobal/gs_start_doy_la.tif",sep="")
hd_jot <- paste(bDir,"/planting_harvest_dates/jtglobal/gs_end_doy_la.tif",sep="")

#outfolder
drun <- "run01_ame_jto" ### unique run number, please change in every run
outf <- paste(rDir,"/",drun, sep="")

###
#Running the model (with normal worldclim)
eco <- suitCalc(climPath=paste(bDir,"/climate_data/worldclim/americas_5min",sep=""), 
                sowDat=sd_jot,
                harDat=hd_jot,
                Gmin=NA,Gmax=NA,Tkmp=0,Tmin=80,Topmin=175,
                Topmax=250,Tmax=300,Rmin=200,Ropmin=350,
                Ropmax=700,Rmax=1600, 
                outfolder=outf,
                cropname=crop,ext=".tif",cropClimate=F)

png(paste(outf,"/out_psuit.png",sep=""), height=2500,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[1]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_tsuit.png",sep=""), height=2500,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[2]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

png(paste(outf,"/out_suit.png",sep=""), height=2500,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()



