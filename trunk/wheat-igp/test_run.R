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
outf <- paste(rDir,'/run6', sep="")

###
# @TRGEM TRDV1 TRDV2 TRLFG TRPHS TRVRN TRHAR TRGFW TRGFN    
#  1     0     0     0     0     -5    -5   0     0    
#  26    26    30    10    5     0     0    16    16    
#  50    50    50    20    25    7     5    35    35    
#  60    60    60    35    35    15    10   45    45    


###
#Running the model
eco <- suitCalc(climPath=paste(bDir,"/cru_data/clm_1980",sep=""), 
                sowDat=sd,
                harDat=hd,
                Gmin=NA,Gmax=NA,Tkmp=-40,Tmin=0,Topmin=100,
                Topmax=300,Tmax=400,Rmin=NA,Ropmin=NA,
                Ropmax=NA,Rmax=NA, 
                outfolder=outf,
                cropname=crop,ext=".asc",cropClimate=T)

jpeg(paste(outf,"/output.jpg",sep=""), quality=100, height=1000,width=1000,units="px",pointsize=22)
par(mar=c(3,3,1,2))
plot(eco[[2]],col=rev(terrain.colors(20)))
plot(wrld_simpl,add=T)
dev.off()




