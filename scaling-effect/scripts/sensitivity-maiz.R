#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

#do 96 sensitivity runs over the whole of West Africa

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

#source functions
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
#bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
#cascadeDir <- "/mnt/see-archive-12_a4scratch/eebjp"
#cascadeDir <- "/nfs/see-archive-12_a4scratch/eebjp"

sensDir <- paste(runDir,"/sens",sep="")
if (!file.exists(sensDir)) {dir.create(sensDir)}

#figure dir is local (on mbp)
figDir <- paste(bDir,"/figures_new",sep="")
if (!file.exists(figDir)) {dir.create(figDir)}

#model run details
trial <- 6
crop_name <- "maiz"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))
msk[which(msk[] < 0)] <- NA
msk[which(msk[] > 0)] <- 1 #1:length(which(msk[] > 0))

#make sensitivity table
sensruns <- expand.grid(TEMP=seq(-1,6,by=1),PREC=seq(-0.9,0.2,by=0.1))
write.csv(sensruns,paste(sensDir,"/sensitivity_runs.csv",sep=""),quote=T,row.names=F)

#resolution
resol <- "12km_exp"
metDir <- paste(clmDir,"/cascade_",resol,sep="")

#loop the sensruns
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  if (!file.exists(tsensDir)) {dir.create(tsensDir)}
  
  tsensMetDir <- paste(tsensDir,"/climate",sep="")
  if (!file.exists(tsensMetDir)) {dir.create(tsensMetDir)}
  
  #create the meteorology for this run
  for (m in 1:12) {
    #m <- 1
    cat(m,"... ",sep="")
    if (!file.exists(paste(tsensMetDir,"/tmax_",m,".tif",sep=""))) {
      tmax <- raster(paste(metDir,"/tmax_",m,".tif",sep="")) + temp_p * 10
      writeRaster(tmax,paste(tsensMetDir,"/tmax_",m,".tif",sep=""),format="GTiff",overwrite=F)
    }
    
    if (!file.exists(paste(tsensMetDir,"/tmean_",m,".tif",sep=""))) {
      tmean <- raster(paste(metDir,"/tmean_",m,".tif",sep="")) + temp_p * 10
      writeRaster(tmean,paste(tsensMetDir,"/tmean_",m,".tif",sep=""),format="GTiff",overwrite=F)
    }
    
    if (!file.exists(paste(tsensMetDir,"/tmin_",m,".tif",sep=""))) {
      tmin <- raster(paste(metDir,"/tmin_",m,".tif",sep="")) + temp_p * 10
      writeRaster(tmin,paste(tsensMetDir,"/tmin_",m,".tif",sep=""),format="GTiff",overwrite=F)
    }
    
    if (!file.exists(paste(tsensMetDir,"/prec_",m,".tif",sep=""))) {
      prec <- raster(paste(metDir,"/prec_",m,".tif",sep=""))
      prec <- prec * (1 + prec_p)
      writeRaster(prec,paste(tsensMetDir,"/prec_",m,".tif",sep=""),format="GTiff",overwrite=F)
    }
  }
  cat("\n")
  
  #run EcoCrop with modified meteorology
  tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 6
  rmin <- 100; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 6
  
  trial <- 6
  outf <- paste(tsensDir,"/run_",trial,sep="")
  
  tpdate <- paste(runDir,"/",resol,"/calendar/plant_",resol,".tif",sep="")
  thdate <- paste(runDir,"/",resol,"/calendar/harvest_",resol,".tif",sep="")
  
  #run the model
  if (!file.exists(paste(outf,"/out_suit.png",sep=""))) {
    eco <- suitCalc(climPath=tsensMetDir, 
                    sowDat=tpdate,
                    harDat=thdate,
                    Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                    Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                    Ropmax=ropmax,Rmax=rmax, 
                    outfolder=outf,
                    cropname=crop_name,ext=".tif",cropClimate=F)
    
    png(paste(outf,"/out_suit.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
    par(mar=c(3,3,1,2))
    rsx <- eco[[3]]; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rev(terrain.colors(20)))
    plot(wrld_simpl,add=T)
    grid(lwd=1.5)
    dev.off()
  }
}







