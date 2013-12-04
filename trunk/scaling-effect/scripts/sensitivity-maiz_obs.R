#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

###
#do 96 sensitivity runs over the whole of West Africa using obs meteorology
###

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
      tmax <- writeRaster(tmax,paste(tsensMetDir,"/tmax_",m,".tif",sep=""),format="GTiff",overwrite=F)
      rm(tmax)
    }
    
    if (!file.exists(paste(tsensMetDir,"/tmean_",m,".tif",sep=""))) {
      tmean <- raster(paste(metDir,"/tmean_",m,".tif",sep="")) + temp_p * 10
      tmean <- writeRaster(tmean,paste(tsensMetDir,"/tmean_",m,".tif",sep=""),format="GTiff",overwrite=F)
      rm(tmean)
    }
    
    if (!file.exists(paste(tsensMetDir,"/tmin_",m,".tif",sep=""))) {
      tmin <- raster(paste(metDir,"/tmin_",m,".tif",sep="")) + temp_p * 10
      tmin <- writeRaster(tmin,paste(tsensMetDir,"/tmin_",m,".tif",sep=""),format="GTiff",overwrite=F)
      rm(tmin)
    }
    
    if (!file.exists(paste(tsensMetDir,"/prec_",m,".tif",sep=""))) {
      prec <- raster(paste(metDir,"/prec_",m,".tif",sep=""))
      prec <- prec * (1 + prec_p)
      prec <- writeRaster(prec,paste(tsensMetDir,"/prec_",m,".tif",sep=""),format="GTiff",overwrite=F)
      rm(prec)
    }
    g=gc(); closeAllConnections()
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
    
    rm(eco); g=gc(); rm(g); closeAllConnections()
  }
}


#loop sensitivity runs.
#1. crop to locations in 12km_exp (with top being lat=15)
#2. calculate average of all pixels
#3. calculate average of pixels where ahar >= 0.1

extn <- extent(msk)
extn@ymax <- 15
msk2 <- crop(msk, extn)
xy <- as.data.frame(xyFromCell(msk2,which(!is.na(msk2[]))))
xy <- cbind(cell=cellFromXY(msk2,xy[,c("x","y")]),xy)

#load area harvested and resample to climate grid
aharv <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
fac <- round(xres(msk2)/xres(aharv))
aharv <- resample(aharv,msk2,method="ngb")
xy$aharv <- extract(aharv, xy[,c("x","y")])

outsens <- data.frame()
for (i in 1:nrow(sensruns)) {
  #i <- 1
  cat("Sensitivity run",i,"\n")
  
  prec_p <- sensruns$PREC[i]
  temp_p <- sensruns$TEMP[i]
  
  #load suitability raster
  tsensDir <- paste(sensDir,"/sens_",i,sep="")
  tsuit <- raster(paste(tsensDir,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  
  #extract values for all pixels
  suit_vals <- extract(tsuit, xy[,c("x","y")])
  suit_m1 <- mean(suit_vals,na.rm=T)
  
  #extract values for aharv>=0.1 pixels
  suit_vals <- extract(tsuit, xy[which(xy$aharv >= 0.1),c("x","y")])
  suit_m2 <- mean(suit_vals,na.rm=T)
  
  #put in data.frame
  outdf <- data.frame(sens=i,prec=prec_p,temp=temp_p,suit_all=suit_m1,suit_har=suit_m2)
  outsens <- rbind(outsens,outdf)
}
write.csv(outsens,paste(sensDir,"/sensitivity_result.csv",sep=""),quote=T,row.names=F)


#4. calculate change in suitability with respect to the unperturbed run
#[(Y(T,P) - Y(T0,P0) ) / (Y(T0,P0) *100 ]All - [(Y(T,P) - Y(T0,P0) ) / (Y(T0,P0) *100 ]0.1.
suit0_all <- outsens$suit_all[which(outsens$prec == 0 & outsens$temp == 0)]
suit0_har <- outsens$suit_har[which(outsens$prec == 0 & outsens$temp == 0)]

outsens$reldiff_all <- (outsens$suit_all - suit0_all) / suit0_all * 100
outsens$reldiff_har <- (outsens$suit_har - suit0_har) / suit0_har * 100

#5. calculate difference between these two (i.e. Y_all - Y_har )
outsens$diff <- outsens$reldiff_all - outsens$reldiff_har

#make a heatmap with this
hplot_df <- outsens[,c("prec","temp","diff")]
hplot_df$prec <- as.factor(hplot_df$prec)
hplot_df$temp <- as.factor(hplot_df$temp)

#example
library(ggplot2)
library(reshape2)

p <- ggplot(data=hplot_df, aes(temp, prec)) + geom_tile(aes(fill = diff), colour = NA)
p <- p + scale_fill_gradient2(name="", low = "red", mid="white", high = "blue", 
                              midpoint=0, limits=c(-20,20),guide="colourbar")
p <- p + theme(legend.key.height=unit(3.5,"cm"),legend.key.width=unit(1.25,"cm"), 
               panel.background=element_rect(fill="white",colour="black"),
               axis.text=element_text(colour="black"))
p <- p + labs(x = "Temperature change (K)", y = "Precipitation change (%)")

pdf(paste(figDir,"/sensitivity_heatmap.pdf",sep=""), height=8,width=10,pointsize=14)
print(p)
dev.off()


###
#produce beanplot of everything & aharv>=0.1
#get values from unperturbed run

suit <- raster(paste(runDir,"/",resol,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))

# trying out rasterVis
# suit_a <- msk2
# suit_a[] <- NA; suit_a[xy$cell] <- extract(suit,xy[,c("x","y")])
# suit_h <- msk2
# suit_h[] <- NA; suit_h[xy$cell[which(xy$aharv >= 0.1)]] <- extract(suit,xy[which(xy$aharv >= 0.1),c("x","y")])
# suitstk <- stack(suit_a,suit_h)
# names(suitstk) <- c("All","Harvested")
# bwplot(suitstk,maxpixels=nrow(xy),par.settings=rasterTheme())

library(beanplot)
bplot_df <- xy
bplot_df$suit <- extract(suit,xy[,c("x","y")])
bplot_df <- bplot_df[which(!is.na(bplot_df$suit)),]

png(paste(figDir,"/beanplot_all_0.1_areas.png",sep=""), res=300,
    height=1200,width=1200,units="px",pointsize=7)
par(mar=c(2.5,4.5,1,1),lwd=0.8)
beanplot(sample(bplot_df$suit,size=5000),bplot_df$suit[which(bplot_df$aharv >= 0.1)],bw=2,
         col="grey 80",overallline = "median",what=c(1,1,1,0),xlab=NA,ylab="Crop suitability (%)",
         xaxt = "n",border="black",beanlinewd=1.5)
axis(1, at=c(1,2), labels=c("All","Harvested"))
grid()
dev.off()


####
#now make a beanplot of each temperature gradient for all data and for ahar>=0.1
sensruns <- read.csv(paste(sensDir,"/sensitivity_runs.csv",sep=""))
sensruns <- cbind(IDRUN=1:nrow(sensruns), sensruns)

bplot_tsens <- xy
for (tpert in seq(-1,6,by=1)) {
  #tpert <- seq(-1,6,by=1)[1]
  tsens <- sensruns[which(sensruns$TEMP == tpert & sensruns$PREC == 0),]
  tsuit <- raster(paste(sensDir,"/sens_",tsens$IDRUN,"/run_",trial,"/",crop_name,"_suitability.tif",sep=""))
  bplot_tsens$value <- extract(tsuit,xy[,c("x","y")])
  names(bplot_tsens)[ncol(bplot_tsens)] <- paste("sens",tsens$IDRUN,sep="")
}

bplot_tsens_h <- bplot_tsens[which(bplot_tsens$aharv >= 0.1),]


png(paste(figDir,"/beanplot_sensitivity_all_areas.png",sep=""), res=300,
    height=1000,width=1500,units="px",pointsize=7)
par(mar=c(2.5,4.5,1,1),lwd=0.8)
beanplot(bplot_tsens[sample(1:nrow(bplot_tsens),size=5000),paste("sens",c(73:80),sep="")],
         col="grey 80",overallline = "median",what=c(1,1,1,0),xlab=NA,ylab="Crop suitability (%)",
         xaxt = "n",border="black",beanlinewd=1.5)
axis(1, at=c(1:8), labels=c("-1","0","+1","+2","+3","+4","+5","+6"))
grid()
dev.off()


#beanplot for temperature sensitivity for areas above 0.1
png(paste(figDir,"/beanplot_sensitivity_0.1_areas.png",sep=""), res=300,
    height=1000,width=1500,units="px",pointsize=7)
par(mar=c(2.5,4.5,1,1),lwd=0.8)
beanplot(bplot_tsens_h[,paste("sens",c(73:80),sep="")],
         col="grey 80",overallline = "median",what=c(1,1,1,0),xlab=NA,ylab="Crop suitability (%)",
         xaxt = "n",border="black",beanlinewd=1.5)
axis(1, at=c(1:8), labels=c("-1","0","+1","+2","+3","+4","+5","+6"))
grid()
dev.off()


