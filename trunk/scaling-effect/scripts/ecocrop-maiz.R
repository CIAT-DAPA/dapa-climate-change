#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

###
#produce EcoCrop runs for maize in the Sahel for the scaling study
###

#load packages
library(rgdal); library(raster); library(maptools); data(wrld_simpl)

#source functions
#src.dir <- "/mnt/a102/eejarv/scaling-effect"
#src.dir <- "/nfs/a102/eejarv/scaling-effect"
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
#bDir <- "/mnt/a102/eejarv/scaling-effect"
#bDir <- "/nfs/a102/eejarv/scaling-effect"
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
figDir <- paste(bDir,"/figures_new",sep="")
#cascadeDir <- "/mnt/see-archive-12_a4scratch/eebjp"
#cascadeDir <- "/nfs/see-archive-12_a4scratch/eebjp"

if (!file.exists(runDir)) {dir.create(runDir)}
if (!file.exists(figDir)) {dir.create(figDir)}
crop_name <- "maiz"

######
###### prepare planting dates first

#get mask from CASCADE output
msk <- raster(paste(cascadeDir,"/cascade_input/Glam_04km_lsm.nc",sep=""))

#load sow and harvest dates
if (!file.exists(paste(bDir,"/calendar/Maize.crop.calendar/cascade_harvest.tif",sep=""))) {
  pdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/plant.filled.asc",sep=""))
  hdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/harvest.filled.asc",sep=""))
  
  #crop the data
  pdate <- crop(pdate,msk)
  hdate <- crop(hdate,msk)
  
  #write crop calendar
  pdate <- writeRaster(pdate,paste(bDir,"/calendar/Maize.crop.calendar/cascade_plant.tif",sep=""),format="GTiff")
  hdate <- writeRaster(hdate,paste(bDir,"/calendar/Maize.crop.calendar/cascade_harvest.tif",sep=""),format="GTiff")
} else {
  pdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_plant.tif",sep=""))
  hdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_harvest.tif",sep=""))
}

#crop and plot the harvested area data
if (!file.exists(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))) {
  ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/harvested.area.fraction.asc",sep=""))
  ahar <- crop(ahar,msk)
  ahar <- writeRaster(ahar,paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""),format="GTiff")
} else {
  ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv.tif",sep=""))
}

png(paste(figDir,"/harvested_area.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- ahar; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rainbow(20))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()


#crop and plot the yield data
if (!file.exists(paste(bDir,"/calendar/Maize.crop.calendar/cascade_yield.tif",sep=""))) {
  yield <- raster(paste(bDir,"/calendar/Maize.crop.calendar/maize_yield.asc",sep=""))
  yield <- crop(yield,msk)
  yield <- writeRaster(yield,paste(bDir,"/calendar/Maize.crop.calendar/cascade_yield.tif",sep=""),format="GTiff")
} else {
  yield <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_yield.tif",sep=""))
}

png(paste(figDir,"/yield.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- yield; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rainbow(20))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()

#### spam data processing
if (!file.exists(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv_spam.tif",sep=""))) {
  ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/spam2000v3r6_harvested-area_total_maize.asc",sep=""))
  ahar <- crop(ahar,msk)
  ahar <- writeRaster(ahar,paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv_spam.tif",sep=""),format="GTiff")
} else {
  ahar <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_aharv_spam.tif",sep=""))
}

png(paste(figDir,"/harvested_area_spam.png",sep=""), height=1000,width=1500,units="px",pointsize=22)
par(mar=c(3,3,1,2))
rsx <- ahar; rsx[which(rsx[]==0)] <- NA; plot(rsx,col=rainbow(20))
plot(wrld_simpl,add=T)
grid(lwd=1.5)
dev.off()


#resample crop calendar to all cascade resolutions
for (resol in c("04km","12km","40km","3deg")) {
  #resol <- "40km"
  cat(resol,"\n")
  if (!file.exists(paste(bDir,"/calendar/Maize.crop.calendar/cascade_harvest_",resol,".tif",sep=""))) {
    tmsk <- raster(paste(cascadeDir,"/cascade_input/Glam_",resol,"_lsm.nc",sep=""))
    tmsk[which(tmsk[] > 0)] <- 1
    tmsk[which(tmsk[] < 0)] <- 0
    
    fac <- round(xres(tmsk)/xres(pdate))
    if (fac == 1) {
      #resample the thing
      tpdate <- resample(pdate,tmsk,method="ngb")
      thdate <- resample(hdate,tmsk,method="ngb")
    } else {
      fac <- xres(tmsk)/xres(pdate)
      #tpdate <- aggregate(pdate,fact=fac,FUN=function(x) {round(min(x,na.rm=T),0)},expand=T)
      tpdate <- pdate
      tpdate <- resample(tpdate,tmsk,method="ngb")
      
      #thdate <- aggregate(hdate,fact=fac,FUN=function(x) {round(mean(x,na.rm=T),0)},expand=T)
      thdate <- hdate
      thdate <- resample(thdate,tmsk,method="ngb")
    }
    
    #write output raster
    tpdate <- writeRaster(tpdate,paste(bDir,"/calendar/Maize.crop.calendar/cascade_plant_",resol,".tif",sep=""),format="GTiff")
    thdate <- writeRaster(thdate,paste(bDir,"/calendar/Maize.crop.calendar/cascade_harvest_",resol,".tif",sep=""),format="GTiff")
    rm(list=c("tpdate","thdate","tmsk"))
  }
}

###
### do initial model runs at worldclim resolution, cut to the mask of 4km
outDir <- paste(clmDir,"/sahel_5min",sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

for (vn in c("prec","tmean","tmin","tmax")) {
  #vn <- "prec"
  cat(vn,"\n")
  for (m in 1:12) {
    #m <- 1
    if (!file.exists(paste(outDir,"/",vn,"_",m,".tif",sep=""))) {
      rs <- raster(paste(bDir,"/climate_data/global_5min/",vn,"_",m,sep=""))
      rs <- crop(rs, msk)
      rs <- writeRaster(rs,paste(outDir,"/",vn,"_",m,".tif",sep=""),format="GTiff")
    }
  }
}


###
### run model for initial testing
#parameter values

###from figure
#rmin <- 300; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 1
#rmin <- 300; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 2
#rmin <- 300; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 3
#rmin <- 200; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 4
#rmin <- 150; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 5
rmin <- 100; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 6

#from CERES-maize and IXIM
#      TBASE TOP1  TOP2  TMAX
#PRFTC  6.2  16.5  33.0  44.0     !Effect of temperature on photosynthesis
#RGFIL  5.5  16.0  39.0  48.5     !Effect of temperature on relative grain filling rate

#@ECO#  ECONAME.........  TBASE TOPT  ROPT
#IB0001 GENERIC MIDWEST1    8.0 34.0  34.0 !Effect of T on development

#from Kim et al. (2007) EEB
#topt1 = 30 | topt2 = 35 #for net photosynthesis
#topt1 = 25 | topt2 = 35 #for canopy carbon exchange rate

#from Lobell et al. (2011) NCC
#tbase=8 | topt=30 #for development

#from Schlenker and Roberts (2009) PNAS and Lobell et al. (2013) NCC
#topmax = 30 | topmin = 20 (?)

#tkill <- 0; tmin <- 100; topmin <- 180; topmax <- 330; tmax <- 470 #trial 1
#tkill <- 0; tmin <- 55; topmin <- 165; topmax <- 330; tmax <- 485 #trial 2
#tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 300; tmax <- 485 #trial 3
#tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 300; tmax <- 485 #trial 4
#tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 5
tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 6

trial <- 6
outf <- paste(runDir,"/calib/run_",trial,sep="")

#run the model
eco <- suitCalc(climPath=paste(clmDir,"/sahel_5min",sep=""), 
                sowDat=pdate@file@name,
                harDat=hdate@file@name,
                Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                Ropmax=ropmax,Rmax=rmax, 
                outfolder=outf,
                cropname=crop_name,ext=".tif",cropClimate=F)

#plot the results
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


####
#### goes with 6 as the final parameterisation. Although 5 and 4 are not too different
####

#1. prepare cascade data (totals per month)
resList <- c("04km_exp","12km_exp","12km","40km")

year <- 2006
dates <- createDateGrid(year)
dates$MONTH <- sapply(dates$MTH.DAY,FUN=function(x) as.numeric(substr(x,2,3)))
dates$DAY <- sapply(dates$MTH.DAY,FUN=function(x) as.numeric(substr(x,5,6)))
dates$MTH.DAY <- NULL
dates <- dates[which(dates$MONTH >= 6 & dates$MONTH <= 10),]
dates <- cbind(SEQ=dates$JD-152,dates)
dates$SEQ[which(dates$JD <= 153)] <- 1
dates$SEQ[which(dates$JD >= 296)] <- 144

#a. calculate totals / means per month
#looping resolutions
for (resol in resList) {
  #resol <- resList[1]
  cat("processing:",resol,"\n")
  if (resol == "04km_exp" | resol == "12km_exp") {
    dataDir <- paste(cascadeDir,"/cascade_input/exp",sep="")
    tres <- gsub("_exp","",resol)
  } else if (resol == "3deg") {
    dataDir <- paste(cascadeDir,"/cascade_input/scale_40",sep="")
    tres <- resol
  } else {
    dataDir <- cascadeDir
    tres <- resol
  }
  
  odataDir <- paste(clmDir,"/cascade_",resol,sep="")
  if (!file.exists(odataDir)) {dir.create(odataDir)}
  
  #iterate variables
  for (vn in c("prec","tmean")) {
    #vn <- "prec"
    cat("...",vn,"\n")
    if (vn == "tmean") {
      stk_tn <- stack(paste(dataDir,"/GLAM_",tres,"_2006_tmin.nc",sep=""))
      stk_tx <- stack(paste(dataDir,"/GLAM_",tres,"_2006_tmax.nc",sep=""))
    } else {
      stk <- stack(paste(dataDir,"/GLAM_",tres,"_2006_",vn,".nc",sep=""))
    }
    
    mthList <- unique(dates$MONTH)
    for (m in mthList) {
      #m <- mthList[1]
      cat("...month",m,"\n")
      if (!file.exists(paste(odataDir,"/",vn,"_",m,".tif",sep=""))) {
        dayList <- dates$SEQ[which(dates$MONTH == m)]
        
        if (vn == "tmean") {
          outList_tn <- list()
          outList_tx <- list()
          outList_te <- list()
        } else {
          outList <- list()
        }
        
        icount <- 1
        for (i in dayList) {
          #i <- dayList[1]
          cat(icount,".",sep="")
          if (vn == "tmean") {
            drs_tn <- stk_tn[[i]]
            drs_tx <- stk_tx[[i]]
            drs_te <- (drs_tn + drs_tx) * 0.5
            
            drs_tn <- drs_tn - 273.15 #K to C
            drs_tx <- drs_tx - 273.15 #K to C
            drs_te <- drs_te - 273.15 #K to C
            
            outList_tn[[icount]] <- drs_tn * 10
            outList_tx[[icount]] <- drs_tx * 10
            outList_te[[icount]] <- drs_te * 10
          } else {
            drs <- stk[[i]]
            drs <- drs * 3600 * 24 #kg/m-2/s-1 to mm
            outList[[icount]] <- drs
          }
          icount <- icount+1
        }
        cat("\n")
        
        #calculate means and write rasters
        if (vn == "tmean") {
          out_rs <- stack(outList_tn)
          out_rs <- calc(out_rs,fun=function(x) {mean(x,na.rm=T)})
          out_rs <- writeRaster(out_rs,paste(odataDir,"/tmin_",m,".tif",sep=""),format="GTiff")
          rm(out_rs)
          
          out_rs <- stack(outList_tx)
          out_rs <- calc(out_rs,fun=function(x) {mean(x,na.rm=T)})
          out_rs <- writeRaster(out_rs,paste(odataDir,"/tmax_",m,".tif",sep=""),format="GTiff")
          rm(out_rs)
          
          out_rs <- stack(outList_te)
          out_rs <- calc(out_rs,fun=function(x) {mean(x,na.rm=T)})
          out_rs <- writeRaster(out_rs,paste(odataDir,"/tmean_",m,".tif",sep=""),format="GTiff")
          rm(out_rs)
        } else {
          out_rs <- stack(outList)
          out_rs <- calc(out_rs,fun=function(x) {sum(x,na.rm=T)})
          out_rs <- writeRaster(out_rs,paste(odataDir,"/prec_",m,".tif",sep=""),format="GTiff")
          rm(out_rs)
        }
      }
    }
  }
}


#b. complete the months that are necessary for running the model, just the land-sea
#   mask with all 0
for (resol in resList) {
  #resol <- resList[1]
  cat("processing:",resol,"\n")
  
  odataDir <- paste(clmDir,"/cascade_",resol,sep="")
  tres <- gsub("_exp","",resol)
  tmsk <- raster(paste(cascadeDir,"/cascade_input/Glam_",tres,"_lsm.nc",sep=""))
  
  for (vn in c("prec","tmin","tmax","tmean")) {
    #vn <- "prec"
    cat("...",vn,"\n")
    for (m in c(1:5,1:12)) {
      #m <- 1
      cat("...month",m,"\n")
      if (!file.exists(paste(odataDir,"/",vn,"_",m,".tif",sep=""))) {
        rsn <- raster(tmsk)
        rsn[] <- 0
        rsn <- writeRaster(rsn,paste(odataDir,"/",vn,"_",m,".tif",sep=""),format="GTiff")
        rm(rsn)
      }
    }
  }
}


#produce 3deg data by aggregation from (04km_exp, 12kmexp, 12km, and 40km)
msk_3d <- raster(paste(cascadeDir,"/cascade_input/Glam_3deg_lsm.nc",sep=""))
msk_3d[which(msk_3d[] > 0)] <- 1
msk_3d[which(msk_3d[] < 0)] <- NA

for (resol in resList) {
  #resol <- resList[1]
  cat("aggregating",resol,"\n")
  odataDir <- paste(clmDir,"/cascade_",resol,sep="")
  
  toutDir <- paste(clmDir,"/cascade_3deg-",resol,sep="")
  if (!file.exists(toutDir)) {dir.create(toutDir)}
  
  tmsk <- raster(paste(cascadeDir,"/cascade_input/Glam_",gsub("_exp","",resol),"_lsm.nc",sep=""))
  tmsk[which(tmsk[] > 0)] <- 1
  tmsk[which(tmsk[] < 0)] <- NA
  
  for (vn in c("prec","tmin","tmax","tmean")) {
    cat("...",vn,"\n")
    for (m in 1:12) {
      #m <- 6
      cat("...",m,"\n")
      if (!file.exists(paste(toutDir,"/",vn,"_",m,".tif",sep=""))) {
        rs <- raster(paste(odataDir,"/",vn,"_",m,".tif",sep=""))
        rs <- mask(rs, tmsk)
        fac <- xres(msk_3d) / xres(rs)
        rs <- aggregate(rs, fact=fac, fun=mean, na.rm=T)
        rs <- resample(rs, msk_3d, method="ngb")
        rs <- mask(rs, msk_3d)
        rs <- writeRaster(rs,paste(toutDir,"/",vn,"_",m,".tif",sep=""),format="GTiff")
      }
    }
  }
}



#2. run model with each of the cascade runs
for (resol in resList) {
  #resol <- resList[1]
  odataDir <- paste(clmDir,"/cascade_",resol,sep="")
  
  trunDir <- paste(runDir,"/",resol,sep="")
  if (!file.exists(trunDir)) {dir.create(trunDir)}
  
  #a. prepare start and end of season
  #option 1. take the start of season from GLAM simulations, the end of season 
  
  #option 2. fix the sowing and harvesting dates to 153 (if earlier) and 296 (if later)
  tpdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_plant_",gsub("_exp","",resol),".tif",sep=""))
  thdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_harvest_",gsub("_exp","",resol),".tif",sep=""))
  
  #correct sowing date
  tpdate2 <- tpdate
  tpdate2[which(tpdate[] < 153)] <- 153
  tpdate2[which(tpdate[] > 270)] <- NA
  
  #correct harvest date
  thdate2 <- thdate
  thdate2[which(thdate[] > 296)] <- 296
  thdate2[which(thdate[] < 150)] <- NA
  thdate2[which(thdate[] < 215)] <- 243
  
  #calculate crop duration
  calcdur <- function(x) {
    a <- x[1] #a is sowing date
    b <- x[2] #b is harvest value
    if (is.na(a) | is.na(b)) {
      z <- NA
    } else {
      if (a > b) {z <- NA} else {z <- b - a}
    }
    return(z)
  }
  
  tdur <- calc(stack(tpdate2,thdate2),fun=calcdur)
  
  #write the three datasets
  tcalDir <- paste(trunDir,"/calendar",sep="")
  if (!file.exists(tcalDir)) {dir.create(tcalDir)}
  
  tpdate2 <- writeRaster(tpdate2,paste(tcalDir,"/plant_",resol,".tif",sep=""),format="GTiff")
  thdate2 <- writeRaster(thdate2,paste(tcalDir,"/harvest_",resol,".tif",sep=""),format="GTiff")
  tdur <- writeRaster(tdur,paste(tcalDir,"/duration_",resol,".tif",sep=""),format="GTiff")
  
  #b. run model with specified parameters
  tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 6
  rmin <- 100; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 6
  
  trial <- 6
  outf <- paste(trunDir,"/run_",trial,sep="")
  
  #run the model
  eco <- suitCalc(climPath=odataDir, 
                  sowDat=tpdate2@file@name,
                  harDat=thdate2@file@name,
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


###
#perform the 3deg runs
trunDir <- paste(runDir,"/3deg",sep="")
if (!file.exists(trunDir)) {dir.create(trunDir)}

tcalDir <- paste(trunDir,"/calendar",sep="")
if (!file.exists(tcalDir)) {dir.create(tcalDir)}

tpdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_plant_3deg.tif",sep=""))
thdate <- raster(paste(bDir,"/calendar/Maize.crop.calendar/cascade_harvest_3deg.tif",sep=""))

#correct sowing date
tpdate2 <- tpdate
tpdate2[which(tpdate[] < 153)] <- 153
tpdate2[which(tpdate[] > 270)] <- NA

#correct harvest date
thdate2 <- thdate
thdate2[which(thdate[] > 296)] <- 296
thdate2[which(thdate[] < 150)] <- NA
thdate2[which(thdate[] < 215)] <- 243

#calculate duration
tdur <- calc(stack(tpdate2,thdate2),fun=calcdur)

#write three rasters
tpdate2 <- writeRaster(tpdate2,paste(tcalDir,"/plant_3deg.tif",sep=""),format="GTiff")
thdate2 <- writeRaster(thdate2,paste(tcalDir,"/harvest_3deg.tif",sep=""),format="GTiff")
tdur <- writeRaster(tdur,paste(tcalDir,"/duration_3deg.tif",sep=""),format="GTiff")

for (resol in resList) {
  #resol <- resList[1]
  odataDir <- paste(clmDir,"/cascade_3deg-",resol,sep="")
  
  trial <- 6
  outf <- paste(trunDir,"/",resol,"-run_",trial,sep="")
  
  #run the model
  eco <- suitCalc(climPath=odataDir, 
                  sowDat=tpdate2@file@name,
                  harDat=thdate2@file@name,
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


