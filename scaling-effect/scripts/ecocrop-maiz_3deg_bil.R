#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2013
stop("!")

###
#produce EcoCrop runs for maize in the Sahel for the scaling study
###

#load packages
library(rgdal); library(raster); library(maptools); data(wrld_simpl); library(ncdf)

#source functions
#src.dir <- "/mnt/a102/eejarv/scaling-effect"
#src.dir <- "/nfs/a102/eejarv/scaling-effect"
src.dir <- "~/Repositories/dapa-climate-change/trunk/scaling-effect"
src.dir2 <- "~/Repositories/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
source(paste(src.dir,"/scripts/EcoCrop-model.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")
figDir <- paste(bDir,"/figures_new",sep="")

if (!file.exists(runDir)) {dir.create(runDir)}
if (!file.exists(figDir)) {dir.create(figDir)}
crop_name <- "maiz"

#get mask from CASCADE output
msk <- raster(paste(lsmDir,"/Glam_12km_lsm.nc",sep=""))

#EcoCrop parameters
rmin <- 100; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 6
tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 6
trial <- 6

####
#### goes with 6 as the final parameterisation. Although 5 and 4 are not too different
####

#1. prepare cascade data (totals per month)
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
resol <- "3deg-12km_exp_bil"
dataDir <- paste(clmDir,"/cascade_",resol,sep="")
tres <- "3deg"

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
    if (!file.exists(paste(dataDir,"/",vn,"_",m,".tif",sep=""))) {
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
        out_rs <- writeRaster(out_rs,paste(dataDir,"/tmin_",m,".tif",sep=""),format="GTiff")
        rm(out_rs)
        
        out_rs <- stack(outList_tx)
        out_rs <- calc(out_rs,fun=function(x) {mean(x,na.rm=T)})
        out_rs <- writeRaster(out_rs,paste(dataDir,"/tmax_",m,".tif",sep=""),format="GTiff")
        rm(out_rs)
        
        out_rs <- stack(outList_te)
        out_rs <- calc(out_rs,fun=function(x) {mean(x,na.rm=T)})
        out_rs <- writeRaster(out_rs,paste(dataDir,"/tmean_",m,".tif",sep=""),format="GTiff")
        rm(out_rs)
      } else {
        out_rs <- stack(outList)
        out_rs <- calc(out_rs,fun=function(x) {sum(x,na.rm=T)})
        out_rs <- writeRaster(out_rs,paste(dataDir,"/prec_",m,".tif",sep=""),format="GTiff")
        rm(out_rs)
      }
    }
  }
}


tmsk <- raster(paste(dataDir,"/prec_6.tif",sep=""))
for (vn in c("prec","tmin","tmax","tmean")) {
  #vn <- "prec"
  cat("...",vn,"\n")
  for (m in c(1:5,1:12)) {
    #m <- 1
    cat("...month",m,"\n")
    if (!file.exists(paste(dataDir,"/",vn,"_",m,".tif",sep=""))) {
      rsn <- raster(tmsk)
      rsn[] <- 0
      rsn <- writeRaster(rsn,paste(dataDir,"/",vn,"_",m,".tif",sep=""),format="GTiff")
      rm(rsn)
    }
  }
}


#2. run model with 3deg bilinear data
dataDir <- paste(clmDir,"/cascade_",resol,sep="")

trunDir <- paste(runDir,"/3deg",sep="")
if (!file.exists(trunDir)) {dir.create(trunDir)}

#a. start and end of season
#sowing and harvest date
tcalDir <- paste(trunDir,"/calendar",sep="")
tpdate2 <- paste(tcalDir,"/plant_3deg.tif",sep="")
thdate2 <- paste(tcalDir,"/harvest_3deg.tif",sep="")

#b. run model with specified parameters
tkill <- 0; tmin <- 80; topmin <- 200; topmax <- 340; tmax <- 440 #trial 6
rmin <- 100; ropmin <- 600; ropmax <- 1500; rmax <- 3000 #trial 6

trial <- 6
outf <- paste(trunDir,"/12km_exp_bil-run_",trial,sep="")

#run the model
eco <- suitCalc(climPath=dataDir, 
                sowDat=tpdate2,
                harDat=thdate2,
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



