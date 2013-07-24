#JRV 2013
#CIAT / CCAFS
stop("!")

#source directory
src.dir <- "~/Repositories/dapa-climate-change/trunk/dnp-sdm/scripts"
source(paste(src.dir,"/calc_future_climate-fun.R",sep=""))

#working directory
#wd <- "/mnt/a102/eejarv/DNP-biodiversity"
wd <- "/nfs/a102/eejarv/DNP-biodiversity"
setwd(wd)


########################################################################
########################################################################
#1. calculate topo 
#load mask (from bioclimatic)
bioDir <- "./env-data/proj_COL/baseline/Global_2_5min/worldclim/1950_2000/_tiffs"
msk <- raster(paste(bioDir,"/bio_1.tif",sep=""))

#load elevation dataset
topDir <- "./env-data/topography"
otopDir <- "./env-data/proj_COL/topography"
if (!file.exists(otopDir)) {dir.create(otopDir)}

#crop the elevation dataset to the desired mask
if (!file.exists(paste(otopDir,"/alt.tif",sep=""))) {
  alt <- raster(paste(topDir,"/alt.tif",sep=""))
  alt_c <- crop(alt,msk)
  alt_c <- aggregate(alt_c,fact=(xres(msk)/xres(alt_c)),fun=mean,na.rm=T)
  xy <- as.data.frame(xyFromCell(alt_c,1:ncell(alt_c)))
  xy <- cbind(CELL=1:ncell(alt_c),xy)
  xy$ALT <- extract(alt_c,xy[,c("x","y")])
  xy$MSK <- extract(msk,xy[,c("x","y")])
  alt_c[xy$CELL[which(is.na(xy$MSK))]] <- NA
  alt_c <- writeRaster(alt_c,paste(otopDir,"/alt.tif",sep=""),format="GTiff")
} else {
  alt_c <- raster(paste(otopDir,"/alt.tif",sep=""))
}

#use Horn (1981) (neighbors = 8) algorithm (mor suitable for smoother surfaces)
if (!file.exists(paste(otopDir,"/slope.tif",sep=""))) {
  slp <- terrain(alt_c,opt='slope',unit='degrees',neighbors=8,
                 filename=paste(otopDir,"/slope.tif",sep=""),format="GTiff")
}

if (!file.exists(paste(otopDir,"/aspect.tif",sep=""))) {
  asp <- terrain(alt_c,opt='aspect',unit='degrees',neighbors=8,
                 filename=paste(otopDir,"/aspect.tif",sep=""),format="GTiff")
}


########################################################################
########################################################################
#2. calculate soil stuff for Colombia
solDir <- "./env-data/soil"
osolDir <- "./env-data/proj_COL/soil"
if (!file.exists(osolDir)) {dir.create(osolDir)}

if (!file.exists(paste(osolDir,"/soildrain_final.tif",sep=""))) {
  soildrain <- raster(paste(solDir,"/soildrain_final.tif",sep=""))
  soildrain_c <- crop(soildrain,msk)
  soildrain_c <- resample(soildrain_c,msk,method="ngb")
  xy <- as.data.frame(xyFromCell(soildrain_c,1:ncell(soildrain_c)))
  xy <- cbind(CELL=1:ncell(soildrain_c),xy)
  xy$SOILDRAIN <- extract(soildrain_c,xy[,c("x","y")])
  xy$MSK <- extract(msk,xy[,c("x","y")])
  soildrain_c[xy$CELL[which(is.na(xy$MSK))]] <- NA
  soildrain_c <- writeRaster(soildrain_c,paste(osolDir,"/soildrain_final.tif",sep=""),format="GTiff")
}



########################################################################
########################################################################
#3. calculate climate-related stuff

#list of scenarios to calculate
sceList <- c("baseline","sres_a1b","sres_a2","sres_b1")
mList <- data.frame()
for (i in 1:length(sceList)) {
  m1 <- data.frame(SCE=sceList[i],MODEL=list.files(paste("./env-data/proj_COL/",sceList[i],"/Global_2_5min",sep="")))
  mList <- rbind(mList,m1)
}

#list of time frames
tfList <- c("2020_2049","2040_2069","2070_2099")

#select a given scenario
for (i in 1:nrow(mList)) {
  #i <- 1
  tsce <- paste(mList$SCE[i])
  tmod <- paste(mList$MODEL[i])
  if (tsce == "baseline") {tframe <- "1950_2000"} else {tframe <- tfList}
  
  for (ttf in tframe) {
    #ttf <- tframe[1]
    cat("\n...processing",tsce,"/",tmod,"/",ttf,"\n")
    
    #set directories
    bioDir <- paste("./env-data/proj_COL/",tsce,"/Global_2_5min/",tmod,"/",ttf,"/_tiffs",sep="")
    mthDir <- bioDir
    
    #load prec_stk if either of the files doesn't exist
    if (!file.exists(paste(bioDir,"/drymonths.tif",sep="")) | !file.exists(paste(bioDir,"/sind.tif",sep=""))) {
      cat("...load monthly precip data\n")
      prec_stk <- stack(paste(mthDir,"/prec_",1:12,".tif",sep=""))
    }
    
    #calculate calculate dry months
    thresh <- 100
    if (!file.exists(paste(bioDir,"/drymonths.tif",sep=""))) {
      cat("...calculate dry months\n")
      cmths_rs <- calc(prec_stk,calc_drym,filename=paste(bioDir,"/drymonths.tif",sep=""),format='GTiff')
    }
    
    #calculate seasonality index
    if (!file.exists(paste(bioDir,"/sind.tif",sep=""))) {
      cat("...calculate Feng's seasonality index\n")
      rmean <- raster(paste(bioDir,"/bio_12.tif",sep=""))
      r_max <- max(rmean[],na.rm=T) #get normalising value from rmean raster
      sind_rs <- calc(prec_stk,calcS,filename=paste(bioDir,"/sind.tif",sep=""),format='GTiff')
    }
  }
}





