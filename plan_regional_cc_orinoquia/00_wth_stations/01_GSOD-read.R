#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#December 2011
#Modified by Carlos Navarro 
# February 2016
stop("error")

src.dir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_scripts"
source(paste(src.dir,"/01a_GHCND-GSOD-functions.R",sep=""))

#base dir
bDir <- "S:/observed/weather_station/gsod"; setwd(bDir)
gsodDir <- paste(bDir,"/organized-data",sep="")
# odir <- "D:/CIAT/Projects/col-cormacarena"
odir <- "D:/cenavarro/col-cormacarena"
reg <- "lat"
  
#gsod stations
stations.gsod <- read.csv(paste(gsodDir,"/ish-history.csv",sep=""))
stations.gsod$LON <- stations.gsod$LON/1000; stations.gsod$LAT <- stations.gsod$LAT/1000
stations.gsod$ELEV..1M. <- stations.gsod$ELEV..1M./10

#1. create extents
require(raster); require(maptools); require(rgdal)

#projection extents
reg.xt <- extent(-90,-30,-40,24)


#plot the extents (for reference -commented!)
#rs <- raster(); rs[] <- rnorm(1:ncell(rs))
#data(wrld_simpl)
#plot(rs,col=colorRampPalette(c("grey10","grey90"))(100)); plot(wrld_simpl,add=T,col='white')
#plot(waf.xt,add=T,col='red'); plot(eaf.xt,add=T,col='blue'); plot(igp.xt,add=T,col='orange')
#plot(afr.xt,add=T,col='black',lty=1); plot(sas.xt,add=T,col='black',lty=2)

#2. define working gridcell
# cellSize <- 1

#3. Make inventory of data (points / day / fit region)
#define initial and final year
yearSeries <- c(1980:2009)

#select stations within 3+degree of interpolation extents
gsod.reg <- stations.gsod[which(stations.gsod$LON>=(reg.xt@xmin-3) & stations.gsod$LON<=(reg.xt@xmax+3)
                          & stations.gsod$LAT>=(reg.xt@ymin-3) & stations.gsod$LAT<=(reg.xt@ymax+3)),]
# gsod.sas <- stations.gsod[which(stations.gsod$LON>=(sas.xt@xmin-3) & stations.gsod$LON<=(sas.xt@xmax+3)
#                           & stations.gsod$LAT>=(sas.xt@ymin-3) & stations.gsod$LAT<=(sas.xt@ymax+3)),]

#clean gsod stations of Africa
gsod.reg <- gsod.reg[-which(gsod.reg$LON == 0 | gsod.reg$LAT == 0),]



#do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=20) #initiate cluster

#export functions
sfExport("convertGSOD")
sfExport("createDateGrid")
sfExport("leap")

#export variables
sfExport("bDir")

IDs <- paste("USAF",gsod.reg$USAF,"_WBAN",gsod.reg$WBAN,sep="")

count <- 1
for (yr in yearSeries) {
  cat(yr,paste("(",count," out of ",length(yearSeries),")",sep=""),"\n")
  gdir <- paste(gsodDir,"/",yr,sep="")
  ogdir <- paste(odir,"/gsod-stations-",reg, "/daily", sep=""); if (!file.exists(ogdir)) {dir.create(ogdir)}
  controlConvert <- function(i) { #define a new function
    convertGSOD(i,yr,gdir,ogdir)
  }
  sfExport("yr"); sfExport("gdir"); sfExport("ogdir")
  system.time(sfSapply(as.vector(IDs), controlConvert))
  count <- count+1
}




##Join all year in one file per station
ogdir <- paste(odir,"/gsod-stations-",reg, "/daily", sep="")
st_ids <- paste(gsod.reg$USAF,"-",gsod.reg$WBAN,sep="")
ogdir_mth <- paste(odir,"/gsod-stations-",reg, "/monthly", sep=""); if (!file.exists(ogdir_mth)) {dir.create(ogdir_mth)}
ogdir_30yravg <- paste(odir,"/gsod-stations-",reg, "/30yr_averages", sep=""); if (!file.exists(ogdir_30yravg)) {dir.create(ogdir_30yravg)}

getMthDataGSOD(ogdir, st_ids, ogdir_mth, ogdir_30yr_avg)


## Add coordinates to the climatologies files
varList <- c("prec", "tmin", "tmax")
st_loc <- as.data.frame(cbind("id"=paste0(gsod.reg$USAF, "-", gsod.reg$WBAN),"Lon"=gsod.reg$LON, "Lat"=gsod.reg$LAT, "Alt"=gsod.reg$ELEV..1M.))
for (var in varList){
  
  clim <- read.csv(paste0(ogdir_30yravg, "/gsod_30yravg_", var, ".csv"), header=T)  
  clim <- na.omit(merge(st_loc, clim, by = "id", all = TRUE))
  write.csv(clim, paste0(ogdir_30yravg, "/gsod_30yravg_", var, ".csv"), row.names=F)
}



