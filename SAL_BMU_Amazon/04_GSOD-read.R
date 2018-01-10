#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#December 2011
#Modified by Carlos Navarro 
# April 2016


#########################
## 01- Read GSOD files ##
#########################

stop("error")

src.dir <- "Z:/DATA/WP2/00_scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

#base dir
bDir <- "S:/observed/weather_station/gsod"; setwd(bDir)
gsodDir <- paste(bDir,"/organized-data",sep="")
# odir <- "D:/CIAT/Projects/col-cormacarena"
odir <- "Z:/DATA/WP2/01_Weather_Stations/GSOD"
reg <- "amz"
  
#gsod stations
stations.gsod <- read.csv(paste(gsodDir,"/ish-history.csv",sep=""))
stations.gsod$LON <- stations.gsod$LON/1000; stations.gsod$LAT <- stations.gsod$LAT/1000
stations.gsod$ELEV..1M. <- stations.gsod$ELEV..1M./10

#1. create extents
require(raster); require(maptools); require(rgdal)

#projection extents
# reg.xt <- extent(-90,-30,-40,24) #Lat
# reg.xt <- extent(-79.5, -72, -11.9, 3) #Napo region
reg.xt <- extent(-80, -66, -16, 5) #Study Region region

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
yearSeries <- c(1960:2009)

#select stations within 3+degree of interpolation extents
gsod.reg <- stations.gsod[which(stations.gsod$LON>=(reg.xt@xmin-3) & stations.gsod$LON<=(reg.xt@xmax+3)
                          & stations.gsod$LAT>=(reg.xt@ymin-3) & stations.gsod$LAT<=(reg.xt@ymax+3)),]
st_ids <- paste(gsod.reg$USAF,"-",gsod.reg$WBAN,sep="")
usaf_ids <- gsod.reg$USAF
st_loc <- as.data.frame(cbind("Station"=gsod.reg$USAF, "Name"=gsod.reg$STATION.NAME, "Lon"=gsod.reg$LON, "Lat"=gsod.reg$LAT, "Alt"=gsod.reg$ELEV..1M.))
write.csv(st_loc, paste0(odir, "/stations_names.csv"), row.names=F)
# gsod.sas <- stations.gsod[which(stations.gsod$LON>=(sas.xt@xmin-3) & stations.gsod$LON<=(sas.xt@xmax+3)
#                           & stations.gsod$LAT>=(sas.xt@ymin-3) & stations.gsod$LAT<=(sas.xt@ymax+3)),]

#clean gsod stations of Africa
# gsod.reg <- gsod.reg[-which(gsod.reg$LON == 0 | gsod.reg$LAT == 0),]

#do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=32) #initiate cluster

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
  ogdir <- paste(odir,"/daily", sep=""); if (!file.exists(ogdir)) {dir.create(ogdir, recursive=T)}
  controlConvert <- function(i) { #define a new function
    convertGSOD(i,yr,gdir,ogdir)
  }
  sfExport("yr"); sfExport("gdir"); sfExport("ogdir")
  system.time(sfSapply(as.vector(IDs), controlConvert))
  count <- count+1
}


# Join all year in one file per station
mergeDailyGSOD(odir, ogdir, st_ids, usaf_ids)

# Monthly aggregation
varList <- c("prec", "tmax", "tmin")
for (var in varList){
  monthly_agg(var, odir, odir) 
}


## Add coordinates to the climatologies files
varList <- c("prec", "tmin", "tmax")
st_loc <- paste0(odir, "/stations_names.csv")
sY=1976
fY=2005

for (var in varList){
  clim_calc(var, odir, odir, st_loc, sY, fY)
}

