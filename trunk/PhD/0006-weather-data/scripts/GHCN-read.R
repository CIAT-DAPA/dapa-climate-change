#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#December 2011
stop("error")

bDir <- "D:/CIAT_work/crop-modelling/climate-data"
source("D:/CIAT_work/crop-modelling/GHCND-GSOD-functions.R")

setwd(bDir)
ghcnDir <- paste(bDir,"/ghcn-daily",sep="")

#read stations locations
stations.ghcn <- read.fortran(paste(ghcnDir,"/ghcnd-stations.txt",sep=""),
						format=c("A11","F9","F10","F7","1X","A2","1X","A31","A3","1X","A3","I6"))
names(stations.ghcn) <- c("ID","LAT","LON","ALT","STATE","NAME","GSN_FLAG","HCN_FLAG","WMO_ID")
#plot(stations$LON,stations$LAT,pch=20,cex=0.7)

#1. create extents (only for interpolation, but use only two extents for fitting: AFRICA & IGP)
#2. define working gridcell size (probably 1 degree)
#3. make inventory of data (count number of data points per day per fitting region)
#4. interpolate to target cellsize all daily data between 1960-2011

#1. create extents
require(raster); require(maptools); require(rgdal)

#projection extents
waf.xt <- extent(-20,20,1,30)
eaf.xt <- extent(25,50,-15,20)
igp.xt <- extent(65,100,5,40)

#fitting extents
afr.xt <- extent(-20,55,-40,30)
sas.xt <- igp.xt

#plot the extents (for reference -commented!)
#rs <- raster(); rs[] <- rnorm(1:ncell(rs))
#data(wrld_simpl)
#plot(rs,col=colorRampPalette(c("grey10","grey90"))(100)); plot(wrld_simpl,add=T,col='white')
#plot(waf.xt,add=T,col='red'); plot(eaf.xt,add=T,col='blue'); plot(igp.xt,add=T,col='orange')
#plot(afr.xt,add=T,col='black',lty=1); plot(sas.xt,add=T,col='black',lty=2)

#2. define working gridcell
cellSize <- 1

#3. Make inventory of data (points / day / fit region) [transform all data to suitable format]
#select stations within 3+degree of interpolation extents
ghcn.afr <- stations.ghcn[which(stations.ghcn$LON>=(afr.xt@xmin-3) & stations.ghcn$LON<=(afr.xt@xmax+3)
                          & stations.ghcn$LAT>=(afr.xt@ymin-3) & stations.ghcn$LAT<=(afr.xt@ymax+3)),]
ghcn.sas <- stations.ghcn[which(stations.ghcn$LON>=(sas.xt@xmin-3) & stations.ghcn$LON<=(sas.xt@xmax+3)
                          & stations.ghcn$LAT>=(sas.xt@ymin-3) & stations.ghcn$LAT<=(sas.xt@ymax+3)),]

#create matrix of dates*stations for a given year to then use apply to get the data
#define initial and final year
yearSeries <- c(1960:2010)

########################### FOR GHCN-D #######################################
####################################### ######################################
ghcn.dates <- as.data.frame(matrix(ncol=367,nrow=nrow(ghcn.afr))) #matrix
colnames(ghcn.dates) <- c("ID",1:366) #names of columns
ghcn.dates$ID <- ghcn.afr$ID #stations IDs into dates matrix
ddir <- paste(ghcnDir,"/ghcnd_all",sep="")
odir <- paste(ghcnDir,"/ghcnd_afr",sep=""); if (!file.exists(odir)) {dir.create(odir)}

#do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=5) #initiate cluster

#export functions
sfExport("convertGHCND")
sfExport("createBaseMat")
sfExport("leap")
sfExport("getDataGHCN")
sfExport("searchData")

#export variables
sfExport("ddir")
sfExport("odir")
sfExport("bDir")

count <- 1
for (id in ghcn.dates$ID) {
  cat(id,paste("(",count," out of ",length(ghcn.dates$ID),")",sep=""),"\n")
  controlConvert <- function(i) { #define a new function
    convertGHCND(id,i,ddir,odir)
  }
  sfExport("id")
  system.time(sfSapply(as.vector(yearSeries), controlConvert))
  count <- count+1
}


#ghcn.dates[1,2:(nday+1)] <- wData$PRCP #to put data into complete matrix for that year
