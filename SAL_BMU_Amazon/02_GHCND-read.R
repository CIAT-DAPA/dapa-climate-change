#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#December 2011
stop("error")

src.dir <- "Z:/DATA/WP2/00_scripts"
# src.dir <- dirname(sys.frame(1)$ofile)
var <- "prec"
source(paste(src.dir,"/GHCND-GSOD-functions-temp.R",sep=""))

#base dir
bDir <- "S:/observed/weather_station/ghcn/raw"; setwd(bDir)
ghcnDir <- paste(bDir,"/daily",sep="")
outDir <- "Z:/DATA/WP2/01_Weather_Stations/GHCN"

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
# rg.xt = extent(-79.5, -72, -11.9, 3)
rg.xt <- extent(-80, -66, -16, 5) #Study Region region
# waf.xt <- extent(-20,20,1,30)
# eaf.xt <- extent(25,50,-15,20)
# igp.xt <- extent(65,100,5,40)

#fitting extents
# afr.xt <- extent(-20,55,-40,30)
# sas.xt <- igp.xt

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
ghcn.rg <- stations.ghcn[which(stations.ghcn$LON>=(rg.xt@xmin-3) & stations.ghcn$LON<=(rg.xt@xmax+3)
                               & stations.ghcn$LAT>=(rg.xt@ymin-3) & stations.ghcn$LAT<=(rg.xt@ymax+3)),]

#create matrix of dates*stations for a given year to then use apply to get the data
#define initial and final year
yearSeries <- c(1960:2010)

########################### FOR GHCN-D #######################################
####################################### ######################################
ghcn.dates <- as.data.frame(matrix(ncol=367,nrow=nrow(ghcn.rg))) #matrix
colnames(ghcn.dates) <- c("ID",1:366) #names of columns
ghcn.dates$ID <- ghcn.rg$ID #stations IDs into dates matrix
ddir <- paste(ghcnDir,"/ghcnd_all",sep="")
odir <- paste(outDir,"/ghcn.rg.", var, sep=""); if (!file.exists(odir)) {dir.create(odir)}

#do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=10) #initiate cluster

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
  outDir <- paste(odir,"/",id,sep="")
  if (file.exists(outDir)) { #check if files exist in folder
    fl <- list.files(outDir,pattern=".csv")
  } else {
    fl <- 0
  }
  
  if (length(fl) != 51) {
    controlConvert <- function(i) { #define a new function
      convertGHCND(id,i,ddir,odir)
    }
    sfExport("id")
    system.time(sfSapply(as.vector(yearSeries), controlConvert))
  }
  count <- count+1
}

sfStop()
#ghcn.dates[1,2:(nday+1)] <- wData$PRCP #to put data into complete matrix for that year


# #  Merge all years in one single file
# stations <- list.dirs(odir, full.names = F)
# stations <- stations[stations != ""]
# 
# dates <- seq(as.Date("1960/1/1"), as.Date("2010/12/31"), "days")
# dates = format(dates,"%Y%m%d")
# dates = cbind.data.frame("Date"=dates,"NA")
# 
# mat <- as.data.frame(matrix(NA,366,length(stations)))
# 
# for (s in 1:length(stations)){
#   
#   years <- list.files(paste0(odir, "/", stations[s]), full.names = F)
#   data <- lapply(paste0(odir, "/", stations[s], "/", years), function(x){read.csv(x, header=T)})
#   
#   allyears <- c()
#   
#   for (j in 1:length(data)){
#     
#     allyears <- rbind(allyears, cbind(paste0(strsplit(years[j],".csv")[1], sprintf("%02d", data[[j]]$MONTH), sprintf("%02d", data[[j]]$DOFM)), data[[j]][,4]))
#     
#   }
#   
#   names(allyears) <- c("Date", stations[s])
#   datSt <- merge(dates,allyears[[j]],by="Date",all.x=T)
#   datosprecip[,j]=final[,3]
#   mat[,j] = allyears
#   
# }




