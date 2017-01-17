#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#read in the Ben's soil data and convert to WFD's grid of 0.5 x 0.5

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); library(ncdf)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
soDir <- paste(wd,"/data/soils",sep="")
sdataDir <- "~/Leeds-work/datasets/soils"
metDir <- paste(wd,"/data/meteorology/wfd_data/Rainf_daily_WFD_GPCC",sep="")

outDir <- paste("~/Leeds-work/climate-for-breeding/data/soils_data")

#read in WFD data for getting mask
mrs <- paste(metDir,"/Rainf_daily_WFD_GPCC_195001.nc",sep="")
nc <- open.ncdf(mrs)
ncdata <- get.var.ncdf(nc,nc$var[["Rainf"]]) #get data from nc connection
brs <- raster(nrow=360,ncol=720,xmn=-180,xmx=180,ymn=-90,ymx=90) #base raster
nt <- ncol(ncdata)
yrs <- brs
yrs[as.numeric(nc$var[["Rainf"]]$dim[[1]]$vals)] <- ncdata[,1]
yrs[which(!is.na(yrs[]))] <- 1
nc <- close.ncdf(nc)

#create data.frame with coordinates of big grid cell (all)
xy <- as.data.frame(xyFromCell(yrs,1:ncell(yrs)))
xy$ID <- 1:nrow(xy)

#load sowing date and remove grid cells that we're not interested in anyway
#for computational efficiency
sd_rs <- raster(paste("~/Leeds-work/climate-for-breeding/data/crop_calendar_sacks/major_maize_harvest.start.tif",sep=""))
xy$sowdate <- extract(sd_rs, xy[,c("x","y")])
xy <- xy[which(!is.na(xy$sowdate)),]
xy$sowdate <- NULL
row.names(xy) <- 1:nrow(xy)

#function to average data within the grid cell
agg_soil <- function(x,target_res,in_rs) {
  #x <- as.numeric(xy[900,])
  lon <- x[1]; lat <- x[2]; id <- x[3]
  cat("...loc ",id," out of ",29324," (",round((id/29324*100),2)," %)\n",sep="")
  
  rs_cut <- crop(in_rs, extent((lon-target_res*.5),(lon+target_res*.5),(lat-target_res*.5),(lat+target_res*.5)))
  ret_val <- mean(rs_cut[],na.rm=T)
  return(ret_val)
}

####
#load and calculate for Shangguan et al. (2014) soil data
sshanDir <- paste(sdataDir,"/shangguan2014",sep="")

#list files
flist <- list.files(sshanDir, pattern="\\.zip")
flist <- flist[grep("VMC",flist)]

#process all files
for (fil in flist) {
  #fil <- flist[1]
  cat("...processing file",fil,"\n")
  
  #first uncompress file as needed
  ncfil <- gsub("\\.zip","\\.nc",fil)
  if (!file.exists(paste(sshanDir,"/",ncfil,sep=""))) {
    setwd(sshanDir)
    system(paste("7z x ",fil,sep=""))
    setwd("~")
  }
  
  filname <- gsub("\\.zip","",fil)
  
  #loop levels (soil depth)
  for (i in 1:4) {
    #i <- 1
    cat("...processing layer",i,"\n")
    if (!file.exists(paste(outDir,"/",filname,"_",i,".tif",sep=""))) {
      #read in raster
      rs <- stack(paste(sshanDir,"/",ncfil,sep=""))
      rs <- rs[[i]]
      
      #crop to appropriate extent
      rs <- crop(rs, yrs)
      
      #resample to 0.5
      ag_val <- apply(xy, 1, agg_soil, xres(yrs), rs)
      rs <- yrs; rs[] <- NA; rs[xy$ID] <- ag_val
      #plot(rs, zlim=c(0,60))
      
      #write raster file
      rs <- writeRaster(rs,paste(outDir,"/",filname,"_",i,".tif",sep=""),format="GTiff")
    }
  }
  
  if (file.exists(paste(sshanDir,"/",ncfil,sep=""))) {
    system(paste("rm -f ",sshanDir,"/",ncfil,sep=""))
  }
}

###
#load the four layers for each of the limits and calculate mean
fnames <- c("sat","dul","rll")
for (i in 1:3) {
  #i <- 1
  cat("...processing",fnames[i],"\n")
  if (!file.exists(paste(outDir,"/",fnames[i],"_lr_shangguan2014.tif",sep=""))) {
    flist <- list.files(outDir,pattern=paste("VMC",i,sep=""))
    rstk <- stack(paste(outDir,"/",flist,sep=""))
    rs <- calc(rstk[[1:6]], fun=function(x) {mean(x,na.rm=T)})
    rs <- writeRaster(rs, paste(outDir,"/",fnames[i],"_lr_shangguan2014.tif",sep=""),format="GTiff")
    rm(rs)
  }
}

#available soil water
asw_rs <- raster(paste(outDir,"/dul_lr_shangguan2014.tif",sep="")) - raster(paste(outDir,"/rll_lr_shangguan2014.tif",sep=""))
asw_rs <- writeRaster(asw_rs, paste(outDir, "/asw_lr_shangguan2014.tif",sep=""), format="GTiff")


