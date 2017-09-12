#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014

stop("!")
#Make an .RData file with all data that is needed for maize GLAM runs. This is to avoid
#constant loading of individual raster data.

#A first file per cropping season with:
#1. grid cell list data frame
#2. planting and harvesting dates
#3. soil data

#note that the grid cells are defined as those having the 24 years of crop yield information

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); library(ncdf)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
metDir <- paste(wd,"/data/meteorology/wfd_data/Rainf_daily_WFD_GPCC",sep="")
outDir <- paste("~/Leeds-work/climate-for-breeding")
sowDir <- paste(outDir,"/data/crop_calendar_sacks",sep="")
solDir <- paste(outDir,"/data/soils_data",sep="")

#output directory
mdataDir <- paste(outDir,"/data/model_data",sep="")
if (!file.exists(mdataDir)) {dir.create(mdataDir)}

###########################################################################################
###########################################################################################
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

#get xy from cell
xy_main <- as.data.frame(xyFromCell(yrs, which(!is.na(yrs[]))))

#add gridcell numbers to xy data.frame
xy_main <- cbind(LOC=cellFromXY(yrs,xy_main[,c("x","y")]), xy_main)
xy_main <- cbind(ID=1:nrow(xy_main), xy_main)

#load crop calendar data
sow_i <- raster(paste(sowDir,"/major_maize_plant.start.tif",sep=""))
sow_f <- raster(paste(sowDir,"/major_maize_plant.end.tif",sep=""))
har_i <- raster(paste(sowDir,"/major_maize_harvest.start.tif",sep=""))
har_f <- raster(paste(sowDir,"/major_maize_harvest.end.tif",sep=""))

#extract crop calendar data
xy_main <- cbind(xy_main, SOW_DATE1=extract(sow_i, xy_main[,c("x","y")]), 
                 SOW_DATE2=extract(sow_f, xy_main[,c("x","y")]),
                 HAR_DATE1=extract(har_i, xy_main[,c("x","y")]),
                 HAR_DATE2=extract(har_f, xy_main[,c("x","y")]))

#change crop calendar to integers
xy_main[,c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")] <- round(xy_main[,c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")],digits=0)

#load soil data
rll <- raster(paste(solDir,"/rll_lr_shangguan2014.tif",sep=""))
dul <- raster(paste(solDir,"/dul_lr_shangguan2014.tif",sep=""))
sat <- raster(paste(solDir,"/sat_lr_shangguan2014.tif",sep=""))
asw <- raster(paste(solDir,"/asw_lr_shangguan2014.tif",sep=""))

#extract soil data
xy_main <- cbind(xy_main, RLL=extract(rll, xy_main[,c("x","y")])*0.01, DUL=extract(dul, xy_main[,c("x","y")])*0.01,
                 SAT=extract(sat, xy_main[,c("x","y")])*0.01, ASW=extract(asw, xy_main[,c("x","y")])*0.01)

#count and remove all with NA
xy_main$NAs <- apply(xy_main[,c("SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2","RLL","DUL","SAT","ASW")],1,
                     FUN=function(x) {length(which(is.na(x)))})
xy_main <- xy_main[which(xy_main$NAs == 0),c("x","y","SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2","RLL","DUL","SAT","ASW")]
row.names(xy_main) <- 1:nrow(xy_main)

#this is for corrections
sow_err <- which(xy_main$SOW_DATE1 > xy_main$SOW_DATE2 & xy_main$SOW_DATE1 > 200 & xy_main$SOW_DATE2 > 200)
a <- xy_main$SOW_DATE1[sow_err]; b <- xy_main$SOW_DATE2[sow_err]
xy_main$SOW_DATE1[sow_err] <- b; xy_main$SOW_DATE2[sow_err] <- a

har_err <- which(xy_main$HAR_DATE1 > xy_main$HAR_DATE2 & xy_main$HAR_DATE1 > 200 & xy_main$HAR_DATE2 > 200)
a <- xy_main$HAR_DATE1[har_err]; b <- xy_main$HAR_DATE2[har_err]
xy_main$SOW_DATE1[har_err] <- b; xy_main$SOW_DATE2[har_err] <- a

#correct dul value
asw_err <- which(xy_main$DUL < xy_main$RLL)
xy_other <- xy_main[-asw_err,]
for (i in asw_err) {
  #i <- asw_err[1]
  xy_i <- xy_main[i,c("x","y")]
  dis_i <- pointDistance(xy_i, xy_other[,c("x","y")],lonlat=T)
  dis_i <- dis_i[which(dis_i[] == min(dis_i))][1]
  nn_i <- as.numeric(names(dis_i))
  xy_main[i,"DUL"] <- xy_other[nn_i,"DUL"]
}

#update asw
xy_main$ASW <- xy_main$DUL - xy_main$RLL

#correct sat values
sat_err <- which(xy_main$SAT < xy_main$DUL)
xy_other <- xy_main[-sat_err,]
for (i in sat_err) {
  #i <- sat_err[1]
  xy_i <- xy_main[i,c("x","y")]
  dis_i <- pointDistance(xy_i, xy_other[,c("x","y")],lonlat=T)
  dis_i <- dis_i[which(dis_i[] == min(dis_i))][1]
  nn_i <- as.numeric(names(dis_i))
  xy_main[i,"SAT"] <- xy_main[i,"DUL"] + (xy_other[nn_i,"SAT"] - xy_other[nn_i,"DUL"])
}

#save this data frame in a RData file
save(list=c("xy_main"),file=paste(mdataDir,"/initial_conditions_major.RData",sep=""))


