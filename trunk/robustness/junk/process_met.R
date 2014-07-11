#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2014

stop("!")
#Resample ISIMIP meteorology to yield data meteorology (using 2nd order conservative remapping)

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)

#directory
wd <- "~/Leeds-work/quest-for-robustness"
srcDir <- paste(wd,"/scripts",sep="")
metDir <- paste(wd,"/data/meteorology",sep="")
yiDir <- paste(wd,"/data/yield_data_maize",sep="")
sowDir <- paste(wd,"/data/crop_calendar_sacks",sep="")

#source needed functions
source(paste(srcDir,"/make_wth.R",sep=""))

#read in yield data for getting mask
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))

#determine extent to cut the resampled netcdf
bbox <- extent(yrs)
if (bbox@xmin < 0)  bbox@xmin <- bbox@xmin+360
if (bbox@xmax < 0)  bbox@xmax <- bbox@xmax+360

#details of file
gcm <- "gfdl-esm2m"
per <- "hist"
years <- "1950"

for (vname in c("pr","rsds","tasmax","tasmin")) {
  #vname <- "pr"
  cat("...processing variable",vname,"\n")
  
  #name of file
  fname <- paste(vname,"_bced_1960_1999_",gcm,"_",per,"_",years,".nc",sep="")
  fnameout <- paste("afr_",vname,"_bced_1960_1999_",gcm,"_",per,"_",years,".nc",sep="")
  
  if (!file.exists(fnameout)) {
    #cdo for remapping onto a different grid
    #cdo remapcon2,r320x160 pr_bced_1960_1999_gfdl-esm2m_hist_1950.nc pr.nc #to change resolution
    setwd(metDir)
    system(paste("cdo remapcon2,r320x160 ",fname," pr_remapped.nc",sep=""))
    
    #optional with bilinear interpolation
    #cdo remapbil,r320x160 pr_bced_1960_1999_gfdl-esm2m_hist_1950.nc pr.nc #to change resolution
    #system(paste("cdo remapbil,r320x160 ",fname," pr_remapbil.nc",sep=""))
    
    #cut to Africa
    system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," pr_remapped.nc ",fnameout,sep=""))
    
    #garbage collection
    system(paste("rm -f pr_remapped.nc",sep=""))
  }
}


###
#for a particular location, extract daily weather using cdo
cellid <- 1792
xy <- xyFromCell(yrs, cellid)
lon <- xy[1]; lat <- xy[2]

#create temporary folder for daily met of location
tempDir <- "./extract_temp"
if (!file.exists(tempDir)) {dir.create(tempDir)}

if (!file.exists(paste(tempDir,"/meteo_cell-",cellid,".met",sep=""))) {
  #loop through files
  for (vname in c("pr","rsds","tasmax","tasmin")) {
    #vname <- "pr"
    odat <- paste(tempDir,"/",vname,"_cell-",cellid,".tab",sep="")
    fname <- paste("afr_",vname,"_bced_1960_1999_",gcm,"_",per,"_",years,".nc",sep="")
    
    #extract the data
    if (!file.exists(odat)) {
      system(paste("cdo -outputtab,year,month,day,lon,lat,value -remapnn,lon=",lon,"_lat=",lat," ",fname," > ",odat,sep=""))
    }
    
    #read in  the data
    idata <- read.table(odat,header=F)
    names(idata) <- c("year","month","day","lon","lat",vname)
    idata <- cbind(jday=1:nrow(idata),idata)
    idata$month <- NULL; idata$day <- NULL
    
    #cbind all variables
    if (vname == "pr") {
      metdata <- idata
    } else {
      metdata <- merge(metdata,idata,by=c("jday","year","lon","lat"))
    }
  }
  
  #sort met data
  metdata <- metdata[order(metdata$year, metdata$jday),]
  row.names(metdata) <- 1:nrow(metdata)
  
  #unit conversion for each variable
  metdata$pr <- metdata$pr * 3600 * 24 #kg m-2 s-1 to mm/day (GLAM takes mm and converts itself to cm)
  metdata$tasmax <- metdata$tasmax - 273.15 #K to C
  metdata$tasmin <- metdata$tasmin - 273.15 #K to C
  metdata$rsds <- metdata$rsds * 24 * 3600 / 1000000 #w/m2/s = J/m2/s / 1000000 * 86400 = MJ/m2/day
  
  #save file with all data
  write.table(metdata,file=paste(tempDir,"/meteo_cell-",cellid,".met",sep=""),col.names=T,row.names=F,sep="\t")
  
  #remove raw files
  system(paste("rm -f ",tempDir,"/*.tab",sep=""))
}

###
#create weather file for preliminary run
#sowing date
sdate <- raster(paste(sowDir,"/major_maize_plant.end.tif",sep=""))
sdate <- as.numeric(extract(sdate, xy))

xin <- data.frame(CELL=cellid,X=lon,Y=lat,SOW_DATE=sdate)
xout <- make_wth(x=xin,wthDir=paste(metDir,"/extract_temp",sep=""),years=1950,fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE"))



