#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#CMIP5 skill analyses
#3. organise CIAT and GHCN monthly time series

library(raster)

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"

source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

yi <- 1961
yf <- 2000


#################################################################################
#################################################################################
#a. organise all input data into NFS (ghcn-ts, wcl-wst, cru-is, wcl-is)
#this bit needs to be done local, since involves copying of files to the nfs
inLocal <- "F:/PhD-work/climate-data-assessment/comparisons/input-data"
ouNFS <- paste(mdDir,"/assessment",sep="")
ouInData <- paste(ouNFS,"/input-data",sep="")
if (!file.exists(ouInData)) {dir.create(ouInData,recursive=T)}

#first copy, compress, tiff the whole folder of worldclim
if (!file.exists(paste(ouInData,"/wcl-data/_ready.ctr",sep=""))) {
  x <- file.copy(from=paste(inLocal,"/wcl-data",sep=""),to=ouInData,recursive=T) #copy whole folder
  x <- AsctoGTiff(paste(ouInData,"/wcl-data",sep="")) #ascii to tif and gzip ascii files
  cf <- file(paste(ouInData,"/wcl-data/_ready.ctr",sep=""),"w") #write control file
  cat("worldclim data were copied, tiff converted and gzipped on",date(),"by",
      paste(as.data.frame(t(Sys.info()))$login),"@",
      paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cf)
  close(cf)
}

#now copy the whole folder of cru
if (!file.exists(paste(ouInData,"/cru-data/_ready.ctr",sep=""))) {
  x <- file.copy(from=paste(inLocal,"/cru-data",sep=""),to=ouInData,recursive=T)
  
  #remove the big .dat files and r script that is there
  setwd(paste(ouInData,"/cru-data",sep=""))
  x <- sapply(list.files(".",pattern="\\.dat"),FUN= function(x) {s <- file.remove(x)})
  x <- file.remove("createCRUAAIGrid.R")
  
  #ascii to GeoTiff
  x <- AsctoGTiff(paste(ouInData,"/cru-data",sep=""))
  cf <- file(paste(ouInData,"/cru-data/_ready.ctr",sep=""),"w")
  cat("cru data were copied, tiff converted and gzipped on",date(),"by",
      paste(as.data.frame(t(Sys.info()))$login),"@",
      paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cf)
  close(cf)
}

#now copy worldclim weather stations
if (!file.exists(paste(ouInData,"/wcl-weather-stations/_ready.ctr",sep=""))) {
  x <- file.copy(from=paste(inLocal,"/wcl-weather-stations",sep=""),to=ouInData,recursive=T)
  cf <- file(paste(ouInData,"/wcl-weather-stations/_ready.ctr",sep=""),"w")
  cat("worldclim weather stations data were copied on",date(),"by",
      paste(as.data.frame(t(Sys.info()))$login),"@",
      paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cf)
  close(cf)
}

#now copy the GHCN data
if (!file.exists(paste(ouInData,"/ghcn-weather-stations/_ready.ctr",sep=""))) {
  if (!file.exists(paste(ouInData,"/ghcn-weather-stations/organized-data",sep=""))) {
    dir.create(paste(ouInData,"/ghcn-weather-stations/organized-data",sep=""),recursive=T)
  }
  
  #copy necessary files
  x <- file.copy(paste(inLocal,"/ghcn-weather-stations/organized-data/ghcn_rain_data_all_xy.csv",sep=""),
                 paste(ouInData,"/ghcn-weather-stations/organized-data",sep=""))
  x <- file.copy(paste(inLocal,"/ghcn-weather-stations/organized-data/ghcn_tmean_data_all_xy.csv",sep=""),
                 paste(ouInData,"/ghcn-weather-stations/organized-data",sep=""))
  x <- file.copy(paste(inLocal,"/ghcn-weather-stations/organized-data/ghcn_tmax_data_all_xy.csv",sep=""),
                 paste(ouInData,"/ghcn-weather-stations/organized-data",sep=""))
  x <- file.copy(paste(inLocal,"/ghcn-weather-stations/organized-data/ghcn_tmin_data_all_xy.csv",sep=""),
                 paste(ouInData,"/ghcn-weather-stations/organized-data",sep=""))
  
  #write control file
  cf <- file(paste(ouInData,"/ghcn-weather-stations/_ready.ctr",sep=""),"w")
  cat("ghcn weather stations data were copied on",date(),"by",
      paste(as.data.frame(t(Sys.info()))$login),"@",
      paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cf)
  close(cf)
}



#################################################################################
#################################################################################
#b. get the monthly series of GHCN between 1961 and 2000. Also calculate dtr
ghData <- paste(ouInData,"/ghcn-weather-stations/organized-data",sep="")

#calculate dtr using tmax and tmin
if (!file.exists(paste(ghData,"/ghcn_",vn,"_data_all_xy.csv",sep=""))) {
  tmax <- read.csv(paste(ghData,"/ghcn_tmax_data_all_xy.csv",sep=""))
  tmin <- read.csv(paste(ghData,"/ghcn_tmin_data_all_xy.csv",sep=""))
  
  names(tmax)[7:ncol(tmax)] <- paste("TMAX.",1:12,sep="")
  names(tmin)[7:ncol(tmax)] <- paste("TMIN.",1:12,sep="")
  
  dtr <- merge(tmax,tmin,by=c("ID","YEAR"),all.x=F,all.y=F,sort=F)
  dtr$REPLICATED.y <- NULL; dtr$LAT.y <- NULL; dtr$LONG.y <- NULL; dtr$ALT.y <- NULL
  dtr_idata <- dtr[,7:ncol(dtr)]
  dtr_odata <- t(apply(dtr_idata,1,calc_dtr))
  dtr_base <- dtr[,1:6]
  dtr_out <- cbind(dtr_base,dtr_odata)
  names(dtr_out) <- names(read.csv(paste(ghData,"/ghcn_tmax_data_all_xy.csv",sep="")))
  write.csv(dtr_out,paste(ghData,"/ghcn_dtr_data_all_xy.csv",sep=""),row.names=F,quote=T)
}


#subselect years for all variables
for (vn in c("rain","tmean","tmax","tmin","dtr")) {
  #vn <- "rain"
  cat("variable",vn,"\n")
  if (!file.exists(paste(ghData,"/ghcn_",vn,"_data_ts_",yi,"-",yf,"_xy.csv",sep=""))) {
    ts <- read.csv(paste(ghData,"/ghcn_",vn,"_data_all_xy.csv",sep=""))
    ts_sel <- ts[which(ts$YEAR >= yi & ts$YEAR < yf),]
    write.csv(ts_sel,paste(ghData,"/ghcn_",vn,"_data_ts_",yi,"-",yf,"_xy.csv",sep=""),quote=T,row.names=F)
    rm(ts); rm(ts_sel); g=gc(); rm(g)
  }
}


#c. get the CIAT monthly series between 1961 and 2000. Only rainfall
#first need to copy the CIAT time series

#i/o folders *run this bit locally, as eljefe is not connected to the data
wd <- "F:/PhD-work/crop-modelling/climate-data/CIAT-daily"
if (!file.exists(paste(ouInData,"/ciat-weather-stations",sep=""))) {
  dir.create(paste(ouInData,"/ciat-weather-stations",sep=""))
}

if (!file.exists(paste(ouInData,"/ciat-weather-stations/_ready.ctr",sep=""))) {
  x <- file.copy(paste(wd,"/stations_details",sep=""),paste(ouInData,"/ciat-weather-stations",sep=""),recursive=T)
  x <- file.copy(paste(wd,"/rnf_organised",sep=""),paste(ouInData,"/ciat-weather-stations",sep=""),recursive=T)
  #write control file
  cf <- file(paste(ouInData,"/ciat-weather-stations/_ready.ctr",sep=""),"w")
  cat("ciat weather stations data were copied on",date(),"by",
      paste(as.data.frame(t(Sys.info()))$login),"@",
      paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cf)
  close(cf)
}

#organising data
ciData <- paste(ouInData,"/ciat-weather-stations",sep="")
metaFolder <- paste(ciData,"/stations_details",sep="")
csvDir <- paste(ciData,"/rnf_organised",sep="")
orgDir <- paste(ciData,"/yearly_files",sep="")
outDir <- paste(orgDir,"/grouped_output-global",sep="")
if (!file.exists(outDir)) {dir.create(outDir,recursive=T)}

### organise the daily data into a proper format
wst <- read.csv(paste(metaFolder,"/stations_rv5.csv",sep=""))
for (year in yi:yf) {
  omx <- loadWriteYear(year,wst,csvDir,outDir)
  #count NAs for each of the days
  allNAs <- apply(omx,2,countNADays); allNAs <- c(year,allNAs[5:370])
  
  #rbind all the stations
  if (year==yi) {
    allStNAs <- allNAs
  } else {
    allStNAs <- rbind(allStNAs,allNAs)
  }
}

write.csv(allStNAs,paste(outDir,"/NA-count.csv",sep=""),row.names=F,quote=F)

###

#d. merge them both in one single dataset




