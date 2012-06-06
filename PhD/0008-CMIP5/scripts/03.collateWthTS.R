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
#i <- 1 #gcm to process

#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"
#i <- 1 #gcm to process

source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))


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
  cat("worldclim weather stations data were copied on",date(),"by",
      paste(as.data.frame(t(Sys.info()))$login),"@",
      paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cf)
  close(cf)
}


#b. get the monthly series of GHCN between 1961 and 2000


#b. get the CIAT monthly series between 1961 and 2000



#c. merge them both in one single dataset




