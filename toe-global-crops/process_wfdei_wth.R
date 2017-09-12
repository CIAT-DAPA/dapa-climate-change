#Julian Ramirez-Villegas, based on process_wfd_wth.R (from robustness)
#CIAT / CCAFS
#Jan 2017
stop("!")

#remap grid of WFDEI dataset into 1x1 grid (using 2nd order conservative remapping)

#all data at: /nfs/a101/earjr/ToE-global-crops-seasons/meteo_data/wfdei_data

#WFDEI:
#Tmax: Tmax_daily_WFDEI
#Tmin: Tmin_daily_WFDEI
#Prec: Rainf_daily_WFDEI_GPCC
#Srad: SWdown_daily_WFDEI

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); library(ncdf4)

#input directories
nfsDir <- "/nfs/a101/earjr"
wd <- paste(nfsDir,"/ToE-global-crops-seasons",sep="")
metDir <- paste(nfsDir,"/quest-for-robustness/data/meteorology/wfd_data",sep="")
yiDir <- paste(wd,"/yield_responses/yield_data/yield_data_maize",sep="")

#output directory
ometDir <- paste(wd,"/yield_responses/meteorology",sep="")
if (!file.exists(ometDir)) {dir.create(ometDir)}

#read in yield data for getting mask
yrs <- raster(paste(yiDir,"/gdhy_2013JAN29_maize_ModelYld500_1982.tif",sep=""))

#determine extent to cut the resampled netcdf
bbox <- extent(yrs)
if (bbox@xmin < 0)  bbox@xmin <- bbox@xmin+360
if (bbox@xmax < 0)  bbox@xmax <- bbox@xmax+360

#details
var_list <- c("Rainf","Tmax","Tmin")
dst_list <- c("WFDEI")
wfdei_yrs <- c(1985:2006)

#function to process all years and months of a variable and dataset
process_wfd_wth <- function(dataset,vname) {
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("\n...processing",dataset,"and",vname,"\n")
  
  bvname <- vname
  if (vname == "Rainf") {suffix <- paste("_daily_",dataset,"_GPCC",sep="")} else {suffix <- paste("_daily_",dataset,sep="")}
  if (dataset == "WFDEI") {if (vname == "Tmax" | vname == "Tmin") {bvname <- "Tair"}}
  if (vname == "Rainf") {vunits <- "kg m-2 s-1"} else if (vname == "SWdown") {vunits <- "W m-2"} else {vunits <- "K"}
  
  idataDir <- paste(metDir,"/",vname,suffix,sep="")
  odataDir <- paste(ometDir,"/",vname,suffix,sep="")
  if (!file.exists(odataDir)) {dir.create(odataDir)}
  
  years <- get(paste(tolower(dataset),"_yrs",sep=""))
  
  #loop years and months
  for (yr in years) {
    #yr <- years[1]
    for (mth in 1:12) {
      #mth <- 1
      cat("...processing year=",yr,"and month=",mth,"\n")
      tmth <- sprintf("%1$02d",mth)
      
      fname <- paste(vname,suffix,"_",yr,tmth,".nc",sep="")
      fnameout <- paste("toe_",vname,suffix,"_",yr,tmth,".nc",sep="")
      
      if (!file.exists(paste(odataDir,"/",fnameout,sep=""))) {
        #read netcdf using ncdf package
        setwd(idataDir)
        nc <- nc_open(fname)
        ncdata <- ncvar_get(nc,nc$var[[bvname]]) #get data from nc connection
        brs <- raster(nrow=360,ncol=720,xmn=-180,xmx=180,ymn=-90,ymx=90) #base raster
        if (dataset == "WFDEI") {nt <- dim(ncdata)[3]} else {nt <- ncol(ncdata)}
        
        #create stack
        stk <- c()
        for (i in 1:nt) {
          rs <- brs
          if (dataset == "WFD") {
            rs[as.numeric(nc$var[[bvname]]$dim[[1]]$vals)] <- ncdata[,i]
            stk <- c(stk,rotate(rs))
          } else {
            tdata <- t(ncdata[,,i]); tdata[which(tdata[] == 1.e20)] <- NA
            rs[] <- t(ncdata[,,i]) #tdata
            rs[which(rs[] >= 1.e20)] <- NA
            rs <- flip(rs,direction="y")
            rs <- shift(rotate(shift(rs, 180)), 180)
            stk <- c(stk,rs)
          }
        }
        
        nc <- nc_close(nc)
        
        #dimension definitions
        dimX <- ncdim_def("lon","degrees_east",seq(0.25,359.75,by=0.5))
        dimY <- ncdim_def("lat","degrees_north",seq(-89.75,89.75,by=0.5))
        dimT <- ncdim_def( "time", "days", nt, unlim=TRUE)
        
        #create variable definitions
        mv <- 1.e20
        var3d <- ncvar_def(vname,vunits,list(dimX,dimY,dimT),mv,prec="double")
        
        #create file
        nc <- nc_create(paste(odataDir,"/",vname,"_rewritten.nc",sep=""), list(var3d))
        for (i in 1:nt) {
          #data2d <- flip(stk[i][[1]],"y")[]
          data2d <- matrix(nrow=720,ncol=360,data=flip(stk[i][[1]],direction="y")[],byrow=F)
          ncvar_put(nc, var3d, data2d, start=c(1,1,i), c(-1,-1,1))
        }
        nc <- nc_close(nc)
        
        setwd(odataDir)
        system(paste("cdo remapcon2,r360x180 ",vname,"_rewritten.nc ",fnameout,sep=""))
        
        #garbage collection
        system(paste("rm -f ",vname,"_rewritten.nc",sep=""))
        setwd(wd)
      }
    }
  }
}

#loop through datasets and variables
for (dataset in dst_list) {
  #dataset <- dst_list[1]
  for (vname in var_list) {
    #vname <- var_list[1]
    process_wfd_wth(dataset,vname)
  }
}


