#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014
# stop("!")

#remap grid of WFD dataset and cut to African domain

#all data at: /nfs/a101/eejarv/quest-for-robustness/data/meteorology/wfd_data
#WFD:
#Tmax: Tmax_daily_WFD
#Tmin: Tmin_daily_WFD
#Prec: Rainf_daily_WFD_GPCC
#Srad: SWdown_daily_WFD

#WFDEI:
#Tmax: Tmax_daily_WFDEI
#Tmin: Tmin_daily_WFDEI
#Prec: Rainf_daily_WFDEI_GPCC
#Srad: SWdown_daily_WFDEI

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); library(ncdf)

#input directories
wd <- "S:/observed/gridded_products/wfd"
metDir <- "S:/observed/gridded_products/wfd/raw"
oDir <- paste("S:/observed/gridded_products/wfd/nc-files/wfd_0_5_deg_lat",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

bbox <- extent(-120,-30,-56,33)
if (bbox@xmin < 0)  bbox@xmin <- bbox@xmin+360
if (bbox@xmax < 0)  bbox@xmax <- bbox@xmax+360

# Get a list of month with and withour 0 in one digit numbers
mthList <- c(1:12)
mthListMod <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
mthMat <- as.data.frame(cbind(mthList, mthListMod))
names(mthMat) <- c("Mth", "MthMod")

#details
var_list <- c("Rainf","SWdown","Tmax","Tmin")
dst_list <- c("WFD","WFDEI")
wfdei_yrs <- c(1979:2012)
wfd_yrs <- c(1960:1990)

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
  odataDir <- paste(oDir,"/",vname,suffix,sep="")
  if (!file.exists(odataDir)) {dir.create(odataDir)}
  
  years <- get(paste(tolower(dataset),"_yrs",sep=""))
  
  #loop years and months
  for (mth in 1:12) {

    for (yr in years) {
      
      mthMod <- paste((mthMat$MthMod[which(mthMat$Mth == mth)]))
      
      cat("...processing year=",yr,"and month=",mth,"\n")
      tmth <- sprintf("%1$02d",mth)
      
      fname <- paste(vname,suffix,"_",yr,tmth,".nc",sep="")
      fnameout <- paste("lat_",vname,suffix,"_",yr,tmth,".nc",sep="")
      
      fnameout_avg <- paste("lat_",vname,suffix,"_",yr,tmth,"_avg.nc",sep="")
      fnameout_std <- paste("lat_",vname,suffix,"_",yr,tmth,"_std.nc",sep="")
      
      if (!file.exists(paste(odataDir,"/",fnameout,sep=""))) {
        #read netcdf using ncdf package
        setwd(idataDir)
        nc <- open.ncdf(fname)
        ncdata <- get.var.ncdf(nc,nc$var[[bvname]]) #get data from nc connection
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
            stk <- c(stk,rotate(flip(rs,direction="y")))
          }
        }
        
        nc <- close.ncdf(nc)
        
        #dimension definitions
        dimX <- dim.def.ncdf("lon","degrees_east",seq(0.25,359.75,by=0.5))
        dimY <- dim.def.ncdf("lat","degrees_north",seq(-89.75,89.75,by=0.5))
        dimT <- dim.def.ncdf( "time", "days", nt, unlim=TRUE)
        
        #create variable definitions
        mv <- 1.e20
        var3d <- var.def.ncdf(vname,vunits,list(dimX,dimY,dimT),mv,prec="double")
        
        #create file
        nc <- create.ncdf(paste(odataDir,"/",vname,"_rewritten.nc",sep=""), list(var3d))
        for (i in 1:nt) {
          data2d <- flip(stk[i][[1]],"y")[]
          put.var.ncdf(nc, var3d, data2d, start=c(1,1,i), c(-1,-1,1))
        }
        nc <- close(nc)
        
        setwd(odataDir)
        # system(paste("cdo remapcon2,r320x160 ",vname,"_rewritten.nc ",odataDir,"/",vname,"_remapped.nc",sep=""))
        
        #cut to Africa
        system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",vname,"_rewritten.nc ",vname,"_cutted.nc",sep=""))
        system(paste("cdo settaxis,",yr,"-",mthMod,"-01,00:00,1day ",vname,"_cutted.nc ",fnameout,sep=""))
        
        #garbage collection
        system(paste("rm -f ",vname,"_rewritten.nc",sep=""))
        system(paste("rm -f ",vname,"_cutted",sep=""))
        setwd(wd)
        
      } else {cat("...processed year=",yr,"and month=",mth,"\n")}

      setwd(odataDir)
      
      cat(" Calculating avg and std daily: future ", fnameout, " avg and std\n")
      system(paste("cdo -s monavg ", fnameout, " ",  fnameout_avg, sep=""))
      system(paste("cdo -s monstd ", fnameout, " ",  fnameout_std, sep=""))
      
      setwd(wd)
      
    }
    
    setwd(odataDir)
        
    avgNcList <- paste("lat_",vname,suffix,"_",1960:1990,tmth,"_avg.nc",sep="")
    system(paste("cdo ensavg ", paste(avgNcList, collapse=" "), " ", "lat_",vname,suffix,"_1960_1990_",tmth,"_avg.nc", sep=""))
        
    stdNcList <- paste("lat_",vname,suffix,"_",1960:1990,tmth,"_std.nc",sep="")
    system(paste("cdo ensavg ", paste(stdNcList, collapse=" "), " ", "lat_",vname,suffix,"_1960_1990_",tmth,"_std.nc", sep=""))
    
    for (nc in avgNcList){
      file.remove(paste(nc))
    }
    
    for (nc in stdNcList){
      file.remove(paste(nc))                       
    }
    
    setwd(wd)
  }
      
  for (mth in 1:12) {
    #mth <- 1
    mthMod <- paste((mthMat$MthMod[which(mthMat$Mth == mth)]))
    
    
    if (!file.exists(paste(odataDir, "/lat_",vname,suffix,"_1960_1990_", mthMod, ".nc", sep=""))) {
      
      #Merge WFD files
      cat("...merging=",vname,"\n")
      ncList <- paste(odataDir, "/lat_", vname, suffix, "_", 1960:1990, mthMod, ".nc", sep="")
      cat("...merging=",ncList,"\n")
      system(paste("cdo mergetime ", paste(ncList, collapse=" "), " ", odataDir, "/lat_",vname,suffix,"_1960_1990_", mthMod, ".nc",sep=""))
    
      }

    #     for (nc in ncList){
#       system(paste("rm -f ", odataDir, "/", nc, sep=""))
#     }
    
  }
}



#loop through datasets and variables
# for (dataset in dst_list) {
dataset <- dst_list[1]
for (vname in var_list) {
  #vname <- var_list[1]
  process_wfd_wth(dataset,vname)
}
# }
