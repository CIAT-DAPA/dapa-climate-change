#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014

#read in / write elevation data

#input directories
#wd <- "~/Leeds-work/quest-for-robustness"
wd <- "~/Leeds-work/quest-for-robustness"
metDir <- paste(wd,"/data/meteorology/wfd_data",sep="")
yiDir <- paste(wd,"/data/yield_data_maize",sep="")

#read in yield data for getting mask
yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))

#determine extent to cut the resampled netcdf
bbox <- extent(yrs)
if (bbox@xmin < 0)  bbox@xmin <- bbox@xmin+360
if (bbox@xmax < 0)  bbox@xmax <- bbox@xmax+360

#go to dir
setwd("/Users/eejarv/Leeds-work/quest-for-robustness/data/meteorology")

fname <- "WFD-land-lat-long-z.nc"
nc <- open.ncdf(fname)
ncdata <- get.var.ncdf(nc,nc$var$Z) #get elev. data from nc connection
londata <- get.var.ncdf(nc,nc$var$Longitude) #get lon data from nc connection
latdata <- get.var.ncdf(nc,nc$var$Latitude) #get lat data from nc connection
brs <- raster(nrow=360,ncol=720,xmn=-180,xmx=180,ymn=-90,ymx=90) #base raster
nd <- dim(ncdata) #number of data points
brs[cellFromXY(brs,xy=cbind(x=londata,y=latdata))] <- ncdata #assign data points values to base raster
writeRaster(brs, "./../elevation/elevation_wfd.tif", format="GTiff") #write tiff
nc <- close.ncdf(nc)

#write a nc for further resampling
dimX <- dim.def.ncdf("lon","degrees_east",seq(0.25,359.75,by=0.5))
dimY <- dim.def.ncdf("lat","degrees_north",seq(-89.75,89.75,by=0.5))
dimT <- dim.def.ncdf( "time", "days", 0, unlim=TRUE)

#create variable definitions
mv <- 1.e20
var3d <- var.def.ncdf("Z","meters",list(dimX,dimY,dimT),mv,prec="double")

#write the netcdf
nc <- create.ncdf("./../elevation/elevation.nc", list(var3d))
data2d <- flip(rotate(brs),"y")[]
put.var.ncdf(nc, var3d, data2d, start=c(1,1,1), c(-1,-1,1))
nc <- close(nc)

#resampling
setwd("./../elevation")
system(paste("cdo remapcon2,r320x160 elevation.nc elevation_remapped.nc",sep=""))
system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," elevation_remapped.nc elevation_af.nc",sep=""))
system(paste("rm -f elevation_remapped.nc",sep=""))


