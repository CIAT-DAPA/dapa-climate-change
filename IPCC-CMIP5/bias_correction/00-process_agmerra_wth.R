#Carlos Navarro 
#May 2015

#AgMerra:
#Tmax: tmax
#Tmin: tmin
#Tmean: tavg
#Prec: prate
#Srad: srad
#Wsmean: wndspd

#load libraries
library(sp); library(raster); library(rgdal); library(maptools); library(ncdf); library(ncdf4)

#input directories
wd <-  "U:/cropdata/agcfsr/daily/nc-files/_primary_files"
# oDir <- "/home/cnavarro/wfdei/" 
oDir <- "U:/cropdata/agcfsr/daily/nc-files"

if (!file.exists(oDir)) {dir.create(oDir)}

# Get a list of month with and withour 0 in one digit numbers
mthList <- c(1:12)
mthListMod <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
mthMat <- as.data.frame(cbind(mthList, mthListMod))
names(mthMat) <- c("Mth", "MthMod")

#details
var_list <- c("tmax","tmin","tavg","prate", "srad", "wndspd")
agmerra_yrs <- c(1980:2010)
agcfsr_yrs <- c(1980:2010)
#wfdei_yrs <- c(1979:2012)
wfd_yrs <- c(1950:2001)
dst_list <- c("AgCFSR", "AgMERRA")

#function to process all years and months of a variable and dataset
process_wth <- function(dataset,vname) {
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("\n...processing",dataset,"and",vname,"\n")
  
  bvname <- vname
  setwd(wd)
  
  years <- get(paste(tolower(dataset),"_yrs",sep=""))
  
  vname <- var_list[1]
  
  #loop years and months
  for (yr in years) {
    yr <- years[1]
    rs <- raster(paste0(dataset, "_", yr, "_", vname, ".nc4"))
  

  }
}
 

# #loop through datasets and variables
for (dataset in dst_list) {
  
  dataset <- "dst_list[2]"
  
  for (vname in var_list) { 
    process_wth(dataset,vname)
    
  }
}

