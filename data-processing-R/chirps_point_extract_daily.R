
## Libraries
require(raster)
require(rgdal)


## Params
yi <- 1981
yf <- 2020
ptName <- "m133" 
iDirP <- "S:/observed/gridded_products/chirps/monthly/world"
vars <- c("prec")
lon <- -78.661
lat <- -1.7208
oDir <- "D:/cenavarro/Request/ecu_ekorural"

##################################################
## Extract CHRIPS by country                 ###
##################################################

## Loop across variables
for (var in vars){
  
  ## List files by years
  cat(">. Listing files", "\n")
  
  if (var == "prec"){
    
    dtsLs <-  list.files(path=iDirP, pattern=paste0("v2p0chirps*.bil"),full.names = T,ignore.case=F)
    prefix <- "prec"
    varLn <- "Precipitation"
    unit <- "mm/day"
    
  } else if (var == "tmin") {
    
    ## List files by years
    dtsLs <-  list.files(path=iDirT, pattern=paste0("Tmin.*.tif"),full.names = T,ignore.case=F)
    prefix <- "Tmin"
    varLn <- "MinimumTemperature"
    unit <- "CelsiusDegrees"
    
  } 
  
  years <- paste(yi:yf, sep="", collapse="|")
  dtsLs_yrs_leap <- dtsLs[grepl(years,dtsLs)]
  dtsLs_yrs <- dtsLs_yrs_leap
  # dtsLs_yrs <- dtsLs_yrs_leap[!grepl(".2.29.tif",dtsLs_yrs_leap)] #no leap
  
  ## Load CHIRPS data and cut by mask
  ptsVal <- extract(stack(dtsLs_yrs), data.frame(x=lon,y=lat))
  
  
  ## Output file
  oFile <- paste0(oDir, "/", ptName, "_", yi, "-", yf, ".csv")
  
  vals <- cbind(date=dtsLs, vals=paste(ptsVal))
  write.csv(vals, oFile, row.names=F)
  
}
  