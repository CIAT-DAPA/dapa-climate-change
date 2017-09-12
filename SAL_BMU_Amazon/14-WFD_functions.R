require(raster)
require(ncdf)
require(rgdal)
# require(ncdf4)

# source("14-WFD_functions.R")

WFD_cut <- function(baseDir="T:/gcm/cmip5/isi_mip", region=extent(-80, -66, -16, 5), outDir="Z:/DATA/WP2/03_Future_data/isi_mip_ft_0_5deg") {

  setwd(baseDir)
  if (!file.exists(outDir)) {dir.create(outDir, recursive = TRUE)}
  varList <- c("prec", "tmax", "tmin")
  
  for (var in varList){
    
    if(!file.exists(paste0(outDir, "/", var, "_1971_2000_climatology.nc"))){
      
      nc <- paste0(baseDir, "/", var, "_daily_ts_wfd_1950_2001.nc")
      oNc <- paste0(outDir, "/", var, "_1961_2000")
      
      ## Cut region and years
      cat("Cut region ", var, "\n")
      system(paste0("cdo -sellonlatbox,",region@xmin,",",region@xmax,",",region@ymin,",",region@ymax," -selyear,1961/2000 ", nc, " ", oNc, "_daily.nc"))
      system(paste0("cdo monavg ", oNc, "_daily.nc ", oNc, "_monthly.nc"))
      
      
      ## Multiyear mean calcs
      cat("Clim calcs ", var, "\n")
      system(paste("cdo -ymonavg -selyear,1971/2000 ", oNc, "_monthly.nc ", outDir, "/", var, "_1971_2000_climatology.nc", sep=""))

      
    }
    
  }
  
}




## Cut CRU-TS 3.21 raw
baseDir <- "U:/cropdata/wfd/daily/nc-files"
region <- extent(-80, -66, -16, 5)
outDir <- "Z:/DATA/WP2/02_Gridded_data/wfd_0_5deg"
otp <- WFD_cut(baseDir, region, outDir)
