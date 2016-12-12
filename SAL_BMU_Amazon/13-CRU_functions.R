require(raster)
require(ncdf)
require(rgdal)
# require(ncdf4)

# source("13-CRU_functions.R")

CRU_cut <- function(baseDir="T:/gcm/cmip5/isi_mip", region=extent(-80, -66, -16, 5), outDir="Z:/DATA/WP2/03_Future_data/isi_mip_ft_0_5deg") {

  setwd(baseDir)
  if (!file.exists(outDir)) {dir.create(outDir, recursive = TRUE)}
  varList <- list("pre"="prec", "tmx"="tmax", "tmn"="tmin", "dtr"="dtr", "tmn"="tmean")
  
  for (v in 1:length(varList)){
    
    if(!file.exists(paste0(outDir, "/", varList[[v]], "_1976_2005_climatology.nc"))){
      
      nc <- paste0(baseDir, "/", names(varList[v]), "/cru_ts3.21.1901.2012.", names(varList[v]), ".dat.nc")
      oNc <- paste0(outDir, "/", varList[v], "_1961_2010_monthly.nc")
      
      ## Cut region and years
      cat("Cut region ", varList[[v]], "\n")
      system(paste("cdo -sellonlatbox,",region@xmin,",",region@xmax,",",region@ymin,",",region@ymax," -selyear,1961/2010 ", nc, " ", oNc, sep=""))
      
      ## Multiyear mean calcs
      cat("Clim calcs ", varList[[v]], "\n")
      system(paste("cdo -ymonavg -selyear,1986/2005 ", oNc, " ", outDir, "/", varList[[v]], "_1986_2005_climatology.nc", sep=""))
      system(paste("cdo -ymonavg -selyear,1971/2000 ", oNc, " ", outDir, "/", varList[[v]], "_1971_2000_climatology.nc", sep=""))
      system(paste("cdo -ymonavg -selyear,1976/2005 ", oNc, " ", outDir, "/", varList[[v]], "_1976_2005_climatology.nc", sep=""))
      
    }
    
  }
  
}




## Cut CRU-TS 3.21 raw
baseDir <- "S:/observed/gridded_products/cru-ts-v3-21/raw"
region <- extent(-80, -66, -16, 5)
outDir <- "Z:/DATA/WP2/02_Gridded_data/cru_0_5deg"
otp <- CRU_cut(baseDir, region, outDir)
