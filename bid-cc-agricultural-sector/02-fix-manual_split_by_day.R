require(raster)
require(ncdf)


##Run over R 32bits

## Extract by day from NC raw files

for (i in 9491:12775){
    
  a <- raster("T:/gcm/mri/corrected-raw-cmip5-data/tasmax_day_CCSM4_rcp45_r1i1p1_20060101-20401231.nc", band=i)
  writeRaster(a, paste("G:/cenavarro/bid/gcm_raw_res/ncar_ccsm4/2020_2049/by-day/tasmax-", getZ(a), ".nc", sep=""), format="CDF", overwrite=T)
  
}


## Merge by-months


varlist <- c("pr", "tasmax", "tasmin")
mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))

for (var in varlist){
  
  for (yr in 2032:2040){
    
    for (mth in mthList){
      
      cat(paste("Merge", var, " ", yr, " ", mth))
      ncList <- list.files(path="G:/cenavarro/bid/gcm_raw_res/ncar_ccsm4/2020_2049/by-day/", pattern=paste(var, "-", yr, "-", mth, "-*", sep=""), full.names=TRUE)
      
      for (nc in ncList){
        
        ncsplit <- strsplit(basename(nc), '[-]')
        day <- sapply(strsplit(sapply(ncsplit, "[[", 4), '[.]'), "[[", 1)
        
        system(paste("cdo setdate,", yr, "-", mth, "-", day, ",00:00 ", nc, " ", "G:/cenavarro/bid/gcm_raw_res/ncar_ccsm4/2020_2049/by-day-dates/", basename(nc), sep=""))
        
      }
      
      ncList <- list.files(path="G:/cenavarro/bid/gcm_raw_res/ncar_ccsm4/2020_2049/by-day-dates/", pattern=paste(var, "-", yr, "-", mth, "-*", sep=""), full.names=TRUE)
      
      system(paste("cdo mergetime ", paste(ncList, collapse=" "), " G:/cenavarro/bid/gcm_raw_res/ncar_ccsm4/2020_2049/by-month/", var, "_", yr, "_", mth, ".nc",sep=""))
      #       system(paste("cdo -settaxis,", yr, "-", mth, "-01,00:00:00,1day G:/cenavarro/bid/gcm_raw_res/ncar_ccsm4/2020_2049/by-day/", var, "_", yr, "_", mth, ".nc G:/cenavarro/bid/gcm_raw_res/ncar_ccsm4/1960_1990/by-month/", var, "_", yr, "_", mth, ".nc", sep=""))
      
    }  
    
  }
  
}
