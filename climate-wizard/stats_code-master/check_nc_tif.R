#### Check WOCAT INDICES


### NetCDF (outputs daily_extremes scripts)
require(raster)

iDir <- "X:/data/AR5_Global_Daily_25k/out_stats/ACCESS1-0"
oDir <- "D:/CIAT/Projects/wocat/test-runs"
setwd(iDir)

ncLs <- list.files(iDir, pattern = "\\_historical_ACCESS1-0_1950-2005.nc4$", recursive = F)
extract (stack(ncLs[[2]]), c(-74.41, 9.58))

for (nc in ncLs){
  
  ncRs <- raster(nc)
  
  tiff(paste(oDir, "/plot_", nc, ".tif", sep=""), width=800, height=600, pointsize=8, compression='lzw',res=100)
  plot(ncRs)
  title(paste(nc))
  dev.off()
  
}


### NetCDF (outputs daily_extremes scripts)

iDir <- "X:/data/AR5_Global_Daily_25k/out_stats_tiff/ACCESS1-0"
oDir <- "D:/CIAT/Projects/wocat/test-runs"
setwd(iDir)

ncLs <- list.files(iDir, pattern = "\\_historical_ACCESS1-0_1950-2005.tif$", recursive = F)
extract (stack(ncLs[[2]]), c(-74.41, 9.58))

for (nc in ncLs){
  
  ncRs <- raster(nc)
  
  tiff(paste(oDir, "/plot_", nc, ".tif", sep=""), width=800, height=600, pointsize=8, compression='lzw',res=100)
  plot(ncRs)
  title(paste(nc))
  dev.off()
  
}

