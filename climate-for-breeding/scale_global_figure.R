#load library
library(raster)

#load raster files
rs_slope <- raster("/nfs/a101/earak/ME_maize_Africa_breeding/output_globalmaize_TTR/tifs/TTR_slope_map_rcp85_1995_2060_global.tif")
rs_ahar <- raster("~/Leeds-work/climate-for-breeding/data/maize_5min.nc",level=1)

#set zero area harvested to NA
rs_ahar[which(rs_ahar[] == 0)] <- NA

#2-step aggregation
rsx <- aggregate(rs_ahar, fact=3, fun=mean, expand=F, na.rm=T)
rsx <- aggregate(rsx, fact=2, fun=mean, expand=F, na.rm=F)

#final resample
rsx <- resample(rsx, raster(rs_slope), method="ngb")

#remove non-cropped pixels
rs_slope[which(is.na(rsx[]))] <- NA


