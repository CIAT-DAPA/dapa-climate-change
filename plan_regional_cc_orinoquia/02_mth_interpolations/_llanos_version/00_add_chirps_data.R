##################################
#### Add CHIRPS Stations #########
##################################

require(raster)
require(dismo)
require(som)

iDir <- "S:/observed/gridded_products/chirps/monthly/llanos_0_1deg"
wDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/stations-averages"
rDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima"

# List of months
mthLs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

## Region mask
mask <- raster(paste0(rDir, "/_masks/mask_rectangle.tif"))
depLim <- raster(paste0(rDir, "/_masks/mask_v1.tif"))
refExt <- extent(mask)

## Load original station data to 
raw_data <- read.csv(paste0(wDir, "/rain_lla.csv"))

### Read CHRIPS output data, write in a friendly format
## Cut border and calc annual
# rs <- stack(paste0(iDir, "/ppt", mthLs, "_1981to2010.bil"))
# rs_clip <- crop(rs, refExt)
# for (i in 1:nlayers(rs_clip)){
#   writeRaster(rs_clip[[i]], paste0(iDir, "/prec_", i, ".tif"), format="GTiff", overwrite=T, datatype='INT2S')
# }
# writeRaster(sum(rs_clip), paste0(iDir, "/prec_ann.tif"), format="GTiff", overwrite=T, datatype='INT2S')

# ## Aggregate to 0.5 deg and calc annual
# rs_clip <- stack(paste0(iDir, "/prec_", 1:12, ".tif"))
# rs_agg <- aggregate(rs_clip, fact=5)
# for (i in 1:nlayers(rs_agg)){
#   writeRaster(rs_agg[[i]], paste0(oDir, "/prec_", i, ".tif"), format="GTiff", overwrite=T, datatype='INT2S')
# }
# writeRaster(sum(rs_agg), paste0(oDir, "/prec_ann.tif"), format="GTiff", overwrite=T, datatype='INT2S')
## Load gap mask resampled in ArcGIS...
# gap <- raster(paste0(rDir, "/_masks/msk_rain_05"))

### Read CHRIPS re-formated
rs <- stack(paste0(iDir, "/prec_", 1:12, ".tif"))

## Cut with gap mask
gap <- raster(paste0(rDir, "/_masks/msk_rain_025"))
gap[!is.na(gap)]<- 0
gap[is.na(gap)] <- 1
gap[which(gap[]==0)]<- NA
# writeRaster(gap, paste0(rDir, "/_masks/mask_rain_lla.tif"), format="GTiff", overwrite=T, datatype='INT2S')

## Calc random points based on kernel-density function (computed in ArcGIS)
wgRs <- raster(paste0(rDir, "/_masks/rain_weigth_1deg.tif"))
wgRs <- resample(wgRs, gap)
wgGap <- mask(1-(wgRs/(maxValue(wgRs)- minValue(wgRs))), gap)

# Calculate the density within departaments based on raw_data
gapMsk <- mask(setExtent(crop(gap, extent(depLim)), extent(depLim), keepres = F) , depLim)
denPres <- nrow(raw_data[which(raw_data$COUNTRY=="ideam_llanos"),])/length(depLim[!is.na(depLim)])
npts <- length(gap[!is.na(gap)]) * denPres
pts <- randomPoints(wgGap, n=npts, ext=extent(wgGap), prob=T)

rs_pts <- round(extract(rs, pts), digits = 0)
alt <- extract(raster(paste0(rDir, "/baseline/llanos/_region/alt-prj-lla.asc")), pts)

# Combine info and data
climData <- as.data.frame(cbind(paste0("ch", 1:nrow(pts)), "chirps", paste0("ch", 1:nrow(pts)), paste0("ch", 1:nrow(pts)), "chirps", round(pts, 2), alt, rs_pts, 30, rowSums(rs_pts)))
names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS", "ANN")

combClimData <- na.omit(rbind(raw_data, climData))
write.csv(combClimData, paste0(wDir, "/rain_lla.csv"), row.names = F)

  