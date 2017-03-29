# Author: Carlos Navarro
# Date: January 2017


require(raster)
require(rgeos)
require(rgdal)
require(rasterVis)

mask <- readOGR("D:/CIAT/_tools/AdminBoundaries/SHP_files/HND_adm/HND0.shp", layer= "HND0")
mask1 <- readOGR("D:/CIAT/_tools/AdminBoundaries/SHP_files/HND_adm/HND1.shp", layer= "HND1")
anomDir <- "T:/gcm/cmip5/downscaled/ensemble/rcp45/global_2_5min/2040_2069"
wclDir <- "S:/observed/gridded_products/worldclim/Global_2_5min"
oDir <- "D:/CIAT/climate_change/scenarios-cc-hnd"


# Create output dir

if (!file.exists(paste0(oDir))) {dir.create(paste0(oDir), recursive = TRUE)}

# Cut Downscaled and WCL

rsPrecF <- mask(crop(raster(paste0(anomDir, "/bio_12")), extent(mask)), mask)
rsTempF <- mask(crop(raster(paste0(anomDir, "/bio_1")), extent(mask)), mask) / 10

rsPrecH <- mask(crop(raster(paste0(wclDir, "/bio_12")), extent(mask)), mask)
rsTempH <- mask(crop(raster(paste0(wclDir, "/bio_1")), extent(mask)), mask) /10


# Calc anomalies

anomPrec <- (rsPrecF - rsPrecH) / rsPrecH * 100
anomTemp <- rsTempF - rsTempH


# Plot prec

plot <- setZ(anomPrec, c("changes_prec"))
names(plot) <- c("changes_prec")
zvalues <- seq(-10, 10, 2) # Define limits
myTheme <- BuRdTheme() # Define squeme of colors
myTheme$regions$col=colorRampPalette(c("darkred","red","snow","blue", "darkblue"))(length(zvalues)-1) # Set new colors
myTheme$strip.border$col = "white" # Eliminate frame from maps
myTheme$axis.line$col = 'white' # Eliminate frame from maps

# Plot via levelplot
tiff(paste(oDir, "/chg_prec.tif", sep=""), width=1000, height=600, pointsize=8, compression='lzw',res=100)
print(levelplot(plot, at = zvalues, margin=FALSE, auto.key=FALSE, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask1, col="grey", lwd=0.5)))
dev.off()


# Plot temp 

plot <- setZ(anomTemp, c("changes_temp"))
names(plot) <- c("changes_temp")
zvalues <- seq(0, 3, 0.25)
myTheme <- BuRdTheme()
myTheme$regions$col=colorRampPalette(c("snow","yellow","orange", "red", "darkred"))(length(zvalues)-1)
myTheme$strip.border$col = "white"
myTheme$axis.line$col = 'white'

# Plot via levelplot
tiff(paste(oDir, "/chg_temp.tif", sep=""), width=1000, height=600, pointsize=8, compression='lzw',res=100)
print(levelplot(plot, at = zvalues, margin=FALSE, auto.key=FALSE, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask1, col="grey", lwd=0.5)))
dev.off()
