#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

require(raster)

gcmDir <- "F:/climate_change/IPCC_CMIP3/20C3M/filled"
gcm <- "bccr_bcm2_0"
gcmrsDir <- paste(gcmDir, "/", gcm, "/1961_1990", 
monthList <- c(5:7)

wclDir <- "F:/Clim_30s/10min_wcl_asciis" #"C:/CIAT_work/_tools/dapa-climate-change/trunk/EcoCrop/data/climate"

#Load GCM & WorldClim data
#Load each raster and also compute the total

vn <- "prec"
ext <- ".asc"
for (m in monthList) {
	wclrs <- raster(paste(wclDir, "/", vn, "_", m, ext, sep=""))
	gcmrs <- 
}
