# Carlos Navarro 
# CIAT - CCAFS
# November 2012

# Set params
bDir <- "D:/col-usaid/02-monthly-interpolations/outputs_yearlyv2/average"
oDir <- bDir
years <- 1980:1980
varList <- c("dtr", "prec", "tmax", "tmin", "tmean")
mask <- raster("D:/col-usaid/02-monthly-interpolations/region/mask/ris0.tif")

# Set libraries
require(raster)
require(maptools)
require(rgdal)

# Temporal dir for raster library
if (!file.exists(paste0(oDir, "/tmp"))) {dir.create(paste0(oDir, "/tmp"), recursive = TRUE)}
rasterOptions(tmpdir= paste0(oDir, "/tmp"))

setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}

for (yr in years){
  
  for (i in 1:length(varList)){
    
    cat("Croping ", varList[i], yr, "\n")
    var <- varList[i] 

    rsStk <- stack(paste0(var, "_", yr, "_", 1:12, ".asc"))
    
    if (!file.exists(paste0(oDir, "/", var, "_", yr, "_", 12, ".tif"))) {
      
      rsCrop <- resample(crop(rsStk, extent(mask)), mask)
      rsMask <- mask(rsCrop, mask)
      
      if (var == "prec"){
        rsMask <- round(rsMask, digits = 0)
      } else if (var == "rhum"){
        rsMask <- round(rsMask * 100, digits = 0)
      } else {
        rsMask <- round(rsMask * 10, digits = 0)
      }
      
      for (i in 1:12){
        
        oTif <- paste0(oDir, "/", var, "_", yr, "_", i, ".tif")
        tifWrite <- writeRaster(rsMask[[i]], oTif, format="GTiff", overwrite=T, datatype='INT2S')
        cat(paste0(" ", var, "_",i, " cut done\n"))
        
      }
    }
  }
}
