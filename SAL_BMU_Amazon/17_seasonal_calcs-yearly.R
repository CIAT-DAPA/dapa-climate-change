# Carlos Navarro 
# CIAT - CCAFS
# November 2012

# Set params
bDir <- "D:/cenavarro/lat-sal/outputs/average"
oDir <- bDir
years <- c("1981_2010")
varList <- c("dtr", "prec", "tmax", "tmin", "tmean")
# mask <- raster("D:/col-usaid/02-monthly-interpolations/region/mask/ris0.tif")

# List of seasons
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

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
  
  for (var in varList){
    
    # Load averages files 
    iAvg <- stack(paste(bDir,'/', var, "_", 1:12, ".tif",sep=''))
    
    # Loop throught seasons
    for (i in 1:length(seasons)){
      
      cat("Calcs ", var, yr,   names(seasons[i]), "\n")
      
      if (var == "prec"){
        
        sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
        
      } else {
        
        sAvg = calc(iAvg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
        
      }
      
      writeRaster(sAvg, paste(bDir,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
      
    } 
  }
}


