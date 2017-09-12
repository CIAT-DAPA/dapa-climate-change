## For prec, tmin, tmax, tmean

require(raster)
require(maptools)
require(rgdal)

# writeRaster(raster("D:/CIAT/Projects/col-usaid/02_monthly_interpolation/region/mask/ris0") * 0 + 1, "D:/CIAT/Projects/col-usaid/02_monthly_interpolation/region/mask/ris0.tif", format="GTiff", overwrite=T, datatype='INT2S')

iDir <- "D:/CIAT/Projects/col-usaid/02_monthly_interpolation/outputs_complemented"
mask <- raster("D:/CIAT/Projects/col-usaid/02_monthly_interpolation/region/mask/ris0.tif")

periods <- c("1976-1985", "1980-2010", "1986-1995", "1996-2005", "2006-2015")

varList <- c("dtr", "prec", "tmax", "tmin", "tmean")

for (period in periods){
  
  dirbase <- paste0(iDir, "/", period, "/average")
  setwd(dirbase)
  
  outDir <- paste(dirbase, sep="")
  if (!file.exists(outDir)) {dir.create(outDir)}
  
  # for(rs in rsList){
  for (var in varList){
    
    rsStk <- stack(paste0(dirbase, "/", var, "_", 1:12, ".asc"))
    
    if (!file.exists(paste0(outDir, "/", var, "_", 12, ".tif"))) {
      
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
        
        oTif <- paste0(outDir, "/", var, "_",i, ".tif")
        tifWrite <- writeRaster(rsMask[[i]], oTif, format="GTiff", overwrite=T, datatype='INT2S')
        cat(paste0(" ", var, "_",i, " cut done\n"))
        
      }
    }
  }
  
}
  
  





#############
#  Current  #
#############

require(raster)
require(maptools)
library(rgeos)
library(rgdal)


iDir <- "D:/CIAT/Projects/col-usaid/02_monthly_interpolation/outputs_complemented"

# List of seasons
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

periods <- c("1976-1985", "1980-2010", "1986-1995", "1996-2005", "2006-2015")
for (period in periods){
  
  climPath <- paste0(iDir, "/", period, "/average")
  
  
  # Load averages files 
  tmean_avg<- stack(paste(climPath,'/tmean_', 1:12, ".tif",sep=''))
  prec_avg<- stack(paste(climPath,'/prec_',1:12, ".tif",sep=''))
  tmin_avg <- stack(paste(climPath,'/tmin_',1:12, ".tif",sep=''))
  tmax_avg <- stack(paste(climPath,'/tmax_',1:12, ".tif",sep=''))
  dtr_avg <- stack(paste(climPath,'/dtr_',1:12, ".tif",sep=''))
  
  
  
  # Loop throught seasons
  for (i in 1:length(seasons)){
    
    # Mean tmean
    promedio = calc(tmean_avg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
    writeRaster(promedio,paste(climPath,'/tmean_', names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
    
    # Mean tmin
    promedio = calc(tmin_avg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
    writeRaster(promedio,paste(climPath,'/tmin_', names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
    
    # Mean tmax
    promedio = calc(tmax_avg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
    writeRaster(promedio,paste(climPath,'/tmax_', names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
    
    # Mean dtr
    promedio = calc(dtr_avg[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
    writeRaster(promedio,paste(climPath,'/dtr_', names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
    
    # Anual rainfall
    total_rain = calc(prec_avg[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
    writeRaster(total_rain,paste(climPath,'/prec_', names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
    
  }
  
}



