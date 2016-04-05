require(raster)
require(maptools)
library(rgeos)
library(rgdal)

iDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations-llanos/outputs"
oDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations-llanos/outputs/average"

# varList <- c("tmin", "tmax")
varList <- c("rain", "tmin", "tmax")

## Average of all folds

for (i in 1:length(varList)){
 
  var <- varList[i] 
  
  for (mth in 1:12) {
    
    cat("Averaging over", var, mth, "\n")
    if (!file.exists(paste(oDir, "/", varList[i], "_", mth, ".asc", sep=""))) {
      
      mthStack <- stack(paste(iDir, "/", varList[i], "/fold-", 1:25, "/tile-1/", varList[i], "_", mth, ".asc", sep=""))
      
      if (varList[i] == "rain"){
        var <- "prec"
        mthStack[which(mthStack[]<0)]=0
      }
        
      cat("Mean Stack\n")
      meanRaster <- mean(mthStack)
      
      cat("Mean Std\n")
      fun <- function(x) { sd(x) }
      stdRaster <- calc(mthStack, fun)
      
      cat("Writing\n")
      writeRaster(meanRaster, paste(oDir, "/", var,"_", mth, ".asc", sep=""), format="ascii", overwrite=F)
      writeRaster(stdRaster, paste(oDir, "/", var, "_", mth, "_std.asc", sep=""), format="ascii", overwrite=F)
      
    }
    
  }
  
}

# 
# ## Mean temperature calculation
# cat("Calculate tmean\n")
# for(mth in 1:12){
# 
#   tmax <- raster(paste0(oDir, "/tmax_", i,".asc"))
#   tmin <- raster(paste0(oDir, "/tmin_", i,".asc"))
#   tmean <- (tmax + tmin) / 2
#   
#   writeRaster(tmean, paste(oDir, "/tmean_", mth, ".asc", sep=""), format="ascii", overwrite=F)
#   
# }
# 

# 
# ## Promedios trimestrales
# 
# mthList <- paste(oDir, "/prec_", c(1, 2, 12), ".asc", sep="") 
# mthStack <- stack(lapply(mthList, FUN=raster))
# meanRaster <- sum(mthStack)
# 
# writeRaster(meanRaster, paste(oDir, "/prec_djf.asc", sep=""), format="ascii", overwrite=F)
# 
# 
# mthList <- paste(oDir, "/prec_", c(6, 7, 8), ".asc", sep="") 
# mthStack <- stack(lapply(mthList, FUN=raster))
# meanRaster <- sum(mthStack)
# 
# writeRaster(meanRaster, paste(oDir, "/prec_jja.asc", sep=""), format="ascii", overwrite=F)
# 
# 
# 
# mthList <- paste(oDir, "/tmean_", c(1, 2, 12), ".asc", sep="") 
# mthStack <- stack(lapply(mthList, FUN=raster))
# meanRaster <- mean(mthStack)
# 
# writeRaster(meanRaster, paste(oDir, "/tmean_djf.asc", sep=""), format="ascii", overwrite=F)
# 
# 
# mthList <- paste(oDir, "/tmean_", c(6, 7, 8), ".asc", sep="") 
# mthStack <- stack(lapply(mthList, FUN=raster))
# meanRaster <- mean(mthStack)
# 
# writeRaster(meanRaster, paste(oDir, "/tmean_jja.asc", sep=""), format="ascii", overwrite=F)
# 
# 
# 
# # Promedios anuales
# 
# mthList <- paste(oDir, "/prec_", 1:12, ".asc", sep="") 
# mthStack <- stack(lapply(mthList, FUN=raster))
# meanRaster <- sum(mthStack)
# 
# writeRaster(meanRaster, paste(oDir, "/prec_ann.asc", sep=""), format="ascii", overwrite=F)
# 
# 
# mthList <- paste(oDir, "/tmean_", 1:12, ".asc", sep="") 
# mthStack <- stack(lapply(mthList, FUN=raster))
# meanRaster <- mean(mthStack)
# 
# writeRaster(meanRaster, paste(oDir, "/tmean_ann.asc", sep=""), format="ascii", overwrite=F)
# 

