require(raster)
require(maptools)
library(rgeos)
library(rgdal)

iDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/outputs"
oDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/average"
nfolds <- 25
ntiles <- 4

varList <- c("rain", "tmin", "tmax")


## Average of all folds
for (i in 1:length(varList)){
 
  for (t in 1:ntiles){
    
    oDir_t <- paste0(oDir, "/tile-", t)
    if (!file.exists(oDir_t)) {dir.create(oDir_t, recursive = TRUE)}
    
    for (mth in 1:12) {
      
      if (!file.exists(paste(oDir_t, "/", varList[i], "_", mth, ".asc", sep=""))) {
        
        cat(varList[i], "mth-", mth, " tile-",t, " \n")
        mthStack <- stack(paste(iDir, "/", varList[i], "/fold-", 1:25, "/tile-", t, "/", varList[i], "_", mth, ".asc", sep=""))
        
        cat("Mean Stack\n")
        meanRaster <- mean(mthStack)
        
        cat("Mean Std\n")
        fun <- function(x) { sd(x) }
        stdRaster <- calc(mthStack, fun)
        
        cat("Writing\n")
        if (varList[i] == "rain"){
          meanRaster[which(meanRaster[]<0)]=0  
          writeRaster(meanRaster, paste(oDir_t, "/prec_", mth, ".asc", sep=""), format="ascii", overwrite=F)
          writeRaster(stdRaster, paste(oDir_t, "/prec_", mth, "_std.asc", sep=""), format="ascii", overwrite=F)
        } else {
          writeRaster(meanRaster, paste(oDir_t, "/", varList[i],"_", mth, ".asc", sep=""), format="ascii", overwrite=F)
          writeRaster(stdRaster, paste(oDir_t, "/", varList[i], "_", mth, "_std.asc", sep=""), format="ascii", overwrite=F)
        }
        
      }
    }
    
    for (mth in 1:12) {
      
      cat("Merge tiles ", varList[i], "mth-", mth,"\n")
      
      t <- list(paste0(oDir, "/tile-", 1:ntiles, "/", varList[i],"_", mth, ".asc"))
      merge <- do.call(merge, t)
    
      writeRaster(merge, paste(oDir, "/", varList[i], "_", mth, ".asc", sep=""), format="ascii", overwrite=F)
      
    }
    
    
    
  }
  
  
  cat
}


## Mean temperature calculation

for(mth in 1:12){
  
  tmax <- raster(paste0(oDir, "/tmax_", i,".asc"))
  tmin <- raster(paste0(oDir, "/tmin_", i,".asc"))
  tmean <- (tmax + tmin) / 2
  
  writeRaster(ptmean, paste(oDir, "/tmean_", mth, ".asc", sep=""), format="ascii", overwrite=F)
}


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


### Statistical consistency
evalDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_tropico"
metrics <- c()

for (f in 1:nfolds){
  for(t in 1:ntiles){
    met_t <- read.csv(paste(oDir, "/fold-", f, "/tile-", t, "/", vn, "_metrics.csv", sep=""))
    metrics <- rbind(metrics, met_t)    
  }
}

write.csv(metrics, paste(vn, "_all_metrics.csv", sep=""), row.names=F)