require(raster)
require(maptools)
library(rgeos)
library(rgdal)

iDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/outputs"
oDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/outputs/average"

varList <- c("rhum")
# varList <- c("rain", "tmin", "tmax")

## Average of all folds
if (!file.exists(oDir)) {dir.create(oDir, recursive = TRUE)}

for (i in 1:length(varList)){
 
  var <- varList[i] 
  
  for (mth in 1:12) {
    
    cat("Averaging over", var, mth, "\n")
#     if (!file.exists(paste(oDir, "/prec_", mth, ".asc", sep=""))) {
      
      mthStack <- stack(paste(iDir, "/", varList[i], "/fold-", 1:25, "/tile-1/", varList[i], "_", mth, ".asc", sep=""))
      
      if (varList[i] == "rain"){
        var <- "prec"
        mthStack[which(mthStack[]<0)]=0
#         mthStack[which(mthStack[]>1400)]=1400
      }
        
      cat("Mean Stack\n")
      meanRaster <- mean(mthStack)
      
      cat("Mean Std\n")
      fun <- function(x) { sd(x) }
      stdRaster <- calc(mthStack, fun)
      
      cat("Writing\n")
      writeRaster(meanRaster, paste(oDir, "/", var,"_", mth, ".asc", sep=""), format="ascii", overwrite=T)
      writeRaster(stdRaster, paste(oDir, "/", var, "_", mth, "_std.asc", sep=""), format="ascii", overwrite=F)
      
#     }
    
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





avg_folds <- function(iDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/outputs", oDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/average",  nfolds=25, ntiles=2, var="rain", wDir="D:/cenavarro/col-cormacarena/monthly-interpolations/tmp", mth=1) {
  
  require(raster)
  library(rgdal)
  
  if (!file.exists(wDir)) {dir.create(wDir, recursive = TRUE)}
  rasterOptions(tmpdir= wDir)
  
  for (t in 1:ntiles){
    
    oDir_t <- paste0(oDir, "/tile-", t)
    if (!file.exists(oDir_t)) {dir.create(oDir_t, recursive = TRUE)}
    
    if (!file.exists(paste(oDir_t, "/", var, "_", mth, ".tif", sep=""))) {
      
      cat(var, "mth-", mth, " tile-",t, " \n")
      mthStack <- stack(paste(iDir, "/", var, "/fold-", 1:nfolds, "/tile-", t, "/", var, "_", mth, ".asc", sep=""))
      
      cat("Mean Stack\n")
#       meanRaster <- mean(mthStack)
      
      cat("Mean Std\n")
      fun <- function(x) { sd(x) }
      stdRaster <- calc(mthStack, fun)
      
      cat("Writing\n")
      
      if (var == "rain"){
        
#         meanRaster[which(meanRaster[]<0)]=0  
        
        ## Cut by quantile
        #         meanRaster[which(meanRaster[]>quantile(meanRaster,probs = 0.99))]=quantile(meanRaster,probs = 0.99)  
        
#         writeRaster(meanRaster, paste(oDir_t, "/prec_", mth, ".asc", sep="")) #, format="GTiff", overwrite=F, datatype='INT2S')
        writeRaster(stdRaster, paste(oDir_t, "/prec_", mth, "_std.tif", sep=""), format="ascii", overwrite=F)
      } else {
        writeRaster(meanRaster, paste(oDir_t, "/", var,"_", mth, ".asc", sep="")) #, format="GTiff", overwrite=F, datatype='INT2S')
        #         writeRaster(stdRaster, paste(oDir_t, "/", var, "_", mth, "_std.tif", sep=""), format="ascii", overwrite=F)
      }
      
    }
    
    
  }
  
}





iDir <- "D:/cenavarro/workspace/col-cormacarena/monthly-interpolations/outputs"
oDir <- "D:/cenavarro/workspace/col-cormacarena/monthly-interpolations/outputs/average"
nfolds <- 25
ntiles <- 1
var <- "rhum"
wDir <- "D:/cenavarro/tmp"

#Do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=6) #initiate cluster

#export functions
sfExport("avg_folds")
sfExport("iDir")
sfExport("oDir")
sfExport("nfolds")
sfExport("ntiles")
sfExport("var")
sfExport("wDir")

controlavg <- function(i) { #define a new function
  require(raster)
  library(rgdal)
  avg_folds(iDir, oDir, nfolds, ntiles, var, wDir, i)
}

system.time(sfSapply(as.vector(1:12), controlavg))


# Promedios anuales

mthList <- paste(oDir, "/tile-1/prec_", 1:12, ".asc", sep="")
mthStack <- stack(lapply(mthList, FUN=raster))

meanRaster <- sum(mthStack)

writeRaster(meanRaster, paste(oDir, "/tile-1/prec_ann.tif", sep=""), overwrite=F)


# mthList <- paste(oDir, "/tmean_", 1:12, ".asc", sep="")
# mthStack <- stack(lapply(mthList, FUN=raster))
# meanRaster <- mean(mthStack)
# 
# writeRaster(meanRaster, paste(oDir, "/tmean_ann.asc", sep=""), format="ascii", overwrite=F)
