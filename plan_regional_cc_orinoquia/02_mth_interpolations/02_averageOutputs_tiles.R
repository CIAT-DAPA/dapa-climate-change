# Carlos Navarro
# CCAFS / CIAT
# February 2016

# iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/outputs"
# oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/average"
# oStats <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_tropico"
# nfolds <- 25
# ntiles <- 2
# var <- "tmax"
# wDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/tmp"
# mth <- 1
# for (mth in 1:12){
#   avg_folds(iDir, oDir, oStats, nfolds, ntiles, var, wDir, mth)  
# }


avg_folds <- function(iDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/outputs", oDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/average", oStats="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_tropico", nfolds=25, ntiles=2, var="rain", wDir="D:/cenavarro/col-cormacarena/monthly-interpolations/tmp", mth=1) {

  require(raster)
  library(rgdal)
  
  if (!file.exists(wDir)) {dir.create(wDir, recursive = TRUE)}
  rasterOptions(tmpdir= wDir)

  for (t in 2:ntiles){
    
    oDir_t <- paste0(oDir, "/tile-", t)
    if (!file.exists(oDir_t)) {dir.create(oDir_t, recursive = TRUE)}
          
    if (!file.exists(paste(oDir_t, "/", var, "_", mth, ".tif", sep=""))) {
      
      cat(var, "mth-", mth, " tile-",t, " \n")
      mthStack <- stack(paste(iDir, "/", var, "/fold-", c(1:10, 12:20, 22:25), "/tile-", t, "/", var, "_", mth, ".asc", sep=""))
      
      cat("Mean Stack\n")
      meanRaster <- mean(mthStack)
      
#       cat("Mean Std\n")
#       fun <- function(x) { sd(x) }
#       stdRaster <- calc(mthStack, fun)
      
      cat("Writing\n")
      if (var == "rain"){
        meanRaster[which(meanRaster[]<0)]=0  
        
        ## Cut by quantile
#         meanRaster[which(meanRaster[]>quantile(meanRaster,probs = 0.99))]=quantile(meanRaster,probs = 0.99)  
        
        writeRaster(meanRaster, paste(oDir_t, "/prec_", mth, ".tif", sep=""), format="GTiff", overwrite=F, datatype='INT2S')
#         writeRaster(stdRaster, paste(oDir_t, "/prec_", mth, "_std.tif", sep=""), format="ascii", overwrite=F)
      } else {
        writeRaster(meanRaster, paste(oDir_t, "/", var,"_", mth, ".tif", sep=""), format="GTiff", overwrite=F, datatype='INT2S')
#         writeRaster(stdRaster, paste(oDir_t, "/", var, "_", mth, "_std.tif", sep=""), format="ascii", overwrite=F)
      }
      
    }


#     ### Merge tiled files
#     for (mth in 1:12) {
#       
#       cat("Merge tiles ", var, "mth-", mth,"\n")
#       
#       tStk <- stack(paste0(oDir, "/tile-", 1:ntiles, "/", var,"_", mth, ".asc"))
#       merge <- merge(tStk)
#     
#       writeRaster(merge, paste(oDir, "/", var, "_", mth, ".asc", sep=""), format="ascii", overwrite=F)
# 
#     }
    

  }
  
}

## Mean temperature calculation
tmean_calcs <- function(oDir, mth){
          
      tmax <- raster(paste0(oDir, "/tmax_", i,".tif"))
      tmin <- raster(paste0(oDir, "/tmin_", i,".tif"))
      tmean <- (tmax + tmin) / 2
      
      writeRaster(ptmean, paste(oDir, "/tmean_", mth, ".tif", sep=""), format="GTiff", overwrite=F, datatype='INT2S')

}

### Statistical consistency
stats <- function(oDir, oStats, nfolds, ntiles, var){
  
  lsMet <- expand.grid(paste0(oDir, "/fold-", as.vector(1:nfolds)), paste0("/tile-", as.vector(1:ntiles), "/", var, "_metrics.csv"))
  lsMet <- paste0(lsMet$Var1, lsMet$Var2)
  
  allMet <- lapply(lsMet, function(x){read.csv(x, header=T)})
  allMet <- do.call(rbind , allMet)
  write.csv(allMet, paste0(oStats, "/", var, "_all_metrics.csv"), row.names=F)
  
}


