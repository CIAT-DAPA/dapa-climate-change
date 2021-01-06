iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_mapas"

for (i in list.files(iDir, pattern = ".tif")){
  if (!file.exists(paste0(iDir, "/", strsplit(i, ".tif")[[1]]))) {dir.create(paste0(iDir, "/", strsplit(i, ".tif")[[1]]), recursive=T)}  
}




require(raster)
bDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/monthly_data/Future/multimodel_mean/by_country/Bolivia"
varList <- c("prec", "tmax", "tmin")
seasons <- c("djf", "jja", "mam", "son")

for (var in varList){
  
  if (!file.exists(paste0(bDir, "/", var, "_chg_ann.tif"))){
    
    cat(var)
    stk <- stack(paste0(bDir, "/", var, "_chg_", seasons, "_mod.tif"))
    varStk_mean <- sum(stk * 3)/12

    varStk_mean <- writeRaster(varStk_mean, paste0(bDir, "/", var, "_chg_ann.tif"), format="GTiff", overwrite=F)
    
  }
}



require(raster)
require(rgdal)
bDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/monthly_data/Future/multimodel_mean/by_country/Jamaica"
wDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/monthly_data/WFD"
fDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/monthly_data/Future/multimodel_mean"
mask <- raster("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/monthly_data/Future/multimodel_mean/by_country/Jamaica/prec_chg_djf_mod.tif")


varList <- c("prec", "tmax", "tmin")
seasons <- c("djf", "jja", "mam", "son")
stats <- c()

for (var in varList){
  
  for (season in seasons){
    
    cat(var)
    w <- raster(paste0(wDir, "/", var, "_avg_", season, ".tif"))
    f <- raster(paste0(fDir, "/", var, "_avg_", season, ".tif"))
    
    xmin(w) = xmin(w) - 360
    xmax(w) = xmax(w) - 360
    
    wMask <- crop(w, extent(mask))
    wMask <- mask(wMask, mask)
    
    stats <- rbind(stats, cbind("Current",var, season, summary(wMask)[3]))
    
    fMask <- crop(f, extent(mask))
    fMask <- mask(fMask, mask)
    
    stats <- rbind(stats, cbind("Future",var, season, summary(fMask)[3]))
    
    
    
  }
    
}


colnames(stats) <- c("Scenario", "Variable", "Season", "Median")
stats_wr <- write.csv(stats, paste0(bDir, "/stats.csv"))
