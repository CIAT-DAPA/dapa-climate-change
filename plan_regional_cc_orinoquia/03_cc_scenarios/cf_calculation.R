## 3. Calculate Delta Method approach

rcp="rcp26"
period="2030_2059"

reg_cf <- function(baseDir="D:/col-cormacarena/anomalias_GCM", gcmDir="D:/CIAT/Articles/ccafs-climate/1-perfect-sibling-evaluation", wclDir="D:/CIAT/Articles/ccafs-climate/1-perfect-sibling-evaluation/wcl_seasonal/reggrided", rcp="rcp85", period="2030_2059") {
  
  cat(" \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat("XXXXXXXXX GCM CF SEASONAL CALCULATION XXXXXXXXXX \n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX \n")
  cat(" \n")
  
  #gcm=gcmList[1]
  
  gcmList <- list.dirs(paste(baseDir,"/",rcp, sep=""), recursive = FALSE, full.names = FALSE)
#   msk_world <- raster(paste(baseDir, "/_mask/mask_world.nc", sep=""))
#   msk_lat <- raster(paste(baseDir, "/_mask/mask_lat_new", sep=""))
  
  # List of variables, seasons and months
  #varList <- c("prec", "tmax", "tmin", "dtr", "tmean")
  varList <- c("prec", "tmax", "tmin")
  seasons <- c()
  seasons <- cbind(c("ann", "djf", "mam", "jja", "son"), c(1, 12, 3, 6, 9), c(12, 2, 5, 8, 11))
  colnames(seasons) <- c("season", "staMth", "endMth")
  
  for (gcm in gcmList){
    
    # Define and create output directories
    outGCMDirReg <- paste(baseDir,"/out_",rcp,"/", period, sep="")
    outGCMDirCF <- paste(baseDir, "/change_factor_seasonal/reggrided/", rcp, "/", gcm, "/", period, sep="")
    outGCMDirCFLat <- paste(baseDir, "/change_factor_seasonal/reggrided_lat/", rcp, "/", gcm, "/", period, sep="")
    if (!file.exists(outGCMDirCF)) {dir.create(outGCMDirCF, recursive=T)}
    if (!file.exists(outGCMDirCFLat)) {dir.create(outGCMDirCFLat, recursive=T)}
    
    for (var in varList){
      
      if (var =="dtr"){
        
        if (!file.exists(paste(outGCMDirCF, "/", var, ".nc", sep=""))) {
          
          mthStk_down <- stack(paste(outGCMDirCF, "/tmax.nc", sep="")) - stack(paste(outGCMDirCF, "/tmin.nc", sep=""))
          mthStk_down <- writeRaster(mthStk_down, paste(outGCMDirCF, "/", var, ".nc", sep=""), format='CDF', overwrite=TRUE)
          cat(paste(".> CF ", gcm, " ", var, " ", "\tdone!\n", sep=""))
          
        } else {
          
          cat(paste(".> CF ", gcm, " ", var, " ", "\tdone!\n", sep=""))
          
        }
        
        
      } else if (var =="tmean") {
        
        if (!file.exists(paste(outGCMDirCF, "/", var, ".nc", sep=""))) {
          
          mthStk_down <- (stack(paste(outGCMDirCF, "/tmax.nc", sep="")) + stack(paste(outGCMDirCF, "/tmin.nc", sep=""))) / 2
          mthStk_down <- writeRaster(mthStk_down, paste(outGCMDirCF, "/", var, ".nc", sep=""), format='CDF', overwrite=TRUE)
          cat(paste(".> CF ", gcm, " ", var, " ", "\tdone!\n", sep=""))
          
        } else {
          
          cat(paste(".> CF ", gcm, " ", var, " ", "\tdone!\n", sep=""))
          
        }
        
      } else {
        
        if (!file.exists(paste(outGCMDirCF, "/", var, ".nc", sep=""))) {
          
          cat(paste(".> CF ", gcm, " ", var, sep=""))
          
          # Calculations downscaled surfaces
          mthStk_anom <- stack(paste(outGCMDirReg, "/",  var, ".nc", sep=""))
          mthStk_anom <- mask(crop(mthStk_anom, msk_world), msk_world)
          
          mthStk_hist <- stack(paste(wclDir, "/",  var, ".nc", sep=""))
          
          if (var == "prec"){
            mthStk_down = mthStk_hist * abs(1 + mthStk_anom)
          } else {
            mthStk_down = (mthStk_hist/10) + mthStk_anom
          }
          
          mthStk_down <- writeRaster(mthStk_down, paste(outGCMDirCF, "/", var, ".nc", sep=""), format='CDF', overwrite=TRUE)
          
          cat(paste("\tdone!\n", sep=""))
          
        } else {
          cat(paste(".> CF ", gcm, " ", var, " ", "\tdone!\n", sep=""))
          
        }
        
      }
      
      # Calculate seasons 
      for (i in 1:nrow(seasons)){
        
        if (!file.exists(paste(outGCMDirCF, "/", var , "_", seasons[i,1], ".nc", sep=""))) {
          
          cat(paste(".> CF Seasonal ", gcm, " ",var, " ", seasons[i,1], sep=""))
          mthStk_down <- stack(paste(outGCMDirCF, "/", var, ".nc", sep=""))
          
          # Calculations by seasons
          if (seasons[i,1] == "djf"){seasonStk <- mthStk_down[[c(12, 1, 2)]]} else {seasonStk <- mthStk_down[[seasons[i,2]:seasons[i,3]]]}
          if (var == "prec"){
            seasonStk <- sum(stack(seasonStk))
          } else {
            seasonStk <- mean(stack(seasonStk))
          }
          
          # Reggrided by seasons
          seasonStk <- writeRaster(seasonStk, paste(outGCMDirCF, "/", var , "_", seasons[i,1], ".nc", sep=""), format='CDF', overwrite=TRUE)
          #           
          #           # Cut by region and extract values
          #           seasonStkLat <- crop(seasonStk, msk_lat)
          #           seasonStkLat <- setExtent(seasonStkLat, extent(msk_lat), keepres=FALSE, snap=FALSE)
          #           seasonStkLat <- mask(seasonStkLat, msk_lat)
          #           
          #           seasonStkLat <- writeRaster(seasonStkLat, paste(outGCMDirCFLat, "/", var , "_", seasons[i,1], ".nc", sep=""), format='CDF', overwrite=TRUE)
          #           seasonStkLatVal <- rasterToPoints(seasonStkLat)
          #           colnames(seasonStkLatVal) <- c("lon", "lat", "val")
          #           seasonStkLatVal <- write.table(seasonStkLatVal, paste(outGCMDirCFLat, "/", var , "_", seasons[i,1], ".txt", sep=""), row.names=F, sep="\t")
          #           
          cat(paste("\tdone!\n", sep=""))
          
        } else {cat(paste(".> CF Seasonal Lat ", gcm, " ", var, " ", seasons[i,1], "\tdone!\n", sep=""))}
        
      }
      
    }
  }
}
