## Author : Carlos Navarro
## Date   : January 2016

###############################################################################################
########################## DAILY DATA FOR CORMACARENA PROJECT #################################
###############################################################################################

#source("04_daily_data_functions_v2.R")
bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/ensemble_GCM/14_gcm"
mask <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/llanos_adm0.asc"
dem <- "S:/observed/gridded_products/srtm/Altitude_30s/alt"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/datos_diarios/input"
read_diva <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_scripts/Read_DIVA.exe"
model <- "rcp85"

otp <- dat_files_gen(bDir, mask, dem, oDir,model)


## Generation of the daily .dat files 
dat_files_gen <- function(bDir="U:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima", mask="U:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/llanos_adm0.asc", dem="S:/observed/gridded_products/srtm/Altitude_30s/alt", oDir="U:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/datos_diarios/dat_files", read_diva="U:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_scripts/Read_DIVA.exe", model="rcp45"){

  require(raster)
  require(ncdf)
  require(rgdal)
  
  bDir <- paste0(bDir, "/", model)
  oDir <- paste0(oDir, "/", model)

  mask <- raster(mask)
  dem <- raster(dem)
  
  varList <- c("tmin", "tmax", "prec")
  
  coords <- rasterToPoints(mask)[,1:2]
  colnames(coords) <- c("LONGITUD", "LATITUD")
  ncell <- dim(coords)[1]
  alt <- extract(dem, coords)
  
  list <- c()
  values <- c()
  
  cat("Extracting input climate data.. ")
  
  for (mth in 1:12){
    
    for (var in varList){
      
      rs <- raster(paste0(bDir, "/", var, "_", mth, ".asc"))
      rs_cut <- mask(crop(rs, extent(mask)), mask)
      rs_pts <- rasterToPoints(rs_cut)
      
      if (var == "prec"){
        values <- cbind(values, rs_pts[,3])    
      } else {
        values <- cbind(values, rs_pts[,3] / 10)
      }
      
      list <- c(list, paste0(var, mth))
      
    }
    
  }
  
  colnames(values) <- list
  
  nFold <- ncell %/% 7999 + 1
    
    for(j in 1:nFold){
      
  #     outdir<-paste0(oDir,"/",model)
  #     if (!file.exists(outdir)) {dir.create(outdir, recursive=T)}
      
      oFold <- paste0(oDir, "/fold-", sprintf("%02d",j))
      if (!file.exists(oFold)) {dir.create(oFold)}
      
      if (!file.exists(paste0(oFold, "/7999.dat"))){
          
        staCell <- ((j - 1) * 7999) + 1
        if (j == nFold){endCell <- ncell} else {endCell <- staCell + 7998}
        
        cat("\n Creating dat files for Fold ", j, model, staCell, endCell, "\n")
        
        data_matrix <- cbind("POINTID"=1:7999, "LATITUD"=coords[staCell:endCell,2], "LONGITUD"=coords[staCell:endCell,1], "RecNo"=1:7999, "PointNo"=1:7999, coords[staCell:endCell,], "alt"=alt[staCell:endCell], values[staCell:endCell,])
        
        data_matrix <- write.table(data_matrix, paste0(oFold, "/", model, ".txt"), quote = F, row.names = F, sep="\t")
        
        cat("fold ", j)
        system2(paste0(read_diva))
        
      }
      
    }
    
}



