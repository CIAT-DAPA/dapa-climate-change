### Author: Julian Ardila / Carlos Navarro
## DAte : March 2016

# source("05_downscaling_tropico.R")
iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/tropico/rcp85"
dirworl <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/tropico/rcp85"
otp <- fix_wcl(iDir, dirworl)
# 
# source("05_downscaling_tropico.R")
# rcp <- "rcp45"
# dirAnom <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico"
# outDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/tropico"
# dirworl <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/average"
# # otp <- downscaling(rcp,dirAnom,outDir,dirworl)
# 
# #Do the snowfall stuff here
# library(snowfall)
# sfInit(parallel=T,cpus=12) #initiate cluster
# 
# #export functions
# sfExport("rcp")
# sfExport("dirAnom")
# sfExport("outDir")
# sfExport("dirworl")
# sfExport("downscaling")
# 
# controldown <- function(i) { #define a new function
#   # for(i in 1:nrow(index)){
#   otp <- downscaling(rcp,dirAnom,outDir,dirworl, i)
# }
# 
# system.time(sfSapply(as.vector(1:12), controldown))
# 

# 
# source("05_downscaling_tropico.R")
# outDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/tropico"
# rcp <- "rcp26"
# ext <- "tif"
# otp <- bioclim_calc(outDir, rcp, ext)


fix_wcl <- function(iDir="D:/cenavarro/col-cormacarena/monthly-interpolations/average", dirworl="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/average"){

  require(raster)
  require(ncdf)
  
  varList <- c("prec", "tmin", "tmax", "bio")
  
  for (var in varList){
    
    if (var == "bio"){
      mthLs <- 1:19
    } else {
      mthLs <- 1:12  
    }
    
    ### Modificar raster iniciales de WorldClim, que estan movidos 14 grados de longitud
    for (j in mthLs) { 
      
      if (!file.exists(paste0(dirworl, "/", var, "_", j, ".tif"))){
        wclim_var <- raster(paste0(iDir,"/",var,"_",j))  
#         xmin(wclim_var) = xmin(wclim_var) - 14
#         xmax(wclim_var) = xmax(wclim_var) - 14
        writeRaster(wclim_var, paste0(dirworl, "/", var, "_", j, ".tif"), format="GTiff", overwrite=F, datatype='INT2S')  
      }
      
    }
  }
  
}

downscaling <- function(rcp="rcp26", dirAnom="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/tropico",outDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/tropico",dirworl="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/average", j=1){
  
  varList <- c("prec", "tmin", "tmax")
  require(raster)
  require(ncdf)
  
  wDir <- "E:/cenavarro/col-cormacarena/tmp"
  if (!file.exists(wDir)) {dir.create(wDir, recursive = TRUE)}
  rasterOptions(tmpdir= wDir)
  
  for (var in varList){
    
    ## Out dir
    downDir <- paste0(outDir, "/",rcp)
    verDir <- paste0(outDir, "/",rcp, "/_verification")
    if (!file.exists(verDir)) {dir.create(verDir, recursive=T)}
  
#     for(j in 1:12){
      
      ###carga datos de promedios generados de estaciones
      wclStk <- raster(paste0(dirworl,"/",var,"_",j, ".tif"))
      
      ###lista las anomalias en el directorio por rcp
      anomStk <- raster(paste(dirAnom,"/",rcp,"/",var,"_",j,".tif", sep=""))
      
      oFile <- paste(downDir, "/", var, "_", j, ".tif", sep="")
      
      cat("Downscaling over ", rcp, paste(var, "_", j, sep=""), "\n")
      
      if (!file.exists(oFile)){
        
        if (var == "prec"){
          
          down <- wclStk * abs(1 + anomStk / 100 )
          
        } else {
          
          minT <- quantile(wclStk, 0.0002, na.rm=T)
          wclStk[which(wclStk[] < minT)] = minT
          
          down <- wclStk + anomStk
          
        }
        
        ## Escribir archivos de downscaling
        writeRaster(down, oFile, format="GTiff", overwrite=T, datatype='INT2S')
        
        jpeg(paste(verDir, "/", var, "_", j, ".jpg", sep=""),width = 800, height = 500)
          plot(down)
        dev.off()
        
      }

#     }
   
  }

}

bioclim_calc <- function(bDir, rcp, ext){
  
  require(dismo)
  require(raster)
  # require(ncdf)
  
  # Main directory
  downDir <- paste0(bDir, "/", rcp)
  
  # Stack by variables
  prec_stk <- stack(paste0(downDir, "/prec_", 1:12, ".", ext))
  tmin_stk <- stack(paste0(downDir, "/tmin_", 1:12, ".", ext))
  tmax_stk <- stack(paste0(downDir, "/tmax_", 1:12, ".", ext))
  
  # Bioclim variables calculation using dismo package
  bios <- biovars(prec_stk, tmin_stk, tmax_stk)  
  
  for(i in 1:19){
    
    cat("Writting bio", i)
    bioAsc <- writeRaster(bios[[i]], paste0(downDir, "/bio_", i, ".ext"), format="GTiff", overwrite=F, datatype='INT2S')
    cat(" .. done")
  }
  
}
