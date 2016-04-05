# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)

## 01-Interpolated surfaces plots (Re-interpolation WorldCLim)

iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/01-skill_interpolation"
if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
varList <- c("tmax")
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

for (var in varList){
  
    stk <- stack(paste0(iDir, "/", var, "_", 1:12, ".asc"))
    stk_crop <- mask(crop(stk, extent(mask)), mask)
    
    if (var == "prec"){

      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      zvalues <- seq(0, 1200, 50) # Define limits
      myTheme <- BuRdTheme() # Define squeme of colors
      myTheme$regions$col=colorRampPalette(c("snow", "blue", "darkblue", "magenta"))(length(zvalues)-1) # Set new colors
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
      
    } else {
      
      stk_crop <- stk_crop
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      zvalues <- seq(-10, 40, 2)
      # zvalues <- c(-10, -5, 0, 5, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("darkblue", "snow", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  

    }
    
  tiff(paste(oDir, "/interpolated_llanos_", var, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=100)
      
    print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask)))
  
  dev.off()
  
} 



# 02-Anomalies 

# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)

# iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/llanos"
iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/ideam"
bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/02-anomalies"
if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}

varList <- c("prec", "tmax", "tmin")
rcpList <- c("rcp26", "rcp45", "rcp85")
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

for (rcp in rcpList){

  for (var in varList){
    
    # stk_bsl <- stack(paste0(bDir, "/", var, "_", 1:12, ".asc"))
    # stk_fut <- stack(paste0(iDir, "/", rcp, "/", var, "_", 1:12, ".asc"))
    
    # if (var == "prec"){
      
      # stk_anom <- ( stk_fut / stk_bsl ) - 1 
      
    # }else{
      
      # stk_anom <- stk_fut - stk_bsl
      
    # }
    
    stk_anom <- stack(paste0(iDir, "/", rcp, "/", var, "_", 1:12, ".tif"))
    # stk_mask <- mask(crop(stk_anom, extent(mask)), mask)
    
    if (var == "prec"){
      
      stk_crop <- stk_anom / 10
      stk_crop[stk_crop>40] = 40
      stk_crop[stk_crop<-40] = -40
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      zvalues <- seq(-40, 40, 5) # Define limits
      myTheme <- BuRdTheme() # Define squeme of colors
      myTheme$regions$col=colorRampPalette(c("darkred","red","snow","blue", "darkblue"))(length(zvalues)-1) # Set new colors
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
      
    } else {
      
      stk_crop <- stk_anom/10
      stk_crop[stk_crop>4] = 4
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("snow","yellow","orange", "red", "darkred"))(length(zvalues)-1)
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
      
    }
    
    tiff(paste(oDir, "/anomalies_llanos_", rcp, "_", var, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=130)
    
    print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask)))
    
    dev.off()
    
  } 
  
}



## 03- Downscaled 

# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)

iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/llanos"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/02-anomalies"
if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
varList <- c("prec", "tmax", "tmin")
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

rcpList <- c("rcp26", "rcp45", "rcp85")
id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

for (rcp in rcpList){
  
  for (var in varList){
    
    stk_fut <- stack(paste0(iDir, "/", rcp, "/", var, "_", 1:12, ".tif"))
    stk_mask <- mask(crop(stk_fut, extent(mask)), mask)
    
    if (var == "prec"){
      
      plot <- setZ(stk_fut, id)
      names(plot) <- id
      
      zvalues <- seq(0, 1200, 50) # Define limits
      myTheme <- BuRdTheme() # Define squeme of colors
      myTheme$regions$col=colorRampPalette(c("snow", "blue", "darkblue", "magenta"))(length(zvalues)-1) # Set new colors
      # myTheme$regions$col=colorRampPalette(c("snow","blue"))(length(zvalues)-1) # Set new colors
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
      
    } else {
      
      stk_crop <- stk_fut/10
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      zvalues <- seq(-10, 40, 2)
      # zvalues <- c(-10, -5, 0, 5, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
      myTheme <- BuRdTheme()
      # myTheme$regions$col=colorRampPalette(c("yellow","orange", "red", "brown"))(length(zvalues)-1)
      myTheme$regions$col=colorRampPalette(c("darkblue", "snow", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
      
    }
    
    tiff(paste(oDir, "/downscaled_llanos_", rcp, "_", var, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=100)
      print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask)))
    dev.off()
    
  }
  
}





## Scatter plot comparisson
a <- raster("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/monthly-interpolations/outputs/tmax/fold-1/tile-1/tmax_1.asc")
coord <- read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/monthly-interpolations/outputs/tmax/fold-1/tmax_lla.csv")
coords <- cbind(coord[,8], coord[,7])
pts_mod <- extract(a, coords)
plot(coord$JAN, pts_mod)
abline(0, 1, add=T)


