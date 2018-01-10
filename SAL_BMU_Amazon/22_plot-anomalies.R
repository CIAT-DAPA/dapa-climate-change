# Carlos Navarro 
# CIAT - CCAFS
# November 2012


#######################################
#### 01 Plots anomalies by seasons ####
#######################################

# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)

rcpList <- c("rcp26", "rcp45", "rcp60","rcp85")
baseDir <- "Z:/DATA/WP2/03_Future_data/anomalies_2_5min"
perList <- c("2020_2049", "2040_2069", "2070_2099")
varList <- c("prec", "tmin", "tmax")
seasons <- c("djf", "mam", "jja", "son")
id <- c("DJF 2030s", "MAM 2030s", "JJA 2030s", "SON 2030s", "DJF 2050s", "MAM 2050s", "JJA 2050s", "SON 2050s", "DJF 2080s", "MAM 2080s", "JJA 2080s", "SON 2080s")
mask <- readOGR("Z:/GEODATA/AMAZON/SHAPEFILES/Ecoregions/tnc_terrestial_ecoregions_napo.shp", layer= "tnc_terrestial_ecoregions_napo")
oDir <- "Z:/DATA/WP2/04_Evaluations/02_changes_for_future"

for (rcp in rcpList) {
  
  for (var in varList){
    
    ensDir <- paste0(baseDir, "/", rcp, "/ensemble")
    
    stk <- stack()
    for (period in perList){
      stk <- stack(stk, stack(paste0(ensDir, "/", period, "/", var, "_", seasons, ".tif")))
    }
    
    stk_crop <- mask(crop(stk, extent(mask)), mask)
    
    if (var == "prec"){
      
      stk_crop[stk_crop > 40] = 40
      stk_crop[stk_crop < (-40)] = (-40)
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      zvalues <- seq(-40, 40, 5) # Define limits
      myTheme <- BuRdTheme() # Define squeme of colors
      myTheme$regions$col=colorRampPalette(c("darkred","red","snow","blue", "darkblue"))(length(zvalues)-1) # Set new colors
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
      
    } else {
      
      stk_crop <- stk_crop / 10
      stk_crop[stk_crop >5 ] = 5
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      zvalues <- seq(0, 5, 0.5)
      # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("snow","yellow","orange", "red", "darkred"))(length(zvalues)-1)
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
      
    } 
    
    tiff(paste(oDir, "/plot_seasons_", var, "_", rcp, ".tif", sep=""), width=1200, height=2400, pointsize=8, compression='lzw',res=100)
    
    print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"))) # + layer(sp.polygons(mask), col="grey")) # + layer(sp.polygons(geotopo, fill='white', alpha=0.3)))
    
    dev.off()
    
  } 
}



