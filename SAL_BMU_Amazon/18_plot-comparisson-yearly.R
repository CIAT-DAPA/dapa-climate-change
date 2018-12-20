# Carlos Navarro 
# CIAT - CCAFS
# November 2012

#############################
#### 01 Plots by months  ####
#############################

# Load libraries
require(rasterVis)
require(maptools)
require(rgdal)

# Set params
bDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/average"
oDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/performance"
years <- c("1981_2010")
varList <- c("dtr", "prec", "tmax", "tmin", "tmean")
id <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
mask <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")

# Temporal dir for raster library
if (!file.exists(paste0(oDir, "/tmp"))) {dir.create(paste0(oDir, "/tmp"), recursive = TRUE)}
rasterOptions(tmpdir= paste0(oDir, "/tmp"))

setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}

for (yr in years){
  
  for (var in varList){
    
    stk <- stack(paste0(bDir, "/", var, "_", 1:12, ".tif"))
    # stk_crop <- mask(crop(stk, extent(mask)), mask)
    stk_crop <- stk
    
    if (var == "prec"){
      
      stk_crop[which(stk_crop[]>1300)]=1300
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      zvalues <- c(0, 50, 100, 200, 300, 500, 600, 800, 1000, 1300)  # Define limits
      myTheme <- BuRdTheme() # Define squeme of colors
      myTheme$regions$col=colorRampPalette(c("orange", "snow", "blue", "darkblue","magenta"))(length(zvalues)-1) # Set new colors
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      myTheme=rasterTheme(region=brewer.pal('YlGnBu', n=9))
      
    } else if ( var == "rhum") {
      
      stk_crop <- stk_crop
      stk_crop[which(stk_crop[]>100)]=100
      stk_crop[which(stk_crop[]<60)]=60
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      zvalues <- seq(60, 100, 5)
      # zvalues <- c(-10, -5, 0, 5, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("burlywood","snow", "deepskyblue", "darkcyan"))(length(zvalues)-1) # Set new colors
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
      
    } else if ( var == "dtr") {
      
      stk_crop <- stk_crop
      stk_crop <- stk_crop / 10
      stk_crop[which(stk_crop[]>16)]= 16
      
      plot <- setZ(stk_crop, id)
      names(plot) <- toupper(id)
      zvalues <- seq(0, 16, 1)
      # zvalues <- c(-10, -5, 0, 5, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("snow", "yellow", "orange", "red", "darkred"))(length(zvalues)-1) # Set new colors
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
      
    } else {
      
      stk_crop <- stk_crop / 10
      stk_crop[which(stk_crop[]< (6) )]= (6)
      stk_crop[which(stk_crop[]>38)]= 38
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      zvalues <- seq(6, 38, 2)
      # zvalues <- c(-8, -4, 0, 4, 8, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 36)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("darkblue", "snow", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
      
    }
    
    tiff(paste(oDir, "/plot_mths_", var, "_v2.tif", sep=""), width=1200, height=1400, pointsize=8, compression='lzw',res=100)
    
    print(levelplot(plot, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"))) + layer(sp.polygons(mask, col="white")) # + layer(sp.polygons(geotopo)))
    
    dev.off()
    
  } 
}



#############################
#### 02 Plots by seasons ####
#############################

# Load libraries
require(raster)
require(rasterVis)
require(maptools)
require(rgdal)
library(grid)

# Set params
bDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/average"
oDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/performance"
years <- c("1981_2010")
varList <- c("prec", "tmax", "tmin", "tmean")
id <- c("djf", "mam", "jja", "son")
mask <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")
# mask <- readOGR("Z:/WORK_PACKAGES/WP2/00_geodata/Ecoregions/tnc_terrestial_ecoregions_napo.shp", layer= "tnc_terrestial_ecoregions_napo")


# Temporal dir for raster library
if (!file.exists(paste0(oDir, "/tmp"))) {dir.create(paste0(oDir, "/tmp"), recursive = TRUE)}
rasterOptions(tmpdir= paste0(oDir, "/tmp"))

setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}

for (yr in years){
  
  for (var in varList){
  
    stk <- stack(paste0(bDir, "/", var, "_", id, ".tif"))
    stk_crop <- stk
    # stk_crop <- mask(crop(stk, extent(mask)), mask)
    
    if (var == "prec"){
      
      stk_crop[which(stk_crop[]>3500)]=3500
      
      plot <- setZ(stk_crop, id)
      names(plot) <- toupper(id)
      
      zvalues <- c(0, 100, 200, 300, 500, 750, 1000, 1500, 2000, 2500, 3000, 3500)  # Define limits
      # zvalues <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, 625, 750, 875, 1000, 1250, 1500, 1750, 2000, 2250, 2500, 3000, 3500)
      myTheme <- BuRdTheme() # Define squeme of colors
      # myTheme$regions$col=colorRampPalette(c("orange", "snow", "blue", "darkblue","magenta"))(length(zvalues)-1) # Set new colors
      myTheme=rasterTheme(region=brewer.pal('YlGnBu', n=9))
      
    } else if ( var == "rhum") {
      
      stk_crop <- stk_crop
      stk_crop[which(stk_crop[]>100)]=100
      stk_crop[which(stk_crop[]<60)]=60
      
      plot <- setZ(stk_crop, id)
      names(plot) <- toupper(id)
      zvalues <- seq(60, 100, 5)
      # zvalues <- c(-10, -5, 0, 5, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("burlywood","snow", "deepskyblue", "darkcyan"))(length(zvalues)-1) # Set new colors
      # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
      
    } else if ( var == "dtr") {
      
      stk_crop <- stk_crop / 10
      stk_crop[which(stk_crop[]>16)]= 16
      
      plot <- setZ(stk_crop, id)
      names(plot) <- toupper(id)
      zvalues <- seq(0, 16, 1)
      # zvalues <- c(-10, -5, 0, 5, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("snow", "yellow", "orange", "red", "darkred"))(length(zvalues)-1) # Set new colors
      
      
      
    } else {
      
      stk_crop <- stk_crop / 10
      stk_crop[which(stk_crop[]< -10 )]= (-10)
      stk_crop[which(stk_crop[]>35)]= 35
      
      plot <- setZ(stk_crop, id)
      names(plot) <- toupper(id)
      zvalues <- seq(-10, 35, 2.5)
      # zvalues <- c(-10, -5, -2, 0, 2, 5, 10, 15, 20, 25, 30, 35)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("darkblue", "snow", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
      myTheme=rasterTheme(region=rev(brewer.pal('Spectral', n=11)))
      
    }
    
    tiff(paste(oDir, "/plot_seasons_", var, "_v2.tif", sep=""), width=3000, height=1500, compression='lzw',res=300)
    
    print(levelplot(plot, at = zvalues, 
                    layout=c(4, 1), 
                    xlab="", 
                    ylab="", 
                    par.settings = myTheme, 
                    colorkey = list(space = "bottom")
    )
    + layer(sp.polygons(mask, col= "white", lwd=1))
    )
    
    if (var == "prec"){
      grid.text(expression("mm"), 0.2, 0, hjust=6.5, vjust=-6, gp=gpar(fontsize=12))  
    } else {
      grid.text(expression("°C"), 0.2, 0, hjust=9, vjust=-4, gp=gpar(fontsize=12))  
    }
    
    
    dev.off()
    
  } 
}



