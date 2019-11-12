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
library(grid)

rcpList <- c("rcp26", "rcp45", "rcp85")
baseDir <- "D:/OneDrive - CGIAR/CIAT/Projects/lat_sal/03_Future_data/anomalies_2_5min"
perList <- c("2020_2049", "2040_2069", "2070_2099")
varList <- c("tmin", "tmax")
seasons <- c("djf", "mam", "jja", "son")
id <- c("DJF", "MAM", "JJA", "SON", " ", " ", " ", " ", " ", " ", " ", " " )#, "DJF 2050s", "MAM 2050s", "JJA 2050s", "SON 2050s", "DJF 2080s", "MAM 2080s", "JJA 2080s", "SON 2080s")
# mask <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/tnc_terrestial_ecoregions_napo.shp", layer= "tnc_terrestial_ecoregions_napo")
mask <- readOGR("D:/OneDrive - CGIAR/CIAT/Projects/lat_sal/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")
oDir <- "D:/OneDrive - CGIAR/CIAT/Projects/lat_sal/04_Evaluations/02_changes_for_future"

for (rcp in rcpList) {
  
  for (var in varList){
    
    ensDir <- paste0(baseDir, "/", rcp, "/ensemble")
    
    stk <- stack()
      for (period in perList){
      
      stk <- stack(stk, stack(paste0(ensDir, "/", period, "/", var, "_", seasons, ".tif")))
      }
      
      stk_crop <- mask(crop(stk, extent(mask)), mask)
      # stk_crop <- stk
      
      if (var == "prec"){
        
        stk_crop[stk_crop > 50] = 50
        stk_crop[stk_crop < (-30)] = (-30)
        
        plot <- setZ(stk_crop, id)
        names(plot) <- id
        
        zvalues <- seq(-30, 50, 5) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("#b2182b", "#d6604d", "#f4a582", "#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#053061"))(length(zvalues)-1) # Set new colors
        # myTheme$strip.border$col = "white" # Eliminate frame from maps
        # myTheme$axis.line$col = 'white' # Eliminate frame from maps
        # myTheme <- rasterTheme(region=brewer.pal('RdBu', n=11))
        
      } else {
        
        stk_crop <- stk_crop / 10
        stk_crop[stk_crop >6 ] = 6
        
        plot <- setZ(stk_crop, id)
        names(plot) <- id
        
        zvalues <- seq(0, 6, 0.5)
        # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
        myTheme <- BuRdTheme()
        # myTheme$regions$col=colorRampPalette(c("snow","yellow","orange", "red", "darkred"))(length(zvalues)-1)
        # myTheme$strip.border$col = "white"
        # myTheme$axis.line$col = 'white'
        myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))
        
      } 
      
      tiff(paste(oDir, "/plot_seasons_", var, "_", rcp, "_v2.tif", sep=""), width=3000, height=3630, compression='lzw',res=300)
      
      print(levelplot(plot, at = zvalues, 
                      layout=c(4, 3), 
                      xlab="", 
                      ylab="", 
                      par.settings = myTheme, 
                      colorkey = list(space = "bottom")
                      )
      + layer(sp.polygons(mask, col= "white", lwd=1))
      )

      if (var == "prec"){
        grid.text(expression("%"), 0.2, 0, hjust=11, vjust=-4.5, gp=gpar(fontsize=12))  
      } else {
        grid.text(expression("°C"), 0.2, 0, hjust=9, vjust=-4, gp=gpar(fontsize=12))  
      }
      
      dev.off()
    
    }
  } 




mask <- readOGR("Z:/WORK_PACKAGES/WP2/00_geodata/tnc_terrestial_ecoregions_napo.shp", layer= "tnc_terrestial_ecoregions_napo")
stats <- c()
current <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/average"
period <- perList[3]

for (var in varList){
  
  
  for (i in 1:length(mask) ){
    
    
    stk <- stack()
    for (rcp in rcpList){
      ensDir <- paste0(baseDir, "/", rcp, "/ensemble")
      stk <- stack(stk, stack(paste0(ensDir, "/", perList, "/", var, "_ann.tif"))) 
    }
    
    rg <- mask[mask$ECO_NUM == as.vector(mask$ECO_NUM)[i], ]
    
    stk_crop <- mask(crop(stk, extent(rg)), rg)
    
    rs_current <- mask(crop(raster(paste0(current, "/", var, "_ann.tif")), extent(rg)), rg)
    
    stats <- rbind(stats, 
                   cbind(paste(rg$ECO_NAME), names(rs_current), cellStats(rs_current, mean)/10),
                   cbind(paste(rg$ECO_NAME), names(stk_crop), cellStats(stk_crop, mean)/10) )
    
  } 
  
  
}

write.csv(stats, paste(oDir, "/plot_seasons_stats.csv", sep=""))




## Plot Uncertainties

## Anual Temperature Uncertainty: tmean, Std, 2050, C Deg, rcp85
## GCM and RCP

rcpLs <- c("rcp26", "rcp45", "rcp85")
rcp <- "rcp85"
var <- "prec"
period <- "2040_2069"
gcm <- "ensemble"
season <- "ann"

iDir <- "D:/OneDrive - CGIAR/CIAT/Projects/lat_sal/03_Future_data/anomalies_2_5min"
fun <- function(x) { sd(x) }

stdGCM <- raster(paste0(iDir, "/", rcp, "/", gcm, "/", period, "/", var, "_", season, ".tif")) 
stdRCP <- calc(stack(paste0(iDir, "/", rcpLs, "/", gcm, "/", period, "/", var, "_", season, ".tif")) , fun)

stdStk <- stack(stdGCM, stdRCP)
id <- c("GCM", "RCP")
## Plot
stk_crop <- mask(crop(stdStk, extent(mask)), mask)

if (var == "prec"){
  
  stk_crop <- stk_crop * 100
  
  plot <- setZ(stk_crop, id)
  names(plot) <- id
  
  zvalues <- seq(-10, 20, 5) # Define limits
  myTheme <- BuRdTheme() # Define squeme of colors
  # myTheme$regions$col=colorRampPalette(c("#b2182b", "#d6604d", "#f4a582", "#f7f7f7","#d1e5f0","#92c5de","#4393c3","#2166ac", "#053061"))(length(zvalues)-1) # Set new colors
  # myTheme$strip.border$col = "white" # Eliminate frame from maps
  # myTheme$axis.line$col = 'white' # Eliminate frame from maps
  myTheme <- rasterTheme(region=brewer.pal('Greys', n=9))
  
} else {
  
  stk_crop <- stk_crop / 10

  plot <- setZ(stk_crop, id)
  names(plot) <- id
  
  zvalues <- seq(0, 3, 0.5)
  # zvalues <- c(0, 0.25, 0.5, 0.75, 1, 1.5, 2, 2.5, 3, 3.5, 4, 5, 6)
  myTheme <- BuRdTheme()
  # myTheme$regions$col=colorRampPalette(c("snow","yellow","orange", "red", "darkred"))(length(zvalues)-1)
  # myTheme$strip.border$col = "white"
  # myTheme$axis.line$col = 'white'
  myTheme=rasterTheme(region=brewer.pal('Greys', n=5))
  
} 

tiff(paste(oDir, "/plot_std_gcm-rcm_", var, ".tif", sep=""), width=1400, height=1300, compression='lzw',res=300)

print(levelplot(plot, at = zvalues, 
                layout=c(2, 1), 
                # scales = list(draw=FALSE), 
                xlab="", 
                ylab="", 
                # par.strip.text=list(cex=0),
                par.settings = myTheme, 
                colorkey = list(space = "bottom")
)
+ layer(sp.polygons(mask, col= "white", lwd=1))
)

if (var == "prec"){
  grid.text(expression("%"), 0.2, 0, hjust=3, vjust=-3.5, gp=gpar(fontsize=12))  
} else {
  grid.text(expression("°C"), 0.2, 0, hjust=3, vjust=-3.5, gp=gpar(fontsize=12))  
}

dev.off()

