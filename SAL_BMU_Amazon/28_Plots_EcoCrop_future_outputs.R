# Carlos Navarro 
# CIAT - CCAFS
# January 2017

##############################
###### EcoCrop Plots  ########
##############################

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)

# Set params
bDir <- "Z:/DATA/WP2/05_EcoCrop_runs/outputs"
oDir <- "Z:/DATA/WP2/05_EcoCrop_runs/evaluation/suit_change"
uDir <- "Z:/DATA/WP2/05_EcoCrop_runs/uncertainties"
mask <- readOGR("Z:/GEODATA/AMAZON/SHAPEFILES/Ecoregions/tnc_terrestial_ecoregions_napo.shp", layer= "tnc_terrestial_ecoregions_napo")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
yearLs <- c("2020_2049", "2040_2069", "2070_2099")
id <- c("2030s", "2050s", "2080s")

# List of simulated crops 
# cropLs <- list.dirs(path = bDir, full.names = F, recursive = F)
cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "bean", "rice", "cocoa", "sugar_cane", "panela_cane")

# Temporal dir for raster library
if (!file.exists(paste0(oDir, "/tmp"))) {dir.create(paste0(oDir, "/tmp"), recursive = TRUE)}
rasterOptions(tmpdir= paste0(oDir, "/tmp"))

for(crop in cropLs){
  
  for (rcp in rcpLs){
    
    if (!file.exists(paste(oDir, "/suitchg_", crop, "_", rcp, ".tif", sep=""))){
      
      # Load current suitability 
      suitH <- raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif"))
      
      
      # Current suitability plot
      
      if (!file.exists(paste(oDir, "/suit_", crop, ".tif", sep=""))){
        
        suitH_crop <- mask(crop(suitH, extent(mask)), mask)
        
        # Set limits
        suitH_crop[which(suitH_crop[]< 0)] = 0
        suitH_crop[which(suitH_crop[]> 100) ] = 100
        
        # Plot settings
        plot <- setZ(suitH_crop, c("current"))
        names(plot) <- c("current")
        zvalues <- seq(0, 100, 10)
        myTheme <- BuRdTheme()
        myTheme$regions$col=colorRampPalette(c("snow", "yellowgreen", "forestgreen"))(length(zvalues)-1) # Set new colors
        myTheme$strip.border$col = "white"
        myTheme$axis.line$col = 'white'
        
        # Plot via levelplot
        tiff(paste(oDir, "/suit_", crop, ".tif", sep=""), width=400, height=1000, pointsize=8, compression='lzw',res=100)
        print(levelplot(plot, at = zvalues, margin=FALSE, auto.key=FALSE, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask, col="grey", lwd=0.5)))
        dev.off()
        
      }
      
      if(!file.exists(paste(oDir, "/suitchg_", crop, "_", rcp, ".tif", sep=""))){
        
        # Load future suitability in stack (all years by rcp)
        suitF <- stack(paste0(uDir, "/mean_", crop, "_", rcp, "_", yearLs, ".tif"))
        
        # Change calculation
        suitC <- suitF - suitH
        suitC_crop <- mask(crop(suitC, extent(mask)), mask)
        
        # Set limits
        suitC_crop[which(suitC_crop[]< (-40))] = (-40)
        suitC_crop[which(suitC_crop[]> 40) ] = 40
        
        # Plot settings
        plot <- setZ(suitC_crop, id)
        names(plot) <- id
        zvalues <- seq(-40, 40, 5)
        myTheme <- BuRdTheme()
        myTheme$regions$col=colorRampPalette(c("darkred", "red", "orange", "snow", "yellowgreen", "forestgreen", "darkolivegreen"))(length(zvalues)-1)
        myTheme$strip.border$col = "white"
        myTheme$axis.line$col = 'white'
        
        # Plot via levelplot
        tiff(paste(oDir, "/suitchg_", crop, "_", rcp, ".tif", sep=""), width=1200, height=1000, pointsize=8, compression='lzw',res=100)
        print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(3, 1), xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom")) + layer(sp.polygons(mask, col="grey", lwd=0.5)))
        dev.off()
        
      }
      
    }
    
  }
  
}






# Plot uncertainty
uDir <- "Z:/DATA/WP2/05_EcoCrop_runs/uncertainties"
cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "bean", "rice", "cocoa", "sugar_cane", "panela_cane")
oDir <- "Z:/DATA/WP2/05_EcoCrop_runs/evaluation/uncertainties"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069", "2070_2099")

stats <- c("mean", "mean-bottom25p", "mean-top25p", "agreement", "sd")

if(!file.exists(oDir)){
  dir.create(oDir, recursive = T)
}

for (rcp in rcpLs){
  
  for (period in periodLs){
    
    for(stati in stats){
      
      if (stati == "agreement"){
        zvalues <- seq(0, 16, 1) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("yellow", "orange", "blue")) # Set new colors
        
      } else if (stati == "sd"){
        zvalues <- seq(0, 50, 5) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("white", "black")) # Set new colors
        
      } else {
        
        zvalues <- seq(0, 100, 10) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("red", "white", "darkgreen")) # Set new colors
      }
      
      stk_crop <- stack(paste0(uDir, "/", stati, "_", cropLs, "_", rcp, "_", period, ".tif"))
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      
      tiff(paste(oDir, "/", stati, "-", rcp, "-", period, ".tif", sep=""), width=1000, height=400, pointsize=8, compression='lzw',res=100)
      print(levelplot(plot, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"), main=paste0(stati, " ", rcp, " ", period)) + layer(sp.polygons(mask)))
      dev.off()
      
      cat(stati, rcp, period, "\n")
      
    }
  }
}



