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
bDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/outputs"
oDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/evaluation/blogpost"
zDir <- "Z:/WORK_PACKAGES/WP2/00_zones"
uDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/uncertainties"

rcp <- "rcp45"
year <- "2040_2069"
id <- c("Idoneidad Actual ", "Idoneidad Futura 2050")

# List of simulated crops 
cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german")
cropNameLs <- c("Maíz", "Yuca", "Plátano")
regLs <- c("col", "per")

buf = 1

for (reg in regLs){
  
  if (reg == "per"){
    depto <- "loreto"
  } else {
    depto <- "caqueta"
  }
  
  pilot <- readOGR( paste0(zDir, "/01_pilot_zones/", reg, "_prj.shp"), layer = paste0(reg, "_prj") )
  mask <- readOGR( paste0(zDir, "/", reg, "_", depto, ".shp"), layer = paste0(reg, "_", depto) )
  
  # ext <- extent(extent(mask)@xmin - buf, extent(mask)@xmax + buf, extent(mask)@ymin - buf, extent(mask)@ymax + buf)
  # adm <- readOGR( paste0(zDir, "/", toupper(reg) , "1.shp"), layer = paste0(toupper(reg), "1") )
  
  for(i in 1:length(cropLs)){
    
    crop <- cropLs[i]
    cropname <- cropNameLs[i]
    
    # Load current suitability 
    rsStk <- stack(raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif")), 
                   raster(paste0(uDir, "/mean_", crop, "_", rcp, "_", year, ".tif")))
    
    rsStk <- mask(crop(rsStk, extent(mask)), mask)
    
    # Set limits
    rsStk[which(rsStk[]< 0)] = 0
    rsStk[which(rsStk[]> 100) ] = 100
    # rsStk[which(rsStk[] == 0)] = NA
    
    
    if(!file.exists(paste(oDir, "/suit_comp_", cropname, "_", reg, "_", depto, ".tif", sep=""))){
      
      # Plot settings
      plot <- setZ(rsStk, id)
      names(plot) <- id
      zvalues <- seq(0, 100, 5)
      myTheme <- BuRdTheme()
      myTheme$regions$col=colorRampPalette(c("darkred", "red", "orange", "yellowgreen", "forestgreen", "darkgreen"))(length(zvalues)-1)
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      
      # Plot via levelplot
      if (reg == "per"){
        tiff(paste(oDir, "/suit_comp_", tolower(cropname), "_", reg, ".tif", sep=""), width=600, height=400, pointsize=8, compression='lzw',res=100)
      } else {
        tiff(paste(oDir, "/suit_comp_", tolower(cropname), "_", reg, ".tif", sep=""), width=800, height=400, pointsize=8, compression='lzw',res=100)
      }
      
      
      print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(2, 1), xlab="", ylab="", main=cropname, par.settings = myTheme, colorkey = list(space = "bottom", title=expression("%"))) 
            # + layer(sp.polygons(pilot, col= "red", lwd=1))
            + layer(sp.polygons(mask, lwd=1)) 
            )
      dev.off()
      
    }
    
  }
  
}

