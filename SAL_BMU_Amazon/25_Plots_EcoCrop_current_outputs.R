# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)
# library(RColorBrewer)

# Set params
bDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/outputs"
oDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/evaluation/suit_calibration"
mask <- raster("Z:/WORK_PACKAGES/WP2/00_zones/region/5km/alt-prj-amz.asc")
mask_ctr <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")
# mask_napo <- readOGR("Z:/GEODATA/AMAZON/SHAPEFILES/Ecoregions/tnc_terrestial_ecoregions_napo.shp", layer= "tnc_terrestial_ecoregions_napo")
yearLs <- c("2020_2049", "2040_2069", "2070_2099")
id <- c("2030s", "2050s", "2080s")

monfDir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/crop_data/cropdist_monfreda/raw_grids"
occDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/ocurrences"
ispamDir <- "U:/cropdata/ispam/iSPAM_V3R6/SPAM_harv-area"

if(!file.exists(oDir)){
  dir.create(oDir, recursive = T)
}


# List of simulated crops 
# cropLs <- list.dirs(path = bDir, full.names = F, recursive = F)
cropLs <- c("cocoa", "sugar_cane", "sugar_cane_eitzinger", "panela_cane", "coffee", "coffee_eitzinger", "palmito") #"panela_cane"

# my.palette <- brewer.pal(n = 9, name = "RdYlGn")
colfunc <- colorRampPalette(c("red", "yellow", "green3"))

for(crop in cropLs){
  
  if (crop == "bean") { 
    cropmod <- "beans"
  } else if (crop == "sugar_cane" || crop == "panela_cane") {
    cropmod <- "sugarcane"
  } else {
    cropmod <- strsplit(crop, "_")[[1]][1]
  }
  
  cat(cropmod, "\n")
  # if(cropmod == "plantain"){ 
  
  # Load current suitability 
  suitH <- raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif"))
  suitH[which(suitH[] == 0)] <- NA
  
  # Plot with monfreda
  tiff(paste(oDir, "/suitocc_", crop, ".tif", sep=""), width=1400, height=2000, pointsize=8, compression='lzw',res=200)
  
  # Plot
  plot(suitH, col=colfunc(10), ext = extent(mask))
  # plot(monf, add=T, alpha=0.70, legend= F)
  
  # Load occurrences gbif 
  if(file.exists(paste0(occDir, "/gbif_", cropmod, ".csv"))){
    occ <- read.csv(paste0(occDir, "/gbif_", cropmod, ".csv"), header = T)
    occ <- occ[which(occ$decimallongitude >= xmin(mask) & occ$decimallongitude <= xmax(mask) &
                       occ$decimallatitude >= ymin(mask) & occ$decimallatitude <= ymax(mask)), ]
    points(cbind(occ$decimallongitude, occ$decimallatitude), cex=0.1)
  }
  
  # Load monfreda presence area 
  if(file.exists(paste0(monfDir, "/", cropmod, "_ha"))){
    monf <- raster(paste0(monfDir, "/", cropmod, "_ha"))
    monf <- crop(monf, extent(mask))
    monf[which(monf[] <= 0.01)] = NA
    points(rasterToPoints(monf), cex=0.1)
  }
  
  # Load ispam presence area (if exits)
  if(file.exists(paste0(ispamDir, "/spam2000v3r6_harvested-area_total_", cropmod, ".asc"))){
    
    ispam <- raster(paste0(ispamDir, "/spam2000v3r6_harvested-area_total_", cropmod, ".asc"))
    ispam <- crop(ispam, extent(mask))
    ispam[which(ispam[] <= 100)] = NA
    
    # Plot with ispam
    # tiff(paste(oDir, "/suitoccispam_", crop, ".tif", sep=""), width=1200, height=1000, pointsize=8, compression='lzw',res=100)
    # plot(suitH)
    
    # plot(ispam, add=T, alpha=0.70, legend= F)
    points(rasterToPoints(ispam), cex=0.1, col = "blue")
    # points(cbind(occ$decimallongitude, occ$decimallatitude), cex=0.1)
    
    # plot(mask_ctr, add=T)
    
    # dev.off()
    
  }
  
  if(cropmod == "plantain"){
    plot(readOGR(paste0(occDir, "/banana_systems.shp"), layer="banana_systems"), lwd=0.5, border="red", add=T)
  }
  
  plot(mask_ctr, add=T, lwd=0.2)
#   plot(mask_napo, add=T, lwd=0.2)
  dev.off()
    
# } 
}