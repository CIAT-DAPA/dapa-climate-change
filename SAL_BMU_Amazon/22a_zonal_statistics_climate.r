#######################################################################################
#### Purpose: Statistics of SAL climate projections for future
#### Author: Carlos Navarro
#### Email: c.e.navarro@cgiar.org
#######################################################################################

# Load libraries
require(raster)
require(maptools)
require(rgdal)
require(dismo)

# Set params
cDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/average"
fDir <- "Z:/WORK_PACKAGES/WP2/03_Future_data/anomalies_2_5min"
varLs <- c("prec", "tmax", "tmin")
oDir <- "Z:/WORK_PACKAGES/WP2/04_Evaluations/03_climate_statistics"
# mask <- "Z:/WORK_PACKAGES/WP2/00_zones/rg_poly_countries.shp"
mask <- "Z:/WORK_PACKAGES/WP2/00_geodata/tnc_terrestial_ecoregions_napo.shp"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
prdLs <- c("2020_2049", "2040_2069", "2070_2099")
id <- "ECO_NUM"

if (!file.exists(paste0(oDir))) {dir.create(paste0(oDir), recursive = TRUE)}

# Read mask
poly <- readOGR(mask) 


#####################################
### Climate statistics by month #####
#####################################

# Open empty data frame
stats <- c()

## Looping vars
for (var in varLs){
  
  cat(paste("\nCalcs stats", var))
  
  # Load current climate files
  cStk <- stack(paste0(cDir, "/", var, "_", 1:12, ".tif"))
  
  ## Rasterize polygon
  cStk_crop <- crop(cStk, extent(poly))
  extent(cStk_crop) <- extent(poly)
  poly_rs <- rasterize(poly, cStk_crop[[1]], id)
  
  ## Calculate stats for current
  cStk_stat <- zonal(cStk_crop, poly_rs, "mean")
  stats <- rbind(stats, cbind("current", "1981-2010", var, "mean", cStk_stat))
  
  cat(paste("\n >. Calcs stats current", var))
  
  for (rcp in rcpLs){
    
    for (prd in prdLs){
      
      # Load future climate files
      fStk <- stack(paste0(fDir, "/", rcp, "/ensemble/", prd, "/", var, "_", 1:12, ".tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future
      fStk_stat <- zonal(fStk_crop, poly_rs, "mean")
      stats <- rbind(stats, cbind(rcp, prd, var, "mean", fStk_stat))
      
      # Load future climate files (STD)
      fStk <- stack(paste0(fDir, "/", rcp, "/ensemble/", prd, "/", var, "_", 1:12, "_sd.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (STD)
      fStk_stat <- zonal(fStk_crop, poly_rs, "mean")
      stats <- rbind(stats, cbind(rcp, prd, var, "std", fStk_stat))
      
      cat(paste("\n >. Calcs stats", rcp, prd, var))
      
    }
    
  }
  
}

## Set colnames stat table
colnames(stats) <- c("RCP", "Period", "Variable", "Stat", "Zone", 1:12)

# Write the outputs
write.csv(stats, paste0(oDir, "/climate_stats_by_month.csv"), row.names=F)
 
cat("\nDone!!!")






#######################################
### Climate statistics by bioclim #####
#######################################

# Set params
cDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/average"
fDir <- "Z:/WORK_PACKAGES/WP2/03_Future_data/downscaling_bsl_2_5min_ens"
varLs <- c("bio")
oDir <- "Z:/WORK_PACKAGES/WP2/04_Evaluations/03_climate_statistics"
# mask <- "Z:/WORK_PACKAGES/WP2/00_zones/rg_poly_countries.shp"
mask <- "Z:/WORK_PACKAGES/WP2/00_geodata/tnc_terrestial_ecoregions_napo.shp"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
prdLs <- c("2020_2049", "2040_2069", "2070_2099")
id <- "ECO_NUM"

if (!file.exists(paste0(oDir))) {dir.create(paste0(oDir), recursive = TRUE)}

# Read mask
poly <- readOGR(mask) 

# Open empty data frame
stats <- c()

## Looping vars
for (var in varLs){
  
  cat(paste("\nCalcs stats", var))
  
  # Load current climate files
  cStk <- stack(paste0(cDir, "/", var, "_", 1:19, ".tif"))
  
  ## Rasterize polygon
  cStk_crop <- crop(cStk, extent(poly))
  extent(cStk_crop) <- extent(poly)
  poly_rs <- rasterize(poly, cStk_crop[[1]], id)
  
  ## Calculate stats for current
  cStk_stat <- zonal(cStk_crop, poly_rs, "mean")
  stats <- rbind(stats, cbind("current", "1981-2010", var, "mean", cStk_stat))
  
  cat(paste("\n >. Calcs stats current", var))
  
  for (rcp in rcpLs){
    
    for (prd in prdLs){
      
      # Load future climate files
      fStk <- stack(paste0(fDir, "/", rcp, "/", prd, "/", var, "_", 1:19, ".tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future
      fStk_stat <- zonal(fStk_crop, poly_rs, "mean")
      stats <- rbind(stats, cbind(rcp, prd, var, "mean", fStk_stat))
      
      # Load future climate files (STD)
      fStk <- stack(paste0(fDir, "/", rcp, "/", prd, "/", var, "_", 1:19, "_sd.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (STD)
      fStk_stat <- zonal(fStk_crop, poly_rs, "mean")
      stats <- rbind(stats, cbind(rcp, prd, var, "std", fStk_stat))
      
      # Load future climate files (Q25)
      fStk <- stack(paste0(fDir, "/", rcp, "/", prd, "/", var, "_", 1:19, "_q25.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (Q25)
      fStk_stat <- zonal(fStk_crop, poly_rs, "mean")
      stats <- rbind(stats, cbind(rcp, prd, var, "q25", fStk_stat))
      
      # Load future climate files (Q75)
      fStk <- stack(paste0(fDir, "/", rcp, "/", prd, "/", var, "_", 1:19, "_q75.tif"))
      fStk_crop <- crop(fStk, extent(poly))
      extent(fStk_crop) <- extent(poly)
      poly_rs <- rasterize(poly, fStk_crop[[1]], id)
      
      ## Calculate stats for future (Q75)
      fStk_stat <- zonal(fStk_crop, poly_rs, "mean")
      stats <- rbind(stats, cbind(rcp, prd, var, "q75", fStk_stat))
      
      cat(paste("\n >. Calcs stats", rcp, prd, var))
      
    }
    
  }
  
}

## Set colnames stat table
colnames(stats) <- c("RCP", "Period", "Variable", "Stat", "Zone", 1:19)

# Write the outputs
write.csv(stats, paste0(oDir, "/climate_stats_by_month.csv"), row.names=F)

cat("\nDone!!!")
