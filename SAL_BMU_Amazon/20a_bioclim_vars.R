######################################
#### Purpose: Calcs bios SAL project
#### Author: Carlos Navarro
#### Email: c.e.navarro@cgiar.org
######################################

# Load libraries
require(raster)
require(maptools)
require(rgdal)
require(dismo)

# Set params
cDir <- "Z:/WORK_PACKAGES/WP2/02_Gridded_data/baseline_2_5min_v2/average"
fDir <- "Z:/WORK_PACKAGES/WP2/03_Future_data/downscaling_bsl_2_5min"
varLs <- c("prec", "tmax", "tmin")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
prdLs <- c("2020_2049", "2040_2069", "2070_2099")

cat(paste("\nCalcs bios current"))

# Load current climate files
prec_stk <- stack(paste0(cDir, "/prec_", 1:12, ".tif"))
tmin_stk <- stack(paste0(cDir, "/tmin_", 1:12, ".tif")) / 10
tmax_stk <- stack(paste0(cDir, "/tmax_", 1:12, ".tif")) / 10

## Calculate bioclimatic variables
bios <- biovars(prec_stk, tmin_stk, tmax_stk)  
for(i in 1:19){ writeRaster(bios[[i]], paste0(cDir, "/bio_", i, ".tif"), overwrite=T) }


cat(paste("\nCalcs bios future"))

for (rcp in rcpLs){
  
  gcmList <- list.dirs(paste0(fDir, "/", rcp),recursive = FALSE, full.names = FALSE)
  gcmList <- gcmList [! gcmList %in% "ensemble"]
  
  for (gcm in gcmList){
    
    for (prd in prdLs){
      
      # Load future climate files
      prec_stk <- stack(paste0(fDir, "/", rcp, "/", gcm, "/", prd, "/prec_", 1:12, ".tif"))
      tmin_stk <- stack(paste0(fDir, "/", rcp, "/", gcm, "/", prd, "/tmin_", 1:12, ".tif")) / 10
      tmax_stk <- stack(paste0(fDir, "/", rcp, "/", gcm, "/", prd, "/tmax_", 1:12, ".tif")) / 10
      
      ## Calculate bioclimatic variables
      bios <- biovars(prec_stk, tmin_stk, tmax_stk)
      for(i in 1:19){ writeRaster(bios[[i]], paste0(fDir, "/", rcp, "/", gcm, "/", prd, "/bio_", i, ".tif"), overwrite=T) }
      
      cat(paste("\n >. Calcs bios", rcp, prd, gcm, "done"))
      
      }
    
    
  }
  
}

