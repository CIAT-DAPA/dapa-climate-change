### Author : Carlos Navarro c.e.navarro@cgiar.org
### Date : Jan 2016

#############################################################################
################### CALCULATE BIOCLIMATIC VARIABLES #########################
#############################################################################
# 
# source("06_bioclim_calcs.R")
# bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/downscaling/llanos"
# rcp <- "rcp45"
# ext <- "nc"
# otp <- bioclim_calc(bDir, rcp, ext)

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
    bioAsc <- writeRaster(bios[[i]], paste0(downDir, "/bio_", i, ".asc"))
    cat(" .. done")
  }
  
}
