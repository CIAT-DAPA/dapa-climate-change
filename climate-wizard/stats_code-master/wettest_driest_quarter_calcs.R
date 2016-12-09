## Author: Carlos Navarro
## c.e.navarro@cgiar.org 
## Date: Dec 2016
## Purpose: Calculations based on wet/dry seasons

## Set libraries
require(raster)
require(ncdf4)

## Arguments
iDir <- paste0("F:/ClimateWizard/data/AR5_Global_Daily_25k/out_stats/", gcm)
rcp <- "historical" # "rcp45", "rcp85"
iY <- 1950 #2006
eY <- 2005 #2099
thr = -1.5 #Threshold severe dryness

## GCM List
gcmList <- c("ACCESS1-0", "bcc-csm1-1", "BNU-ESM", "CanESM2", "CCSM4", "CESM1-BGC", "CNRM-CM5", "CSIRO-Mk3-6-0", "ensemble", "GFDL-CM3", "GFDL-ESM2G", "GFDL-ESM2M", "inmcm4",
             "IPSL-CM5A-LR", "IPSL-CM5A-MR", "MIROC-ESM", "MIROC-ESM-CHEM", "MIROC5", "MPI-ESM-LR", "MPI-ESM-MR", "MRI-CGCM3", "NorESM1-M")

# added as fgobal institution attribute to output files
txtinst = "Santa Clara U.,Climate Central,The Nature Conservancy,International Center for Tropical Agriculture"

## Loop around GCMs
for (gcm in gcmList){
  # gcm <- "GFDL-CM3"
  wetSeasonIndices(iDir, rcp, iY, eY)
  drySeasonIndices(iDir, rcp, iY, eY)
  spi_1month(iDir, rcp, iY, eY, thr)
}


wetSeasonIndices <- function(iDir="", rcp="", iY="", eY=""){

  # iDir <- dirname(fn)
  
  # iDir <- paste0("D:/CIAT/Projects/wocat/AR5_Global_Daily_25k/out_stats/", gcm)
  # iDir <- paste0("//ccafsserver/ClimateWizard/data/AR5_Global_Daily_25k/out_stats/", gcm)
  
  pr_stk <- stack(paste0(iDir, "/PTOT_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".quarter.nc"))
  r02_stk <- stack(paste0(iDir, "/R02_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".quarter.nc"))
  tas_stk <- stack(paste0(iDir, "/tas_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".quarter.nc"))
  
  oPwet <- paste0(iDir, "/PWET_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  oR02wet <- paste0(iDir, "/R02WET_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  oSdiiwet <- paste0(iDir, "/SDIIWET_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  oTaswet <- paste0(iDir, "/TWET_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  
  if (!file.exists(pr_stk)){
    
    if (!file.exists(oTaswet)){
        
      pwet <- stack()
      r02wet <- stack()
      taswet <- stack()
      
      # Get wettest quarter for each year and then concatenate all years
      for (y in 1:(eY-iY+1)){
        
        cat("Wet calcs: year", y, "/", eY-iY, "\n")
        qSum <- stack()
        
        for (i in 1:10){
          qi <- (y-1)*10 + i
          qSum_i <- sum(pr_stk[[qi:(qi+2)]])
          qSum <- stack(qSum, qSum_i)
        }
      
        names(qSum)  <- c(paste(y,1:10))
        pwet <- stack(pwet, max(qSum))
        
        # Indicator of wettest quarter (1-10)
        qWet <- which.max(qSum)
        
        # Convert in 10 layers with 0 and 1
        qWet_stk <- stack()
        
        for (j in 1:10){
          qWet_j <- qWet
          qWet_j[which(qWet_j[]!=j)] <- NA
          qWet_j[which(qWet_j[]==j)] <- 1
          qWet_stk <- stack(qWet_stk, qWet_j)
        }
        
        r02_i <- merge(qWet_stk * r02_stk[[ (10*y-9):(10*y) ]])
        r02wet <- stack(r02wet, r02_i)
        
        tas_i <- merge(qWet_stk * tas_stk[[(10*y-9):(10*y)]])
        taswet <- stack(taswet, tas_i)
  
      }
      
      # Write PWET and set years to time axis
      writeRaster(pwet, paste0(iDir, "/pwet_tmp.nc", overwrite=T))
      system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/pwet_tmp.nc ", oPwet))
      unlink(paste(iDir, "pwet_tmp.nc"))
      
      # Edit PWET file
      nc_edit <- nc_open(oPwet, write = T)
      ncvar_rename(nc_edit, "variable", "pwet")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
      # Write R02WET and set years to time axis
      writeRaster(r02wet, paste0(iDir, "/r02wet_tmp.nc", overwrite=T))
      system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/r02wet_tmp.nc ", oR02wet))
      unlink(paste(iDir, "r02wet_tmp.nc"))
      
      # Edit R02WET file
      nc_edit <- nc_open(oR02wet, write = T)
      ncvar_rename(nc_edit, "variable", "r02wet")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
      # Write SDII 
      system(paste0("cdo mondiv ", oPwet," ", oR02wet, " ", oSdiiwet))
      
      # Edit TASWET file
      nc_edit <- nc_open(oSdiiwet, write = T)
      ncvar_rename(nc_edit, "variable", "sdiiwet")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
      # Write TASWET and set years to time axis
      writeRaster(taswet, paste0(iDir, "/taswet_tmp.nc", overwrite=T))
      system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/taswet_tmp.nc ", oTaswet))
      unlink(paste(iDir, "taswet_tmp.nc"))
      
      # Edit TASWET file
      nc_edit <- nc_open(oTaswet, write = T)
      ncvar_rename(nc_edit, "variable", "taswet")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
    }
    
  }
  
}


drySeasonIndices <- function(iDir="", rcp="", iY="", eY=""){
  
  # iDir <- dirname(fn)
  
  # iDir <- paste0("D:/CIAT/Projects/wocat/AR5_Global_Daily_25k/out_stats/", gcm)
  # iDir <- paste0("//ccafsserver/ClimateWizard/data/AR5_Global_Daily_25k/out_stats/", gcm)
  
  pr_stk <- stack(paste0(iDir, "/PTOT_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".quarter.nc"))
  r02_stk <- stack(paste0(iDir, "/R02_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".quarter.nc"))
  tas_stk <- stack(paste0(iDir, "/tas_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".quarter.nc"))
  
  oPdry <- paste0(iDir, "/PDRY_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  oR02dry <- paste0(iDir, "/R02DRY_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  oSdiidry <- paste0(iDir, "/SDIIDRY_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  oTasdry <- paste0(iDir, "/TDRY_BCSD_", rcp, "_", gcm, "_", iY, "-", eY, ".nc")
  
  if (!file.exists(pr_stk)){
    
    if (!file.exists(oTasdry)){
      
      pdry <- stack()
      r02dry <- stack()
      tasdry <- stack()
      
      # Get drytest quarter for each year and then concatenate all years
      for (y in 1:(eY-iY+1)){
        
        cat("Dry calcs: year", y, "/", eY-iY, "\n")
        qSum <- stack()
        
        for (i in 1:10){
          qi <- (y-1)*10 + i
          qSum_i <- sum(pr_stk[[qi:(qi+2)]])
          qSum <- stack(qSum, qSum_i)
        }
        
        names(qSum)  <- c(paste(y,1:10))
        pdry <- stack(pdry, min(qSum))
        
        # Indicator of drytest quarter (1-10)
        qDry <- which.min(qSum)
        
        # Convert in 10 layers with 0 and 1
        qDry_stk <- stack()
        
        for (j in 1:10){
          qDry_j <- qDry
          qDry_j[which(qDry_j[]!=j)] <- NA
          qDry_j[which(qDry_j[]==j)] <- 1
          qDry_stk <- stack(qDry_stk, qDry_j)
        }
        
        r02_i <- merge(qDry_stk * r02_stk[[ (10*y-9):(10*y) ]])
        r02dry <- stack(r02dry, r02_i)
        
        tas_i <- merge(qDry_stk * tas_stk[[(10*y-9):(10*y)]])
        tasdry <- stack(tasdry, tas_i)
        
      }
      
      # Write PDRY and set years to time axis
      writeRaster(pdry, paste0(iDir, "/pdry_tmp.nc", overwrite=T))
      system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/pdry_tmp.nc ", oPdry))
      unlink(paste(iDir, "pdry_tmp.nc"))
      
      # Edit PDRY file
      nc_edit <- nc_open(oPdry, write = T)
      ncvar_rename(nc_edit, "variable", "pdry")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
      # Write R02DRY and set years to time axis
      writeRaster(r02dry, paste0(iDir, "/r02dry_tmp.nc", overwrite=T))
      system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/r02dry_tmp.nc ", oR02dry))
      unlink(paste(iDir, "r02dry_tmp.nc"))
      
      # Edit R02DRY file
      nc_edit <- nc_open(oR02dry, write = T)
      ncvar_rename(nc_edit, "variable", "r02dry")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
      # Write SDII 
      system(paste0("cdo mondiv ", oPdry," ", oR02dry, " ", oSdiidry))
      
      # Edit TASDRY file
      nc_edit <- nc_open(oSdiidry, write = T)
      ncvar_rename(nc_edit, "variable", "sdiidry")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
      # Write TASDRY and set years to time axis
      writeRaster(tasdry, paste0(iDir, "/tasdry_tmp.nc", overwrite=T))
      system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/tasdry_tmp.nc ", oTasdry))
      unlink(paste(iDir, "tasdry_tmp.nc"))
      
      # Edit TASDRY file
      nc_edit <- nc_open(oTasdry, write = T)
      ncvar_rename(nc_edit, "variable", "tasdry")
      ncatt_put(nc_edit, 0, "Institution", txtinst)
      nc_close(nc_edit)
      
    }
    
  }
  
}


spi_1month <- function(fn="", rcp="", iY="", eY="", thr=""){
  
  ## Documentation about spi: 
  ## http://www.wamis.org/agm/pubs/SPI/WMO_1090_EN.pdf 
  ## http://www.climasig.es/metod2.html#i9
  # 0 to -0.99 Mild dryness
  # -1.00 to -1.49 Moderate dryness
  # -1.5 to -1.99 Severe dryness
  # < -2.0 Extreme dryness
  
  # iDir <- dirname(fn)
  # iDir <- paste0("D:/CIAT/Projects/wocat/AR5_Global_Daily_25k/out_stats/", gcm)
  # iDir <- paste0("//ccafsserver/ClimateWizard/data/AR5_Global_Daily_25k/out_stats/", gcm)
  
  oDroi <- paste0(iDir, "/DROI_BCSD_", rcp, "_", gcm, "_", iY, "_", eY, ".nc")
  oDrof <- paste0(iDir, "/DROF_BCSD_", rcp, "_", gcm, "_", iY, "_", eY, ".nc")
  
  nYears <- eY - iY + 1 
  pr_stk <- stack(paste0(iDir, "/PTOT_BCSD_", rcp, "_", gcm, "_", iY, "_", eY, ".monthly.nc"))
  
  
  
  pr_stk_ref <- stack(paste0(iDir, "/PTOT_BCSD_30yravg_", gcm, "_", iY, "_", eY, ".nc"))
  
  ## Anomaly month i
  
  anom_avg <- mean( pr_stk / pr_stk_ref )
  anom_std <- std( pr_stk / pr_stk_ref )
  
  anom_stk <- stack(layers=nlayers(pr_stk))
  
  spi <-  ( ( pr_stk - pr_stk_ref ) - anom_avg ) / anom_std 
  # for (i in 1:12){anom_i <- ( ( pr_stk[[seq(i, nYears * 12, 12)]] - pr_stk_ref[[i]] ) - anom_avg ) / anom_std }
  
  # Write SPI (droi) and set years to time axis
  writeRaster(spi, paste0(iDir, "/droi_tmp.nc"))
  system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/droi_tmp.nc ", oDroi))
  unlink(paste0(iDir, "/droi_tmp.nc"))
  
  # Calculate frequence of SPI
  spi_f <- spi
  spi_f[which(spi_f[]<= thr)] <- 1/12
  spi_f[which(spi_f[]> thr)] <- 0
  
  writeRaster(spi, paste0(iDir, "/drof_tmp_monthly.nc"))
  system(paste0("cdo yearsum ", iDir, "/drof_tmp_monthly.nc ", iDir, "/drof_yearly.nc "))
  system(paste0("cdo settaxis,", iY, "-01-00,12:00:00,1year ", iDir, "/drof_yearly.nc ", oDrof))
  
}

