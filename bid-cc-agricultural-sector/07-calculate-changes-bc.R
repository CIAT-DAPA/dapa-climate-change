require(raster)
require(ncdf)


#Arguments
iDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_0_5deg_lat"
stFile <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/sample_type_climates_without_1.csv"
oDir <- "D:/CIAT/Projects/lat-bid-cc-agricultural-sector/02_climate_data/bc_0_5deg_lat"
var <- "tmax"

stFile <- read.csv(stFile)
coordinates <- stFile[8:9]    

if (var == "tmax" || var == "tmin"){

  gcmList <- list.dirs(iDir, recursive = FALSE, full.names = FALSE)
  
  for (i in 1:length(gcmList)){
      
    cat(gcmList[i])
    
    avgHisNc <- stack(paste(iDir, "/", gcmList[i], "/1971_2000/", "bc_", var, "_1971_2000_mon_avg.nc", sep=""))
    avgFutNc <- stack(paste(iDir, "/", gcmList[i], "/2020_2049/", "bc_", var, "_2020_2049_mon_avg.nc", sep=""))
    plot(avgHisNc)  
    plot(avgFutNc)
    
    for (mth in 1:12){
      
      chgNc <- avgFutNc[[mth]] - avgHisNc[[mth]]
      writeRaster(chgNc, paste(iDir, "/", gcmList[i], "/2020_2049/", var, "_chg_", mth, ".nc", sep=""), overwrite=T)
      
    }
  
  }
  
  for (mth in 1:12){
    
    cat(paste("Averaging", mth, sep=""))
    
    fun <- function(x) { sd(x) }
    
    chgNcStkAvg <-  mean(stack(paste(iDir, "/", gcmList, "/2020_2049/",  var, "_chg_", mth, ".nc", sep="")))
    chgNcStkStd <- calc(stack(paste(iDir, "/", gcmList, "/2020_2049/",  var, "_chg_", mth, ".nc", sep="")), fun)
    
    writeRaster(chgNcStkAvg, paste(oDir, "/", var, "_chg_", mth, ".nc", sep=""), overwrite=T)
    writeRaster(chgNcStkStd, paste(oDir, "/", var, "_chg_", mth, "_std.nc", sep=""), overwrite=T)
    
  }
  
  for (i in 1:length(gcmList)){
    
    cat(gcmList[i])
    
    avgHisNc <- stack(paste(iDir, "/", gcmList[i], "/1971_2000/", "bc_", var, "_1971_2000_mon_avg.nc", sep=""))
    
    avgFutNc <- stack(paste(iDir, "/", gcmList[i], "/2020_2049/", "bc_", var, "_2020_2049_mon_avg.nc", sep=""))
    
    value <- extract(avgHisNc, coordinates)
    
    dataMatrix <- cbind(stFile$Cell_ID_prec_rsds, coordinates, value)
    colnames(dataMatrix) <- c("CellID", "Lon", "Lat", 1:12)
    
    write.csv(dataMatrix, paste(oDir, "/", gcmList[i], "/2020_2049/fut_", var, ".csv", sep=""), row.names=F)
    
    }
  
} else {
  
  if(var == "prec"){
    gcmList <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "cnrm_cm5", "csiro_mk3_6_0", "gfld_esm2g", "gfld_esm2m", "inm_cm4", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "ipsl_cm5b_lr", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_es", "mpi_esm_lr", "mpi_esm_mr", "mri_cgcm3", "ncc_noresm1_m")  
  } else {
    gcmList <- c("bnu_esm", "cccma_canesm2", "cnrm_cm5", "csiro_mk3_6_0", "gfld_esm2g", "gfld_esm2m", "inm_cm4", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mpi_esm_lr", "mpi_esm_mr", "mri_cgcm3", "ncc_noresm1_m")  
  }
  
  basRs <- raster(nrows=178, ncols=180, xmn=-120, xmx=-30, ymn=-56, ymx=33)
  
  for (i in 1:length(gcmList)){
    
    cat(gcmList[i])  
    
    datFileHis <- read.csv(paste(iDir, "/", gcmList[i], "/1971_2000/", "bc_", var, "_1950_2000_climatology.csv", sep=""))
    datFileFut <- read.csv(paste(iDir, "/", gcmList[i], "/2020_2049/", "bc_", var, "_2020_2049_climatology.csv", sep=""))
    
    datFileHis <- datFileHis[complete.cases(datFileHis),]
    row.names(datFileHis) <- NULL
    coordsHis <- cbind(datFileHis[2],datFileHis[3])
    
    datFileFut <- datFileFut[complete.cases(datFileFut),]
    row.names(datFileFut) <- NULL
    coordsFut <- cbind(datFileFut[2],datFileFut[3])
    
    for (mth in 1:12){
      
      outNcHis <- paste(iDir, "/", gcmList[i], "/1971_2000/", "bc_", var, "_", mth, ".nc", sep="")
      outNcFut <- paste(iDir, "/", gcmList[i], "/2020_2049/", "bc_", var, "_", mth, ".nc", sep="")
      
      if (!file.exists(outNcFut)) {
        
        valuehis <- datFileHis[mth+3]
        valuefut <- datFileFut[mth+3]
        
        outRsHis <- rasterize(coordsHis, basRs, valuehis, fun='last')
        outRsFut <- rasterize(coordsFut, basRs, valuefut, fun='last')
    
        outRsHis <- writeRaster(outRsHis, outNcHis, overwrite=FALSE)
        outRsFut <- writeRaster(outRsFut, outNcFut, overwrite=FALSE)

        
      }
    }
  }
  
  
  for (i in 1:length(gcmList)){
    
    cat(gcmList[i])
    
    avgHisNc <- stack(paste(iDir, "/", gcmList[i], "/1971_2000/", "bc_", var, "_", 1:12, ".nc", sep=""))
    avgFutNc <- stack(paste(iDir, "/", gcmList[i], "/2020_2049/", "bc_", var, "_", 1:12, ".nc", sep=""))
    
    for (mth in 1:12){
      
      chgNc <- avgFutNc[[mth]] - avgHisNc[[mth]]
      writeRaster(chgNc, paste(iDir, "/", gcmList[i], "/2020_2049/", var, "_chg_", mth, ".nc", sep=""), overwrite=T)
      
    }
    
  }
  
  for (mth in 1:12){
    
    cat(paste("Averaging", mth, sep=""))
    
    fun <- function(x) { sd(x) }
    
    chgNcStkAvg <-  mean(stack(paste(iDir, "/", gcmList, "/2020_2049/",  var, "_chg_", mth, ".nc", sep="")))
    chgNcStkStd <- calc(stack(paste(iDir, "/", gcmList, "/2020_2049/",  var, "_chg_", mth, ".nc", sep="")), fun)
    
    writeRaster(chgNcStkAvg, paste(oDir, "/", var, "_chg_", mth, ".nc", sep=""), overwrite=T)
    writeRaster(chgNcStkStd, paste(oDir, "/", var, "_chg_", mth, "_std.nc", sep=""), overwrite=T)
    
  }
  
  
  for (i in 1:length(gcmList)){
    
    cat(gcmList[i])
    avgHisNc <- stack(paste(iDir, "/", gcmList[i], "/1971_2000/", "bc_", var, "_", 1:12, ".nc", sep=""))
        
    value <- extract(avgHisNc, coordinates)
    
    dataMatrix <- cbind(stFile$Cell_ID_prec_rsds, coordinates, value)
    colnames(dataMatrix) <- c("CellID", "Lon", "Lat", 1:12)
    
    write.csv(dataMatrix, paste(oDir, "/", gcmList[i], "/1971_2000/bsl_", var, ".csv", sep=""), row.names=F)
    
  }
  

}



