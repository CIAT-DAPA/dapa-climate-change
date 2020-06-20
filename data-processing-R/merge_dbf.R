rm(list=ls(all=TRUE))
setwd("D:/Workspace/Request/unal/request_unal")
oDir <- "D:/Workspace/Request/unal"
graphics.off()

library("foreign")
library("purrr")
library("dplyr")

readDBA <- function(file){
  df <- read.dbf(file, as.is=FALSE)
  df$fileName <- file
  return(df)
}

varLs <- c("prec", "tmin", "tmax")
mthLs <- 1:12

perLs <- c("2020_2049", "2040_2069", "2060_2089", "2070_2099")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")

dts <- data.frame()
  
for (rcp in rcpLs){
  
  if (rcp == "rcp26") {
    gcmLs <- c("bcc_csm1_1", "bcc_csm1_1_m", "bnu_esm", "cccma_canesm2", "cesm1_cam5", "cnrm_cm5", "csiro_mk3_6_0", "fio_esm", "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_h", "giss_e2_r", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "lasg_fgoals_g2", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_es", "mpi_esm_lr", "mpi_esm_mr", "mri_cgcm3", "ncar_ccsm4", "ncc_noresm1_m", "nimr_hadgem2_ao")
  } 
  
  if (rcp == "rcp45") {
    gcmLs <- c("bcc_csm1_1", "bcc_csm1_1_m", "bnu_esm", "cccma_canesm2", "cesm1_bgc", "cesm1_cam5", "cnrm_cm5", "csiro_access1_0", "csiro_access1_3", "csiro_mk3_6_0", "fio_esm", "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_h_cc", "giss_e2_r", "giss_e2_r_cc", "inm_cm4", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "lasg_fgoals_g2", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_cc", "mohc_hadgem2_es", "mpi_esm_lr", "mri_cgcm3", "ncar_ccsm4", "ncc_noresm1_m", "nimr_hadgem2_ao")
    
  }
  
  if (rcp == "rcp60") {
    gcmLs <- c("bcc_csm1_1", "bcc_csm1_1_m", "cesm1_cam5", "csiro_mk3_6_0", "fio_esm", "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_h", "giss_e2_r", "ipsl_cm5a_lr", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_es", "mri_cgcm3", "ncar_ccsm4", "ncc_noresm1_m", "nimr_hadgem2_ao")
  }
  
  if (rcp == "rcp85") {
    gcmLs <- c("bcc_csm1_1", "bcc_csm1_1_m", "bnu_esm", "cccma_canesm2", "cesm1_bgc", "cesm1_cam5", "cnrm_cm5", "csiro_access1_0", "csiro_access1_3", "csiro_mk3_6_0", "ec_earth", "fio_esm", "gfdl_cm3", "gfdl_esm2g", "gfdl_esm2m", "giss_e2_h", "giss_e2_r", "inm_cm4", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "ipsl_cm5b_lr", "lasg_fgoals_g2", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_cc", "mohc_hadgem2_es", "mpi_esm_lr", "mpi_esm_mr", "mri_cgcm3", "ncar_ccsm4", "ncc_noresm1_m", "nimr_hadgem2_ao")
  }
  
  for (gcm in gcmLs){
    
    for (per in perLs){
      
      for (var in varLs){
        
        for (mth in mthLs){
          
          dbf_i <- read.dbf(paste0(rcp, "_", gcm, "_", per, "_", var, "_", mth, ".dbf"), as.is=FALSE)
          
          #Delta in celsius degrees /100
          dts <- rbind(dts, cbind(rcp, gcm, per, var, mth, value=dbf_i[,4]/100))
         
          
        }
        
      }
      
    }
  }
  
}


setwd("D:/Workspace/Request/unal/wcl")

dts <- data.frame()

for (var in varLs){
  
  for (mth in mthLs){
    
    dbf_i <- read.dbf(paste0(var, "_", mth, ".dbf"), as.is=FALSE)
    
    if (var == "tmin" || var == "tmax"){
    
      #Delta in celsius degrees /10
      dts <- rbind(dts, cbind(period="1960-1990", var, mth, value=dbf_i[,4]/10))
      
    } else {
      
      dts <- rbind(dts, cbind(period="1960-1990", var, mth, value=dbf_i[,4]))
      
    }
    
    
  }
  
}


write.csv(dts, paste0(oDir, "/ext_wcl_ptsvalle.csv"), quote = F, row.names = F)

