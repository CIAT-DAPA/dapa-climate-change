# Climate data revision
# H. Achicanoy
# CIAT, 2017

# R options
g <- gc(reset = T); rm(list = ls()); options(warn = -1); options(scipen = 999)
OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){
  wk_dir <- "/mnt/workspace_cluster_9/Sustainable_Food_System/Input_data/"; setwd(wk_dir); rm(wk_dir)
} else {
  if(OSys == "Windows"){
    wk_dir <- "//dapadfs/workspace_cluster_9/Sustainable_Food_System/Input_data"; setwd(wk_dir); rm(wk_dir)
  }
}; rm(OSys)

# Load future climate data

modelos <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
             "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

lapply(1:length(modelos), function(gcm_i){
  
  gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[gcm_i], "/Futuro/")
  
  load(paste0(gcm, "Precipitation.RDat"))
  # Nested list
  # Information by pixel and year: 8000 pixels, 29 years
  # So each single object is a data.frame (1 rows; 365-6 columns)
  Prec[[pixel]][[year]]
  
  load(paste0(gcm, "Srad.Rdat"))
  # Nested list
  # Information by pixel and year: 8000 pixels, 29 years
  # So each single object is a numeric vector (365-6 positions)
  Srad[[pixel]][[year]]
  
  load(paste0(gcm, "Temperatura_2.Rdat"))
  # Information in a matrix: 8199 pixels, 30 years (but it seems that only the last year wasn't used)
  Tmax[[year]] # matrix (8199 rows; 365-6 columns)
  Tmin[[year]] # matrix (8199 rows; 365-6 columns)
  # And there's an algorithm to match properly pixels according to their coordinates
  
  # Step 1: Correct days a year when Tmax <= Tmin, it's mandatory to be consistant with Tmax > Tmin
  
  for(i in 1:length(Tmax)){
    
    cellID <- which(Tmax[[i]] <= Tmin[[i]])
    rc_fromCell <- arrayInd(cellID, dim(Tmax[[i]]))
    
    if(length(rc_fromCell) > 0){
      
      for(j in 1:nrow(rc_fromCell)){
        
        Tmax[[i]][rc_fromCell[j,1], rc_fromCell[j,2]]
        Tmin[[i]][rc_fromCell[j,1], rc_fromCell[j,2]]
      }
      
    }
    
  }
  
  # Step 2: Apply correction factor for Solar Radiation in cases where Srad is in another units
  
})