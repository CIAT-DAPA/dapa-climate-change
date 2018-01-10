sfStop()
stop("error")
source("31-biasCorrection_qmap.R")

#Set variables
iDir <- "Z:/DATA/WP2/06_Clustering_analyses"
# iDir <- "/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses"

obsDir <- "Z:/DATA/WP2/02_Gridded_data/chirps_0_25deg_amz"
# obsDir <- '/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/02_Gridded_data/chirps_0_05deg_amz'
# obsDir <- "U:/cropdata/agmerra/daily/nc-files/by-month"
# obsDir <-'/mnt/data_cluster_5/cropdata/agmerra/daily/nc-files/by-month'

outDir <- "Z:/DATA/WP2/06_Clustering_analyses/data/bc_0_25deg_amz"
# outDir <- "/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses/data/bc_quantile_0_05deg_amz"

county <- "Ucayali"
var <- "rsds"

region <- "amz"
obsPer <- "1981_2010"
gcmHistPer <- "1981_2005"
gcmFutPerLs <- c("2020_2049", "2040_2069")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")

# gcm="bcc_csm1_1"
# gcmList <- list.dirs(gcmHistDir, recursive = FALSE, full.names = FALSE)
# gcmList <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "ipsl_cm5a_lr","miroc_esm", "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m", "mri_cgcm3", "nimr_hadgem2_ao")
gcmList <- c("mohc_hadgem2_es", "nimr_hadgem2_ao")

## Parameters ###
library(snowfall)
sfInit(parallel=T,cpus=2) #initiate cluster

# Export functions
sfExport("BC_Qmap")

#export variables
sfExport("iDir")
sfExport("obsDir")
sfExport("region")
sfExport("obsPer")
sfExport("gcmHistPer")
sfExport("gcmFutPerLs")
sfExport("county")
sfExport("outDir")
sfExport("var")
sfExport("rcpLs")
sfExport("gcmList")

control <- function(i) { #define a new function
  
  library(qmap)
  library(ncdf)
  library(raster)
  library(lubridate)
  
  cat(" .> ", paste("\t ", i, sep=""), "\t processing!\n")
  BC_Qmap(iDir, obsDir, region, obsPer, gcmHistPer, gcmFutPerLs, county, outDir, var, gcmList, rcpLs, i)
  
}

system.time(sfSapply(as.vector(1:length(gcmList)), control))


#stop the cluster calculation
sfStop()

cat("GCM BC Qmap done!")
