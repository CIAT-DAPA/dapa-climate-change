# Processing future data in order to calculate climatic indices by counties in Kenya
# KACCAL project
# H. Achicanoy
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                            Processing data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Define GCM directory
gcmDir <- '/mnt/data_cluster_2/gcm/cmip5/raw/daily'

# Load packages
options(warn=-1)
library(raster)
library(ncdf)
library(ncdf4)
library(maptools)
library(ff)
library(data.table)
library(miscTools)

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# List of GCMs and scenarios
gcm_list <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3",
              "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm",
              "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m")

rTest <- stack(paste(gcmDir, '/rcp26/bcc_csm1_1/r1i1p1/rsds_day_bcc-csm1-1_rcp26_r1i1p1_20060101-21001231.nc', sep=''))
