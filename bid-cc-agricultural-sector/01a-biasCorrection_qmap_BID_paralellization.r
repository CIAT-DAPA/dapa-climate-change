## Parameters ###
library(snowfall)
sfInit(parallel=T,cpus=10) #initiate cluster

stop("error")
source("01a-biasCorrection_qmap_BID.R")

#Set directories
wfdDir <- "//dapadfs/data_cluster_4/observed/gridded_products/wfd"
gcmHistDir <- "D:/cenavarro/bid/gcm_0_5deg_lat"
gcmFutDir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/"
outDir <- "D:/cenavarro/bid/bc_0_5deg_lat"
var <- "rsds"

# Export functions
sfExport("BC_Qmap")

#export variables
sfExport("wfdDir")
sfExport("gcmFutDir")
sfExport("gcmHistDir")
sfExport("outDir")
sfExport("var")

gcmList <- list.dirs(gcmHistDir, recursive = FALSE, full.names = FALSE)

for (i in 1:length(gcmList)){
    
  # gcm <- paste(as.matrix(gcmList[i]))
  gcm <- gcmList[i]
  sfExport("gcm")    
  
  control <- function(i) { #define a new function
    
    library(qmap)
    library(ncdf)
    library(raster)
    library(lubridate)
    
    cat(" .> ", paste("\t ", i, sep=""), "\t processing!\n")
    BC_Qmap(wfdDir, gcmHistDir, gcmFutDir, outDir, var, i)
    
  }
  
  system.time(sfSapply(as.vector(paste(as.matrix(gcmList)[1:length(gcmList)])), control))
  
}


#stop the cluster calculation
sfStop()

cat("GCM BC Qmap done!")