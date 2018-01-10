# Carlos Navarro
# CCAFS / CIAT
# February 2016

library(snowfall)
require(raster)
require(foreign)

stop("error")
sfStop()

# Set params 
vn <- "rain"
bDir <- "D:/cenavarro/lat-sal"
train.per <- 0.85
tile <- 1
ntiles <- 1
suffix <-"amz"
nfolds <- 25 
cpus <- 25

# Folders 
srcDir <- paste0("Z:/DATA/WP2/00_scripts/src")
anuDir <- paste0(bDir, "/anu/Anuspl43/bin")
# stDir <- paste0(bDir, "/stations-averages")
stDir <- "Z:/DATA/WP2/01_Weather_Stations/MERGE/climatology/combined"
rDir <- paste0(bDir, "/region/5km")
oDir <- paste0(bDir, "/outputs/", vn)
year <- "1981_2010"

# years <- c("1971_2000", "1976_2005", , "1986_2005")
  
#Reading altitude raster (regional level)
cat("Reading mask file \n")
msk <- raster(paste0(rDir, "/alt-prj-",suffix, ".asc"))
xt <- extent(msk)
xt@xmin <- xt@xmin; xt@xmax <- xt@xmax; xt@ymin <- xt@ymin; xt@ymax <- xt@ymax
rm(msk)

# Main function
setwd(srcDir)
source("fitSplines.R")

# Do the snowfall stuff here
sfInit(parallel=T,cpus=cpus) #initiate cluster

# Export functions
# sfExport("srcDir")
sfExport("splineFitting")
sfExport("stDir")
sfExport("anuDir")
sfExport("rDir")
sfExport("oDir")
sfExport("year")
sfExport("nfolds")
sfExport("train.per")
sfExport("vn")
sfExport("tile")
sfExport("ntiles")
sfExport("suffix")
sfExport("xt")
sfExport("tile")

controlSplitting <- function(i) { #define a new function
  
  require(raster)
  require(foreign)
  
  oyDir <- paste0(oDir, "/", year)
  
  source("writeDatFile.R"); source("createFitFile.R"); source("createValFile.R"); source("createPrjFile.R"); source("accuracy.R")  
  splineFitting(anuDir, stDir, rDir, oyDir, nfolds, train.per, vn, ntiles, unix=F, suffix, xt, i, year)
  
}

system.time(sfSapply(as.vector(1:nfolds), controlSplitting))

sfStop()

