# Carlos Navarro
# CCAFS / CIAT
# February 2016

iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/outputs"
oDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/average"
oStats <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_tropico"
nfolds <- 25
ntiles <- 2
var <- "rain"
wDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/tmp"

source("02_averageOutputs_tiles.R")

#Do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=2) #initiate cluster

#export functions
sfExport("avg_folds")
sfExport("iDir")
sfExport("oDir")
sfExport("oStats")
sfExport("nfolds")
sfExport("ntiles")
sfExport("var")
sfExport("wDir")

controlavg <- function(i) { #define a new function
  require(raster)
  library(rgdal)
  avg_folds(iDir, oDir, oStats, nfolds, ntiles, var, wDir, i)
}

system.time(sfSapply(as.vector(1:25), controlavg))
