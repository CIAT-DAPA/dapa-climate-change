# Carlos Navarro
# CCAFS / CIAT
# February 2016
stop("error")

vn <- "tmax"

setwd("D:/cenavarro/col-cormacarena/monthly-interpolations/_scripts")
anuDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/anu/Anuspl43/bin"
stDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/stations-averages"
rDir <- "D:/cenavarro/col-cormacarena/monthly-interpolations/_region"
oDir <- paste0("D:/cenavarro/col-cormacarena/monthly-interpolations/outputs/", vn)
# oDir <- paste0("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/tropico/outputs/", vn)

train.per <- 0.85
tile <- 1
ntiles <- 1
suffix <-"ame"
nfolds <- 25
source("01_fitSplines.R")

#do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=25) #initiate cluster

# for (i in 1:nfolds){
#   splineFitting(anuDir, stDir, rDir, oDir, i, train.per, vn, ntiles, unix=F, suffix)
# }

#export functions
sfExport("splineFitting")
sfExport("stDir")
sfExport("anuDir")
sfExport("rDir")
sfExport("oDir")
sfExport("train.per")
sfExport("vn")
sfExport("tile")
sfExport("ntiles")
sfExport("suffix")
sfExport("tile")

controlSplitting <- function(i) { #define a new function
  require(raster)
  require(foreign)
  splineFitting(anuDir, stDir, rDir, oDir, i, train.per, vn, ntiles, unix=F, suffix)
}

system.time(sfSapply(as.vector(1:25), controlSplitting))
