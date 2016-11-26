# Check units Observed data, GCM historical and GCM future
# H. Achicanoy
# CIAT, 2016

##########################################################
# Tmax
##########################################################

library(data.table)
# Load observed data
load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Busia/tmax/tmax.RData')
tmaxObs <- ch2014_year; rm(ch2014_year)
dfObs <- as.matrix(tmaxObs[[10]])
plot(dfObs[1,-c(1:3)], type='l', ylim=c(22, 40))
lapply(1:nrow(dfObs), function(i){
  lines(dfObs[i,-c(1:3)])
})

county <- raster('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Busia_base.tif')

# Load GCM historical (ok)
dir <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/1971_2000/by-month'
tmaxFiles <- list.files(path=dir, pattern='^tmax_1988_*', full.names=TRUE)
library(raster)
library(ncdf4)
tmaxFiles <- lapply(tmaxFiles, raster::stack)
tmaxFiles <- stack(tmaxFiles)
tmaxFiles <- mask(x=crop(tmaxFiles, extent(county)), mask=county)
tmaxFiles <- rasterToPoints(tmaxFiles)
plot(tmaxFiles[1,-c(1:2)], type='l', ylim=c(15, 35))
lapply(1:nrow(dfObs), function(i){
  lines(tmaxFiles[i,-c(1:2)])
})

# Load GCM future (ok)
dirFut <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/2021_2045/rcp85/by-month'
tmaxFutFiles <- list.files(path=dirFut, pattern='^tmax_2024_*', full.names=TRUE)
library(raster)
library(ncdf4)
tmaxFutFiles <- lapply(tmaxFutFiles, raster::stack)
tmaxFutFiles <- stack(tmaxFutFiles)
tmaxFutFiles <- mask(x=crop(tmaxFutFiles, extent(county)), mask=county)
tmaxFutFiles <- rasterToPoints(tmaxFutFiles)
plot(tmaxFutFiles[1,-c(1:2)], type='l', ylim=c(15, 35))
lapply(1:nrow(dfObs), function(i){
  lines(tmaxFutFiles[i,-c(1:2)])
})

##########################################################
# Tmin
##########################################################

library(data.table)
load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Busia/tmin/tmin.RData')
tminObs <- ch2014_year; rm(ch2014_year)
dfObs <- as.matrix(tminObs[[10]])
plot(dfObs[1,-c(1:3)], type='l', ylim=c(10, 23))
lapply(1:nrow(dfObs), function(i){
  lines(dfObs[i,-c(1:3)])
})

library(raster)
county <- raster('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Busia_base.tif')

# Load GCM historical (ok)
dir <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/1971_2000/by-month'
tminFiles <- list.files(path=dir, pattern='^tmin_1988_*', full.names=TRUE)
library(raster)
library(ncdf4)
tminFiles <- lapply(tminFiles, raster::stack)
tminFiles <- stack(tminFiles)
tminFiles <- mask(x=crop(tminFiles, extent(county)), mask=county)
tminFiles <- rasterToPoints(tminFiles)
plot(tminFiles[1,-c(1:2)], type='l', ylim=c(15, 35))
lapply(1:nrow(dfObs), function(i){
  lines(tminFiles[i,-c(1:2)])
})

# Load GCM future (ok)
dirFut <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/2021_2045/rcp85/by-month'
tminFutFiles <- list.files(path=dirFut, pattern='^tmin_2024_*', full.names=TRUE)
library(raster)
library(ncdf4)
tminFutFiles <- lapply(tminFutFiles, raster::stack)
tminFutFiles <- stack(tminFutFiles)
tminFutFiles <- mask(x=crop(tminFutFiles, extent(county)), mask=county)
tminFutFiles <- rasterToPoints(tminFutFiles)
plot(tminFutFiles[1,-c(1:2)], type='l', ylim=c(15, 35))
lapply(1:nrow(dfObs), function(i){
  lines(tminFutFiles[i,-c(1:2)])
})

##########################################################
# Prec
##########################################################

library(data.table)
load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Busia/prec/prec.RData')
precObs <- chirps_year; rm(chirps_year)
dfObs <- as.matrix(precObs[[10]])
plot(dfObs[1,-c(1:3)], type='l', ylim=c(0, 80))
lapply(1:nrow(dfObs), function(i){
  lines(dfObs[i,-c(1:3)])
})

library(raster)
county <- raster('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Busia_base.tif')

# Load GCM historical (ok)
dir <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/1971_2000/by-month'
precFiles <- list.files(path=dir, pattern='^prec_1988_*', full.names=TRUE)
library(raster)
library(ncdf4)
precFiles <- lapply(precFiles, raster::stack)
precFiles <- stack(precFiles)
precFiles <- mask(x=crop(precFiles, extent(county)), mask=county)
precFiles <- rasterToPoints(precFiles)
plot(precFiles[1,-c(1:2)], type='l', ylim=c(0, 80))
lapply(1:nrow(dfObs), function(i){
  lines(precFiles[i,-c(1:2)])
})

# Load GCM future (ok)
dirFut <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/2021_2045/rcp85/by-month'
precFutFiles <- list.files(path=dirFut, pattern='^prec_2024_*', full.names=TRUE)
library(raster)
library(ncdf4)
precFutFiles <- lapply(precFutFiles, raster::stack)
precFutFiles <- stack(precFutFiles)
precFutFiles <- mask(x=crop(precFutFiles, extent(county)), mask=county)
precFutFiles <- rasterToPoints(precFutFiles)
plot(precFutFiles[1,-c(1:2)], type='l', ylim=c(0, 80))
lapply(1:nrow(dfObs), function(i){
  lines(precFutFiles[i,-c(1:2)])
})

##########################################################
# SRad
##########################################################

library(data.table)
load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Busia/dswrf/dswrf.RData')
dswrfObs <- ch2014_year; rm(ch2014_year)
dfObs <- as.matrix(dswrfObs[[10]])
plot(dfObs[1,-c(1:3)], type='l', ylim=c(10, 30))
lapply(1:nrow(dfObs), function(i){
  lines(dfObs[i,-c(1:3)])
})

library(raster)
county <- raster('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Busia_base.tif')

# Load GCM historical (ok)
dir <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/1971_2000/by-month'
dswrFiles <- list.files(path=dir, pattern='^rsds_1988_*', full.names=TRUE)
library(raster)
library(ncdf4)
dswrFiles <- lapply(dswrFiles, raster::stack)
dswrFiles <- stack(dswrFiles)
dswrFiles <- mask(x=crop(dswrFiles, extent(county)), mask=county)
dswrFiles <- rasterToPoints(dswrFiles)
plot(dswrFiles[1,-c(1:2)]*0.0864, type='l', ylim=c(0, 30))
lapply(1:nrow(dfObs), function(i){
  lines(dswrFiles[i,-c(1:2)]*0.0864)
})

# Load GCM future (ok)
dirFut <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/2021_2045/rcp85/by-month'
dswrFutFiles <- list.files(path=dirFut, pattern='^rsds_2024_*', full.names=TRUE)
library(raster)
library(ncdf4)
dswrFutFiles <- lapply(dswrFutFiles, raster::stack)
dswrFutFiles <- stack(dswrFutFiles)
dswrFutFiles <- mask(x=crop(dswrFutFiles, extent(county)), mask=county)
dswrFutFiles <- rasterToPoints(dswrFutFiles)
plot(dswrFutFiles[1,-c(1:2)]*0.0864, type='l', ylim=c(0, 30))
lapply(1:nrow(dfObs), function(i){
  lines(dswrFutFiles[i,-c(1:2)]*0.0864)
})
