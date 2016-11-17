# Checking raster's errors: Tana River, Taita Taveta & Makueni
# H. Achicanoy
# CIAT, 2016

library(raster)

inDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/data'
nValCell <- nrow(rasterToPoints((raster(paste(inDir, '/Kenya_counties_rst/Tana_River_base.tif', sep='')))))

# Step 1. Check bias-corrected files

bcDir <- paste(inDir, '/bc_quantile_0_05deg_lat', sep='')
gcmList <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"
perList <- c('1971_2000', '2021_2045', '2041_2065')
rcpList <- paste("rcp", c(26, 45, 60, 85), sep="")
varList <- c('tmax', 'tmin', 'prec', 'dswrf')

clim <- lapply(1:length(gcmList), function(i){
  
  cat('\nLoading info from GCM:', gcmList[i], '\n')
  
  clim <- lapply(1:length(perList), function(j){
    
    cat('Loading info from period:', perList[j], '\n')
    
    if(perList[j]!='1971_2000'){
      
      clim <- lapply(1:length(rcpList), function(k){
        
        cat('Loading info from RCP:', rcpList[k], '\n')
        
        clim <- lapply(1:length(varList), function(l){
          
          load(paste(bcDir, '/', gcmList[i], '/', perList[j], '/', rcpList[k], '/Tana_River', '/', varList[l], '/bc_qmap_', varList[l], '_', perList[j], '.RData', sep=''))
          n <- nrow(gcmFutBC[complete.cases(gcmFutBC),])
          dfSumm <- data.frame(GCM=gcmList[i], Period=perList[j], RCP=rcpList[k], Variable=varList[l], ncell=n)
          
          return(dfSumm)
          
        })
        
        clim <- do.call(rbind, clim)
        return(clim)
        
      })
      
      clim <- do.call(rbind, clim)
      
    } else {
      
      clim <- lapply(1:length(varList), function(l){
        
        load(paste(bcDir, '/', gcmList[i], '/', perList[j], '/Tana_River', '/', varList[l], '/bc_qmap_', varList[l], '_', perList[j], '.RData', sep=''))
        n <- nrow(gcmHistBC[complete.cases(gcmHistBC),])
        dfSumm <- data.frame(GCM=gcmList[i], Period=perList[j], RCP='Historical', Variable=varList[l], ncell=n)
        
        return(dfSumm)
        
      })
      clim <- do.call(rbind, clim)
      
    }
    
    return(clim)
    
  })
  
  clim <- do.call(rbind, clim)
  return(clim)
  
})
clim <- do.call(rbind, clim)
write.csv(clim, '/home/hachicanoy/check_ncell_tana_river.csv', row.names=FALSE)

clim <- read.csv('/home/hachicanoy/check_ncell_tana_river.csv')

library(ggplot2)
gg <- ggplot(data=clim, aes(x=Variable, y=ncell)) + geom_jitter(aes(colour=Period), alpha=0.4, size=2)
gg <- gg + facet_wrap(~RCP, scales='free')
gg <- gg + scale_fill_brewer(palette="Spectral")
gg <- gg + theme_bw() + geom_hline(yintercept=nValCell, colour=2) + ylab('Number of cells with values')
gg <- gg + geom_hline(yintercept=500, colour=4)
gg <- gg + theme(axis.text.x  = element_text(size=10, angle=10))
gg <- gg + theme(axis.text.y  = element_text(size=10))
gg <- gg + theme(legend.title = element_text(size=12))
gg <- gg + theme(legend.text  = element_text(size=10))
ggsave(filename='/home/hachicanoy/ncell_rcp_period.pdf', plot=gg, width=9, height=7, units='in')

library(ggplot2)
gg <- ggplot(data=clim, aes(x=Variable, y=ncell)) + geom_jitter(aes(colour=RCP), alpha=0.4, size=2)
gg <- gg + facet_wrap(~GCM, scales='free')
gg <- gg + scale_fill_brewer(palette="Spectral")
gg <- gg + theme_bw() + geom_hline(yintercept=nValCell, colour=2) + ylab('Number of cells with values')
gg <- gg + geom_hline(yintercept=500, colour=4)
gg <- gg + theme(axis.text.x  = element_text(size=10, angle=10))
gg <- gg + theme(axis.text.y  = element_text(size=10))
gg <- gg + theme(legend.title = element_text(size=12))
gg <- gg + theme(legend.text  = element_text(size=10))
ggsave(filename='/home/hachicanoy/ncell_rcp_gcm.pdf', plot=gg, width=9, height=7, units='in')

# Here it was identified that only precipitation files have errors even for historical information using GCMs

# Step 2. Check precipitation input files

# Observed data. OK

obsDir <- "/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Tana_River/prec"
load(paste(obsDir, '/prec.RData', sep=''))
library(data.table)
chirps_ncell <- lapply(1:length(chirps_year), function(i){
  z <- chirps_year[[i]]
  z <- as.matrix(z)
  n <- nrow(z[complete.cases(z),])
  df <- data.frame(Year=names(chirps_year)[i], ncell=n)
  return(df)
})
chirps_ncell <- do.call(rbind, chirps_ncell)

# historical GCM. 
countyMask <- raster(paste("/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Tana_River_base.tif", sep=""))

nValCell <- lapply(1:length(gcmList), function(i){
  
  gcmHistDir <- paste("/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/", gcmList[i], "/1971_2000", sep="")
  
  years.gcm.hist <- 1971:2000
  gcmDataProcess <- function(yr)
  {
    monthProcess <- function(mth)
    {
      gcmHist <- stack(paste(gcmHistDir, '/by-month/prec_', yr, '_', mth, '.nc', sep=''))
      gcmHist <- mask(crop(gcmHist, extent(countyMask)), countyMask)
      gcmHist.dat <- as.data.frame(rasterToPoints(gcmHist))
      names(gcmHist.dat)[1:2] <- c('lon', 'lat')
      gcmHist.dat <- cbind(data.frame(cellID=cellFromXY(object=gcmHist, xy=gcmHist.dat[,c('lon', 'lat')])), gcmHist.dat)
      n <- dim(gcmHist.dat)[2] - 3 # Calculate number of days in month
      names(gcmHist.dat)[4:length(names(gcmHist.dat))] <- as.character(seq(as.Date(paste(yr, '-', mth,'-01', sep='')), as.Date(paste(yr, '-', mth, '-', n, sep='')), by=1))
      return(gcmHist.dat)
    }
    monthProcess <- Vectorize(monthProcess, vectorize.args='mth')
    monthData <- as.list(monthProcess(mth=c(paste('0', 1:9, sep=''), 10:12)))
    monthData <- reshape::merge_recurse(monthData)
    return(monthData)
  }
  library(foreach)
  library(doMC)
  registerDoMC(20)
  gcmHist.dat.all <- foreach(i=1:length(years.gcm.hist)) %dopar% {
    gcmHist.dat.all <- gcmDataProcess(yr=years.gcm.hist[i])
  }
  
  nValCell <- lapply(1:length(gcmHist.dat.all), function(j){
    n <- nrow(gcmHist.dat.all[[j]][complete.cases(gcmHist.dat.all[[j]]),])
    df <- data.frame(Year=, ncell=n, GCM=gcmList[i])
    return(df)
  })
  
  nValCell <- do.call(rbind, nValCell)
  return(nValCell)
})
nValCell <- do.call(rbind, nValCell)


# future GCM. 
# after KACCAL_season_indexes_future.R
cmpl_casesID <- which(complete.cases(df))
mData <- as.data.frame(df)
mData <- mData[-cmpl_casesID,]
mData$lon <- mData$lat <- NULL

library(tidyr)
library(dplyr)

colnames(mData) <- gsub(pattern='-', replacement='_', colnames(mData))
colnames(mData)[-1] <- paste('d', colnames(mData)[-1], sep='')
mData <- mData %>% gather(Date, Value, d2025_01_01:d2025_12_31)
mData$cellID <- as.factor(mData$cellID)

library(ggplot2)
library(viridis)
gg <- ggplot(mData, aes(x=Date, y=cellID, fill=Value))
gg <- gg + geom_tile() # color=Value, size=10
gg <- gg + scale_fill_viridis(name="Precipitation")
gg <- gg + coord_equal(ratio = 1/5)
gg <- gg + labs(x=NULL, y=NULL, title="Missing values through time")
# gg <- gg + theme_tufte(base_family="Helvetica")
gg <- gg + theme(plot.title=element_text(hjust=0, size=17))
gg <- gg + theme(axis.ticks=element_blank())
gg <- gg + theme(axis.text.x=element_text(size=2, angle=90))
gg <- gg + theme(axis.text.y=element_text(size=2))
gg <- gg + theme(legend.title=element_text(size=11, face='bold'))
gg <- gg + theme(legend.text=element_text(size=10))
gg <- gg + geom_vline(xintercept=182, colour=2)
ggsave(filename='/home/hachicanoy/missingValues2025.pdf', plot=gg, width=15, height=5, units='in')

# Makueni: d20210825
mMap <- countyMask
mMap[df[,'cellID']] <- df[,'2021-08-25']
plot(mMap)

# Taita Taveta: d2021_08_26
mMap <- countyMask
mMap[df[,'cellID']] <- df[,'2021-08-26']
plot(mMap)

# =-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-= #
# CHECK AND RESOLVE TROUBLES WITH RASTERS
# =-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=--=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-= #

library(lubridate)
library(raster)
library(data.table)

# Choose a county where problems happen
'Taita Taveta' # 'Makueni', 'Tana River'

# Load GCM bias-corrected information
load('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/bcc_csm1_1/2021_2045/rcp26/Tana_River/prec/bc_qmap_prec_2021_2045.RData')
BCmatrix <- gcmFutBC; rm(gcmFutBC)
# Choose a random year where problems happen
BCmatrix2 <- BCmatrix[,c(1:3, which(year(colnames(BCmatrix)[-(1:3)])==2022))] # 2022, 2030, 2041
ts_bcorrected <- as.numeric(BCmatrix2[49,])

# Choose a random pixel with problems to check out, using its associated coordinates

# Load GCM original information
dir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/data/gcm_0_05deg_lat/bcc_csm1_1/2021_2045/rcp26/by-month'
l2022 <- list.files(dir, pattern='prec_2041_', full.names=TRUE)
l2022 <- lapply(l2022, stack)
l2022 <- stack(l2022)
ts_original <- as.numeric(raster::extract(l2022, data.frame(lon=37.175, lat=-1.775001)))

# Load CHIRPS information
load('/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/Makueni/prec/prec.RData')
CHmatrix <- chirps_year[[1]]
CHmatrix <- as.matrix(CHmatrix)
ts_chirps <- as.numeric(CHmatrix[49,-c(1:3)])

# Plotting in the same graphic those three time-series
plot(ts_original, ty='l', col=1)
lines(ts_bcorrected[-(1:3)], ty='l', col=4)
lines(ts_chirps, ty='l', col=2)

# plot(ts_original, ts_bcorrected[-(1:3)], pch=20)

