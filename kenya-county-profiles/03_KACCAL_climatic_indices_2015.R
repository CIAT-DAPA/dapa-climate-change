# Calculating climatic indices by counties in Kenya - Historical information first and second season
# KACCAL project
# H. Achicanoy
# CIAT, 2016

source('/mnt/workspace_cluster_8/Kenya_KACCAL/scripts/KACCAL_calc_risk_indices_modified.R')

# Load packages
options(warn=-1)
library(raster)
library(ncdf)
library(ncdf4)
library(maptools)
library(ff)
library(data.table)
library(miscTools)
library(compiler)

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

years_analysis <- paste('y', 1981:2005, sep='')
inputDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables'
outputDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical'

seasonList <- c('first', 'second')

calc_climIndices <- function(county='Kilifi', season='first'){
  
  cat('\n\n\n*** Processing:', gsub(pattern=' ', replacement='_', county, fixed=TRUE), 'county ***\n\n')
  countyDir <- paste(inputDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), sep='')
  
  if(dir.exists(countyDir))
  {
    indexes <- paste(outputDir, '/', season, '_season/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_', season, '_season_2015.RData', sep='')
    
    # @@@ if(!file.exists(indexes)){
    
    cat('Loading raster mask for:', gsub(pattern=' ', replacement='_', county), '\n')
    countyMask <- raster(paste("/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/", gsub(pattern=' ', replacement='_', county), "_base.tif", sep=""))
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Solar radiation for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    if(season == 'first'){
      load(paste(countyDir, '/dswrf/dswrf_fs_wet_days.RData', sep=''))
      dswrf <- first_season_var; rm(first_season_var)
    } else {
      if(season == 'second'){
        load(paste(countyDir, '/dswrf/dswrf_ss_wet_days.RData', sep=''))
        dswrf <- second_season_var; rm(second_season_var)
      }
    }
    dswrf <- dswrf[years_analysis]
    dswrf <- lapply(1:length(dswrf), function(i){z <- as.data.frame(dswrf[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(dswrf)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(dswrf) <- years_analysis
    dswrf <- reshape::merge_recurse(dswrf)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Daily precipitation for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    if(season == 'first'){
      load(paste(countyDir, '/prec/prec_fs_wet_days.RData', sep=''))
      prec <- chirps_wet_days; rm(chirps_wet_days)
    } else {
      if(season == 'second'){
        load(paste(countyDir, '/prec/prec_ss_wet_days.RData', sep=''))
        prec <- chirps_wet_days; rm(chirps_wet_days)
      }
    }
    years_prec <- gsub(pattern='y', replacement='', names(prec))
    # prec <- prec[years_analysis]
    prec <- lapply(1:length(prec), function(i){z <- as.data.frame(prec[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(prec)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(prec) <- years_analysis
    prec <- reshape::merge_recurse(prec)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Soil data\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/soil/soil_data.RData', sep=''))
    soil <- soil_data_county; rm(soil_data_county)
    soil <- soil[,c("cellID","lon.x","lat.x","id_coarse","rdepth","d.25","d.100","d.225","d.450","d.800","d.1500","soilcp")]
    names(soil)[2:3] <- c('lon','lat')
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Maximum temperature for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    if(season == 'first'){
      load(paste(countyDir, '/tmax/tmax_fs_wet_days.RData', sep=''))
      tmax <- first_season_var; rm(first_season_var)
    } else {
      if(season == 'second'){
        load(paste(countyDir, '/tmax/tmax_ss_wet_days.RData', sep=''))
        tmax <- second_season_var; rm(second_season_var)
      }
    }
    tmax <- tmax[years_analysis]
    tmax <- lapply(1:length(tmax), function(i){z <- as.data.frame(tmax[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmax)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(tmax) <- years_analysis
    tmax <- reshape::merge_recurse(tmax)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Minimum temperature for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    if(season == 'first'){
      load(paste(countyDir, '/tmin/tmin_fs_wet_days.RData', sep=''))
      tmin <- first_season_var; rm(first_season_var)
    } else {
      if(season == 'second'){
        load(paste(countyDir, '/tmin/tmin_ss_wet_days.RData', sep=''))
        tmin <- second_season_var; rm(second_season_var)
      }
    }
    tmin <- tmin[years_analysis]
    tmin <- lapply(1:length(tmin), function(i){z <- as.data.frame(tmin[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmin)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(tmin) <- years_analysis
    tmin <- reshape::merge_recurse(tmin)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Calculate: Mean temperature for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    library(parallel)
    tmean <- mclapply(1:length(years_analysis), function(j){
      
      # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
      
      library(lubridate)
      tmaxYears <- year(colnames(tmax)[-c(1:3)])
      
      # cat('Select 100-wet days by year\n')
      datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])), x=tmaxYears, fixed=TRUE)
      
      # cat('Define number of cells for final raster\n')
      ncell <- length(Reduce(intersect, list(tmax[,'cellID'], tmin[,'cellID'])))
      
      # cat('Define cells to analyse\n')
      pixelList <- Reduce(intersect, list(tmax[,'cellID'], tmin[,'cellID']))
      
      tmean <- tmeanCMP(TMAX=tmax[match(pixelList, tmax[,'cellID']), datesID+3],
                        TMIN=tmin[match(pixelList, tmin[,'cellID']), datesID+3],
                        season_ini=1, season_end=100)
      tmean <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), tmean)
      colnames(tmean) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
      
      return(tmean)
      
    }, mc.cores=20)
    names(tmean) <- years_analysis
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    ### CLIMATIC INDICES to calculate
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    ### 1. Tmean: Mean daily temperature averaged for a specified period
    cat('*** 1. Calculate mean daily temperature through list of years ***\n')
    TMEAN <- lapply(1:length(years_analysis), function(j){
      
      pixelList <- tmean[[j]][,'cellID']
      
      tmean <- rowMeans(tmean[[j]][, 4:ncol(tmean[[j]])])
      tmean <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), tmean)
      colnames(tmean) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_analysis[[j]])))
      
      return(tmean)
      
    })
    names(TMEAN) <- years_analysis
    TMEAN <- reshape::merge_recurse(TMEAN)
    
    
    ### 2. GDD_1: Crop duration. Growing degree days calculated using a capped-top function with TB=10 ºC
    cat('*** 2. Calculate crop duration with TB=10 ºC through list of years ***\n') # FIX
    GDD_1 <- lapply(1:length(years_analysis), function(j){
      # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
      gdd_1 <- apply(X=tmean[[j]][,4:ncol(tmean[[j]])], MARGIN=1, FUN=calc_cdurCMP, t_thresh=10)
      gdd_1 <- cbind(tmean[[j]][,1:3], gdd_1)
      colnames(gdd_1)[4] <- as.character(gsub(pattern='y', replacement='', years_analysis[[j]]))
      return(gdd_1)
    })
    names(GDD_1) <- years_analysis
    GDD_1 <- reshape::merge_recurse(GDD_1)
    
    
    ### 3. GDD_2: Crop duration. Growing degree days calculated using a capped-top function with TO=25 ºC
    cat('*** 3. Calculate crop duration with TB=25 ºC through list of years ***\n') # FIX
    GDD_2 <- lapply(1:length(years_analysis), function(j){
      # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
      gdd_2 <- apply(X=tmean[[j]][,4:ncol(tmean[[j]])], MARGIN=1, FUN=calc_cdurCMP, t_thresh=25)
      gdd_2 <- cbind(tmean[[j]][,1:3], gdd_2)
      colnames(gdd_2)[4] <- as.character(gsub(pattern='y', replacement='', years_analysis[[j]]))
      return(gdd_2)
    })
    names(GDD_2) <- years_analysis
    GDD_2 <- reshape::merge_recurse(GDD_2)
    
    
    ### 4. ND_t35: Heat stress. Total number of days with maximum temperature greater or equal to 35 ºC.
    cat('*** 4. Calculate total number of days with maximum temperature greater or equal to 35 ºC through list of years ***\n')
    ND_t35 <- lapply(1:length(years_analysis), function(j){
      # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
      
      library(lubridate)
      tmaxYears <- year(colnames(tmax)[-c(1:3)])
      
      # cat('Select 100-wet days by year\n')
      datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])), x=tmaxYears, fixed=TRUE)
      
      # cat('Define number of cells for final raster\n')
      ncell <- length(tmax[,'cellID'])
      
      # cat('Define cells to analyse\n')
      pixelList <- tmax[,'cellID']
      
      nd_t35 <- apply(X=tmax[match(pixelList, tmax[,'cellID']), datesID+3], MARGIN=1, FUN=calc_htsCMP, t_thresh=35)
      nd_t35 <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), nd_t35)
      colnames(nd_t35) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_analysis[[j]])))
      return(nd_t35)
      
    })
    names(ND_t35) <- years_analysis
    ND_t35 <- reshape::merge_recurse(ND_t35)
    
    
    ### 5. P_tot: Annual total precipitation
    # Load precipitation for all year
    # load(paste(countyDir, '/prec/prec.RData', sep=''))
    # prec_all <- chirps_year; rm(chirps_year)
    # prec_all <- prec_all[years_analysis]
    # prec_all <- lapply(1:length(prec_all), function(i){z <- as.data.frame(prec_all[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(prec_all)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    cat('*** 5. Calculate total precipitation by season through list of years ***\n')
    TOTRAIN <- lapply(1:length(years_prec), function(j){
      
      library(lubridate)
      precYears <- year(colnames(prec)[-c(1:3)])
      datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_prec[[j]])), x=precYears, fixed=TRUE)
      ncell <- length(prec[,'cellID'])
      pixelList <- prec[,'cellID']
      
      train <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=calc_totrainCMP)
      train <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), train)
      colnames(train) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_prec[[j]])))
      
      # train <- apply(X=prec[[j]][,4:ncol(prec[[j]])], MARGIN=1, FUN=calc_totrainCMP)
      # train <- cbind(prec_all[[j]][,1:3], train)
      # colnames(train)[4] <- as.character(gsub(pattern='y', replacement='', years_analysis[[j]]))
      
      return(train)
    })
    names(TOTRAIN) <- years_prec
    TOTRAIN <- reshape::merge_recurse(TOTRAIN)
    
    
    # 6. CDD: Drought stress. Maximum number of consecutive dry days (i.e. with precipitation < 1 mm day-1)
    cat('*** 6. Calculate maximum number of consecutive dry days through list of years ***\n')
    CDD <- lapply(1:length(years_prec), function(j){
      
      # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
      
      library(lubridate)
      precYears <- year(colnames(prec)[-c(1:3)])
      
      # cat('Select 100-wet days by year\n')
      datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_prec[[j]])), x=precYears, fixed=TRUE)
      
      # cat('Define number of cells for final raster\n')
      ncell <- length(prec[,'cellID'])
      
      # cat('Define cells to analyse\n')
      pixelList <- prec[,'cellID']
      
      cons_days <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=dr_stressCMP, p_thresh=1)
      cons_days <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), cons_days)
      colnames(cons_days) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_prec[[j]])))
      
      return(cons_days)
      
    })
    names(CDD) <- years_prec
    CDD <- reshape::merge_recurse(CDD)
    
    
    # 7. P95D: Flash floods. Maximum 5-day running average precipitation
    cat('*** 7. Calculate maximum 5-day running average precipitation through list of years ***\n')
    P5D <- lapply(1:length(years_prec), function(j){
      
      # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
      
      library(lubridate)
      precYears <- year(colnames(prec)[-c(1:3)])
      
      # cat('Select 100-wet days by year\n')
      datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_prec[[j]])), x=precYears, fixed=TRUE)
      
      # cat('Define number of cells for final raster\n')
      ncell <- length(prec[,'cellID'])
      
      # cat('Define cells to analyse\n')
      pixelList <- prec[,'cellID']
      
      library(caTools)
      p5d <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=function(x){z <- caTools::runmean(x, k=5, endrule='NA'); z <- max(z, na.rm=TRUE); return(z)})
      p5d <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), p5d)
      colnames(p5d) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_prec[[j]])))
      
      return(p5d)
      
    })
    names(P5D) <- years_prec
    P5D <- reshape::merge_recurse(P5D)
    
    
    # 8. P_95: Flash floods. 95th percentile of daily precipitation
    cat('*** 8. Calculate 95th percentile of daily precipitation through list of years ***\n')
    P_95 <- lapply(1:length(years_prec), function(j){
      
      # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
      
      library(lubridate)
      precYears <- year(colnames(prec)[-c(1:3)])
      
      # cat('Select 100-wet days by year\n')
      datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_prec[[j]])), x=precYears, fixed=TRUE)
      
      # cat('Define number of cells for final raster\n')
      ncell <- length(prec[,'cellID'])
      
      # cat('Define cells to analyse\n')
      pixelList <- prec[,'cellID']
      
      library(caTools)
      p_95 <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=quantile, probs=.95, na.rm=TRUE)
      p_95 <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), p_95)
      colnames(p_95) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_prec[[j]])))
      
      return(p_95)
      
    })
    names(P_95) <- years_prec
    P_95 <- reshape::merge_recurse(P_95)
    
    
    # 9. ND_WS: Drought stress. Maximum number of consecutive days with ratio of actual to potential evapotranspiration (ETa/ETp) ratio below 0.5.
    cat('*** 9. Processing watbal_wrapper function for each cell\n')
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Solar radiation for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/dswrf/dswrf.RData', sep=''))
    dswrfAll <- ch2014_year; rm(ch2014_year)
    dswrfAll <- dswrfAll[years_analysis]
    dswrfAll <- lapply(1:length(dswrfAll), function(i){z <- as.data.frame(dswrfAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(dswrfAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(dswrfAll) <- years_analysis
    dswrfAll <- reshape::merge_recurse(dswrfAll)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Daily precipitation for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/prec/prec.RData', sep=''))
    precAll <- chirps_year; rm(chirps_year)
    precAll <- precAll[years_analysis]
    precAll <- lapply(1:length(precAll), function(i){z <- as.data.frame(precAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(precAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(precAll) <- years_analysis
    precAll <- reshape::merge_recurse(precAll)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Maximum temperature for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/tmax/tmax.RData', sep=''))
    tmaxAll <- ch2014_year; rm(ch2014_year)
    tmaxAll <- tmaxAll[years_analysis]
    tmaxAll <- lapply(1:length(tmaxAll), function(i){z <- as.data.frame(tmaxAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmaxAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(tmaxAll) <- years_analysis
    tmaxAll <- reshape::merge_recurse(tmaxAll)
    
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    cat('Loading: Minimum temperature for first wet season\n')
    ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
    
    load(paste(countyDir, '/tmin/tmin.RData', sep=''))
    tminAll <- ch2014_year; rm(ch2014_year)
    tminAll <- tminAll[years_analysis]
    tminAll <- lapply(1:length(tminAll), function(i){z <- as.data.frame(tminAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tminAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
    names(tminAll) <- years_analysis
    tminAll <- reshape::merge_recurse(tminAll)
    
    ncells <- length(Reduce(intersect, list(tmaxAll[,'cellID'], tminAll[,'cellID'], precAll[,'cellID'], dswrfAll[,'cellID'], soil[,'cellID'])))
    pixelList <- Reduce(intersect, list(tmaxAll[,'cellID'], tminAll[,'cellID'], precAll[,'cellID'], dswrfAll[,'cellID'], soil[,'cellID']))
    
    # Created function
    NDWSProcess <- function(j){ # By pixel
      library(lubridate)
      # cat(' Processing pixel:', pixelList[j],'\n')
      
      daysList <- Reduce(intersect, list(colnames(tmaxAll[,-c(1:3)]), colnames(tminAll[,-c(1:3)]),
                                         colnames(precAll[,-c(1:3)]), colnames(dswrfAll[,-c(1:3)])))
      
      out_all <- soil[which(soil$cellID==pixelList[j]), c('cellID', 'lon', 'lat')]
      out_all <- do.call("rbind", replicate(length(daysList), out_all, simplify=FALSE))
      out_all$SRAD <- as.numeric(dswrfAll[which(dswrfAll$cellID==pixelList[j]), match(daysList, colnames(dswrfAll))])
      out_all$TMIN <- as.numeric(tminAll[which(tminAll$cellID==pixelList[j]), match(daysList, colnames(tminAll))])
      out_all$TMAX <- as.numeric(tmaxAll[which(tmaxAll$cellID==pixelList[j]), match(daysList, colnames(tmaxAll))])
      out_all$RAIN <- as.numeric(precAll[which(precAll$cellID==pixelList[j]), match(daysList, colnames(precAll))])
      rownames(out_all) <- daysList
      
      soilcp <- soil[which(soil$cellID==pixelList[j]), 'soilcp']
      
      watbal_loc <- watbal_wrapper(out_all=out_all, soilcp=soilcp) # If we need more indexes are here
      watbal_loc <- watbal_loc[,c('cellID', 'lon', 'lat', 'ERATIO')]
      
      if(season=='first'){
        load(paste(countyDir, '/indx_fs_wet_days.RData', sep=''))
      } else {
        if(season=='second'){
          load(paste(countyDir, '/indx_ss_wet_days.RData', sep=''))
        } else {
          cat('Incorrect season, please verify.\n')
        }
      }
      
      indexs_wet_days <- indexs_wet_days[years_analysis]
      indexs_wet_days <- lapply(1:length(indexs_wet_days), function(i){z <- as.matrix(indexs_wet_days[[i]]); return(z)})
      
      ndws_year_pixel <- unlist(lapply(1:length(years_analysis), function(k){
        wetDays_year <- as.numeric(indexs_wet_days[[k]][which(indexs_wet_days[[k]][,'cellID']==pixelList[j]), 4:ncol(indexs_wet_days[[k]])])
        wetDays <-watbal_loc[wetDays_year,]
        ndws <- calc_wsdays(wetDays$ERATIO, season_ini=1, season_end=100, e_thresh=0.5)
        return(ndws)
      }))
      
      NDWS <- watbal_loc[1, c('cellID', 'lon', 'lat')]
      NDWS <- cbind(NDWS, t(ndws_year_pixel))
      colnames(NDWS)[4:ncol(NDWS)] <- as.character(gsub(pattern='y', replacement='', years_analysis))
      rownames(NDWS) <- 1:nrow(NDWS)
      
      return(NDWS)
    }
    library(compiler)
    NDWSProcessCMP <- cmpfun(NDWSProcess)
    
    # Running process for all pixel list
    library(parallel)
    NDWS <- mclapply(1:ncells, FUN=NDWSProcessCMP, mc.cores=20)
    NDWS <- do.call(rbind, NDWS)
    
    # Save all indexes
    clim_indexes <- list(TMEAN=TMEAN, GDD_1=GDD_1, GDD_2=GDD_2, ND_t35=ND_t35,
                         TOTRAIN=TOTRAIN, CDD=CDD, P5D=P5D, P_95=P_95, NDWS=NDWS)
    
    # Save according to season
    if(season == 'first'){
      seasonDir <- paste(outputDir, '/first_season', sep='')
      if(!dir.exists(seasonDir)){dir.create(seasonDir)}
      save(clim_indexes, file=paste(seasonDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_first_season_2015.RData', sep=''))
    } else {
      if(season == 'second'){
        seasonDir <- paste(outputDir, '/second_season', sep='')
        if(!dir.exists(seasonDir)){dir.create(seasonDir)}
        save(clim_indexes, file=paste(seasonDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_second_season_2015.RData', sep=''))
      }
    }
    
    # @@@ } else {
    # @@@   cat('Process has been done before. It is not necessary recalculate.\n')
    # @@@ }
    
  } else {
    cat('Process failed\n')
  }
  
  return(cat('Process done\n'))
  
}
# calc_climIndices_v <- Vectorize(calc_climIndices, vectorize.args='county')

countyList <- countyList[6,]

library(parallel)
mclapply(1:length(seasonList), function(j){
  lapply(1:length(countyList$County), function(i){
    calc_climIndices(county=countyList$County[i], season=seasonList[[j]])
  })
}, mc.cores=2)
