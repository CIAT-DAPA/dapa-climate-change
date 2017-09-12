# Calculating climatic indices by counties in Kenya - Future information first and second season
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
library(lubridate)

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

countyList <- countyList[3,]

periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"

# years_analysis <- paste('y', 1981:2005, sep='')
inputDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat'
outputDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/future'

seasonList <- c('first', 'second')

calc_climIndices <- function(county='Kilifi', season='first'){
  
  cat('\n\n\n*** Processing:', gsub(pattern=' ', replacement='_', county, fixed=TRUE), 'county ***\n\n')
  
  lapply(1:length(periodList), function(j){
    
    cat('Processing period:', periodList[j], '\n')
    
    lapply(1:length(rcpList), function(k){
      
      cat('Processing RCP:', rcpList[k], '\n')
      
      lapply(1:length(gcmList), function(l){
        
        cat('Processing GCM:', gcmList[l], '\n')
        
        indexes <- paste(outputDir, '/', season,'_season/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_', season,'_season.RData', sep='')
        
        #if(!file.exists(indexes)){
        
        years_analysis <- as.numeric(unlist(strsplit(x=periodList[j], split='_')))
        years_analysis <- years_analysis[1]:years_analysis[2]
        
        countyDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', county), sep='')
        if(length(list.files(path=countyDir, recursive=TRUE))==14){
          
          cat('Loading raster mask for:', gsub(pattern=' ', replacement='_', county), '\n')
          countyMask <- raster(paste("/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/", gsub(pattern=' ', replacement='_', county), "_base.tif", sep=""))
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Solar radiation for corresponding wet season\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          if(season == 'first'){
            load(paste(countyDir, '/dswrf/bc_qmap_dswrf_', periodList[j], '_fs_wet_days.RData', sep=''))
            dswrf <- first_season_var; rm(first_season_var)
          } else {
            if(season == 'second'){
              load(paste(countyDir, '/dswrf/bc_qmap_dswrf_', periodList[j], '_ss_wet_days.RData', sep=''))
              dswrf <- second_season_var; rm(second_season_var)
            }
          }
          dswrf <- lapply(1:length(dswrf), function(i){z <- as.data.frame(dswrf[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(dswrf)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
          names(dswrf) <- as.character(years_analysis)
          dswrf <- reshape::merge_recurse(dswrf)
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Daily precipitation for corresponding wet season\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          if(season == 'first'){
            load(paste(countyDir, '/prec/bc_qmap_prec_', periodList[j], '_fs_wet_days.RData', sep=''))
            prec <- chirps_wet_days; rm(chirps_wet_days)
          } else {
            if(season == 'second'){
              load(paste(countyDir, '/prec/bc_qmap_prec_', periodList[j], '_ss_wet_days.RData', sep=''))
              prec <- chirps_wet_days; rm(chirps_wet_days)
            }
          }
          prec <- lapply(1:length(prec), function(i){z <- as.data.frame(prec[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(prec)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
          names(prec) <- as.character(years_analysis)
          prec <- reshape::merge_recurse(prec)
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Soil data\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/', gsub(pattern=' ', replacement='_', county), '/soil/soil_data.RData', sep=''))
          soil <- soil_data_county; rm(soil_data_county)
          soil <- soil[,c("cellID","lon.x","lat.x","id_coarse","rdepth","d.25","d.100","d.225","d.450","d.800","d.1500","soilcp")]
          names(soil)[2:3] <- c('lon','lat')
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Maximum temperature for corresponding wet season\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          if(season == 'first'){
            load(paste(countyDir, '/tmax/bc_qmap_tmax_', periodList[j], '_fs_wet_days.RData', sep=''))
            tmax <- first_season_var; rm(first_season_var)
          } else {
            if(season == 'second'){
              load(paste(countyDir, '/tmax/bc_qmap_tmax_', periodList[j], '_ss_wet_days.RData', sep=''))
              tmax <- second_season_var; rm(second_season_var)
            }
          }
          tmax <- lapply(1:length(tmax), function(i){z <- as.data.frame(tmax[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmax)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
          names(tmax) <- as.character(years_analysis)
          tmax <- reshape::merge_recurse(tmax)
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Minimum temperature for corresponding wet season\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          if(season == 'first'){
            load(paste(countyDir, '/tmin/bc_qmap_tmin_', periodList[j], '_fs_wet_days.RData', sep=''))
            tmin <- first_season_var; rm(first_season_var)
          } else {
            if(season == 'second'){
              load(paste(countyDir, '/tmin/bc_qmap_tmin_', periodList[j], '_ss_wet_days.RData', sep=''))
              tmin <- second_season_var; rm(second_season_var)
            }
          }
          tmin <- lapply(1:length(tmin), function(i){z <- as.data.frame(tmin[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmin)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
          names(tmin) <- as.character(years_analysis)
          tmin <- reshape::merge_recurse(tmin)
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Calculate: Mean temperature for corresponding wet season\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          library(foreach)
          library(doMC)
          registerDoMC(20)
          tmean <- foreach(j=1:length(years_analysis)) %dopar% {
            
            #cat(' Processing year:', years_analysis[[j]],'\n')
            library(lubridate)
            tmaxYears <- year(colnames(tmax)[-c(1:3)])
            
            #cat('Select 100-wet days by year\n')
            datesID <- grep(pattern=as.numeric(years_analysis[[j]]), x=tmaxYears, fixed=TRUE)
            
            #cat('Define number of cells for final raster\n')
            ncell <- length(Reduce(intersect, list(tmax[,'cellID'], tmin[,'cellID'])))
            
            #cat('Define cells to analyse\n')
            pixelList <- Reduce(intersect, list(tmax[,'cellID'], tmin[,'cellID']))
            
            tmean <- tmeanCMP(TMAX=tmax[match(pixelList, tmax[,'cellID']), datesID+3],
                              TMIN=tmin[match(pixelList, tmin[,'cellID']), datesID+3],
                              season_ini=1, season_end=100)
            tmean <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), tmean)
            colnames(tmean) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
            
            return(tmean)
          }
          names(tmean) <- as.character(years_analysis)
          
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          ### CLIMATIC INDICES to calculate
          cat('Calculate CLIMATE INDEXES\n')
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
          names(TMEAN) <- as.character(years_analysis)
          TMEAN <- reshape::merge_recurse(TMEAN)
          
          # plot(as.numeric(TMEAN[1, 4:ncol(TMEAN)]), ty='l', ylim=c(25, 35))
          # for(i in 2:nrow(TMEAN)) {lines(as.numeric(TMEAN[i, 4:ncol(TMEAN)]))}
          
          ### 2. GDD_1: Crop duration. Growing degree days calculated using a capped-top function with TB=10 ºC
          cat('*** 2. Calculate crop duration with TB=10 ºC through list of years ***\n')
          GDD_1 <- lapply(1:length(years_analysis), function(j){
            #cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
            gdd_1 <- apply(X=tmean[[j]][,4:ncol(tmean[[j]])], MARGIN=1, FUN=calc_cdurCMP, t_thresh=10)
            gdd_1 <- cbind(tmean[[j]][,1:3], gdd_1)
            colnames(gdd_1)[4] <- as.character(gsub(pattern='y', replacement='', years_analysis[[j]]))
            return(gdd_1)
          })
          names(GDD_1) <- as.character(years_analysis)
          GDD_1 <- reshape::merge_recurse(GDD_1)
          
          # plot(as.numeric(GDD_1[1, 4:ncol(GDD_1)]), ty='l', ylim=c(15, 25))
          # for(i in 2:nrow(GDD_1)) {lines(as.numeric(GDD_1[i, 4:ncol(GDD_1)]))}
          
          ### 3. GDD_2: Crop duration. Growing degree days calculated using a capped-top function with TO=25 ºC
          cat('*** 3. Calculate crop duration with TB=25 ºC through list of years ***\n')
          GDD_2 <- lapply(1:length(years_analysis), function(j){
            #cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
            gdd_2 <- apply(X=tmean[[j]][,4:ncol(tmean[[j]])], MARGIN=1, FUN=calc_cdurCMP, t_thresh=25)
            gdd_2 <- cbind(tmean[[j]][,1:3], gdd_2)
            colnames(gdd_2)[4] <- as.character(gsub(pattern='y', replacement='', years_analysis[[j]]))
            return(gdd_2)
          })
          names(GDD_2) <- as.character(years_analysis)
          GDD_2 <- reshape::merge_recurse(GDD_2)
          
          # plot(as.numeric(GDD_2[1, 4:ncol(GDD_2)]), ty='l', ylim=c(0,10))
          # for(i in 2:nrow(GDD_2)) {lines(as.numeric(GDD_2[i, 4:ncol(GDD_2)]))}
          
          ### 4. ND_t35: Heat stress. Total number of days with maximum temperature greater or equal to 35 ºC.
          cat('*** 4. Calculate total number of days with maximum temperature greater or equal to 35 ºC through list of years ***\n')
          ND_t35 <- lapply(1:length(years_analysis), function(j){
            
            #cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
            library(lubridate)
            tmaxYears <- year(as.Date(colnames(tmax)[-c(1:3)]))
            
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
          
          # plot(as.numeric(ND_t35[1, 4:ncol(ND_t35)]), ty='l', ylim=c(0, 100))
          # for(i in 2:nrow(ND_t35)) {lines(as.numeric(ND_t35[i, 4:ncol(ND_t35)]))}
          
          ### 5. P_tot: Annual total precipitation
          cat('*** 5. Calculate total precipitation by season through list of years ***\n')
          TOTRAIN <- lapply(1:length(years_analysis), function(j){
            
            library(lubridate)
            precYears <- year(colnames(prec)[-c(1:3)])
            datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])), x=precYears, fixed=TRUE)
            ncell <- length(prec[,'cellID'])
            pixelList <- prec[,'cellID']
            
            train <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=calc_totrainCMP)
            train <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), train)
            colnames(train) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_analysis[[j]])))
            
            return(train)
          })
          names(TOTRAIN) <- years_analysis
          TOTRAIN <- reshape::merge_recurse(TOTRAIN)
          
          # plot(as.numeric(TOTRAIN[1, 4:ncol(TOTRAIN)]), ty='l', ylim=c(0,3000))
          # for(i in 2:nrow(TOTRAIN)) {lines(as.numeric(TOTRAIN[i, 4:ncol(TOTRAIN)]))}
          
          # 6. CDD: Drought stress. Maximum number of consecutive dry days (i.e. with precipitation < 1 mm day-1)
          cat('*** 6. Calculate maximum number of consecutive dry days through list of years ***\n')
          CDD <- lapply(1:length(years_analysis), function(j){
            
            # cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
            library(lubridate)
            precYears <- year(as.Date(colnames(prec)[-c(1:3)]))
            
            # cat('Select 100-wet days by year\n')
            datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])), x=precYears, fixed=TRUE)
            
            # cat('Define number of cells for final raster\n')
            ncell <- length(prec[,'cellID'])
            
            # cat('Define cells to analyse\n')
            pixelList <- prec[,'cellID']
            
            cons_days <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=dr_stressCMP, p_thresh=1)
            cons_days <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), cons_days)
            colnames(cons_days) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_analysis[[j]])))
            
            return(cons_days)
            
          })
          names(CDD) <- years_analysis
          CDD <- reshape::merge_recurse(CDD)
          
          # plot(as.numeric(CDD[1, 4:ncol(CDD)]), ty='l', ylim=c(0, 100))
          # for(i in 2:nrow(CDD)) {lines(as.numeric(CDD[i, 4:ncol(CDD)]))}
          
          # 7. P95D: Flash floods. Maximum 5-day running average precipitation
          cat('*** 7. Calculate maximum 5-day running average precipitation through list of years ***\n')
          P5D <- lapply(1:length(years_analysis), function(j){
            
            #cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
            library(lubridate)
            precYears <- year(as.Date(colnames(prec)[-c(1:3)]))
            
            #cat('Select 100-wet days by year\n')
            datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])), x=precYears, fixed=TRUE)
            
            #cat('Define number of cells for final raster\n')
            ncell <- length(prec[,'cellID'])
            
            #cat('Define cells to analyse\n')
            pixelList <- prec[,'cellID']
            
            library(caTools)
            p5d <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=function(x){z <- caTools::runmean(x, k=5, endrule='NA'); z <- max(z, na.rm=TRUE); return(z)})
            p5d <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), p5d)
            colnames(p5d) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_analysis[[j]])))
            
            return(p5d)
            
          })
          names(P5D) <- years_analysis
          P5D <- reshape::merge_recurse(P5D)
          
          # plot(as.numeric(P5D[1, 4:ncol(P5D)]), ty='l', ylim=c(0, 200))
          # for(i in 2:nrow(P5D)) {lines(as.numeric(P5D[i, 4:ncol(P5D)]))}
          
          # 8. P_95: Flash floods. 95th percentile of daily precipitation
          cat('*** 8. Calculate 95th percentile of daily precipitation through list of years ***\n')
          P_95 <- lapply(1:length(years_analysis), function(j){
            
            #cat(' Processing year:', as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])),'\n')
            library(lubridate)
            precYears <- year(as.Date(colnames(prec)[-c(1:3)]))
            
            #cat('Select 100-wet days by year\n')
            datesID <- grep(pattern=as.numeric(gsub(pattern='y', replacement='', years_analysis[[j]])), x=precYears, fixed=TRUE)
            
            #cat('Define number of cells for final raster\n')
            ncell <- length(prec[,'cellID'])
            
            #cat('Define cells to analyse\n')
            pixelList <- prec[,'cellID']
            
            library(caTools)
            p_95 <- apply(X=prec[match(pixelList, prec[,'cellID']), datesID+3], MARGIN=1, FUN=quantile, probs=.95, na.rm=TRUE)
            p_95 <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), p_95)
            colnames(p_95) <- c('cellID', 'lon', 'lat', as.character(gsub(pattern='y', replacement='', years_analysis[[j]])))
            
            return(p_95)
            
          })
          names(P_95) <- years_analysis
          P_95 <- reshape::merge_recurse(P_95)
          
          # plot(as.numeric(P_95[1, 4:ncol(P_95)]), ty='l', ylim=c(0,100))
          # for(i in 2:nrow(P_95)) {lines(as.numeric(P_95[i, 4:ncol(P_95)]))}
          
          # 9. ND_WS: Drought stress. Maximum number of consecutive days with ratio of actual to potential evapotranspiration (ETa/ETp) ratio below 0.5.
          cat('*** 9. Processing watbal_wrapper function for each cell\n')
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Solar radiation for complete period\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          load(paste(countyDir, '/dswrf/bc_qmap_dswrf_', periodList[j],'.RData', sep=''))
          dswrfAll <- gcmFutBC
          dswrfAll <- as.data.frame(dswrfAll)
          dswrfAll <- dswrfAll[complete.cases(dswrfAll),]
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Daily precipitation for complete period\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          load(paste(countyDir, '/prec/bc_qmap_prec_', periodList[j], '.RData', sep=''))
          precAll <- gcmFutBC
          precAll <- as.data.frame(precAll)
          precAll <- precAll[complete.cases(precAll),]
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Maximum temperature for complete period\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          load(paste(countyDir, '/tmax/bc_qmap_tmax_', periodList[j], '.RData', sep=''))
          tmaxAll <- gcmFutBC
          tmaxAll <- as.data.frame(tmaxAll)
          tmaxAll <- tmaxAll[complete.cases(tmaxAll),]
          
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          cat('Loading: Minimum temperature for complete period\n')
          ### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
          
          load(paste(countyDir, '/tmin/bc_qmap_tmin_', periodList[j], '.RData', sep=''))
          tminAll <- gcmFutBC
          tminAll <- as.data.frame(tminAll)
          tminAll <- tminAll[complete.cases(tminAll),]
          
          ncells <- length(Reduce(intersect, list(tmaxAll[,'cellID'], tminAll[,'cellID'], precAll[,'cellID'], dswrfAll[,'cellID'], soil[,'cellID'])))
          pixelList <- Reduce(intersect, list(tmaxAll[,'cellID'], tminAll[,'cellID'], precAll[,'cellID'], dswrfAll[,'cellID'], soil[,'cellID']))
          
          # Created function
          NDWSProcess <- function(j){ # By pixel
            library(lubridate)
            #cat(' Processing pixel:', pixelList[j],'\n')
            
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
            
            indexs_wet_days <- lapply(1:length(indexs_wet_days), function(i){z <- as.matrix(indexs_wet_days[[i]]); return(z)})
            
            ndws_year_pixel <- unlist(lapply(1:length(years_analysis), function(k){
              wetDays_year <- as.numeric(indexs_wet_days[[k]][which(indexs_wet_days[[k]][,'cellID']==pixelList[j]), 4:ncol(indexs_wet_days[[k]])])
              wetDays <- watbal_loc[wetDays_year,]
              ndws <- calc_wsdays(wetDays$ERATIO, season_ini=1, season_end=100, e_thresh=0.5)
              return(ndws)
            }))
            
            NDWS <- watbal_loc[1, c('cellID', 'lon', 'lat')]
            NDWS <- cbind(NDWS, t(ndws_year_pixel))
            colnames(NDWS)[4:ncol(NDWS)] <- as.character(gsub(pattern='y', replacement='', years_analysis))
            rownames(NDWS) <- 1:nrow(NDWS)
            
            return(NDWS)
          }
          #library(compiler)
          #NDWSProcessCMP <- cmpfun(NDWSProcess)
          
          # Running process for all pixel list
          library(parallel)
          NDWS <- mclapply(1:ncells, FUN=NDWSProcess, mc.cores=20)
          NDWS <- do.call(rbind, NDWS)
          
          # plot(as.numeric(NDWS[1, 4:ncol(NDWS)]), ty='l', ylim=c(0,100))
          # for(i in 2:nrow(NDWS)) {lines(as.numeric(NDWS[i, 4:ncol(NDWS)]))}
          
          # Save all indexes
          clim_indexes <- list(TMEAN=TMEAN, GDD_1=GDD_1, GDD_2=GDD_2, ND_t35=ND_t35,
                               TOTRAIN=TOTRAIN, CDD=CDD, P5D=P5D, P_95=P_95, NDWS=NDWS)
          
          # Save according to season
          if(season == 'first'){
            seasonDir <- paste(outputDir, '/first_season/', gcmList[l], '/', periodList[j], '/', rcpList[k], sep='')
            if(!dir.exists(seasonDir)){dir.create(seasonDir, recursive=TRUE)}
            save(clim_indexes, file=paste(seasonDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_first_season.RData', sep=''))
          } else {
            if(season == 'second'){
              seasonDir <- paste(outputDir, '/second_season/', gcmList[l], '/', periodList[j], '/', rcpList[k], sep='')
              if(!dir.exists(seasonDir)){dir.create(seasonDir, recursive=TRUE)}
              save(clim_indexes, file=paste(seasonDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), '_second_season.RData', sep=''))
            }
          }
          
        } else {
          
          cat('Process failed. Climatic information is not processed yet.\n')
          
        }
        
        # } else {
        #   
        #   cat('Process has been done before. It is not necessary recalculate.\n')
        #   
        # }
        
      })
    })
  })
  
  return(cat('Process done.\n'))
  
}

countyList <- countyList[2,]

lapply(1:length(seasonList), function(j){
  lapply(1:nrow(countyList), function(i){
    calc_climIndices(county=countyList$County[i], season=seasonList[j])
  })
})
