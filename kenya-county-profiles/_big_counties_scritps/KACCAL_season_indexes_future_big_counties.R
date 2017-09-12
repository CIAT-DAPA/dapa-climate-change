
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                     First season estimates from future GCMs precipitation
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

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

# Important functions
rsum.lapply <- function(x, n=3L) # Calculate rollin sum
{
  lapply(1:(length(x)-n+1), function(i)
  {
    # Sum for n consecutive days
    z <- sum(x[i:(i+n-1)])
    # Indices used to calculate the sum
    seq.sum <- as.numeric(i:(i+n-1))
    # List with SUM and INDICES
    results <- list(z, seq.sum)
    return(results)
  })
}
cumulative.r.sum <- function(results){ unlist(lapply(results, function(x){z <- x[[1]]; return(z)})) } # Extract the SUM
cumulative.r.ind <- function(results){ lapply(results, function(x){z <- x[[2]]; return(z)}) } # Extract INDICES related to SUM

periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"

# First semester: 181-182 days (depending on leap years)

library(data.table)
countyList <- countyList[3,]

lapply(1:nrow(countyList), function(i)
{
  cat('***Processing county:', countyList$County[[i]], '***\n')
  
  lapply(1:length(periodList), function(j){
    
    lapply(1:length(rcpList), function(k){
      
      lapply(1:length(gcmList), function(l){
        
        precFile <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec/bc_qmap_prec_', periodList[j],'.RData', sep='')
        
        if(file.exists(precFile)){
          
          t1 <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec/bc_qmap_prec_', periodList[j], '_fs_wet_days.RData', sep='')
          t2 <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/indx_fs_wet_days.RData', sep='')
          
          if(!file.exists(t1) & !file.exists(t2)){
            
            load(precFile)
            library(lubridate)
            years <- sort(unique(as.numeric(year(as.Date(colnames(gcmFutBC[,-c(1:3)]))))))
            
            library(parallel)
            chirps_wet_days <- mclapply(1:length(years), function(m)
            {
              
              # Select data of each year
              df <- gcmFutBC[,c(1:3, which(year(as.Date(colnames(gcmFutBC[,-c(1:3)])))==years[m])+3)]
              
              # Identify first wet season per pixel
              wet100fs_county <- lapply(1:nrow(df), function(k)
              {
                first_season <- rsum.lapply(x=as.numeric(df[k,])[-c(1:3)][1:181], n=100)
                sum.ind <- cumulative.r.ind(first_season)[[which.max(cumulative.r.sum(first_season))]]
                df_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(df[k,])[-c(1:3)][1:181][sum.ind])))
                names(df_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
                
                sum.ind_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(sum.ind))))
                names(sum.ind_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
                
                wet100fs <- list(prec=df_upd,
                                 ind=sum.ind_upd)
                return(wet100fs)
              })
              wet_days <- lapply(1:length(wet100fs_county), function(k){z <- wet100fs_county[[k]][[1]]; return(z)})
              wet_indx <- lapply(1:length(wet100fs_county), function(k){z <- wet100fs_county[[k]][[2]]; return(z)})
              wet_days <- do.call(rbind, wet_days)
              wet_indx <- do.call(rbind, wet_indx)
              wet100fs_county <- list(prec=wet_days,
                                      ind=wet_indx)
              return(wet100fs_county)
              
            }, mc.cores=20)
            
            indexs_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[2]]; return(z)})
            chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[1]]; return(z)})
            chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]]; z <- data.table(z); return(z)})
            names(chirps_wet_days) <- paste('y', years, sep='')
            names(indexs_wet_days) <- paste('y', years, sep='')
            
            save(chirps_wet_days, file=paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec/bc_qmap_prec_', periodList[j], '_fs_wet_days.RData', sep=''))
            save(indexs_wet_days, file=paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/indx_fs_wet_days.RData', sep=''))
            
            return(cat('Process has been done for:', countyList$County[[i]], '\n'))
            
          } else {
            cat('Process has been done before. It is not necessary recalculate.\n')
          }
          
        } else {
          cat('Process failed: Precipitation file is not ready yet.\n')
        }
        
      })
      
    })
    
  })
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                     Second season estimates from future GCMs precipitation
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

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

# Important functions
rsum.lapply <- function(x, n=3L) # Calculate rollin sum
{
  lapply(1:(length(x)-n+1), function(i)
  {
    # Sum for n consecutive days
    z <- sum(x[i:(i+n-1)])
    # Indices used to calculate the sum
    seq.sum <- as.numeric(i:(i+n-1))
    # List with SUM and INDICES
    results <- list(z, seq.sum)
    return(results)
  })
}
cumulative.r.sum <- function(results){ unlist(lapply(results, function(x){z <- x[[1]]; return(z)})) } # Extract the SUM
cumulative.r.ind <- function(results){ lapply(results, function(x){z <- x[[2]]; return(z)}) } # Extract INDICES related to SUM

periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"

# Second semester: 184 days

library(data.table)
countyList <- countyList[3,]

lapply(1:nrow(countyList), function(i)
{
  
  cat('***Processing county:', countyList$County[[i]], '***\n')
  
  lapply(1:length(periodList), function(j){
    
    lapply(1:length(rcpList), function(k){
      
      lapply(1:length(gcmList), function(l){
        
        precFile <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec/bc_qmap_prec_', periodList[j],'.RData', sep='')
        
        if(file.exists(precFile)){
          
          t1 <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec/bc_qmap_prec_', periodList[j], '_ss_wet_days.RData', sep='')
          t2 <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/indx_ss_wet_days.RData', sep='')
          
          if(!file.exists(t1) & !file.exists(t2)){
            
            load(precFile)
            library(lubridate)
            years <- sort(unique(as.numeric(year(as.Date(colnames(gcmFutBC[,-c(1:3)]))))))
            
            library(parallel)
            chirps_wet_days <- mclapply(1:length(years), function(m)
            {
              
              # Select data of each year
              df <- gcmFutBC[,c(1:3, which(year(as.Date(colnames(gcmFutBC[,-c(1:3)])))==years[m])+3)]
              df <- df[complete.cases(df),]
              
              # Identify first wet season per pixel
              wet100ss_county <- lapply(1:nrow(df), function(k)
              {
                second_season <- rsum.lapply(x=as.numeric(df[k,])[-c(1:3)][182:length(as.numeric(df[k,])[-c(1:3)])], n=100)
                sum.ind <- cumulative.r.ind(second_season)[[which.max(cumulative.r.sum(second_season))]]
                df_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(df[k,])[-c(1:3)][182:length(as.numeric(df[k,])[-c(1:3)])][sum.ind])))
                names(df_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
                
                sum.ind_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], (182:length(as.numeric(df[k,])[-c(1:3)]))[sum.ind])))
                names(sum.ind_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
                
                wet100ss <- list(prec=df_upd,
                                 ind=sum.ind_upd)
                return(wet100ss)
              })
              wet_days <- lapply(1:length(wet100ss_county), function(k){z <- wet100ss_county[[k]][[1]]; return(z)})
              wet_indx <- lapply(1:length(wet100ss_county), function(k){z <- wet100ss_county[[k]][[2]]; return(z)})
              wet_days <- do.call(rbind, wet_days)
              wet_indx <- do.call(rbind, wet_indx)
              wet100ss_county <- list(prec=wet_days,
                                      ind=wet_indx)
              return(wet100ss_county)
              
            }, mc.cores=20)
            
            indexs_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[2]]; return(z)})
            chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[1]]; return(z)})
            chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]]; z <- data.table(z); return(z)})
            names(chirps_wet_days) <- paste('y', years, sep='')
            names(indexs_wet_days) <- paste('y', years, sep='')
            
            save(chirps_wet_days, file=paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec/bc_qmap_prec_', periodList[j], '_ss_wet_days.RData', sep=''))
            save(indexs_wet_days, file=paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/indx_ss_wet_days.RData', sep=''))
            
            return(cat('Process has been done for:', countyList$County[[i]], '\n'))
            
          } else {
            cat('Process has been done before. It is not necessary recalculate.\n')
          }
          
        } else {
          cat('Process failed: Precipitation file is not ready yet.\n')
        }
        
      })
      
    })
    
  })
  
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                          Temperatures and solar radiation first and second season estimates
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

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

periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"

library(data.table)
countyList <- countyList[3,]

lapply(1:nrow(countyList), function(i)
{
  # i=1; # Kilifi
  # j=1; # 2021_2045
  # k=2; # RCP45
  # l=8; # miroc_esm
  cat('\n***Processing county:', countyList$County[[i]], '***\n\n')
  
  lapply(1:length(periodList), function(j){
    
    cat('Processing period:', periodList[j], '\n')
    
    lapply(1:length(rcpList), function(k){
      
      cat('Processing RCP:', rcpList[k], '\n')
      
      lapply(1:length(gcmList), function(l){
        
        cat('Processing GCM:', gcmList[l], '\n')
        
        # Define county directory
        countyDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/bc_quantile_0_05deg_lat/', gcmList[l], '/', periodList[j], '/', rcpList[k], '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
        if(length(list.files(path=countyDir, pattern='*_wet_days.RData$'))==2){
          
          cat('100-wettest days was calculated. Continue processing.\n')
          cat('Loading indexes by wet season/year/pixel\n')
          load(paste(countyDir, '/indx_fs_wet_days.RData', sep='')) # First 100-wettest season
          frs_wet_season <- indexs_wet_days; rm(indexs_wet_days)
          load(paste(countyDir, '/indx_ss_wet_days.RData', sep='')) # Second 100-wettest season
          scn_wet_season <- indexs_wet_days; rm(indexs_wet_days)
          
          # Variables to process
          varList <- c('tmax', 'tmin', 'dswrf')
          
          lapply(1:length(varList), function(m)
          {
            
            if(length(list.files(paste(countyDir, '/', varList[m], sep=''), recursive=TRUE))>0){
              
              if(length(list.files(paste(countyDir, '/', varList[m], sep=''), recursive=TRUE))==1){
                
                cat('Loading:', varList[[m]], 'data\n')
                
                countyVarDir <- paste(countyDir, '/', varList[[m]], sep='')
                load(paste(countyVarDir, '/bc_qmap_', varList[[m]], '_', periodList[j], '.RData', sep=''))
                
                # library(parallel)
                library(foreach)
                library(doMC)
                registerDoMC(20)
                
                ch2014_var_wet_days <- foreach(n=1:length(frs_wet_season)) %dopar% {
                  
                  # cat('Selecting data for year:', as.numeric(gsub(pattern='y', replacement='', names(frs_wet_season)[n])), '\n')
                  
                  daysYr <- which(year(as.Date(colnames(gcmFutBC)[4:ncol(gcmFutBC)]))==as.numeric(gsub(pattern='y', replacement='', names(frs_wet_season)[n])))
                  df <- gcmFutBC[,c(1:3, daysYr+3)]
                  
                  if(length(daysYr) < 365){
                    missDays <- setdiff(as.character(seq(as.Date(paste(as.numeric(gsub(pattern='y', replacement='', names(frs_wet_season)[n])), '-01-01', sep='')), as.Date(paste(as.numeric(gsub(pattern='y', replacement='', names(frs_wet_season)[n])), '-12-31', sep='')), by=1)), colnames(gcmFutBC[,daysYr+3]))
                    missDays <- missDays[-which(mday(as.Date(missDays))==29 & month(as.Date(missDays))==2)]
                    missDays_mean <- lapply(1:length(missDays), function(x){
                      prev_forDays <- as.character(c(as.Date(missDays[x])-c(1:3), as.Date(missDays[x])+c(1:3)))
                      grep2 <- Vectorize(grep, vectorize.args='pattern')
                      mtchDays <- unlist(grep2(pattern=prev_forDays, x=colnames(df[,-c(1:3)]), fixed=TRUE))
                      miss_day <- data.frame(rowMeans(df[,mtchDays], na.rm=TRUE))
                      colnames(miss_day) <- as.character(missDays[x])
                      return(miss_day)
                    })
                    missDays_mean <- do.call(cbind, missDays_mean)
                    df <- cbind(df, missDays_mean)
                    df <- df[,c('cellID', 'lon', 'lat', sort(colnames(df)[4:ncol(df)]))]
                  }
                  
                  df <- as.data.frame(df)
                  df_frs <- frs_wet_season[[n]]
                  df_scn <- scn_wet_season[[n]]
                  
                  pixelList <- Reduce(intersect, list(df$cellID, df_frs$cellID, df_scn$cellID))
                  
                  wet100_county <- lapply(1:length(pixelList), function(o)
                  {
                    
                    #cat('Selecting 100-wettest days in pixel:', pixelList[o], '\n')
                    
                    df_upd <- df[which(df$cellID==pixelList[o]),]
                    cell   <- df_upd[,'cellID']
                    
                    if(length(which(df_frs$cellID==cell)) > 0){
                      
                      indx_frs <- as.numeric(df_frs[which(df_frs$cellID==cell),-c(1:3)]) # Indexes related to first season
                      df_upd_frs <- cbind(df_upd[,c('cellID', 'lon', 'lat')], df_upd[,-c(1:3)][,indx_frs])
                      names(df_upd_frs)[4:length(df_upd_frs)] <- paste('d', 1:100, sep='')
                      
                      indx_scn <- as.numeric(df_scn[which(df_scn$cellID==cell),-c(1:3)]) # Indexes related to second season
                      df_upd_scn <- cbind(df_upd[,c('cellID', 'lon', 'lat')], df_upd[,-c(1:3)][,indx_scn])
                      names(df_upd_scn)[4:length(df_upd_scn)] <- paste('d', 1:100, sep='')
                      
                      wet100 <- list(first_season=df_upd_frs,
                                     second_season=df_upd_scn)
                      return(wet100)
                    } else {
                      return(cat('CellID:', cell, 'does not exists, continue with the process\n'))
                    }
                    
                  })
                  
                  first_season <- lapply(1:length(wet100_county), function(k){z <- wet100_county[[k]][[1]]; return(z)})
                  second_season <- lapply(1:length(wet100_county), function(k){z <- wet100_county[[k]][[2]]; return(z)})
                  first_season <- do.call(rbind, first_season)
                  second_season <- do.call(rbind, second_season)
                  wet100_county <- list(f_season=first_season,
                                        s_season=second_season)
                  
                  return(wet100_county)
                }
                
                first_season_var <- lapply(1:length(ch2014_var_wet_days), function(k){z <- ch2014_var_wet_days[[k]][[1]]; z <- data.table(z); return(z)})
                names(first_season_var) <- names(frs_wet_season)
                
                second_season_var <- lapply(1:length(ch2014_var_wet_days), function(k){z <- ch2014_var_wet_days[[k]][[2]]; z <- data.table(z); return(z)})
                names(second_season_var) <- names(frs_wet_season)
                
                save(first_season_var, file=paste(countyVarDir, '/bc_qmap_', varList[[m]], '_', periodList[j], '_fs_wet_days.RData', sep=''))
                save(second_season_var, file=paste(countyVarDir, '/bc_qmap_', varList[[m]], '_', periodList[j], '_ss_wet_days.RData', sep=''))
                
              } else {
                cat('Process have been done.\n')
              }
              
            } else {
              cat('Process failed. At least one variable has not been calculated.\n')
            }
            
          })
          
        } else {
          cat('100-wettest days was not calculated. Stop process.\n')
        }
        
      })
    })
  })
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})
