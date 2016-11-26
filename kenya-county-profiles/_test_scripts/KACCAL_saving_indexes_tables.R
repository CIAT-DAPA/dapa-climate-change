# Sharing information
# H. Achicanoy
# CIAT, 2016

options(warn=-1)
if(!require(data.table)){install.packages('data.table'); library(data.table)} else {library(data.table)}
if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)}
if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)}
if(!require(rasterVis)){install.packages('rasterVis'); library(rasterVis)} else {library(rasterVis)}
if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)}
if(!require(trend)){install.packages('trend'); library(trend)} else {library(trend)}
if(!require(ggthemes)){install.packages('ggthemes'); library(ggthemes)} else {library(ggthemes)}
if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)}
if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)}
if(!require(FactoMineR)){install.packages('FactoMineR'); library(FactoMineR)} else {library(FactoMineR)}
if(!require(factoextra)){install.packages('factoextra'); library(factoextra)} else {library(factoextra)}
if(!require(lubridate)){install.packages('lubridate'); library(lubridate)} else {library(lubridate)}
if(!require(plyr)){install.packages('plyr'); library(plyr)} else {library(plyr)}

countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot'))

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Saving historical processed indexes in tables
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

histDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical'
seaList <- c('first_season', 'second_season')

seasonMetrics <- lapply(1:length(seaList), function(i){
  
  seaDir   <- paste(histDir, '/', seaList[i], sep='')
  list2015 <- list.files(path=seaDir, pattern='*2015.RData$', full.names=TRUE)
  counList <- gsub(pattern=paste('_', seaList[i], '_2015.RData', sep=''), replacement='', list2015)
  counList <- strsplit(counList, split='/', fixed=TRUE)
  counList <- unlist(lapply(1:length(counList), function(x){z <- counList[[x]][length(counList[[x]])]; return(z)}))
  
  countyMetrics <- lapply(1:length(list2015), function(j){
    load(list2015[[j]])
    clim_indexes <- lapply(1:length(clim_indexes), function(k){
      z <- clim_indexes[[k]]
      z <- as.data.frame(z)
      z$Index <- names(clim_indexes)[k]
      z$Season <- seaList[i]
      return(z)
    })
    clim_indexes <- do.call(rbind.fill, clim_indexes)
    clim_indexes <- clim_indexes[,c('cellID', 'lon', 'lat', as.character(1981:2015), 'Index', 'Season')]
    clim_indexes$County <- counList[j]
    return(clim_indexes)
  })
  countyMetrics <- do.call(rbind, countyMetrics)
  return(countyMetrics)
})
seasonMetrics <- do.call(rbind, seasonMetrics)
write.csv(seasonMetrics, '/mnt/workspace_cluster_8/Kenya_KACCAL/results/tables/historical_indexes.csv', row.names=FALSE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Saving future processed indexes in tables
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

futDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/future'
seaList <- c('first_season', 'second_season')
periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"

countyList <- list.files(path=futDir, pattern='*.RData', full.names=TRUE, recursive=TRUE)
countyList <- strsplit(x=countyList, split='/', fixed=TRUE)
countyList <- unique(unlist(lapply(1:length(countyList), function(i){z <- countyList[[i]][length(countyList[[i]])]; return(z)})))
countyList <- gsub(pattern='_first_season.RData', replacement='', countyList)
countyList <- gsub(pattern='_second_season.RData', replacement='', countyList)
countyList <- unique(countyList)

lapply(1:length(countyList), function(m){
  
  lapply(1:length(periodList), function(k){
    
    seaData <- lapply(1:length(seaList), function(i){
      
      gcmData <- lapply(1:length(gcmList), function(j){
        
        rcpData <- lapply(1:length(rcpList), function(l){
          
          load(paste(futDir, '/', seaList[i], '/', gcmList[j], '/', periodList[k], '/', rcpList[l], '/', countyList[m], '_', seaList[i], '.RData', sep=''))
          clim_indexes <- lapply(1:length(clim_indexes), function(n){
            z <- clim_indexes[[n]]
            z <- as.data.frame(z)
            z$Index <- names(clim_indexes)[n]
            z$County <- countyList[m]
            z$Season <- seaList[i]
            z$GCM <- gcmList[j]
            z$RCP <- rcpList[l]
            return(z)
          })
          clim_indexes <- do.call(rbind, clim_indexes)
          return(clim_indexes)
          
        })
        
        rcpData <- do.call(rbind, rcpData)
        return(rcpData)
        
      })
      
      gcmData <- do.call(rbind, gcmData)
      return(gcmData)
      
    })
    
    seaData <- do.call(rbind, seaData)
    write.csv(seaData, paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/tables/future/', countyList[m], '_indexes_', periodList[k], '.csv', sep=''), row.names=FALSE)
    return(cat('Done!\n'))
    
  })
  
})


gg <- ggplot(wrapFutClimInd_median[wrapFutClimInd_median$RCP=='rcp85' & wrapFutClimInd_median$Season=='First' & wrapFutClimInd_median$Index=='TMEAN',], aes(x=Years, y=Median)) + geom_line(size=1.2)
gg <- gg + theme_bw() + ylab(expression("Average temperature ("*~degree*C*")")) + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(2020, 2065, 5)) + scale_y_continuous(limits=c(25, 34))
gg <- gg + geom_hline(yintercept=mean(wrapFutClimInd_median[wrapFutClimInd_median$RCP=='rcp85' & wrapFutClimInd_median$Season=='First' & wrapFutClimInd_median$Index=='TMEAN','Median']), colour=2)
ggsave(filename='/home/hachicanoy/averageFut.pdf', plot=gg, width=7, height=7, units='in')

