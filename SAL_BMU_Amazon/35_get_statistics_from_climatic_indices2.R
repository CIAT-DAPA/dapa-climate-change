# Get stats from climatic indexes
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

countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot'))
countyList <- countyList[6,]
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Historical trends by county until 2005. Done for all!
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapClimInd <- lapply(1:nrow(countyList), function(i){
  
  cat('Loading and processing Climatic Indexes for', countyList$County[[i]], 'baseline\n')
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/first_season/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'_first_season.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/second_season/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'_second_season.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:ncol(first_season[[m]])])),
                              Index   = names(first_season)[m],
                              Years   = 1981:2005,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:ncol(second_season[[m]])])),
                              Index   = names(second_season)[m],
                              Years   = 1981:2005,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  allWrap <- rbind(first_seasonWrap, second_seasonWrap)
  rm(first_seasonWrap, second_seasonWrap)
  
  gg <- ggplot(allWrap, aes(x=Years, y=Average, colour=Season)) + geom_line() + geom_point()
  gg <- gg + facet_wrap(~ Index, scales='free') + theme_bw()
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!dir.exists(outDir)){
    dir.create(outDir, recursive=TRUE)
  }
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_historicalTrend.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_historicalTrend.pdf', sep=''), plot=gg, width=9, height=8, units='in')
  }
  
  indList <- unique(as.character(allWrap$Index))
  seasonList <- unique(as.character(allWrap$Season))
  trendAll <- lapply(1:length(indList), function(j){
    trendSeason <- lapply(1:length(seasonList), function(k){
      indexTS <- ts(allWrap$Average[which(allWrap$Index==indList[j] & allWrap$Season==seasonList[k])], start=1981, end=2005, frequency=1)
      mann.kendall_res <- mk.test(indexTS)
      trendTest <- data.frame(Index=indList[j], Season=seasonList[k], p_value=mann.kendall_res$pvalg)
      #sens.slope(indexTS)
      return(trendTest)
    })
    trendSeason <- do.call(rbind, trendSeason)
    return(trendSeason)
  })
  trendAll <- do.call(rbind, trendAll)
  trendAll$p_value <- round(trendAll$p_value, 3)
  
  gg <- ggplot(trendAll, aes(x=Season, y=p_value, fill=Season, colour=Season)) + geom_bar(stat="identity")
  gg <- gg + facet_wrap(~ Index, scales='free') + theme_bw()
  gg <- gg + geom_hline(yintercept=0.05)
  gg <- gg + ylab('p-value Mann-Kendall test') + scale_y_continuous(limits=c(0, 1))
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend.pdf', sep=''), plot=gg, width=9, height=8, units='in')
  }
  
})
rm(wrapClimInd)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Historical trends by county until 2015
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapClimInd_2015 <- lapply(1:nrow(countyList), function(i){
  
  cat('Loading and processing Climatic Indexes for', countyList$County[[i]], 'baseline\n')
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/first_season/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'_first_season_2015.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/second_season/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'_second_season_2015.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:ncol(first_season[[m]])])),
                              Index   = names(first_season)[m],
                              Years   = as.numeric(colnames(first_season[[m]])[4:ncol(first_season[[m]])]),
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:ncol(second_season[[m]])])),
                              Index   = names(second_season)[m],
                              Years   = as.numeric(colnames(second_season[[m]])[4:ncol(second_season[[m]])]),
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  allWrap <- rbind(first_seasonWrap, second_seasonWrap)
  rm(first_seasonWrap, second_seasonWrap)
  
  gg <- ggplot(allWrap, aes(x=Years, y=Average, colour=Season)) + geom_line() + geom_point()
  gg <- gg + facet_wrap(~ Index, scales='free') + theme_bw()
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_historicalTrend_2015.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_historicalTrend_2015.pdf', sep=''), plot=gg, width=9, height=8, units='in')
  }
  
  indList <- unique(as.character(allWrap$Index))
  seasonList <- unique(as.character(allWrap$Season))
  trendAll <- lapply(1:length(indList), function(j){
    trendSeason <- lapply(1:length(seasonList), function(k){
      years <- allWrap$Years[which(allWrap$Index==indList[j] & allWrap$Season==seasonList[k])]
      indexTS <- ts(allWrap$Average[which(allWrap$Index==indList[j] & allWrap$Season==seasonList[k])], start=min(years), end=max(years), frequency=1)
      mann.kendall_res <- mk.test(indexTS)
      trendTest <- data.frame(Index=indList[j], Season=seasonList[k], p_value=mann.kendall_res$pvalg)
      #sens.slope(indexTS)
      return(trendTest)
    })
    trendSeason <- do.call(rbind, trendSeason)
    return(trendSeason)
  })
  trendAll <- do.call(rbind, trendAll)
  trendAll$p_value <- round(trendAll$p_value, 3)
  
  gg <- ggplot(trendAll, aes(x=Season, y=p_value, fill=Season, colour=Season)) + geom_bar(stat="identity")
  gg <- gg + facet_wrap(~ Index, scales='free') + theme_bw()
  gg <- gg + geom_hline(yintercept=0.05)
  gg <- gg + ylab('p-value Mann-Kendall test') + scale_y_continuous(limits=c(0, 1))
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2015.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2015.pdf', sep=''), plot=gg, width=9, height=8, units='in')
  }
  
  return(allWrap)
  
})
allWrap <- wrapClimInd_2015[[1]]

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Total precipitation
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# For both seasons
gg <- ggplot(allWrap[allWrap$Index=='TOTRAIN',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab('Total precipitation (mm)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, 600))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_season_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_season_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# For first season
gg <- ggplot(allWrap[allWrap$Index=='TOTRAIN' & allWrap$Season=='First',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab('Total precipitation (mm)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + theme(legend.position="none")
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, 600))
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_firstSeason_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_firstSeason_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# For second season
gg <- ggplot(allWrap[allWrap$Index=='TOTRAIN' & allWrap$Season=='Second',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab('Total precipitation (mm)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + theme(legend.position="none")
gg <- gg + scale_colour_manual(values=c('Second'='turquoise3'))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, 600))
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_secondSeason_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_secondSeason_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# For annual precipitation
completeTOTRAIN <- as.data.frame(dplyr::summarise(group_by(allWrap[allWrap$Index=='TOTRAIN',], Index, Years), sum(Average)))
colnames(completeTOTRAIN)[3] <- 'Average'
gg <- ggplot(completeTOTRAIN, aes(x=Years, y=Average)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab('Total precipitation (mm)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, 1000))
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_complete_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TOTRAIN_complete_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Mean temperature
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# For both seasons
gg <- ggplot(allWrap[allWrap$Index=='TMEAN',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab(expression("Average temperature ("*~degree*C*")")) + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2005, 5)) + scale_y_continuous(limits=c(23, 27))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_season_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_season_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# For first season
gg <- ggplot(allWrap[allWrap$Index=='TMEAN' & allWrap$Season=='First',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab(expression("Average temperature ("*~degree*C*")")) + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + theme(legend.position="none")
gg <- gg + scale_x_continuous(breaks=seq(1980, 2005, 5)) + scale_y_continuous(limits=c(23, 27))
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_firstSeason_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_firstSeason_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# For second season
gg <- ggplot(allWrap[allWrap$Index=='TMEAN' & allWrap$Season=='Second',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab(expression("Average temperature ("*~degree*C*")")) + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + theme(legend.position="none")
gg <- gg + scale_colour_manual(values=c('Second'='turquoise3'))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2005, 5)) + scale_y_continuous(limits=c(23, 27))
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_secondSeason_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_secondSeason_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# For annual temperature
completeTMEAN <- as.data.frame(dplyr::summarise(group_by(allWrap[allWrap$Index=='TMEAN',], Index, Years), mean(Average)))
colnames(completeTMEAN)[3] <- 'Average'
gg <- ggplot(completeTMEAN, aes(x=Years, y=Average)) + geom_line(size=1.2) + geom_point(size=3)
gg <- gg + theme_bw() + ylab(expression("Average temperature ("*~degree*C*")")) + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2005, 5)) + scale_y_continuous(limits=c(23, 27))
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_complete_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_TMEAN_complete_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Floods
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# For both seasons
gg <- ggplot(allWrap[allWrap$Index=='P_95',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + theme_bw() + ylab('Floods. 95 percentile of daily precipitation (mm/day)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, max(allWrap$Average[allWrap$Index=='P_95'])+5))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_floodsP95_season_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_floodsP95_season_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_floodsP95_season_2015.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_floodsP95_season_2015.png", sep=""), wait=TRUE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Drought
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# For both seasons
gg <- ggplot(allWrap[allWrap$Index=='NDWS',], aes(x=Years, y=Average, colour=Season)) + geom_area(fill=Season) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + theme_bw() + ylab('Drought. Number of consecutive days with drought stress (days)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, max(allWrap$Average[allWrap$Index=='NDWS'])+5))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_droughtNDWS_season_2015.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_droughtNDWS_season_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
}

system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_droughtNDWS_season_2015.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_droughtNDWS_season_2015.png", sep=""), wait=TRUE)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Future trends by county
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"
seasonList <- c('first', 'second')

wrapFutClimInd <- lapply(1:nrow(countyList), function(g){
  
  cat('\n*** Processing information of:', countyList$County[[g]], '***\n\n')
  
  wrapFutClimInd <- lapply(1:length(seasonList), function(h){
    
    cat('Season:', seasonList[h], '\n')
    
    wrapFutClimInd <- lapply(1:length(periodList), function(i){
      
      cat('Period:', periodList[i], '\n')
      
      wrapClimInd <- lapply(1:length(rcpList), function(j){
        
        cat('RCP:', rcpList[j], '\n')
        
        wrapClimInd <- lapply(1:length(gcmList), function(k){
          
          cat('GCM:', gcmList[k], '\n')
          
          load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/future/', seasonList[h], '_season/', gcmList[k], '/', periodList[i], '/', rcpList[j], '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_', seasonList[h], '_season.RData', sep=''))
          first_seasonFut <- clim_indexes; rm(clim_indexes)
          
          years_analysis <- unlist(strsplit(periodList[i], split='_'))
          years_analysis <- as.numeric(years_analysis)
          years_analysis <- years_analysis[1]:years_analysis[2]
          
          wrapClimInd <- lapply(1:length(first_seasonFut), function(m){
            wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_seasonFut[[m]][,4:ncol(first_seasonFut[[m]])])),
                                      Index   = names(first_seasonFut)[m],
                                      GCM     = gcmList[k],
                                      RCP     = rcpList[j],
                                      Period  = periodList[i],
                                      County  = countyList$County[g],
                                      Season  = seasonList[h],
                                      Years   = years_analysis)
            return(wrapClimInd)
          })
          wrapClimInd <- do.call(rbind, wrapClimInd)
          return(wrapClimInd)
        })
        wrapClimInd <- do.call(rbind, wrapClimInd)
        return(wrapClimInd)
      })
      wrapClimInd <- do.call(rbind, wrapClimInd)
      return(wrapClimInd)
    })
    wrapFutClimInd <- do.call(rbind, wrapFutClimInd)
    return(wrapFutClimInd)
  })
  wrapFutClimInd <- do.call(rbind, wrapFutClimInd)
  
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='first' & wrapFutClimInd$Period=='2021_2045',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[g]), sep='')
  if(!dir.exists(outDir)){
    dir.create(outDir, recursive=TRUE)
  }
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2021_2045.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2021_2045.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='second' & wrapFutClimInd$Period=='2021_2045',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2021_2045.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2021_2045.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='first' & wrapFutClimInd$Period=='2041_2065',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2041_2065.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2041_2065.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='second' & wrapFutClimInd$Period=='2041_2065',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2041_2065.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2041_2065.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  return(wrapFutClimInd)
  
})
wrapFutClimInd <- do.call(rbind, wrapFutClimInd)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Calculate median through GCMs using extreme percentiles to show future trends by period
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapFutClimInd_median <- lapply(1:nrow(countyList), function(g){
  
  wrapFutClimInd_median <- lapply(1:length(seasonList), function(h){
    
    wrapFutClimInd_median <- lapply(1:length(periodList), function(i){
      
      wrapClimInd <- wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season==seasonList[h] & wrapFutClimInd$Period==periodList[i],]
      wrapClimInd_median <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), median(Average)))
      colnames(wrapClimInd_median)[4] <- 'Median'
      quantileFun <- function(x){z <- stats::quantile(x, probs=0.05); return(z)}
      aux <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), quantileFun(Average)))
      wrapClimInd_median$p0_05 <- as.numeric(aux[,ncol(aux)])
      quantileFun <- function(x){z <- stats::quantile(x, probs=0.95); return(z)}
      aux <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), quantileFun(Average)))
      wrapClimInd_median$p0_95 <- as.numeric(aux[,ncol(aux)])
      
      wrapClimInd_median$Period <- periodList[i]
      proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
      wrapClimInd_median$Season <- proper(seasonList[h])
      wrapClimInd_median$County <- countyList$County[g]
      
      return(wrapClimInd_median)
      
    })
    wrapFutClimInd_median <- do.call(rbind, wrapFutClimInd_median)
    return(wrapFutClimInd_median)
  })
  wrapFutClimInd_median <- do.call(rbind, wrapFutClimInd_median)
  
  # Period: 2021-2045
  gg <- ggplot(wrapFutClimInd_median[wrapFutClimInd_median$Period=='2021_2045',], aes(x=Years, y=Median, colour=Season))
  gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y')
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.x = element_text(size=10),
                   axis.text.y = element_text(size=14),
                   axis.title.x = element_text(face="bold",size=15),
                   axis.title.y = element_text(face="bold",size=15),
                   legend.text = element_text(size=14),
                   legend.title = element_text(face="bold",size=15))
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), sep='')
  if(!dir.exists(outDir)){
    dir.create(outDir, recursive=TRUE)
  }
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2021_2045.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2021_2045.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  # Period: 2041-2065
  gg <- ggplot(wrapFutClimInd_median[wrapFutClimInd_median$Period=='2041_2065',], aes(x=Years, y=Median, colour=Season))
  gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y')
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.x = element_text(size=10),
                   axis.text.y = element_text(size=14),
                   axis.title.x = element_text(face="bold",size=15),
                   axis.title.y = element_text(face="bold",size=15),
                   legend.text = element_text(size=14),
                   legend.title = element_text(face="bold",size=15))
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2041_2065.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2041_2065.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  return(wrapFutClimInd_median)
  
})
wrapFutClimInd_median <- do.call(rbind, wrapFutClimInd_median)

# Significance test
periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
seasonList <- c('first', 'second')
indList    <- unique(as.character(wrapFutClimInd_median$Index))

trendFutAll <- lapply(1:nrow(countyList), function(i){
  
  trendPeriod <- lapply(1:length(periodList), function(j){
    
    trendRCP <- lapply(1:length(rcpList), function(k){
      
      trendSeason <- lapply(1:length(seasonList), function(l){
        
        trendIndex <- lapply(1:length(indList), function(m){
          
          years_analysis <- unlist(strsplit(periodList[j], split='_'))
          years_analysis <- as.numeric(years_analysis)
          proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
          
          indexTS <- ts(wrapFutClimInd_median$Median[which(wrapFutClimInd_median$Index==indList[m] & wrapFutClimInd_median$RCP==rcpList[k] & wrapFutClimInd_median$Season==proper(seasonList[l]) & wrapFutClimInd_median$Period==periodList[j] & wrapFutClimInd_median$County==countyList$County[i])], start=years_analysis[1], end=years_analysis[2], frequency=1)
          mann.kendall_res <- mk.test(indexTS)
          trendTest <- data.frame(Index=indList[m], RCP=rcpList[k], p_value=mann.kendall_res$pvalg, Season=proper(seasonList[l]), Period=periodList[j], County=countyList$County[i])
          #sens.slope(indexTS)
          return(trendTest)
        })
        trendIndex <- do.call(rbind, trendIndex)
        
      })
      trendSeason <- do.call(rbind, trendSeason)
      return(trendSeason)
      
    })
    trendRCP <- do.call(rbind, trendRCP)
    return(trendRCP)
    
  })
  trendPeriod <- do.call(rbind, trendPeriod)
  
  gg <- ggplot(trendPeriod[trendPeriod$Period=='2021_2045',], aes(x=RCP, y=p_value, fill=Season, colour=Season)) + geom_bar(stat='identity', position="dodge")
  gg <- gg + facet_wrap(~ Index)
  gg <- gg + theme_bw() + geom_hline(yintercept=0.05)
  gg <- gg + ylab('p-value Mann-Kendall test') + scale_y_continuous(limits=c(0, 1))
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2021_2045.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2021_2045.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  gg <- ggplot(trendPeriod[trendPeriod$Period=='2041_2065',], aes(x=RCP, y=p_value, fill=Season, colour=Season)) + geom_bar(stat='identity', position="dodge")
  gg <- gg + facet_wrap(~ Index)
  gg <- gg + theme_bw() + geom_hline(yintercept=0.05)
  gg <- gg + ylab('p-value Mann-Kendall test') + scale_y_continuous(limits=c(0, 1))
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2041_2065.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2041_2065.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  return(trendPeriod)
  
})
trendFutAll <- do.call(rbind, trendFutAll)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Calculate median through GCMs using extreme percentiles to show future trends by 2021-2065
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Median for all years through 2021-2065
wrapFutClimInd_median <- lapply(1:length(seasonList), function(h){
  
  wrapClimInd <- wrapFutClimInd[wrapFutClimInd$Season==seasonList[h],]
  wrapClimInd_median <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), median(Average)))
  colnames(wrapClimInd_median)[4] <- 'Median'
  quantileFun <- function(x){z <- stats::quantile(x, probs=0.05); return(z)}
  aux <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), quantileFun(Average)))
  wrapClimInd_median$p0_05 <- as.numeric(aux[,ncol(aux)])
  quantileFun <- function(x){z <- stats::quantile(x, probs=0.95); return(z)}
  aux <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), quantileFun(Average)))
  wrapClimInd_median$p0_95 <- as.numeric(aux[,ncol(aux)])
  
  proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  wrapClimInd_median$Season <- proper(seasonList[h])
  return(wrapClimInd_median)
  
})
wrapFutClimInd_median <- do.call(rbind, wrapFutClimInd_median)

# Facet plot
gg <- ggplot(wrapFutClimInd_median, aes(x=Years, y=Median, colour=Season))
gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line()
gg <- gg + facet_grid(Index ~ RCP, scales='free_y')
gg <- gg + theme_bw() + ylab('Median of each index with 90% interval of uncertainty')
gg <- gg + theme(axis.text.x = element_text(size=10),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), sep='')
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_allFutureTrend_2021_2065.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_allFutureTrend_2021_2065.pdf', sep=''), plot=gg, width=10, height=12, units='in')
}

# Individual plots with all RCPs
indList    <- unique(as.character(wrapFutClimInd_median$Index))
lapply(1:length(indList), function(i){
  db <- wrapFutClimInd_median[wrapFutClimInd_median$Index==indList[i],]
  rownames(db) <- 1:nrow(db)
  gg <- ggplot(db, aes(x=Years, y=Median, colour=Season))
  gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line(size=1.2) + geom_point(size=3)
  gg <- gg + facet_wrap(~RCP)
  gg <- gg + theme_bw() + ylab('Median of each index with 90% interval of uncertainty')
  gg <- gg + theme(axis.text.x = element_text(size=14),
                   axis.text.y = element_text(size=14),
                   axis.title.x = element_text(face="bold",size=15),
                   axis.title.y = element_text(face="bold",size=15),
                   legend.text = element_text(size=14),
                   legend.title = element_text(face="bold",size=15),
                   strip.text = element_text(size=15))
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
  if(!dir.exists(outDir)){
    dir.create(outDir, recursive=TRUE)
  }
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_', indList[i], '_allRCP_2021_2065.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_', indList[i], '_allRCP_2021_2065.pdf', sep=''), plot=gg, width=12, height=12, units='in')
  }
})

#################################################################################
# Floods
#################################################################################

db <- wrapFutClimInd_median[wrapFutClimInd_median$Index=='P_95',]
rownames(db) <- 1:nrow(db)
gg <- ggplot(db, aes(x=Years, y=Median, colour=Season))
gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line(size=1.2) + geom_point(size=3) +  scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3')) + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + facet_wrap(~RCP)
gg <- gg + theme_bw() + ylab('Floods. 95 percentile of daily precipitation (mm/day)')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15),
                 strip.text = element_text(size=15))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
if(!dir.exists(outDir)){
  dir.create(outDir, recursive=TRUE)
}
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_P_95_floodsP95_allRCP_2021_2065.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_P_95_floodsP95_allRCP_2021_2065.pdf', sep=''), plot=gg, width=12, height=12, units='in')
}

system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_P_95_floodsP95_allRCP_2021_2065.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_P_95_floodsP95_allRCP_2021_2065.png", sep=""), wait=TRUE)

#################################################################################
# Drought
#################################################################################

db <- wrapFutClimInd_median[wrapFutClimInd_median$Index=='NDWS',]
rownames(db) <- 1:nrow(db)
gg <- ggplot(db, aes(x=Years, y=Median, colour=Season))
gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line(size=1.2) + geom_point(size=3) +  scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3')) + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + facet_wrap(~RCP)
gg <- gg + theme_bw() + ylab('Drought. Number of consecutive days with drought stress (days)')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15),
                 strip.text = element_text(size=15))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
if(!dir.exists(outDir)){
  dir.create(outDir, recursive=TRUE)
}
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_NDWS_drought_allRCP_2021_2065.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_NDWS_drought_allRCP_2021_2065.pdf', sep=''), plot=gg, width=12, height=12, units='in')
}

system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_NDWS_drought_allRCP_2021_2065.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_NDWS_drought_allRCP_2021_2065.png", sep=""), wait=TRUE)

#################################################################################

# Individual plots
indList <- unique(as.character(wrapFutClimInd_median$Index))
rcpList <- paste("rcp", c(26, 45, 60, 85), sep="")
lapply(1:length(indList), function(i){
  
  lapply(1:length(rcpList), function(j){
    
    db <- wrapFutClimInd_median[wrapFutClimInd_median$Index==indList[i] & wrapFutClimInd_median$RCP==rcpList[j],]
    rownames(db) <- 1:nrow(db)
    gg <- ggplot(db, aes(x=Years, y=Median, colour=Season))
    gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line(size=1.2) + geom_point(size=3)
    # gg <- gg + facet_wrap(~RCP)
    gg <- gg + theme_bw() + ylab('Median of each index with 90% interval of uncertainty')
    gg <- gg + theme(axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14),
                     axis.title.x = element_text(face="bold",size=15),
                     axis.title.y = element_text(face="bold",size=15),
                     legend.text = element_text(size=14),
                     legend.title = element_text(face="bold",size=15),
                     strip.text = element_text(size=15),
                     plot.title = element_text(size=20))
    gg <- gg + scale_x_continuous(breaks=seq(2020, 2065, 5))
    gg <- gg + ggtitle(paste('Index: ', indList[i], ' - RCP: ', rcpList[j] ,sep=''))
    outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
    if(!dir.exists(outDir)){
      dir.create(outDir, recursive=TRUE)
    }
    if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_', indList[i], '_', rcpList[j], '_2021_2065.pdf', sep=''))){
      ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_', indList[i], '_', rcpList[j], '_2021_2065.pdf', sep=''), plot=gg, width=12, height=10, units='in')
    }
    
  })
  
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Cluster analysis to identify climate change scenarios by county using absolute changes
# (Here we use only future absolute changes)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapFutClimInd2 <- wrapFutClimInd
wrapFutClimInd2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2, Index, GCM, RCP, County, Season, Years), median(Average)))
colnames(wrapFutClimInd2)[ncol(wrapFutClimInd2)] <- 'Average'

county <- 'Busia'
scenarioClustering2 <- function(county){
  
  seasonList <- c('first', 'second')
  
  # *** Load historical data first season
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[1], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[1], '_season.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:ncol(first_season[[m]])])),
                              Index   = names(first_season)[m],
                              Years   = 1981:2005,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  first_seasonWrap <- as.data.frame(dplyr::summarise(group_by(first_seasonWrap, Index, Season), mean(Average)))
  colnames(first_seasonWrap)[3] <- 'Average'
  
  # *** Load historical data second season
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[2], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[2], '_season.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:ncol(second_season[[m]])])),
                              Index   = names(second_season)[m],
                              Years   = 1981:2005,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  second_seasonWrap <- as.data.frame(dplyr::summarise(group_by(second_seasonWrap, Index, Season), mean(Average)))
  colnames(second_seasonWrap)[3] <- 'Average'
  
  all_seasons <- rbind(first_seasonWrap, second_seasonWrap); rm(first_seasonWrap, second_seasonWrap)
  
  # *** Load future data
  wrapFutClimInd_county <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2[wrapFutClimInd2$County==county,], Index, GCM, RCP, Season), mean(Average)))
  colnames(wrapFutClimInd_county)[5] <- 'Average'
  wrapFutClimInd_county$Average[which(wrapFutClimInd_county$Average==-Inf)] <- 0
  
  # *** Calculate absolute change of future based on historical data
  seasonList <- c('first', 'second')
  proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  wrapFutClimInd_absChange <- wrapFutClimInd_county
  for(i in 1:9){
    for(j in 1:length(seasonList)){
      wrapFutClimInd_absChange$Average[which(wrapFutClimInd_absChange$Index==all_seasons$Index[i] & wrapFutClimInd_absChange$Season==seasonList[j])] <- wrapFutClimInd_absChange$Average[which(wrapFutClimInd_absChange$Index==all_seasons$Index[i] & wrapFutClimInd_absChange$Season==seasonList[j])] - all_seasons$Average[which(all_seasons$Index==all_seasons$Index[i] & all_seasons$Season==proper(seasonList[j]))]
    }
  }; rm(i, j)
  
  wrapFutClimInd_absChange$Season       <- gsub(pattern='first', replacement='S1', x=wrapFutClimInd_absChange$Season)
  wrapFutClimInd_absChange$Season       <- gsub(pattern='second', replacement='S2', x=wrapFutClimInd_absChange$Season)
  wrapFutClimInd_absChange$combination  <- paste(wrapFutClimInd_absChange$GCM, '-', wrapFutClimInd_absChange$RCP, '-', wrapFutClimInd_absChange$Season, sep='')
  
  wrapFutClimInd_absChange$GCM <- NULL
  wrapFutClimInd_absChange$RCP <- NULL
  wrapFutClimInd_absChange$Season <- NULL
  
  wrapFutClimInd_absChange <- wrapFutClimInd_absChange %>% spread(key=Index, value=Average)
  
  rownames(wrapFutClimInd_absChange) <- wrapFutClimInd_absChange$combination
  wrapFutClimInd_absChange$combination <- NULL
  sd0 <- which(apply(X=wrapFutClimInd_absChange, MARGIN=2, FUN=sd)==0)
  if(length(sd0)>0){
    wrapFutClimInd_absChange <- wrapFutClimInd_absChange[,-sd0]
  }
  
  ### Exploring correlations between variables
  library(gplots)
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
  corMat <- cor(wrapFutClimInd_absChange, method='spearman')
  diag(corMat) <- NA
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/cluster_analysis/combined/absolute_changes_allYears/', gsub(pattern=' ', replacement='_', county), sep='')
  if(!dir.exists(outDir)){dir.create(outDir, recursive=TRUE)}
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''), height=8, width=8)
    heatmap.2(corMat, col=my_palette, density.info="none", trace="none", dendrogram="column", key.title='', key.xlab='Spearman correlation', margins=c(9,9))
    dev.off()
  }
  
  # Doing PCA using absolute changes and doing hierarchical clustering on principal components
  # because our variables are correlated
  library(FactoMineR)
  res_pca  <- FactoMineR::PCA(X=wrapFutClimInd_absChange, graph=FALSE)
  set.seed(1235)
  res_hcpc <- FactoMineR::HCPC(res_pca, nb.clust=-1, graph=FALSE) # Define number of SCENARIOS
  
  # Description of variables within each cluster
  # res_hcpc$desc.var
  
  library(factoextra)
  library(corrplot)
  
  # Visualize eigenvalues/variances
  gg <- fviz_eig(res_pca, addlabels=TRUE, hjust = -0.3) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''), plot=gg, width=7, height=7, units='in')
  }
  
  # Visualize variables with contributions. Use gradient color
  gg <- fviz_pca_var(res_pca, col.var="contrib")+ scale_color_gradient2(low="white", mid="blue", high="red", midpoint = 96) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
  }
  
  # Quality of representation of each variable
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''), height=7, width=8)
    par(mfrow=c(1,3))
    corrplot(res_pca$var$cos2[,1:2], is.corr=FALSE) # Representation quality of each variable
    corrplot(res_pca$var$contrib[,1:2], is.corr=FALSE) # Contribution of each variable to dimension
    corrplot(res_pca$var$cor[,1:2], method="ellipse", is.corr=TRUE) # Correlation of each variable to dimension
    dev.off()
  }
  
  # ENCONTRAR UNA FORMA ADECUADA DE MOSTRAR LOS CLUSTERS EN UN ESPACIO BIVARIADO $$$
  # Biplot of individuals and variables. Only variables are labelled
  gg <- fviz_pca_biplot(res_pca,  label="var", habillage=res_hcpc$data.clust$clust, addEllipses=TRUE, ellipse.level=0.95) + theme_bw()
  # gg <- gg + scale_color_manual(values=c("black", "red", "forestgreen"))
  # gg <- gg + scale_fill_manual(values=c("black", "red", "forestgreen")) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
  }
  
  # Make boxplots for set of variables
  df_cluster <- res_hcpc$data.clust
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChangeClusters.csv', sep=''))){
    write.csv(df_cluster, paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChangeClusters.csv', sep=''), row.names=TRUE)
  }
  # colnames(df_cluster) <- gsub(pattern='-', replacement='_', colnames(df_cluster))
  df_cluster$combination <- rownames(df_cluster)
  df_cluster <- df_cluster %>% gather(Index, Value, TMEAN:NDWS)
  df_cluster$Index <- as.character(df_cluster$Index)
  aux <- strsplit(x=df_cluster$combination, split='-')
  aux <- lapply(1:length(aux), function(i){
    z <- as.data.frame(t(aux[[i]]))
    return(z)
  })
  aux <- do.call(rbind, aux)
  df_cluster$GCM <- aux$V1
  df_cluster$RCP <- aux$V2
  df_cluster$Season <- aux$V3
  rm(aux)
  df_cluster$combination <- NULL
  df_cluster$Season <- gsub(pattern='S1', replacement='First', df_cluster$Season)
  df_cluster$Season <- gsub(pattern='S2', replacement='Second', df_cluster$Season)
  names(df_cluster)[1] <- 'Scenario'
  df_cluster$Index <- factor(x=df_cluster$Index, levels=c('TMEAN', 'GDD_1', 'GDD_2', 'ND_t35', 'TOTRAIN', 'CDD', 'P5D', 'P_95', 'NDWS'))
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_boxplot(aes(colour=Season))
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Absolute change respect to baseline')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  return(cat('Process done.\n'))
  
}
county <- 'Busia'
scenarioClustering2(county=county)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Cluster analysis to identify climate change scenarios by county using absolute changes
# (Here we use historical and future absolute changes)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapFutClimInd2 <- wrapFutClimInd
wrapFutClimInd2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2, Index, GCM, RCP, County, Season, Years), median(Average)))
colnames(wrapFutClimInd2)[ncol(wrapFutClimInd2)] <- 'Average'

county <- 'Taita Taveta'
scenarioClustering3 <- function(county){
  
  seasonList <- c('first', 'second')
  
  # *** Load historical data first season
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[1], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[1], '_season.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  
  ###
  first_seasonWrap1 <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:23])),
                              Index   = names(first_season)[m],
                              Years   = 1981:2000,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap1 <- do.call(rbind, first_seasonWrap1)
  first_seasonWrap1 <- as.data.frame(dplyr::summarise(group_by(first_seasonWrap1, Index, Season), mean(Average)))
  colnames(first_seasonWrap1)[3] <- 'Average'
  
  ###
  first_seasonWrap2 <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,(ncol(first_season[[m]])-20+1):ncol(first_season[[m]])])),
                              Index   = names(first_season)[m],
                              Years   = 1986:2005,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap2 <- do.call(rbind, first_seasonWrap2)
  first_seasonWrap2 <- as.data.frame(dplyr::summarise(group_by(first_seasonWrap2, Index, Season), mean(Average)))
  colnames(first_seasonWrap2)[3] <- 'Average'
  
  fs_absChanges <- first_seasonWrap2$Average - first_seasonWrap1$Average; rm(first_seasonWrap1, first_seasonWrap2)
  fs_absChanges <- data.frame(Index=names(first_season), Average=fs_absChanges)
  fs_absChanges <- fs_absChanges[order(fs_absChanges$Index),]
  
  ###
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:ncol(first_season[[m]])])),
                              Index   = names(first_season)[m],
                              Years   = 1981:2005,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  first_seasonWrap <- as.data.frame(dplyr::summarise(group_by(first_seasonWrap, Index, Season), mean(Average)))
  colnames(first_seasonWrap)[3] <- 'Average'
  
  # *** Load historical data second season
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[2], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[2], '_season.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  ###
  second_seasonWrap1 <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:23])),
                              Index   = names(second_season)[m],
                              Years   = 1981:2000,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap1 <- do.call(rbind, second_seasonWrap1)
  second_seasonWrap1 <- as.data.frame(dplyr::summarise(group_by(second_seasonWrap1, Index, Season), mean(Average)))
  colnames(second_seasonWrap1)[3] <- 'Average'
  
  ###
  second_seasonWrap2 <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,(ncol(second_season[[m]])-20+1):ncol(second_season[[m]])])),
                              Index   = names(second_season)[m],
                              Years   = 1986:2005,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap2 <- do.call(rbind, second_seasonWrap2)
  second_seasonWrap2 <- as.data.frame(dplyr::summarise(group_by(second_seasonWrap2, Index, Season), mean(Average)))
  colnames(second_seasonWrap2)[3] <- 'Average'
  
  ss_absChanges <- second_seasonWrap2$Average - second_seasonWrap1$Average; rm(second_seasonWrap1, second_seasonWrap2)
  ss_absChanges <- data.frame(Index=names(second_season), Average=ss_absChanges)
  ss_absChanges <- ss_absChanges[order(ss_absChanges$Index),]
  
  #
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:ncol(second_season[[m]])])),
                              Index   = names(second_season)[m],
                              Years   = 1981:2005,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  second_seasonWrap <- as.data.frame(dplyr::summarise(group_by(second_seasonWrap, Index, Season), mean(Average)))
  colnames(second_seasonWrap)[3] <- 'Average'
  
  all_seasons <- rbind(first_seasonWrap, second_seasonWrap); rm(first_seasonWrap, second_seasonWrap)
  
  # *** Load future data
  wrapFutClimInd_county <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2[wrapFutClimInd2$County==county,], Index, GCM, RCP, Season), mean(Average)))
  colnames(wrapFutClimInd_county)[5] <- 'Average'
  wrapFutClimInd_county$Average[which(wrapFutClimInd_county$Average==-Inf)] <- 0
  
  # *** Calculate absolute change of future based on historical data
  seasonList <- c('first', 'second')
  indList <- names(first_season)
  proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  wrapFutClimInd_absChange <- wrapFutClimInd_county
  for(i in 1:9){
    for(j in 1:length(seasonList)){
      wrapFutClimInd_absChange$Average[which(wrapFutClimInd_absChange$Index==indList[i] & wrapFutClimInd_absChange$Season==seasonList[j])] <- wrapFutClimInd_absChange$Average[which(wrapFutClimInd_absChange$Index==indList[i] & wrapFutClimInd_absChange$Season==seasonList[j])] - all_seasons$Average[which(all_seasons$Index==indList[i] & all_seasons$Season==proper(seasonList[j]))]
    }
  }; rm(i, j)
  
  wrapFutClimInd_absChange$Season       <- gsub(pattern='first', replacement='S1', x=wrapFutClimInd_absChange$Season)
  wrapFutClimInd_absChange$Season       <- gsub(pattern='second', replacement='S2', x=wrapFutClimInd_absChange$Season)
  wrapFutClimInd_absChange$combination  <- paste(wrapFutClimInd_absChange$GCM, '-', wrapFutClimInd_absChange$RCP, sep='')
  wrapFutClimInd_absChange$Index_season <- paste(wrapFutClimInd_absChange$Index, '-', wrapFutClimInd_absChange$Season, sep='')
  
  wrapFutClimInd_absChange$Index  <- NULL
  wrapFutClimInd_absChange$GCM <- NULL
  wrapFutClimInd_absChange$RCP <- NULL
  wrapFutClimInd_absChange$Season <- NULL
  
  wrapFutClimInd_absChange <- wrapFutClimInd_absChange %>% spread(key=Index_season, value=Average)
  
  rownames(wrapFutClimInd_absChange) <- wrapFutClimInd_absChange$combination
  wrapFutClimInd_absChange$combination <- NULL
  wrapFutClimInd_absChange <- wrapFutClimInd_absChange[,c(grep(pattern='S1$', colnames(wrapFutClimInd_absChange), fixed=FALSE), grep(pattern='S2$', colnames(wrapFutClimInd_absChange), fixed=FALSE))]
  wrapFutClimInd_absChange[nrow(wrapFutClimInd_absChange)+1,] <- c(fs_absChanges$Average, ss_absChanges$Average)
  rownames(wrapFutClimInd_absChange)[nrow(wrapFutClimInd_absChange)] <- c('baseline-Historical')
  sd0 <- which(apply(X=wrapFutClimInd_absChange, MARGIN=2, FUN=sd)==0)
  if(length(sd0)>0){
    wrapFutClimInd_absChange <- wrapFutClimInd_absChange[,-sd0]
  }
  
  ### Exploring correlations between variables
  library(gplots)
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
  corMat <- cor(wrapFutClimInd_absChange, method='spearman')
  diag(corMat) <- NA
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/cluster_analysis/combined/absolute_changes_allYears/', gsub(pattern=' ', replacement='_', county), sep='')
  if(!dir.exists(outDir)){dir.create(outDir, recursive=TRUE)}
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''), height=8, width=8)
    heatmap.2(corMat, col=my_palette, density.info="none", trace="none", dendrogram="column", key.title='', key.xlab='Spearman correlation', margins=c(9,9))
    dev.off()
  }
  
  # Doing PCA using absolute changes and doing hierarchical clustering on principal components
  # because our variables are correlated
  library(FactoMineR)
  res_pca  <- FactoMineR::PCA(X=wrapFutClimInd_absChange, graph=FALSE)
  set.seed(1235)
  res_hcpc <- FactoMineR::HCPC(res_pca, nb.clust=-1, graph=FALSE) # Define number of SCENARIOS
  
  library(factoextra)
  library(corrplot)
  
  # Visualize eigenvalues/variances
  gg <- fviz_eig(res_pca, addlabels=TRUE, hjust = -0.3) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''), plot=gg, width=7, height=7, units='in')
  }
  
  # Visualize variables with contributions. Use gradient color
  gg <- fviz_pca_var(res_pca, col.var="contrib")+ scale_color_gradient2(low="white", mid="blue", high="red", midpoint = 96) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
  }
  
  # Quality of representation of each variable
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''), height=7, width=8)
    par(mfrow=c(1,3))
    corrplot(res_pca$var$cos2[,1:2], is.corr=FALSE) # Representation quality of each variable
    corrplot(res_pca$var$contrib[,1:2], is.corr=FALSE) # Contribution of each variable to dimension
    corrplot(res_pca$var$cor[,1:2], method="ellipse", is.corr=TRUE) # Correlation of each variable to dimension
    dev.off()
  }
  
  # ENCONTRAR UNA FORMA ADECUADA DE MOSTRAR LOS CLUSTERS EN UN ESPACIO BIVARIADO $$$
  # Biplot of individuals and variables. Only variables are labelled
  gg <- fviz_pca_biplot(res_pca,  label="var", habillage=res_hcpc$data.clust$clust, addEllipses=TRUE, ellipse.level=0.95) + theme_bw()
  # gg <- gg + scale_color_manual(values=c("black", "red", "forestgreen"))
  # gg <- gg + scale_fill_manual(values=c("black", "red", "forestgreen")) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
  }
  
  # Make boxplots for set of variables
  df_cluster <- res_hcpc$data.clust
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChangeClusters.csv', sep=''))){
    write.csv(df_cluster, paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChangeClusters.csv', sep=''), row.names=TRUE)
  }
  # colnames(df_cluster) <- gsub(pattern='-', replacement='_', colnames(df_cluster))
  df_cluster$combination <- rownames(df_cluster)
  colnames(df_cluster) <- gsub(pattern='-', replacement='_', x=colnames(df_cluster), fixed=TRUE)
  df_cluster <- df_cluster %>% gather(Index, Value, CDD_S1:TOTRAIN_S2)
  df_cluster$Index <- as.character(df_cluster$Index)
  df_cluster$Season <- unlist(lapply(strsplit(x=df_cluster$Index, split='_S'), function(x){return(x[2])}))
  df_cluster$Season <- gsub(pattern='1', replacement='First', df_cluster$Season)
  df_cluster$Season <- gsub(pattern='2', replacement='Second', df_cluster$Season)
  df_cluster$Index <- unlist(lapply(strsplit(x=df_cluster$Index, split='_S'), function(x){return(x[1])}))
  aux <- strsplit(x=df_cluster$combination, split='-')
  aux <- lapply(1:length(aux), function(i){
    z <- as.data.frame(t(aux[[i]]))
    return(z)
  })
  aux <- do.call(rbind, aux)
  df_cluster$GCM <- aux$V1
  df_cluster$RCP <- aux$V2
  rm(aux)
  df_cluster$combination <- NULL
  names(df_cluster)[1] <- 'Scenario'
  df_cluster$Index <- factor(x=df_cluster$Index, levels=c('TMEAN', 'GDD_1', 'GDD_2', 'ND_t35', 'TOTRAIN', 'CDD', 'P5D', 'P_95', 'NDWS'))
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_boxplot(aes(colour=Season))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Absolute change respect to baseline')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_jitter(aes(colour=Season))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Absolute change respect to baseline')
  gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
  gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='gold')
  gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
  # gg <- gg + geom_point(df_cluster)
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline2.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline2.pdf', sep=''), plot=gg, width=9, height=10, units='in')
  }
  
  return(cat('Process done.\n'))
  
}
county <- 'Taita Taveta'
scenarioClustering3(county=county)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Cluster analysis to identify climate change scenarios by county using trends
# (Here we use historical and future trends)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapFutClimInd2 <- wrapFutClimInd
wrapFutClimInd2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2, Index, GCM, RCP, County, Season, Years), median(Average)))
colnames(wrapFutClimInd2)[ncol(wrapFutClimInd2)] <- 'Average'

county <- 'Taita Taveta'
scenarioTrendsClustering <- function(county){
  
  seasonList <- c('first', 'second')
  
  # *** Load historical data first season
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[1], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[1], '_season.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:ncol(first_season[[m]])])),
                              Index   = names(first_season)[m],
                              Years   = 1981:2005,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  indList <- as.character(unique(first_seasonWrap$Index))
  first_seasonWrap <- lapply(1:length(indList), function(m){
    timeSer <- first_seasonWrap[first_seasonWrap$Index==indList[m],]
    grep2 <- Vectorize(grep, vectorize.args='pattern')
    timeSer <- ts(timeSer$Average[grep2(pattern=1981:2005, timeSer$Years, fixed=TRUE)], start=min(timeSer$Years), end=max(timeSer$Years), frequency=1)
    slope <- sens.slope(timeSer); slope <- slope$b.sen
    df <- data.frame(slope=slope, Index=paste(indList[m], '-S1', sep=''))
    return(df)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  first_seasonWrap$Index <- as.character(first_seasonWrap$Index)
  first_seasonWrap <- first_seasonWrap[order(first_seasonWrap$Index),]
  
  # *** Load historical data second season
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[2], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[2], '_season.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:ncol(second_season[[m]])])),
                              Index   = names(second_season)[m],
                              Years   = 1981:2005,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  indList <- as.character(unique(second_seasonWrap$Index))
  second_seasonWrap <- lapply(1:length(indList), function(m){
    timeSer <- second_seasonWrap[second_seasonWrap$Index==indList[m],]
    grep2 <- Vectorize(grep, vectorize.args='pattern')
    timeSer <- ts(timeSer$Average[grep2(pattern=1981:2005, timeSer$Years, fixed=TRUE)], start=min(timeSer$Years), end=max(timeSer$Years), frequency=1)
    slope <- sens.slope(timeSer); slope <- slope$b.sen
    df <- data.frame(slope=slope, Index=paste(indList[m], '-S2', sep=''))
    return(df)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  second_seasonWrap$Index <- as.character(second_seasonWrap$Index)
  second_seasonWrap <- second_seasonWrap[order(second_seasonWrap$Index),]
  
  # *** Load future data
  indList <- as.character(unique(wrapFutClimInd2$Index))
  gcmList <- as.character(unique(wrapFutClimInd2$GCM))
  rcpList <- as.character(unique(wrapFutClimInd2$RCP))
  seasonList <- as.character(unique(wrapFutClimInd2$Season))
  wrapFutClimInd_county <- lapply(1:length(indList), function(n){
    df <- lapply(1:length(gcmList), function(o){
      df <- lapply(1:length(rcpList), function(p){
        df <- lapply(1:length(seasonList), function(r){
          timeSer <- wrapFutClimInd2[wrapFutClimInd2$Index==indList[n] & wrapFutClimInd2$GCM==gcmList[o] & wrapFutClimInd2$RCP==rcpList[p] & wrapFutClimInd2$Season==seasonList[r],]
          timeSer$Average[which(timeSer$Average==-Inf)] <- 0
          grep2 <- Vectorize(grep, vectorize.args='pattern')
          years <- c(min(timeSer$Years, na.rm=TRUE), max(timeSer$Years, na.rm=TRUE))
          timeSer <- ts(timeSer$Average[grep2(pattern=years[1]:years[2], timeSer$Years, fixed=TRUE)], start=years[1], end=years[2], frequency=1)
          slope <- sens.slope(timeSer); slope <- slope$b.sen
          df <- data.frame(Index=indList[n], GCM=gcmList[o], RCP=rcpList[p], Season=seasonList[r], slope=slope)
          return(df)
        })
        df <- do.call(rbind, df)
        return(df)
      })
      df <- do.call(rbind, df)
      return(df)
    })
    df <- do.call(rbind, df)
    return(df)
  })
  wrapFutClimInd_county <- do.call(rbind, wrapFutClimInd_county)
  
  wrapFutClimInd_county$Season       <- gsub(pattern='first', replacement='S1', x=wrapFutClimInd_county$Season)
  wrapFutClimInd_county$Season       <- gsub(pattern='second', replacement='S2', x=wrapFutClimInd_county$Season)
  wrapFutClimInd_county$combination  <- paste(wrapFutClimInd_county$GCM, '-', wrapFutClimInd_county$RCP, sep='')
  wrapFutClimInd_county$Index_season <- paste(wrapFutClimInd_county$Index, '-', wrapFutClimInd_county$Season, sep='')
  
  wrapFutClimInd_county$Index <- NULL
  wrapFutClimInd_county$GCM <- NULL
  wrapFutClimInd_county$RCP <- NULL
  wrapFutClimInd_county$Season <- NULL
  wrapFutClimInd_county$slope <- wrapFutClimInd_county$slope*20
  
  wrapFutClimInd_county <- wrapFutClimInd_county %>% spread(key=Index_season, value=slope)
  
  rownames(wrapFutClimInd_county) <- wrapFutClimInd_county$combination
  wrapFutClimInd_county$combination <- NULL
  wrapFutClimInd_county <- wrapFutClimInd_county[,c(grep(pattern='S1$', colnames(wrapFutClimInd_county), fixed=FALSE), grep(pattern='S2$', colnames(wrapFutClimInd_county), fixed=FALSE))]
  
  sd0 <- which(apply(X=wrapFutClimInd_county, MARGIN=2, FUN=sd)==0)
  # Future and baseline in the same table
  wrapFutClimInd_county[nrow(wrapFutClimInd_county)+1,] <- c(first_seasonWrap$slope, second_seasonWrap$slope) * 20
  rownames(wrapFutClimInd_county)[nrow(wrapFutClimInd_county)] <- c('Baseline-Historical')
  if(length(sd0)>0){
    wrapFutClimInd_county <- wrapFutClimInd_county[,-sd0]
  }
  
  ### Exploring correlations between variables
  library(gplots)
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
  corMat <- cor(wrapFutClimInd_county, method='spearman')
  diag(corMat) <- NA
  outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/cluster_analysis/combined/trends_allYears/', gsub(pattern=' ', replacement='_', county), sep='')
  if(!dir.exists(outDir)){dir.create(outDir, recursive=TRUE)}
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''), height=8, width=8)
    heatmap.2(corMat, col=my_palette, density.info="none", trace="none", dendrogram="column", key.title='', key.xlab='Spearman correlation', margins=c(9,9))
    dev.off()
  }
  
  # Doing PCA using estimated slopes and doing hierarchical clustering on principal components
  # because our variables are correlated
  library(FactoMineR)
  res_pca  <- FactoMineR::PCA(wrapFutClimInd_county, graph=FALSE)
  set.seed(1235)
  res_hcpc <- FactoMineR::HCPC(res_pca, nb.clust=-1, graph=FALSE) # Define number of SCENARIOS
  
  # Description of variables within each cluster
  # res_hcpc$desc.var
  
  library(factoextra)
  library(corrplot)
  
  # Visualize eigenvalues/variances
  gg <- fviz_eig(res_pca, addlabels=TRUE, hjust = -0.3) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''), plot=gg, width=7, height=7, units='in')
  }
  
  # Visualize variables with contributions. Use gradient color
  gg <- fviz_pca_var(res_pca, col.var="contrib")+ scale_color_gradient2(low="white", mid="blue", high="red", midpoint = 96) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
  }
  
  # Quality of representation of each variable
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''), height=7, width=8)
    par(mfrow=c(1,3))
    corrplot(res_pca$var$cos2[,1:2], is.corr=FALSE) # Representation quality of each variable
    corrplot(res_pca$var$contrib[,1:2], is.corr=FALSE) # Contribution of each variable to dimension
    corrplot(res_pca$var$cor[,1:2], method="ellipse", is.corr=TRUE) # Correlation of each variable to dimension
    dev.off()
  }
  
  # ENCONTRAR UNA FORMA ADECUADA DE MOSTRAR LOS CLUSTERS EN UN ESPACIO BIVARIADO $$$
  # Biplot of individuals and variables. Only variables are labelled
  gg <- fviz_pca_biplot(res_pca,  label="var", habillage=res_hcpc$data.clust$clust, addEllipses=TRUE, ellipse.level=0.95) + theme_bw()
  # gg <- gg + scale_color_manual(values=c("black", "red", "forestgreen"))
  # gg <- gg + scale_fill_manual(values=c("black", "red", "forestgreen")) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
  }
  
  # Make boxplots for set of variables
  df_cluster <- res_hcpc$data.clust
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsClusters.csv', sep=''))){
    write.csv(df_cluster, paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsClusters.csv', sep=''), row.names=TRUE)
  }
  # colnames(df_cluster) <- gsub(pattern='-', replacement='_', colnames(df_cluster))
  df_cluster$Combination <- rownames(df_cluster)
  colnames(df_cluster) <- gsub(pattern='-', replacement='_', x=colnames(df_cluster), fixed=TRUE)
  df_cluster <- df_cluster %>% gather(Index, Value, CDD_S1:TOTRAIN_S2)
  df_cluster$Index <- as.character(df_cluster$Index)
  df_cluster$Season <- unlist(lapply(strsplit(x=df_cluster$Index, split='_S'), function(x){return(x[2])}))
  df_cluster$Season <- gsub(pattern='1', replacement='First', df_cluster$Season)
  df_cluster$Season <- gsub(pattern='2', replacement='Second', df_cluster$Season)
  df_cluster$Index <- unlist(lapply(strsplit(x=df_cluster$Index, split='_S'), function(x){return(x[1])}))
  df_cluster$Combination <- as.character(df_cluster$Combination)
  aux <- strsplit(x=df_cluster$Combination, split='-')
  aux <- lapply(1:length(aux), function(i){
    z <- as.data.frame(t(aux[[i]]))
    return(z)
  })
  aux <- do.call(rbind, aux)
  df_cluster$GCM <- aux$V1
  df_cluster$RCP <- aux$V2
  rm(aux)
  names(df_cluster)[1] <- 'Scenario'
  df_cluster$Index <- factor(x=df_cluster$Index, levels=c('TMEAN', 'GDD_1', 'GDD_2', 'ND_t35', 'TOTRAIN', 'CDD', 'P5D', 'P_95', 'NDWS'))
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_boxplot(aes(colour=Season))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Change by 20 years')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario.pdf', sep=''), plot=gg, width=8, height=9, units='in')
  }
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_jitter(aes(colour=Season))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Change by 20 years')
  gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
  gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='gold')
  gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
  # gg <- gg + geom_point(df_cluster)
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario2.pdf', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario2.pdf', sep=''), plot=gg, width=9, height=10, units='in')
  }
  
  return(cat('Process done.\n'))
  
}
county <- 'Taita Taveta'
scenarioTrendsClustering(county=county)
