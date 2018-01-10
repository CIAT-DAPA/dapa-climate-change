# Graphics to publish
# H. Achicanoy
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#  Load packages and previous information ----
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

options(warn=-1); options(scipen=999)
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
countyList <- countyList[13,]
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

periodList <- c('2021_2045', '2041_2065')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"
# ----

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Historical trends by county until 2015 ----
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
# ----

# Historical temperature plot (TMEAN)
# ----

gg <- ggplot(allWrap[allWrap$Index=='TMEAN',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + theme_bw() + ylab(expression("Average temperature ("*~degree*C*")")) + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(min(allWrap$Average[allWrap$Index=='TMEAN'])-1, max(allWrap$Average[allWrap$Index=='TMEAN'])+1))

outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_mean_temperature.eps', sep=''), plot=gg, width=10, height=7, units='in')
# ----

# Historical rainfall plot (TOTRAIN)
# ----

gg <- ggplot(allWrap[allWrap$Index=='TOTRAIN',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + theme_bw() + ylab('Total precipitation (mm)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, max(allWrap$Average[allWrap$Index=='TOTRAIN'])+10))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_annual_precipitation.eps', sep=''), plot=gg, width=10, height=7, units='in')
# ----

# Historical flooding plot (P5D)
# ----

# gg <- ggplot(allWrap[allWrap$Index=='P5D',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
# gg <- gg + theme_bw() + ylab('Maximum 5-day running average precipitation (mm/day)') + xlab('Years')
# gg <- gg + theme(axis.text.x = element_text(size=14),
#                  axis.text.y = element_text(size=14),
#                  axis.title.x = element_text(face="bold",size=15),
#                  axis.title.y = element_text(face="bold",size=15),
#                  legend.text = element_text(size=14),
#                  legend.title = element_text(face="bold",size=15))
# gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, max(allWrap$Average[allWrap$Index=='P5D'])+10))
# outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
# if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_floodsP5DPublsh_season_2015.pdf', sep=''))){
#   ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_floodsP5DPublsh_season_2015.pdf', sep=''), plot=gg, width=10, height=7, units='in')
#   system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_floodsP5DPublsh_season_2015.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_floodsP5DPublsh_season_2015.png", sep=""), wait=TRUE)
# }
# ----

# Historical flooding plot (P_95)
# ----

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
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_extreme_floods_events.eps', sep=''), plot=gg, width=10, height=7, units='in')
# ----

# Historical drought plot (NDWS)
# ----

gg <- ggplot(allWrap[allWrap$Index=='NDWS',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + theme_bw() + ylab('Drought. Number of consecutive days with drought stress (days)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, max(allWrap$Average[allWrap$Index=='NDWS'])+5))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_drought_stress_events.eps', sep=''), plot=gg, width=10, height=7, units='in')
# ----

# Historical drought spells plot (CDD)
# ----

gg <- ggplot(allWrap[allWrap$Index=='CDD',], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
gg <- gg + theme_bw() + ylab('Maximum number of consecutive dry days (days)') + xlab('Years')
gg <- gg + theme(axis.text.x = element_text(size=14),
                 axis.text.y = element_text(size=14),
                 axis.title.x = element_text(face="bold",size=15),
                 axis.title.y = element_text(face="bold",size=15),
                 legend.text = element_text(size=14),
                 legend.title = element_text(face="bold",size=15))
gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, max(allWrap$Average[allWrap$Index=='CDD'])+5))
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/historical_trends/', gsub(pattern=' ', replacement='_', countyList$County[[1]]), sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_dry_spells_events.eps', sep=''), plot=gg, width=10, height=7, units='in')
# ----

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Future trends by county ----
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
# ----

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Calculate median through GCMs using extreme percentiles to show future trends by period ----
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
# ----

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Calculate median through GCMs using extreme percentiles to show future trends by 2021-2065 ----
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
# ----

# Future flooding plot (P_95)
# ----

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
  system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_P_95_floodsP95_allRCP_2021_2065.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_P_95_floodsP95_allRCP_2021_2065.png", sep=""), wait=TRUE)
}
# ----

# Future drought plot (NDWS)
# ----

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
  system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_NDWS_drought_allRCP_2021_2065.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_NDWS_drought_allRCP_2021_2065.png", sep=""), wait=TRUE)
}
# ----

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Integrating historical and future trends to explore changes
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# ----

# Historical data
allWrap2 <- as.data.frame(dplyr::summarise(group_by(allWrap, Index, Season), mean(Average)))
allWrap2$std <- as.data.frame(dplyr::summarise(group_by(allWrap, Index, Season), sd(Average)))[,ncol(as.data.frame(dplyr::summarise(group_by(allWrap, Index, Season), sd(Average))))]
names(allWrap2) <- c('Index', 'Season', 'Mean', 'Std')

# Future data
wrapFutClimInd_median2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd_median, Index, RCP, Season), mean(Median)))
wrapFutClimInd_median2$std <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd_median, Index, RCP, Season), sd(Median)))[,ncol(as.data.frame(dplyr::summarise(group_by(wrapFutClimInd_median, Index, RCP, Season), mean(Median))))]
names(wrapFutClimInd_median2) <- c('Index', 'RCP', 'Season', 'Mean', 'Std')

county <- allWrap2
county$RCP <- 'Historical'
county <- rbind(county, wrapFutClimInd_median2)

county2 <- county[county$Index=='P_95'|county$Index=='NDWS',]
county2 <- county2[county2$RCP=='Historical'|county2$RCP=='rcp26'|county2$RCP=='rcp85',]
county2$RCP <- gsub(pattern='rcp', replacement='RCP ', county2$RCP)
county2$Index <- as.character(county2$Index)
county2$Index <- gsub(pattern='NDWS', replacement='Drought', county2$Index)
county2$Index <- gsub(pattern='P_95', replacement='Floods', county2$Index)

county2$Index <- gsub(pattern='TMEAN', replacement='Mean temperature', county2$Index)
county2$Index <- gsub(pattern='TOTRAIN', replacement='Total rainfall', county2$Index)
county2$Index <- gsub(pattern='CDD', replacement='Consecutive dry days', county2$Index)

library(ggplot2)

limits <- aes(ymax=Mean+Std, ymin=Mean-Std)

p <- ggplot(county2, aes(fill=Season, y=Mean, x=RCP)) + geom_bar(stat="identity", position="dodge") + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
dodge <- position_dodge(width=0.9)
p <- p + facet_grid(Index ~ Season, scales="free")
p <- p + geom_errorbar(limits, position=dodge, width=0.25)
p <- p + xlab('') + ylab('')
p <- p + theme_bw()

if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_droughts_floods.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_droughts_floods.pdf', sep=''), plot=p, width=6, height=6, units='in')
  system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_droughts_floods.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_tmean.png", sep=""), wait=TRUE)
}

if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_totrain_floods.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_totrain_floods.pdf', sep=''), plot=p, width=6, height=6, units='in')
  system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_totrain_floods.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_totrain_floods.png", sep=""), wait=TRUE)
}

if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_totrain_dspell.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_totrain_dspell.pdf', sep=''), plot=p, width=6, height=6, units='in')
  system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_totrain_dspell.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_totrain_dspell.png", sep=""), wait=TRUE)
}

if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_totrain_tmean.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_totrain_tmean.pdf', sep=''), plot=p, width=6, height=6, units='in')
  system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_totrain_tmean.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_totrain_tmean.png", sep=""), wait=TRUE)
}

outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_train.pdf', sep=''))){
  ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_cc_train.pdf', sep=''), plot=p, width=6, height=6, units='in')
  system(paste("convert -verbose -density 300 ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_train.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, "/", gsub(pattern=' ', replacement='_', countyList$County[1]), "_cc_train.png", sep=""), wait=TRUE)
}

# TMEAN
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_and_expected_mean_temperature.eps', sep=''), plot=p, width=6, height=6, units='in')

# Flash floods
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_and_expected_extreme_flood_events.eps', sep=''), plot=p, width=6, height=6, units='in')

# TOTRAIN
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_and_expected_annual_precipitation.eps', sep=''), plot=p, width=6, height=6, units='in')

# Drought (NDWS)
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_and_expected_drought_stress_events.eps', sep=''), plot=p, width=6, height=6, units='in')

# Drought spells (CDD)
outDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/results/graphics/future_trends/', gsub(pattern=' ', replacement='_', countyList$County[1]), '/individual_plots', sep='')
ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[1]), '_historical_and_expected_dry_spells_events.eps', sep=''), plot=p, width=6, height=6, units='in')
