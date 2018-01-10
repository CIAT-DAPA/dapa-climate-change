# Get stats from climatic indexes
# H. Achicanoy
# CIAT, 2016

options(warn = -1); options(scipen = 999); g <- gc(); rm(list = ls())
suppressMessages(if(!require(data.table)){install.packages('data.table'); library(data.table)} else {library(data.table)})
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(rasterVis)){install.packages('rasterVis'); library(rasterVis)} else {library(rasterVis)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(trend)){install.packages('trend'); library(trend)} else {library(trend)})
suppressMessages(if(!require(ggthemes)){install.packages('ggthemes'); library(ggthemes)} else {library(ggthemes)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})
suppressMessages(if(!require(tidyr)){install.packages('tidyr'); library(tidyr)} else {library(tidyr)})
suppressMessages(if(!require(FactoMineR)){install.packages('FactoMineR'); library(FactoMineR)} else {library(FactoMineR)})
suppressMessages(if(!require(factoextra)){install.packages('factoextra'); library(factoextra)} else {library(factoextra)})
suppressMessages(if(!require(lubridate)){install.packages('lubridate'); library(lubridate)} else {library(lubridate)})

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 1),
                                   rep('Cluster 2', 1)), County=c('Napo', 'Ucayali')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)
#countyList <- countyList[5,];
rownames(countyList) <- 1:nrow(countyList) # Machakos
#countyList <- countyList[1,]

MAIN_DIR<-'W:/'
#MAIN_DIR<-'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/'
periodList <- c('2020_2049', '2040_2069')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
gcmList    <- c("bcc_csm1_1",
                "bcc_csm1_1_m",
                "csiro_mk3_6_0",
                "gfdl_cm3",
                "ipsl_cm5a_lr",
                "miroc_esm",
                "miroc_esm_chem",
                "miroc_miroc5",
                "mohc_hadgem2_es",
                "mri_cgcm3",
                "ncc_noresm1_m",
                "nimr_hadgem2_ao"
                ) # "mohc_hadgem2_es"

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Historical lines and trends plots by county until 2015 (facet_wrap way)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapClimInd_2015 <- lapply(1:nrow(countyList), function(i){
  
  
  
  
  
  cat('Loading and processing Climatic Indexes for', countyList$County[[i]], 'baseline\n')
  load(paste(paste0(MAIN_DIR,'results/climatic_indices/historical/first_season/'), gsub(pattern=' ', replacement='_', countyList$County[[i]]),'_first_season.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  load(paste(paste0(MAIN_DIR,'results/climatic_indices/historical/second_season/'), gsub(pattern=' ', replacement='_', countyList$County[[i]]),'_second_season.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:ncol(first_season[[m]])], na.rm = T)),
                              Index   = names(first_season)[m],
                              Years   = as.numeric(colnames(first_season[[m]])[4:ncol(first_season[[m]])]),
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:ncol(second_season[[m]])], na.rm = T)),
                              Index   = names(second_season)[m],
                              Years   = as.numeric(colnames(second_season[[m]])[4:ncol(second_season[[m]])]),
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  allWrap <- rbind(first_seasonWrap, second_seasonWrap)
  rm(first_seasonWrap, second_seasonWrap)
  
  gg <- ggplot(allWrap, aes(x=Years, y=Average, colour=Season)) + geom_line() + geom_point() + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + facet_wrap(~ Index, scales='free') + theme_bw()
  #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/historical_trends/'
  outDir <- paste(paste0(MAIN_DIR,'results/graphics/historical_trends/'), gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_historicalTrend_2015.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_historicalTrend_2015.pdf', sep=''), plot=gg, width=10, height=9, units='in')
  #  system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_historicalTrend_2015.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_historicalTrend_2015.png", sep=""), wait=TRUE)
    #file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_historicalTrend_2015.pdf', sep=''))
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
  
  gg <- ggplot(trendAll, aes(x=Season, y=p_value, fill=Season, colour=Season)) + geom_bar(stat="identity") + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3')) + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + facet_wrap(~ Index, scales='free') + theme_bw()
  gg <- gg + geom_hline(yintercept=0.05)
  gg <- gg + ylab('p-value Mann-Kendall test') + scale_y_continuous(limits=c(0, 1))
  if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2015.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2015.pdf', sep=''), plot=gg, width=9, height=8, units='in')
   # system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_significanceTrend_2015.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_significanceTrend_2015.png", sep=""), wait=TRUE)
   # file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2015.pdf', sep=''))
  }
  
  return(allWrap)
  
})

names(wrapClimInd_2015) <- as.character(countyList$County)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Historical lines plots by county until 2015 (individual way)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Varying per county: eps files
lapply(1:nrow(countyList), function(i){
  
  allWrap <- wrapClimInd_2015[[countyList$County[i]]]
  indexList <- unique(as.character(allWrap$Index))
  
  # Varying per index for both two seasons
  lapply(1:length(indexList), function(j){
    
    gg <- ggplot(allWrap[allWrap$Index == indexList[j],], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
    gg <- gg + theme_bw() + xlab('Years')
    if(indexList[j] == "TMEAN"){gg <- gg + ylab(expression("Average temperature ("*~degree*C*")"))}
    if(indexList[j] == "GDD_1"){gg <- gg + ylab(expression("Growing degree days with TB = 10 ("*~degree*C*"/day)"))}
    if(indexList[j] == "GDD_2"){gg <- gg + ylab(expression("Growing degree days with TO = 25 ("*~degree*C*"/day)"))}
    if(indexList[j] == "ND_t35"){gg <- gg + ylab(expression("Total number of days with Tmax >= 35"*~degree*C*" (days)"))}
    if(indexList[j] == "TOTRAIN"){gg <- gg + ylab("Total precipitation (mm)")}
    if(indexList[j] == "CDD"){gg <- gg + ylab("Maximum number of consecutive dry days (days)")}
    if(indexList[j] == "P5D"){gg <- gg + ylab("Maximum 5-day running average precipitation (mm/day)")}
    if(indexList[j] == "P_95"){gg <- gg + ylab("Floods. 95th percentile of daily precipitation (mm/day)")}
    if(indexList[j] == "NDWS"){gg <- gg + ylab("Drought. Number of consecutive days with drought stress (days)")}
    if(indexList[j] == "SLGP"){gg <- gg + ylab("Starting day of growing season (days)")}
    if(indexList[j] == "LGP"){gg <- gg + ylab("Length of growing season (days)")}
    gg <- gg + theme(axis.text.x  = element_text(size=14),
                     axis.text.y  = element_text(size=14),
                     axis.title.x = element_text(face="bold",size=15),
                     axis.title.y = element_text(face="bold",size=15),
                     legend.text  = element_text(size=14),
                     legend.title = element_text(face="bold",size=15))
    gg <- gg + stat_smooth(method = "lm", se = FALSE)
    gg <- gg + scale_x_continuous(breaks = seq(1980, 2015, 5)) + scale_y_continuous(limits=c(min(allWrap$Average[allWrap$Index == indexList[j]])-1, max(allWrap$Average[allWrap$Index == indexList[j]])+1))
    
    outDir <- paste(paste0(MAIN_DIR,'results/graphics/historical_trends/'), gsub(pattern=' ', replacement='_', countyList$County[i]), '/individualPlots', sep='')
    if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
    #ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[i]), '_historical_', indexList[j], '.eps', sep=''), plot=gg, width=10, height=7, units='in')
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[i]), '_historical_', indexList[j], '.pdf', sep=''), plot=gg, width=10, height=7, units='in')
    
  })
  
  # Varying per index for each season
  lapply(1:length(indexList), function(k){
    
    seasonList <- unique(as.character(allWrap$Season))
    lapply(1:length(seasonList), function(l){
      
      gg <- ggplot(allWrap[allWrap$Index == indexList[k] & allWrap$Season == seasonList[l],], aes(x=Years, y=Average, colour=Season)) + geom_line(size=1.2) + geom_point(size=3) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
      gg <- gg + theme_bw() + xlab('Years')
      if(indexList[k] == "TMEAN"){gg <- gg + ylab(expression("Average temperature ("*~degree*C*")"))}
      if(indexList[k] == "GDD_1"){gg <- gg + ylab(expression("Growing degree days with TB = 10 ("*~degree*C*"/day)"))}
      if(indexList[k] == "GDD_2"){gg <- gg + ylab(expression("Growing degree days with TO = 25 ("*~degree*C*"/day)"))}
      if(indexList[k] == "ND_t35"){gg <- gg + ylab(expression("Total number of days with Tmax >= 35"*~degree*C*" (days)"))}
      if(indexList[k] == "TOTRAIN"){gg <- gg + ylab("Total precipitation (mm)")}
      if(indexList[k] == "CDD"){gg <- gg + ylab("Maximum number of consecutive dry days (days)")}
      if(indexList[k] == "P5D"){gg <- gg + ylab("Maximum 5-day running average precipitation (mm/day)")}
      if(indexList[k] == "P_95"){gg <- gg + ylab("Floods. 95th percentile of daily precipitation (mm/day)")}
      if(indexList[k] == "NDWS"){gg <- gg + ylab("Drought. Number of consecutive days with drought stress (days)")}
      if(indexList[k] == "SLGP"){gg <- gg + ylab("Starting day of growing season (days)")}
      if(indexList[k] == "LGP"){gg <- gg + ylab("Length of growing season (days)")}
      gg <- gg + theme(axis.text.x = element_text(size=14),
                       axis.text.y = element_text(size=14),
                       axis.title.x = element_text(face="bold",size=15),
                       axis.title.y = element_text(face="bold",size=15),
                       legend.text = element_text(size=14),
                       legend.title = element_text(face="bold",size=15))
      gg <- gg + stat_smooth(method = "lm", se = FALSE, color = "black")
      gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(min(allWrap$Average[allWrap$Index == indexList[k]])-1, max(allWrap$Average[allWrap$Index == indexList[k]])+1))
      
      outDir <- paste(paste0(MAIN_DIR,'results/graphics/historical_trends/'), gsub(pattern=' ', replacement='_', countyList$County[i]), '/individualPlots', sep='')
      if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
      ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[i]), '_historical_', indexList[k], '_', seasonList[l], '.eps', sep=''), plot=gg, width=10, height=7, units='in')
      
    })
    
  })
  
  # For annual precipitation
  completeTOTRAIN <- as.data.frame(dplyr::summarise(group_by(allWrap[allWrap$Index == 'TOTRAIN',], Index, Years), sum(Average)))
  colnames(completeTOTRAIN)[3] <- 'Average'
  gg <- ggplot(completeTOTRAIN, aes(x=Years, y=Average)) + geom_line(size=1.2) + geom_point(size=3)
  gg <- gg + theme_bw() + ylab('Total precipitation (mm)') + xlab('Years')
  gg <- gg + theme(axis.text.x = element_text(size=14),
                   axis.text.y = element_text(size=14),
                   axis.title.x = element_text(face="bold",size=15),
                   axis.title.y = element_text(face="bold",size=15),
                   legend.text = element_text(size=14),
                   legend.title = element_text(face="bold",size=15))
  gg <- gg + stat_smooth(method = "lm", se = FALSE, color = "black")
  outDir <- paste(paste0(MAIN_DIR,'results/graphics/historical_trends/'), gsub(pattern=' ', replacement='_', countyList$County[i]), '/individualPlots', sep='')
  gg <- gg + scale_x_continuous(breaks=seq(1980, 2015, 5)) + scale_y_continuous(limits=c(0, max(completeTOTRAIN$Average)+100))
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[i]), '_TOTRAIN_complete_2015.eps', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[i]), '_TOTRAIN_complete_2015.eps', sep=''), plot=gg, width=10, height=7, units='in')
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
  gg <- gg + stat_smooth(method = "lm", se = FALSE, color = "black")
  gg <- gg + scale_x_continuous(breaks=seq(1980, 2005, 5)) + scale_y_continuous(limits=c(min(completeTMEAN$Average)-1, max(completeTMEAN$Average)+1))
  outDir <- paste(paste0(MAIN_DIR,'results/graphics/historical_trends/'), gsub(pattern=' ', replacement='_', countyList$County[i]), '/individualPlots', sep='')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[i]), '_TMEAN_complete_2015.eps', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[i]), '_TMEAN_complete_2015.eps', sep=''), plot=gg, width=10, height=7, units='in')
  }
  
})

# EPS to PNG
# lapply(1:nrow(countyList), function(i){
#   dir <- paste(paste0(MAIN_DIR,'results/graphics/historical_trends/'), gsub(pattern = " ", replacement = "_", countyList$County[i]), "/individualPlots", sep = "")
#   files <- list.files(path = dir, pattern = ".eps$", full.names = F)
#   files <- gsub(pattern = ".eps", replacement = "", x = files)
#   lapply(1:length(files), function(j){
#     system(paste("convert -verbose -density 300 ", dir, '/', files[j], ".eps -quality 100 -sharpen 0x1.0 -alpha off ", dir, '/', files[j], ".png", sep=""), wait=TRUE)
#   })
# })

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Future trends by county
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

periodList <- c('2020_2049', '2040_2069')
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
#gcmList    <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "gfdl_esm2g","gfdl_esm2m","ipsl_cm5a_mr","miroc_esm", "miroc_esm_chem","miroc_miroc5","ncc_noresm1_m") # "mohc_hadgem2_es"
gcmList    <- c("bcc_csm1_1",
                "bcc_csm1_1_m",
                "csiro_mk3_6_0",
                "gfdl_cm3",
                "ipsl_cm5a_lr",
                "miroc_esm",
                "miroc_esm_chem",
                "miroc_miroc5",
                "mohc_hadgem2_es",
                "mri_cgcm3",
                "ncc_noresm1_m",
                "nimr_hadgem2_ao"
) 



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
          
          #
          load(paste(paste0(MAIN_DIR,'results/climatic_indices/future/'), seasonList[h], '_season/', gcmList[k], '/', periodList[i], '/', rcpList[j], '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_', seasonList[h], '_season.RData', sep=''))
          first_seasonFut <- clim_indexes; rm(clim_indexes)
          
          years_analysis <- unlist(strsplit(periodList[i], split='_'))
          years_analysis <- as.numeric(years_analysis)
          years_analysis <- years_analysis[1]:years_analysis[2]
          
          wrapClimInd <- lapply(1:length(first_seasonFut), function(m){
            wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_seasonFut[[m]][,4:ncol(first_seasonFut[[m]])], na.rm = T)),
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
  
  # First season and period 2021-2045
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='first' & wrapFutClimInd$Period=='2020_2049',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/future_trends/'
  outDir <- paste(paste0(MAIN_DIR,'results/graphics/future_trends/'), gsub(pattern=' ', replacement='_', countyList$County[g]), sep='')
  if(!dir.exists(outDir)){ dir.create(outDir, recursive=TRUE) }
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2020_2049.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2020_2049.pdf', sep=''), plot=gg, width=18, height=9, units='in')
    #system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_firstSeason_futureTrend_2020_2049.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_firstSeason_futureTrend_2020_2049.png", sep=""), wait=TRUE)
   # file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2020_2049.pdf', sep=''))
  }
  
  # Second season and period 2021-2045
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='second' & wrapFutClimInd$Period=='2020_2049',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2020_2049.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2020_2049.pdf', sep=''), plot=gg, width=18, height=9, units='in')
    #system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_secondSeason_futureTrend_2020_2049.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_secondSeason_futureTrend_2020_2049.png", sep=""), wait=TRUE)
    #file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2020_2049.pdf', sep=''))
  }
  
  # First season and period 2041-2065
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='first' & wrapFutClimInd$Period=='2040_2069',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2040_2069.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2040_2069.pdf', sep=''), plot=gg, width=18, height=9, units='in')
   # system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_firstSeason_futureTrend_2040_2069.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_firstSeason_futureTrend_2040_2069.png", sep=""), wait=TRUE)
   # file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_firstSeason_futureTrend_2040_2069.pdf', sep=''))
  }
  
  # Second season and period 2041-2065
  gg <- ggplot(wrapFutClimInd[wrapFutClimInd$County==countyList$County[g] & wrapFutClimInd$Season=='second' & wrapFutClimInd$Period=='2040_2069',], aes(x=Years, y=Average, colour=GCM)) + geom_line()
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y') + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2040_2069.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2040_2069.pdf', sep=''), plot=gg, width=18, height=9, units='in')
   # system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_secondSeason_futureTrend_2040_2069.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), "_secondSeason_futureTrend_2040_2069.png", sep=""), wait=TRUE)
  #  file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[g]), '_secondSeason_futureTrend_2040_2069.pdf', sep=''))
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
      wrapClimInd_median <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), median(Average, na.rm = T)))
      colnames(wrapClimInd_median)[4] <- 'Median'
      quantileFun <- function(x){z <- stats::quantile(x, probs=0.05, na.rm = T); return(z)}
      aux <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), quantileFun(Average)))
      wrapClimInd_median$p0_05 <- as.numeric(aux[,ncol(aux)])
      quantileFun <- function(x){z <- stats::quantile(x, probs=0.95, na.rm = T); return(z)}
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
  gg <- ggplot(wrapFutClimInd_median[wrapFutClimInd_median$Period=='2020_2049',], aes(x=Years, y=Median, colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line() + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y')
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.x = element_text(size=10),
                   axis.text.y = element_text(size=14),
                   axis.title.x = element_text(face="bold",size=15),
                   axis.title.y = element_text(face="bold",size=15),
                   legend.text = element_text(size=14),
                   legend.title = element_text(face="bold",size=15))
  #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/future_trends/'
  outDir <- paste(paste0(MAIN_DIR,'results/graphics/future_trends/'), gsub(pattern=' ', replacement='_', countyList$County[[g]]), sep='')
  if(!dir.exists(outDir)){ dir.create(outDir, recursive=TRUE) }
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2020_2049.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2020_2049.pdf', sep=''), plot=gg, width=18, height=15, units='in')
    #system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), "_mergeFutureTrend_2020_2049.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), "_mergeFutureTrend_2020_2049.png", sep=""), wait=TRUE)
    #file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2020_2049.pdf', sep=''))
  }
  
  # Period: 2041-2065
  gg <- ggplot(wrapFutClimInd_median[wrapFutClimInd_median$Period=='2040_2069',], aes(x=Years, y=Median, colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line() + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y')
  gg <- gg + theme_bw()
  gg <- gg + theme(axis.text.x = element_text(size=10),
                   axis.text.y = element_text(size=14),
                   axis.title.x = element_text(face="bold",size=15),
                   axis.title.y = element_text(face="bold",size=15),
                   legend.text = element_text(size=14),
                   legend.title = element_text(face="bold",size=15))
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2040_2069.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2040_2069.pdf', sep=''), plot=gg, width=18, height=15, units='in')
  #  system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), "_mergeFutureTrend_2040_2069.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), "_mergeFutureTrend_2040_2069.png", sep=""), wait=TRUE)
  #  file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[g]]), '_mergeFutureTrend_2040_2069.pdf', sep=''))
  }
  
  return(wrapFutClimInd_median)
  
})
wrapFutClimInd_median <- do.call(rbind, wrapFutClimInd_median)

# Significance test
periodList <- c('2020_2049', '2040_2069')
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
  
  gg <- ggplot(trendPeriod[trendPeriod$Period=='2020_2049',], aes(x=RCP, y=p_value, fill=Season, colour=Season)) + geom_bar(stat='identity', position="dodge") + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3')) + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + facet_wrap(~ Index)
  gg <- gg + theme_bw() + geom_hline(yintercept=0.05)
  gg <- gg + ylab('p-value Mann-Kendall test') + scale_y_continuous(limits=c(0, 1))
  #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/future_trends/'
  outDir <- paste(paste0(MAIN_DIR,'results/graphics/future_trends/'), gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2020_2049.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2020_2049.pdf', sep=''), plot=gg, width=8, height=9, units='in')
   # system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_significanceTrend_2020_2049.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_significanceTrend_2020_2049.png", sep=""), wait=TRUE)
   # file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2020_2049.pdf', sep=''))
  }
  
  gg <- ggplot(trendPeriod[trendPeriod$Period=='2040_2069',], aes(x=RCP, y=p_value, fill=Season, colour=Season)) + geom_bar(stat='identity', position="dodge") + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3')) + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + facet_wrap(~ Index)
  gg <- gg + theme_bw() + geom_hline(yintercept=0.05)
  gg <- gg + ylab('p-value Mann-Kendall test') + scale_y_continuous(limits=c(0, 1))
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2040_2069.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2040_2069.pdf', sep=''), plot=gg, width=8, height=9, units='in')
   # system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_significanceTrend_2040_2069.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), "_significanceTrend_2040_2069.png", sep=""), wait=TRUE)
   # file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_significanceTrend_2040_2069.pdf', sep=''))
  }
  
  return(trendPeriod)
  
})
trendFutAll <- do.call(rbind, trendFutAll)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Calculate median through GCMs using extreme percentiles to show future trends by 2021-2065
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Varying per county
lapply(1:nrow(countyList), function(x){
  
  wrapFutClimInd <- wrapFutClimInd[wrapFutClimInd$County == countyList$County[x],]
  
  # Median for all years through 2021-2065
  wrapFutClimInd_median <- lapply(1:length(seasonList), function(h){
    
    wrapClimInd <- wrapFutClimInd[wrapFutClimInd$Season==seasonList[h],]
    wrapClimInd_median <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), median(Average, na.rm = T)))
    colnames(wrapClimInd_median)[4] <- 'Median'
    quantileFun <- function(x){z <- stats::quantile(x, probs=0.05, na.rm = T); return(z)}
    aux <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), quantileFun(Average)))
    wrapClimInd_median$p0_05 <- as.numeric(aux[,ncol(aux)])
    quantileFun <- function(x){z <- stats::quantile(x, probs=0.95, na.rm = T); return(z)}
    aux <- as.data.frame(dplyr::summarise(group_by(wrapClimInd, Index, RCP, Years), quantileFun(Average)))
    wrapClimInd_median$p0_95 <- as.numeric(aux[,ncol(aux)])
    
    proper <- function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
    wrapClimInd_median$Season <- proper(seasonList[h])
    return(wrapClimInd_median)
    
  })
  wrapFutClimInd_median <- do.call(rbind, wrapFutClimInd_median)
  
  # Facet plot
  gg <- ggplot(wrapFutClimInd_median, aes(x=Years, y=Median, colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line() + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + facet_grid(Index ~ RCP, scales='free_y')
  gg <- gg + theme_bw() + ylab('Median of each index with 90% interval of uncertainty')
  gg <- gg + theme(axis.text.x = element_text(size=10),
                   axis.text.y = element_text(size=14),
                   axis.title.x = element_text(face="bold",size=15),
                   axis.title.y = element_text(face="bold",size=15),
                   legend.text = element_text(size=14),
                   legend.title = element_text(face="bold",size=15))
  #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/future_trends/'
  outDir <- paste(paste0(MAIN_DIR,'results/graphics/future_trends/'), gsub(pattern=' ', replacement='_', countyList$County[x]), sep='')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_allFutureTrend_2020_2069.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_allFutureTrend_2020_2069.pdf', sep=''), plot=gg, width=21, height=15, units='in')
   # system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), "_allFutureTrend_2020_2069.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), "_allFutureTrend_2020_2069.png", sep=""), wait=TRUE)
   # file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_allFutureTrend_2020_2069.pdf', sep=''))
  }
  
  # Individual plots with all RCPs
  indList    <- unique(as.character(wrapFutClimInd_median$Index))
  lapply(1:length(indList), function(i){
    db <- wrapFutClimInd_median[wrapFutClimInd_median$Index==indList[i],]
    rownames(db) <- 1:nrow(db)
    gg <- ggplot(db, aes(x=Years, y=Median, colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
    gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line(size=1.2) + geom_point(size=3) + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
    gg <- gg + facet_wrap(~RCP)
    gg <- gg + theme_bw() + ylab('Median of each index with 90% interval of uncertainty')
    gg <- gg + theme(axis.text.x = element_text(size=14),
                     axis.text.y = element_text(size=14),
                     axis.title.x = element_text(face="bold",size=15),
                     axis.title.y = element_text(face="bold",size=15),
                     legend.text = element_text(size=14),
                     legend.title = element_text(face="bold",size=15),
                     strip.text = element_text(size=15))
    #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/future_trends/'
    outDir <- paste(paste0(MAIN_DIR,'results/graphics/future_trends/'), gsub(pattern=' ', replacement='_', countyList$County[x]), '/individual_plots', sep='')
    if(!dir.exists(outDir)){ dir.create(outDir, recursive=TRUE) }
    if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_allRCP_2020_2069.png', sep=''))){
      ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_allRCP_2020_2069.pdf', sep=''), plot=gg, width=12, height=12, units='in')
     # system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], "_allRCP_2020_2069.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], "_allRCP_2020_2069.png", sep=""), wait=TRUE)
     # file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_allRCP_2020_2069.pdf', sep=''))
    }
  })
  
  # Individual plots per each RCP
  indList <- unique(as.character(wrapFutClimInd_median$Index))
  rcpList <- paste("rcp", c(26, 45, 60, 85), sep="")
  lapply(1:length(indList), function(i){
    
    lapply(1:length(rcpList), function(j){
      
      db <- wrapFutClimInd_median[wrapFutClimInd_median$Index==indList[i] & wrapFutClimInd_median$RCP==rcpList[j],]
      rownames(db) <- 1:nrow(db)
      gg <- ggplot(db, aes(x=Years, y=Median, colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
      gg <- gg + geom_ribbon(aes(ymin=p0_05, ymax=p0_95, fill=Season, linetype=NA) ,alpha=.1) + geom_line(size=1.2) + geom_point(size=3) + scale_fill_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
      gg <- gg + theme_bw() + ylab('Median of each index with 90% interval of uncertainty')
      gg <- gg + theme(axis.text.x = element_text(size=14),
                       axis.text.y = element_text(size=14),
                       axis.title.x = element_text(face="bold",size=15),
                       axis.title.y = element_text(face="bold",size=15),
                       legend.text = element_text(size=14),
                       legend.title = element_text(face="bold",size=15),
                       strip.text = element_text(size=15),
                       plot.title = element_text(size=20))
      gg <- gg + scale_x_continuous(breaks=seq(2020, 2069, 5))
      gg <- gg + ggtitle(paste('Index: ', indList[i], ' - RCP: ', rcpList[j] ,sep=''))
      #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/future_trends/'
      outDir <- paste(paste0(MAIN_DIR,'results/graphics/future_trends/'), gsub(pattern=' ', replacement='_', countyList$County[x]), '/individual_plots', sep='')
      if(!dir.exists(outDir)){ dir.create(outDir, recursive=TRUE) }
      if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_', rcpList[j], '_2020_2069.png', sep=''))){
        ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_', rcpList[j], '_2020_2069.pdf', sep=''), plot=gg, width=12, height=10, units='in')
#         system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_', rcpList[j], "_2020_2069.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_', rcpList[j], "_2020_2069.png", sep=""), wait=TRUE)
#         file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[x]), '_', indList[i], '_', rcpList[j], '_2020_2069.png', sep=''))
       }
      
    })
    
  })
  
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Integrating historical and future trends to explore changes
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

barCharts <- function(histData = wrapClimInd_2015, futData = wrapFutClimInd_median){
  
  if(class(histData) == "list"){
    nms <- names(histData)
    histData <- lapply(1:length(histData), function(i){
      z <- histData[[i]]
      z$County <- nms[i]
      return(z)
    })
    histData <- do.call(rbind, histData)
  }
  
  # Calculate mean and sd for historical data
  allWrap2 <- as.data.frame(dplyr::summarise(group_by(histData, County, Index, Season), mean(Average)))
  allWrap2$std <- as.data.frame(dplyr::summarise(group_by(histData, County, Index, Season), sd(Average)))[,ncol(as.data.frame(dplyr::summarise(group_by(histData, County, Index, Season), sd(Average))))]
  names(allWrap2) <- c('County', 'Index', 'Season', 'Mean', 'Std')
  
  # Calculate mean and sd for future data
  wrapFutClimInd_median2 <- as.data.frame(dplyr::summarise(group_by(futData, County, Index, RCP, Season), mean(Median)))
  wrapFutClimInd_median2$std <- as.data.frame(dplyr::summarise(group_by(futData, County, Index, RCP, Season), sd(Median)))[,ncol(as.data.frame(dplyr::summarise(group_by(futData, County, Index, RCP, Season), mean(Median))))]
  names(wrapFutClimInd_median2) <- c('County', 'Index', 'RCP', 'Season', 'Mean', 'Std')
  
  county <- allWrap2; rm(allWrap2)
  county$RCP <- 'Historical'
  county <- rbind(county, wrapFutClimInd_median2); rm(wrapFutClimInd_median2)
  
  indexList <- unique(as.character(county$Index))
  cntyList <- unique(as.character(county$County))
  
  lapply(1:length(indexList), function(j){
    
    lapply(1:length(cntyList), function(k){
      
      county2 <- county[county$County == cntyList[k] & county$Index == indexList[j] & (county$RCP == "Historical" | county$RCP == "rcp26" | county$RCP == "rcp85"),]; rownames(county2) <- 1:nrow(county2)
      limits <- aes(ymax = Mean + Std, ymin = Mean - Std)
      
      gg <- ggplot(county2, aes(fill = Season, y = Mean, x = RCP)) + geom_bar(stat = "identity", position = "dodge")
      gg <- gg + scale_fill_manual(values = c('darkgoldenrod3', 'turquoise3'),
                                   breaks = c("First", "Second"),
                                   labels = c("January-June", "July-December"))
      dodge <- position_dodge(width = 0.9)
      gg <- gg + geom_errorbar(limits, position = dodge, width = 0.25, data = county2)
      gg <- gg + xlab('')
      if(indexList[j] == "TMEAN"){gg <- gg + ylab(expression("Average temperature ("*~degree*C*")"))}
      if(indexList[j] == "GDD_1"){gg <- gg + ylab(expression("Growing degree days with TB = 10 ("*~degree*C*"/day)"))}
      if(indexList[j] == "GDD_2"){gg <- gg + ylab(expression("Growing degree days with TO = 25 ("*~degree*C*"/day)"))}
      if(indexList[j] == "ND_t35"){gg <- gg + ylab(expression("Total number of days with Tmax >= 35"*~degree*C*" (days)"))}
      if(indexList[j] == "TOTRAIN"){gg <- gg + ylab("Total precipitation (mm)")}
      if(indexList[j] == "CDD"){gg <- gg + ylab("Maximum number of consecutive dry days (days)")}
      if(indexList[j] == "P5D"){gg <- gg + ylab("Maximum 5-day running average precipitation (mm/day)")}
      if(indexList[j] == "P_95"){gg <- gg + ylab("Floods. 95th percentile of daily precipitation (mm/day)")}
      if(indexList[j] == "NDWS"){gg <- gg + ylab("Drought. Number of consecutive days with drought stress (days)")}
      if(indexList[j] == "SLGP"){gg <- gg + ylab("Starting day of growing season (days)")}
      if(indexList[j] == "LGP"){gg <- gg + ylab("Length of growing season (days)")}
      gg <- gg + theme_bw()
      gg <- gg + theme(axis.text.x  = element_text(size=14),
                       axis.text.y  = element_text(size=14),
                       axis.title.x = element_text(face="bold",size=15),
                       axis.title.y = element_text(face="bold",size=15),
                       legend.text  = element_text(size=14),
                       legend.title = element_text(face="bold",size=15))
      gg <- gg + scale_x_discrete(breaks=c("Historical","rcp26","rcp45","rcp85"),
                                  labels=c("Historical\n(1981-2015)", "RCP2.6\n(2020-2069)", "RCP4.5\n(2020-2069)", "RCP8.5\n(2020-2069)"))
      #'/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/WORK_PACKAGES/WP2/06_Clustering_analyses/results/graphics/future_trends/'
      outDir <- paste(paste0(MAIN_DIR,'results/graphics/future_trends/'), gsub(pattern=' ', replacement='_', countyList$County[k]), sep='')
      if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
      ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', countyList$County[k]), '_barChart_historical_future_changes_', indexList[j], '.pdf', sep=''), plot=gg, width=10, height=7, units='in')
      
    })
    
  })
  
}
barCharts(histData = wrapClimInd_2015, futData = wrapFutClimInd_median)

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Cluster analysis to identify climate change scenarios by county using absolute changes
# (Here we use historical and future absolute changes)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

wrapFutClimInd2 <- wrapFutClimInd
wrapFutClimInd2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2, Index, GCM, RCP, County, Season, Years), median(Average)))
colnames(wrapFutClimInd2)[ncol(wrapFutClimInd2)] <- 'Average'

source(paste0(MAIN_DIR,"cc_scenarios.R"))
#county <- 'Napo'
lapply(countyList$County, function(county){scenarioClustering3(county = county)})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Cluster analysis to identify climate change scenarios by county using trends
# (Here we use historical and future trends)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# wrapFutClimInd2 <- wrapFutClimInd
# wrapFutClimInd2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2, Index, GCM, RCP, County, Season, Years), median(Average)))
# colnames(wrapFutClimInd2)[ncol(wrapFutClimInd2)] <- 'Average'
# 
# #county <- 'Ucayali'
# source(paste0(MAIN_DIR,"cc_scenarios.R"))
# lapply(countyList$County, function(county){scenarioClustering3(county = county)})
############################################################################################################################
############################################################################################################################
wrapFutClimInd2 <- wrapFutClimInd
wrapFutClimInd2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2, Index, GCM, RCP, County, Season, Years), median(Average)))
colnames(wrapFutClimInd2)[ncol(wrapFutClimInd2)] <- 'Average'

source(paste0(MAIN_DIR,"cc_scenarios.R"))
#county <- 'Napo'
lapply(countyList$County, function(county){scenarioTrendsClustering(county = county)})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# Cluster analysis to identify climate change scenarios by county using trends
# (Here we use historical and future trends)
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 
# wrapFutClimInd2 <- wrapFutClimInd
# wrapFutClimInd2 <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2, Index, GCM, RCP, County, Season, Years), median(Average)))
# colnames(wrapFutClimInd2)[ncol(wrapFutClimInd2)] <- 'Average'
# 
# county <- 'Napo'
# #source(paste0(MAIN_DIR,"cc_scenarios.R"))
# lapply(countyList$County, function(county){scenarioTrendsClustering(county = county)})
# 
