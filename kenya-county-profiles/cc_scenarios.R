
# Absolute changes
scenarioClustering3 <- function(county){
  
  seasonList <- c('first', 'second')
  
  # *** Load historical data first season
  load(paste('/mnt/workspace_cluster_12/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[1], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[1], '_season_2015.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  
  ###
  first_seasonWrap1 <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:23], na.rm = T)),
                              Index   = names(first_season)[m],
                              Years   = 1981:2000,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap1 <- do.call(rbind, first_seasonWrap1)
  first_seasonWrap1 <- as.data.frame(dplyr::summarise(group_by(first_seasonWrap1, Index, Season), mean(Average, na.rm = T)))
  colnames(first_seasonWrap1)[3] <- 'Average'
  
  ###
  first_seasonWrap2 <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,(ncol(first_season[[m]])-20+1):ncol(first_season[[m]])], na.rm = T)),
                              Index   = names(first_season)[m],
                              Years   = 1986:2005,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap2 <- do.call(rbind, first_seasonWrap2)
  first_seasonWrap2 <- as.data.frame(dplyr::summarise(group_by(first_seasonWrap2, Index, Season), mean(Average, na.rm = T)))
  colnames(first_seasonWrap2)[3] <- 'Average'
  
  fs_absChanges <- first_seasonWrap2$Average - first_seasonWrap1$Average; rm(first_seasonWrap1, first_seasonWrap2)
  fs_absChanges <- data.frame(Index=names(first_season), Average=fs_absChanges)
  fs_absChanges <- fs_absChanges[order(fs_absChanges$Index),]
  
  ###
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:which(names(first_season[[m]]) == "2005")], na.rm = T)),
                              Index   = names(first_season)[m],
                              Years   = 1981:2005,
                              Season  = 'First')
    return(wrapClimInd)
  })
  first_seasonWrap <- do.call(rbind, first_seasonWrap)
  first_seasonWrap <- as.data.frame(dplyr::summarise(group_by(first_seasonWrap, Index, Season), mean(Average, na.rm = T)))
  colnames(first_seasonWrap)[3] <- 'Average'
  
  # *** Load historical data second season
  load(paste('/mnt/workspace_cluster_12/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[2], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[2], '_season_2015.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  ###
  second_seasonWrap1 <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:23], na.rm = T)),
                              Index   = names(second_season)[m],
                              Years   = 1981:2000,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap1 <- do.call(rbind, second_seasonWrap1)
  second_seasonWrap1 <- as.data.frame(dplyr::summarise(group_by(second_seasonWrap1, Index, Season), mean(Average, na.rm = T)))
  colnames(second_seasonWrap1)[3] <- 'Average'
  
  ###
  second_seasonWrap2 <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,(ncol(second_season[[m]])-20+1):ncol(second_season[[m]])], na.rm = T)),
                              Index   = names(second_season)[m],
                              Years   = 1986:2005,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap2 <- do.call(rbind, second_seasonWrap2)
  second_seasonWrap2 <- as.data.frame(dplyr::summarise(group_by(second_seasonWrap2, Index, Season), mean(Average, na.rm = T)))
  colnames(second_seasonWrap2)[3] <- 'Average'
  
  ss_absChanges <- second_seasonWrap2$Average - second_seasonWrap1$Average; rm(second_seasonWrap1, second_seasonWrap2)
  ss_absChanges <- data.frame(Index=names(second_season), Average=ss_absChanges)
  ss_absChanges <- ss_absChanges[order(ss_absChanges$Index),]
  
  #
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:which(names(first_season[[m]]) == "2005")], na.rm = T)),
                              Index   = names(second_season)[m],
                              Years   = 1981:2005,
                              Season  = 'Second')
    return(wrapClimInd)
  })
  second_seasonWrap <- do.call(rbind, second_seasonWrap)
  second_seasonWrap <- as.data.frame(dplyr::summarise(group_by(second_seasonWrap, Index, Season), mean(Average, na.rm = T)))
  colnames(second_seasonWrap)[3] <- 'Average'
  
  all_seasons <- rbind(first_seasonWrap, second_seasonWrap); rm(first_seasonWrap, second_seasonWrap)
  
  # *** Load future data
  wrapFutClimInd_county <- as.data.frame(dplyr::summarise(group_by(wrapFutClimInd2[wrapFutClimInd2$County==county,], Index, GCM, RCP, Season), mean(Average, na.rm = T)))
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
  outDir <- paste('/mnt/workspace_cluster_12/Kenya_KACCAL/results/graphics/cluster_analysis/combined/absolute_changes_allYears/', gsub(pattern=' ', replacement='_', county), sep='')
  if(!dir.exists(outDir)){dir.create(outDir, recursive = T)}
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.png', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''), height=8, width=8)
    heatmap.2(corMat, col=my_palette, density.info="none", trace="none", dendrogram="column", key.title='', key.xlab='Spearman correlation', margins=c(9,9))
    dev.off()
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_correlationMatrix.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_correlationMatrix.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''))
  }
  
  # Doing PCA using absolute changes and doing hierarchical clustering on principal components
  # because our variables are correlated
  suppressMessages(library(FactoMineR))
  res_pca  <- FactoMineR::PCA(X=wrapFutClimInd_absChange, graph=FALSE)
  set.seed(1235)
  res_hcpc <- FactoMineR::HCPC(res_pca, nb.clust=-1, graph=FALSE) # Define number of SCENARIOS automatically
  
  suppressMessages(library(factoextra))
  suppressMessages(library(corrplot))
  
  # Visualize eigenvalues/variances
  gg <- fviz_eig(res_pca, addlabels=TRUE, hjust = -0.3) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''), plot=gg, width=7, height=7, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_eigenValuesPCA.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_eigenValuesPCA.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''))
  }
  
  # Visualize variables with contributions. Use gradient color
  gg <- fviz_pca_var(res_pca, col.var="contrib")+ scale_color_gradient2(low="white", mid="blue", high="red", midpoint = 96) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_varMapContributionsPCA.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_varMapContributionsPCA.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''))
  }
  
  # Quality of representation of each variable
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.png', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''), height=7, width=8)
    par(mfrow=c(1,3))
    corrplot(res_pca$var$cos2[,1:2], is.corr=FALSE) # Representation quality of each variable
    corrplot(res_pca$var$contrib[,1:2], is.corr=FALSE) # Contribution of each variable to dimension
    corrplot(res_pca$var$cor[,1:2], method="ellipse", is.corr=TRUE) # Correlation of each variable to dimension
    dev.off()
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_rQuality.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_rQuality.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''))
  }
  
  # ENCONTRAR UNA FORMA ADECUADA DE MOSTRAR LOS CLUSTERS EN UN ESPACIO BIVARIADO $$$
  # Biplot of individuals and variables. Only variables are labelled
  gg <- fviz_pca_biplot(res_pca,  label="var", habillage=res_hcpc$data.clust$clust, addEllipses=TRUE, ellipse.level=0.95) + theme_bw()
  # gg <- gg + scale_color_manual(values=c("black", "red", "forestgreen"))
  # gg <- gg + scale_fill_manual(values=c("black", "red", "forestgreen")) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_biplotClusterPCA.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_biplotClusterPCA.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''))
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
  df_cluster$Index <- factor(x=df_cluster$Index, levels=c('TMEAN', 'GDD_1', 'GDD_2', 'ND_t35', 'TOTRAIN', 'CDD', 'P5D', 'P_95', 'NDWS', 'SLGP', 'LGP'))
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_boxplot(aes(colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Absolute change respect to baseline')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline.pdf', sep=''), plot=gg, width=8, height=9, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_absChange_baseline.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_absChange_baseline.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline.pdf', sep=''))
  }
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_jitter(aes(colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Absolute change respect to baseline')
  if(length(unique(df_cluster$Scenario))==1){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
  }
  if(length(unique(df_cluster$Scenario))==2){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
  }
  if(length(unique(df_cluster$Scenario))==3){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
  }
  if(length(unique(df_cluster$Scenario))==4){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
    gg <- gg + annotate('rect', xmin=3.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='mediumpurple1')
  }
  if(length(unique(df_cluster$Scenario))==5){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
    gg <- gg + annotate('rect', xmin=3.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='mediumpurple1')
    gg <- gg + annotate('rect', xmin=4.5, xmax=5.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='rosybrown1')
  }
  if(length(unique(df_cluster$Scenario))==6){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
    gg <- gg + annotate('rect', xmin=3.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='mediumpurple1')
    gg <- gg + annotate('rect', xmin=4.5, xmax=5.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='rosybrown1')
    gg <- gg + annotate('rect', xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='lightgreen')
  }
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline2.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline2.pdf', sep=''), plot=gg, width=9, height=10, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_absChange_baseline2.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_absChange_baseline2.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_absChange_baseline2.pdf', sep=''))
  }
  
  return(cat('Process done.\n'))
  
}

# Trends
scenarioTrendsClustering <- function(county){
  
  seasonList <- c('first', 'second')
  
  # *** Load historical data first season
  load(paste('/mnt/workspace_cluster_12/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[1], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[1], '_season_2015.RData', sep=''))
  first_season <- clim_indexes; rm(clim_indexes)
  
  first_seasonWrap <- lapply(1:length(first_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(first_season[[m]][,4:which(names(first_season[[m]]) == "2005")], na.rm = T)),
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
  load(paste('/mnt/workspace_cluster_12/Kenya_KACCAL/results/climatic_indices/historical/', seasonList[2], '_season/', gsub(pattern=' ', replacement='_', county), '_', seasonList[2], '_season_2015.RData', sep=''))
  second_season <- clim_indexes; rm(clim_indexes)
  
  second_seasonWrap <- lapply(1:length(second_season), function(m){
    wrapClimInd <- data.frame(Average = as.numeric(colMeans(second_season[[m]][,4:which(names(first_season[[m]]) == "2005")], na.rm = T)),
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
  suppressMessages(library(gplots))
  my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
  corMat <- cor(wrapFutClimInd_county, method='spearman')
  diag(corMat) <- NA
  outDir <- paste('/mnt/workspace_cluster_12/Kenya_KACCAL/results/graphics/cluster_analysis/combined/trends_allYears/', gsub(pattern=' ', replacement='_', county), sep='')
  if(!dir.exists(outDir)){dir.create(outDir, recursive=TRUE)}
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.png', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''), height=8, width=8)
    heatmap.2(corMat, col=my_palette, density.info="none", trace="none", dendrogram="column", key.title='', key.xlab='Spearman correlation', margins=c(9,9))
    dev.off()
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_correlationMatrix.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_correlationMatrix.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_correlationMatrix.pdf', sep=''))
  }
  
  # Doing PCA using estimated slopes and doing hierarchical clustering on principal components
  # because our variables are correlated
  suppressMessages(library(FactoMineR))
  res_pca  <- FactoMineR::PCA(wrapFutClimInd_county, graph=FALSE)
  set.seed(1235)
  res_hcpc <- FactoMineR::HCPC(res_pca, nb.clust=-1, graph=FALSE) # Define number of SCENARIOS automatically
  
  # Description of variables within each cluster
  # res_hcpc$desc.var
  
  suppressMessages(library(factoextra))
  suppressMessages(library(corrplot))
  
  # Visualize eigenvalues/variances
  gg <- fviz_eig(res_pca, addlabels=TRUE, hjust = -0.3) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''), plot=gg, width=7, height=7, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_eigenValuesPCA.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_eigenValuesPCA.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_eigenValuesPCA.pdf', sep=''))
  }
  
  # Visualize variables with contributions. Use gradient color
  gg <- fviz_pca_var(res_pca, col.var="contrib")+ scale_color_gradient2(low="white", mid="blue", high="red", midpoint = 96) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_varMapContributionsPCA.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_varMapContributionsPCA.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_varMapContributionsPCA.pdf', sep=''))
  }
  
  # Quality of representation of each variable
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.png', sep=''))){
    pdf(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''), height=7, width=8)
    par(mfrow=c(1,3))
    corrplot(res_pca$var$cos2[,1:2], is.corr=FALSE) # Representation quality of each variable
    corrplot(res_pca$var$contrib[,1:2], is.corr=FALSE) # Contribution of each variable to dimension
    corrplot(res_pca$var$cor[,1:2], method="ellipse", is.corr=TRUE) # Correlation of each variable to dimension
    dev.off()
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_rQuality.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_rQuality.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_rQuality.pdf', sep=''))
  }
  
  # ENCONTRAR UNA FORMA ADECUADA DE MOSTRAR LOS CLUSTERS EN UN ESPACIO BIVARIADO $$$
  # Biplot of individuals and variables. Only variables are labelled
  gg <- fviz_pca_biplot(res_pca,  label="var", habillage=res_hcpc$data.clust$clust, addEllipses=TRUE, ellipse.level=0.95) + theme_bw()
  # gg <- gg + scale_color_manual(values=c("black", "red", "forestgreen"))
  # gg <- gg + scale_fill_manual(values=c("black", "red", "forestgreen")) + theme_bw()
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''), plot=gg, width=7.5, height=7, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_biplotClusterPCA.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_biplotClusterPCA.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_biplotClusterPCA.pdf', sep=''))
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
  df_cluster$Index <- factor(x=df_cluster$Index, levels=c('TMEAN', 'GDD_1', 'GDD_2', 'ND_t35', 'TOTRAIN', 'CDD', 'P5D', 'P_95', 'NDWS', 'SLGP', 'LGP'))
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_boxplot(aes(colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Change by 20 years')
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario.pdf', sep=''), plot=gg, width=8, height=9, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_trendsScenario.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_trendsScenario.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario.pdf', sep=''))
  }
  
  gg <- ggplot(df_cluster, aes(x=Scenario, y=Value)) + geom_jitter(aes(colour=Season)) + scale_color_manual(values=c('First'='darkgoldenrod3', 'Second'='turquoise3'))
  gg <- gg + geom_point(data=df_cluster[grep(pattern='Historical', x=df_cluster$RCP),], aes(x=Scenario, y=Value, colour=Season), size=4)
  gg <- gg + facet_wrap(~ Index, scales='free_y')
  gg <- gg + theme_bw() + geom_hline(yintercept=0) + ylab('Change by 20 years')
  if(length(unique(df_cluster$Scenario))==1){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
  }
  if(length(unique(df_cluster$Scenario))==2){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
  }
  if(length(unique(df_cluster$Scenario))==3){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
  }
  if(length(unique(df_cluster$Scenario))==4){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
    gg <- gg + annotate('rect', xmin=3.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='mediumpurple1')
  }
  if(length(unique(df_cluster$Scenario))==5){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
    gg <- gg + annotate('rect', xmin=3.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='mediumpurple1')
    gg <- gg + annotate('rect', xmin=4.5, xmax=5.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='rosybrown1')
  }
  if(length(unique(df_cluster$Scenario))==6){
    gg <- gg + annotate('rect', xmin=0.5, xmax=1.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='blue')
    gg <- gg + annotate('rect', xmin=1.5, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='coral1')
    gg <- gg + annotate('rect', xmin=2.5, xmax=3.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='forestgreen')
    gg <- gg + annotate('rect', xmin=3.5, xmax=4.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='mediumpurple1')
    gg <- gg + annotate('rect', xmin=4.5, xmax=5.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='rosybrown1')
    gg <- gg + annotate('rect', xmin=5.5, xmax=6.5, ymin=-Inf, ymax=Inf, alpha=0.1, fill='lightgreen')
  }
  # gg <- gg + geom_point(df_cluster)
  if(!file.exists(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario2.png', sep=''))){
    ggsave(filename=paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario2.pdf', sep=''), plot=gg, width=9, height=10, units='in')
    system(paste("convert -verbose -density 300 ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_trendsScenario2.pdf -quality 100 -sharpen 0x1.0 -alpha off ", outDir, '/', gsub(pattern=' ', replacement='_', county), "_trendsScenario2.png", sep=""), wait=TRUE)
    file.remove(paste(outDir, '/', gsub(pattern=' ', replacement='_', county), '_trendsScenario2.pdf', sep=''))
  }
  
  return(cat('Process done.\n'))
  
}
