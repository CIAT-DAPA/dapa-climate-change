# Crop analytics
# Implemented by: H. Achicanoy
# CIAT, 2017

options(warn = -1); options(scipen = 999)

suppressMessages(if(!require(plyr)){install.packages('plyr'); library(plyr)} else {library(plyr)})
suppressMessages(if(!require(ggplot2)){install.packages('ggplot2'); library(ggplot2)} else {library(ggplot2)})
suppressMessages(if(!require(dplyr)){install.packages('dplyr'); library(dplyr)} else {library(dplyr)})

# ------------------------------------------------------------------------------------------------------- #
# Comparing yield results
# ------------------------------------------------------------------------------------------------------- #

cropList <- c("Bean", "Maize", "Rice", "Soybean", "Wheat")
timeList <- c("historical", "future")
run_type <- c("diagnostic", "final")
systList <- c("rainfed", "irrigation")

wk_dir <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis"

for(crop in cropList){
  
  for(time in timeList){
    
    for(run in run_type){
      
      # Load all information per crop, time and run type
      info_dir <- paste(wk_dir, "/", crop, "/", time, "/", run, sep = "")
      if(dir.exists(info_dir)){
        
        if(!file.exists(paste(info_dir, "/", crop, "_all_", time, "_", run,".RDS", sep = ""))){
          
          crop_mgmt <- load("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/08-Cells_toRun/matrices_cultivo/version2017/Wheat_riego.Rdat")
          
          info_crop <- list.files(path = info_dir, pattern = "*.RDat$", full.names = T)
          cult_info <- list.files(path = info_dir, pattern = "*.RDat$", full.names = F)
          cult_info <- gsub(pattern = paste(toupper(crop), "_", sep = ""), replacement = "", x = cult_info)
          cult_info <- gsub(pattern = paste(".RDat", sep = ""), replacement = "", x = cult_info)
          cult_info <- strsplit(x = cult_info, split = "_")
          cult_info <- lapply(1:length(cult_info), function(i){ df <- data.frame(t(cult_info[[i]])); names(df) <- c("System", "Cultivar", "GCM"); return(df) })
          info_crop <- lapply(1:length(info_crop), function(i){
            load(info_crop[[i]])
            Run <- lapply(1:length(Run), function(k){df <- data.frame(Run[[k]]); df$Pixel <- k; return(df)})
            Run <- do.call(rbind, Run)
            Run$System <- cult_info[[i]]$System
            Run$Cultivar <- cult_info[[i]]$Cultivar
            Run$GCM <- cult_info[[i]]$GCM
            return(Run)
          })
          info_crop <- do.call(plyr::rbind.fill, info_crop)
          saveRDS(object = info_crop, file = paste(info_dir, "/", crop, "_all_", time, "_", run,".RDS", sep = ""))
          
        } else {
          info_crop <- readRDS(file = paste(info_dir, "/", crop, "_all_", time, "_", run,".RDS", sep = ""))
          info_crop$Year <- as.numeric(substr(info_crop$SDAT, start = 1, stop = 4))
        }
        
        # Do graphics to explore values and trends
        # ------------------------------------------------------------------------------------------------------- #
        # Dots plot
        # ------------------------------------------------------------------------------------------------------- #
        gg <- info_crop %>% ggplot(aes(x = as.numeric(Pixel), y = as.numeric(HWAH), group = as.factor(Year), colour = System)) + geom_point()
        gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
        ggsave(filename = 'Wheat_diagnostic_vs_final_ggcmi.png', plot = gg, width = 15, height = 10, units = 'in')
        
        # ------------------------------------------------------------------------------------------------------- #
        # Quantile plot
        # ------------------------------------------------------------------------------------------------------- #
        quantileFun <- function(x){z <- stats::quantile(x, probs=0.05); return(z)}
        aux <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, Cultivar, System), quantileFun(HWAH)))
        names(aux)[4] <- "p05"
        
        quantileFun <- function(x){z <- stats::quantile(x, probs=0.50); return(z)}
        aux1 <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, Cultivar, System), quantileFun(HWAH)))
        names(aux1)[4] <- "p50"
        
        quantileFun <- function(x){z <- stats::quantile(x, probs=0.95); return(z)}
        aux2 <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, Cultivar, System), quantileFun(HWAH)))
        names(aux2)[4] <- "p95"
        
        aux <- merge(aux, aux1, by = c("Pixel", "Cultivar", "System")); rm(aux1)
        aux <- merge(aux, aux2, by = c("Pixel", "Cultivar", "System")); rm(aux2)
        
        gg <- aux %>% ggplot(aes(x = Pixel, y = p50, ymin = p05, ymax = p95, colour = System))
        gg <- gg + geom_linerange(position = position_dodge(width = c(0.6, 0.4)), size = 1, alpha = 0.3)
        gg <- gg + geom_point(aes(color = System, shape = System), position = position_dodge(width = c(0.6, 0.4)), size = 3, alpha = 0.3)
        gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
        ggsave(filename = 'Wheat_diagnostic_vs_final_90interval_ggcmi.png', plot = gg, width = 15, height = 10, units = 'in')
        
        
      }
      
    }
    
  }
  
}



# Put all stuff together (run in Linux)

setwd('/home/jmesa/bid_reruns/')
run_type <- c('diagnostic', 'final')
cultivar <- c("Seri82BA", "TajanBA", "DonErnestoBA", "Gerek79BA", "HalconsnaBA", "BrigadierBA")

library(dplyr)
library(plyr)

# Steps:

# 1. Load all data (for each cultivar and run_type [diagnostic, final])
# 2. Put all together in a data.frame (information by pixel, year, run type and cultivar)

Run <- lapply(1:length(run_type), function(i){
  
  Run <- lapply(1:length(cultivar), function(j){
    
    load(paste(run_type[i], '/WHEAT_irrigation_', cultivar[j], '_WFD.RDat', sep = ''))
    Run <- lapply(1:length(Run), function(k){df <- data.frame(Run[[k]]); df$Pixel <- k; return(df)})
    Run <- do.call(rbind, Run); Run$Cultivar <- cultivar[j]; Run$Run_type <- run_type[i]
    # Run$Napp.day <- NULL
    return(Run)
    
  })
  
  Run <- do.call(rbind, Run)
  return(Run)
  
})

Run <- do.call(plyr::rbind.fill, Run)
Run$Year <- as.numeric(substr(Run$SDAT, start = 1, stop = 4))

write.csv(Run, '/home/jmesa/wheat_irrigation_results_ggcmi.csv', row.names = F)
