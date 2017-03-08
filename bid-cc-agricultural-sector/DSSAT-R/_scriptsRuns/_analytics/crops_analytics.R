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

# Bean historical final
# crop <- cropList[1]; time <- timeList[1]; run <- run_type[2]

for(crop in cropList){
  
  for(time in timeList){
    
    for(run in run_type){
      
      # Load all information per crop, time and run type
      info_dir <- paste(wk_dir, "/", crop, "/", time, "/", run, sep = "")
      if(dir.exists(info_dir)){
        
        # Put all together, even GCMs
        if(!file.exists(paste(info_dir, "/", crop, "_all_", time, "_", run,".RDS", sep = ""))){
          
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
          info_crop$Year <- as.numeric(substr(info_crop$SDAT, start = 1, stop = 4))
          
        } else {
          # Load data and extract year
          info_crop <- readRDS(file = paste(info_dir, "/", crop, "_all_", time, "_", run,".RDS", sep = ""))
          info_crop$Year <- as.numeric(substr(info_crop$SDAT, start = 1, stop = 4))
        }
        
        # Graphics directory
        gr_dir <- paste(wk_dir, "/_diagnostic_plots/", crop, "/", time, sep = "")
        if(!dir.exists(gr_dir)){dir.create(gr_dir, recursive = T)}
        
        # Do graphics to explore values and trends
        if(time == "historical"){
          
          # Dots plot
          if(!file.exists(paste(gr_dir, "/", crop, "_dots.png", sep = ""))){
            gg <- info_crop %>% ggplot(aes(x = as.numeric(Pixel), y = as.numeric(HWAH), group = as.factor(Year), colour = System)) + geom_point(alpha = I(1/sqrt(1000)))
            gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
            ggsave(filename = paste(gr_dir, "/", crop, "_dots.png", sep = ""), plot = gg, width = 10, height = 6, units = 'in')
          }
          
          # Quantile plot
          if(!file.exists(paste(gr_dir, "/", crop, "_quantile.png", sep = ""))){
            quantileFun <- function(x){z <- stats::quantile(x, probs=0.05); return(z)}
            aux <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, Cultivar, System), quantileFun(HWAH)))
            names(aux)[4] <- "p05" # Percentile 5%
            quantileFun <- function(x){z <- stats::quantile(x, probs=0.50); return(z)}
            aux1 <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, Cultivar, System), quantileFun(HWAH)))
            names(aux1)[4] <- "p50" # Percentile 50%
            quantileFun <- function(x){z <- stats::quantile(x, probs=0.95); return(z)}
            aux2 <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, Cultivar, System), quantileFun(HWAH)))
            names(aux2)[4] <- "p95" # Percentile 95%
            aux <- merge(aux, aux1, by = c("Pixel", "Cultivar", "System")); rm(aux1)
            aux <- merge(aux, aux2, by = c("Pixel", "Cultivar", "System")); rm(aux2)
            gg <- aux %>% ggplot(aes(x = Pixel, y = p50, ymin = p05, ymax = p95, colour = System))
            gg <- gg + geom_linerange(position = position_dodge(width = c(0.6, 0.4)), size = 1, alpha = 0.3)
            gg <- gg + geom_point(aes(color = System, shape = System), position = position_dodge(width = c(0.6, 0.4)), size = 3, alpha = 0.3)
            gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
            ggsave(filename = paste(gr_dir, "/", crop, "_quantile.png", sep = ""), plot = gg, width = 10, height = 6, units = 'in')
            rm(aux, quantileFun)
          }
          
          # Trends plot
          if(!file.exists(paste(gr_dir, "/", crop, "_trends.png", sep = ""))){
            gg <- info_crop %>% ggplot(aes(x = as.numeric(Year), y = as.numeric(HWAH), colour = System))
            gg <- gg + stat_summary(aes(colour = System), fun.y = median, geom = "line")
            gg <- gg + geom_line(aes(group = interaction(Pixel, GCM, System)), alpha = I(1/sqrt(1000)))
            gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Year') + ylab('Yield (kg/ha)')
            gg <- gg + guides(group = FALSE)
            ggsave(filename = paste(gr_dir, "/", crop, "_trends.png", sep = ""), plot = gg, width = 10, height = 6, units = 'in')
          }
          
        } else {
          if(time == "future"){
            
            # Dots plot
            if(!file.exists(paste(gr_dir, "/", crop, "_dots.png", sep = ""))){
              gg <- info_crop %>% ggplot(aes(x = as.numeric(Pixel), y = as.numeric(HWAH), group = as.factor(Year), colour = System)) + geom_point(alpha = I(1/sqrt(1000)))
              gg <- gg + facet_grid(GCM ~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
              ggsave(filename = paste(gr_dir, "/", crop, "_dots.png", sep = ""), plot = gg, width = 15, height = 10, units = 'in')
            }
            
            # Quantile plot
            if(!file.exists(paste(gr_dir, "/", crop, "_quantile.png", sep = ""))){
              quantileFun <- function(x){z <- stats::quantile(x, probs=0.05); return(z)}
              aux <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, GCM, Cultivar, System), quantileFun(HWAH)))
              names(aux)[5] <- "p05" # Percentile 5%
              quantileFun <- function(x){z <- stats::quantile(x, probs=0.50); return(z)}
              aux1 <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, GCM, Cultivar, System), quantileFun(HWAH)))
              names(aux1)[5] <- "p50" # Percentile 50%
              quantileFun <- function(x){z <- stats::quantile(x, probs=0.95); return(z)}
              aux2 <- as.data.frame(dplyr::summarise(group_by(info_crop, Pixel, GCM, Cultivar, System), quantileFun(HWAH)))
              names(aux2)[5] <- "p95" # Percentile 95%
              aux <- merge(aux, aux1, by = c("Pixel", "GCM", "Cultivar", "System")); rm(aux1)
              aux <- merge(aux, aux2, by = c("Pixel", "GCM", "Cultivar", "System")); rm(aux2)
              gg <- aux %>% ggplot(aes(x = Pixel, y = p50, ymin = p05, ymax = p95, colour = System))
              gg <- gg + geom_linerange(position = position_dodge(width = c(0.6, 0.4)), size = 1, alpha = 0.3)
              gg <- gg + geom_point(aes(color = System, shape = System), position = position_dodge(width = c(0.6, 0.4)), size = 3, alpha = 0.3)
              gg <- gg + facet_grid(GCM ~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
              ggsave(filename = paste(gr_dir, "/", crop, "_quantile.png", sep = ""), plot = gg, width = 15, height = 10, units = 'in')
              rm(aux, quantileFun)
            }
            
            # Trends plot
            if(!file.exists(paste(gr_dir, "/", crop, "_trends.png", sep = ""))){
              gg <- info_crop %>% ggplot(aes(x = as.numeric(Year), y = as.numeric(HWAH), colour = System))
              gg <- gg + stat_summary(aes(colour = System), fun.y = median, geom = "line")
              gg <- gg + geom_line(aes(group = interaction(Pixel, GCM, System)), alpha = I(1/sqrt(1000)))
              gg <- gg + facet_grid(GCM ~ Cultivar) + theme_bw() + xlab('Year') + ylab('Yield (kg/ha)')
              gg <- gg + guides(group = FALSE)
              ggsave(filename = paste(gr_dir, "/", crop, "_trends.png", sep = ""), plot = gg, width = 15, height = 10, units = 'in')
            }
            
          }
        }
        
      } else {
        cat(paste("Combination: ", crop, ", ", time, " and ", run, " does not exist. Continue with next combinations\n", sep = ""))
      }
      
    }
    
  }
  
}
