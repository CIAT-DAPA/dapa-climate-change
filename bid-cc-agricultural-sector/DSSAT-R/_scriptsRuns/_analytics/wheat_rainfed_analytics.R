# Wheat rainfed analytics
# Implemented by: H. Achicanoy
# CIAT, 2017

# ------------------------------------------------------------------------------------------------------- #
# Comparing yield results DIAGNOSTIC vs FINAL
# ------------------------------------------------------------------------------------------------------- #

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
    
    load(paste(run_type[i], '/WHEAT_rainfed_', cultivar[j], '_WFD.RDat', sep = ''))
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

write.csv(Run, '/home/jmesa/wheat_rainfed_results_ggcmi.csv', row.names = F)

# 3. Explore differences produced by changes in second application date of fertilizer (run in Windows)

options(warn = -1); options(scipen = 999)
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

setwd("D:/ToBackup/Modelling/bid-cc-agricultural-sector/Results/final_analyses/Historical/Rainfed")
Run <- read.csv("wheat_rainfed_results_ggcmi.csv")

# ------------------------------------------------------------------------------------------------------- #
# Dots plot
# ------------------------------------------------------------------------------------------------------- #
gg <- Run %>% ggplot(aes(x = as.numeric(Pixel), y = as.numeric(HWAH), group = as.factor(Year), colour = Run_type)) + geom_point()
gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
ggsave(filename = 'Wheat_diagnostic_vs_final_ggcmi.png', plot = gg, width = 15, height = 10, units = 'in')

# ------------------------------------------------------------------------------------------------------- #
# Quantile plot
# ------------------------------------------------------------------------------------------------------- #
quantileFun <- function(x){z <- stats::quantile(x, probs=0.05); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(Run, Pixel, Cultivar, Run_type), quantileFun(HWAH)))
names(aux)[4] <- "p05"

quantileFun <- function(x){z <- stats::quantile(x, probs=0.50); return(z)}
aux1 <- as.data.frame(dplyr::summarise(group_by(Run, Pixel, Cultivar, Run_type), quantileFun(HWAH)))
names(aux1)[4] <- "p50"

quantileFun <- function(x){z <- stats::quantile(x, probs=0.90); return(z)}
aux2 <- as.data.frame(dplyr::summarise(group_by(Run, Pixel, Cultivar, Run_type), quantileFun(HWAH)))
names(aux2)[4] <- "p90"

aux <- merge(aux, aux1, by = c("Pixel", "Cultivar", "Run_type")); rm(aux1)
aux <- merge(aux, aux2, by = c("Pixel", "Cultivar", "Run_type")); rm(aux2)

gg <- aux %>% ggplot(aes(x = Pixel, y = p50, ymin = p05, ymax = p90, colour = Run_type))
gg <- gg + geom_linerange(position = position_dodge(width = c(0.6, 0.4)), size = 1, alpha = 0.5)
gg <- gg + geom_point(aes(color = Run_type, shape = Run_type), position = position_dodge(width = c(0.6, 0.4)), size = 3)
gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
ggsave(filename = 'Wheat_diagnostic_vs_final_90interval_ggcmi.png', plot = gg, width = 15, height = 10, units = 'in')

# ------------------------------------------------------------------------------------------------------- #
# Load crop management matrix
# ------------------------------------------------------------------------------------------------------- #
cultivar <- 1
cul_list <- data.frame(CID=1:6,dsid=c("IB0010","IB0013","IB0016","IB0028","IB0022","IB0026"),
                       culname=c("Seri82BA","TajanBA","DonErnestoBA","Gerek79BA","HalconsnaBA","BrigadierBA"))
run_type <- "final"
sys_type <- "secano"
path_functions <- "C:/Users/haachicanoy/Documents/GitHub/dapa-climate-change/bid-cc-agricultural-sector/DSSAT-R/"
path_project <- "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/"
load(paste0(path_project, "14-ObjectsR/Soil2.RData"))
rm(list=setdiff(ls(), c("values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile","xy_Ref",
                        "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", 
                        "Cod_Ref_and_Position", "profileMatrix","scenario","cul_list","cultivar","run_type","sys_type",
                        "modelos","gcm_i","cleanup_all")))
load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Wheat_",sys_type,".RDat"))
assign("crop_mgmt", get(paste("crop_",sys_type,sep="")))

rm(list=setdiff(ls(), "crop_mgmt"))

crop_mgmt$Pixel <- 1:nrow(crop_mgmt)

# ------------------------------------------------------------------------------------------------------- #
# MIRCA planting dates
# ------------------------------------------------------------------------------------------------------- #
library(rgdal)
latam <- readOGR("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/03-Map_LatinAmerica/Latino_America.shp", layer="Latino_America")

latam_map <- ggplot() + geom_polygon(data = latam, aes(x = long, y = lat, group = group), fill = "black", colour = "grey90", alpha = 1)
latam_map <- latam_map + theme_dark()
latam_map <- latam_map + geom_point(data = crop_mgmt, aes(x = x, y = y, color = as.factor(mirca.start)), alpha = 1, size = 3)
latam_map <- latam_map + xlab('Longitude') + ylab('Latitude') + guides(color = guide_legend(title = 'Mirca start'))
latam_map <- latam_map + coord_equal(ratio=1)
ggsave(filename = 'mirca_start.png', plot = latam_map, width = 10, height = 10, units = "in")

# ------------------------------------------------------------------------------------------------------- #
# Update planting dates using GGCMI data
# ------------------------------------------------------------------------------------------------------- #

# Load GGCMI data
library(ncdf4)
library(raster)

wd_dir <- 'D:/ToBackup/Modelling/bid-cc-agricultural-sector/GGCMI_data/GGCMI'
library(ncdf4)
library(raster)
ggcmi <- brick(paste(wd_dir, "/Wheat_ir_growing_season_dates_v1.25.nc4", sep = ""), varname="planting day")
ggcmi <- ggcmi[[1]]
ggcmi[which(ggcmi[] == -99)] <- NA

planting_dates <- raster::extract(x = ggcmi, y = crop_mgmt[, c('x', 'y')])

crop_mgmt2 <- crop_mgmt
crop_mgmt$mirca.start <- round(planting_dates, 0)

saveRDS(object = crop_mgmt, file = 'D:/ToBackup/Modelling/bid-cc-agricultural-sector/crop_mgmt.rds')

# library(class)
# 
# train <- crop_mgmt[!is.na(crop_mgmt$mirca.start), c("x", "y")]
# test <- crop_mgmt[is.na(crop_mgmt$mirca.start), c("x", "y")]
# 
# train_labels <- crop_mgmt$mirca.start[!is.na(crop_mgmt$mirca.start)]
# test_labels <- crop_mgmt$mirca.start[is.na(crop_mgmt$mirca.start)]
# 
# knn(train = train, test = test, cl = train_labels, k = 50)

library(rgdal)
latam <- readOGR("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/03-Map_LatinAmerica/Latino_America.shp", layer="Latino_America")

# ------------------------------------------------------------------------------------------------------- #
# GGCMI planting dates
# ------------------------------------------------------------------------------------------------------- #
latam_map <- ggplot() + geom_polygon(data = latam, aes(x = long, y = lat, group = group), fill = "black", colour = "grey90", alpha = 1)
latam_map <- latam_map + theme_dark()
latam_map <- latam_map + geom_point(data = crop_mgmt, aes(x = x, y = y, color = as.factor(mirca.start)), alpha = 1, size = 3)
latam_map <- latam_map + xlab('Longitude') + ylab('Latitude') + guides(color = guide_legend(title = 'GGCMI start'))
latam_map <- latam_map + coord_equal(ratio=1)
latam_map
ggsave(filename = 'ggcmi_start.png', plot = latam_map, width = 10, height = 10, units = "in")

# ------------------------------------------------------------------------------------------------------- #
# Low yield pixels 200-300
# ------------------------------------------------------------------------------------------------------- #
latam_map <- ggplot() + geom_polygon(data = latam, aes(x = long, y = lat, group = group), fill = "black", colour = "grey90", alpha = 1)
latam_map <- latam_map + theme_dark()
latam_map <- latam_map + geom_point(data = crop_mgmt[200:300,], aes(x = x, y = y, color = "red"), alpha = 1, size = 3)
latam_map <- latam_map + xlab('Longitude') + ylab('Latitude') + guides(color = guide_legend(title = 'Low yields'))
latam_map <- latam_map + coord_equal(ratio=1)
latam_map
ggsave(filename = 'low_yields.png', plot = latam_map, width = 10, height = 10, units = "in")

# ------------------------------------------------------------------------------------------------------- #
# Relation: MIRCA dates vs yield|temperature
# ------------------------------------------------------------------------------------------------------- #
crop_mgmt$Pixel <- 1:nrow(crop_mgmt)

seri82ba <- Run[Run$Cultivar == "Seri82BA" & Run$Run_type == "diagnostic",]; rownames(seri82ba) <- 1:nrow(seri82ba)
seri82ba <- merge(x = seri82ba, y = crop_mgmt, by = "Pixel")

png(filename = 'temperature_vs_yield.png', width = 1000, height = 600, units = 'px')
par(mfrow = c(1, 2))
plot(x = seri82ba$TMAXA, y = seri82ba$HWAH, ty = "p", pch = 20, col = 1, xlab = 'Maximum temperature', ylab = 'Yield (kg/ha)')
plot(x = seri82ba$TMINA, y = seri82ba$HWAH, ty = "p", pch = 20, col = 1, xlab = 'Minimum temperature', ylab = 'Yield (kg/ha)')
dev.off()

plot(x = seri82ba$y, y = seri82ba$TMAXA, ty = "p", pch = 20, col = 1, xlab = 'Latitude', ylab = 'Maximum temperature')

gg <- ggplot(data = seri82ba, aes(x = y, y = TMAXA, size = as.factor(mirca.start), colour = as.factor(mirca.start))) + geom_point(alpha = 0.7)
gg <- gg + theme_bw() + xlab('Latitude') + ylab('Maximum temperature') + scale_colour_brewer(palette = "Set1")
gg <- gg + guides(size = guide_legend(title = 'Mirca start date'))
gg <- gg + guides(colour = guide_legend(title = 'Mirca start date'))
ggsave(filename = 'latitude_vs_tmax_by_mircastart.png', plot = gg, width = 10, height = 10, units = "in")

# ------------------------------------------------------------------------------------------------------- #
# Comparison between MIRCA and GGCMI
# ------------------------------------------------------------------------------------------------------- #

g <- gc(); rm(list = ls())
options(warn = -1); options(scipen = 999)
library(plyr)
library(ggplot2)
library(dplyr)
library(tidyr)

wk_dir <- "D:/ToBackup/Modelling/bid-cc-agricultural-sector/Results/Historical"
setwd(wk_dir)
allResults <- lapply(list.files(getwd(), pattern = ".csv$"), function(x){
  df <- read.csv(x)
  df$Source <- gsub(pattern = ".csv", replacement = "", x = unlist(strsplit(x = basename(x), split = "_"))[4])
  return(df)
}); allResults <- do.call(rbind.fill, allResults)

df <- allResults[,c("Pixel", "Cultivar", "Run_type", "Year", "Source", "HWAH")]
df <- df %>% spread(Source, HWAH)

# Overall scatterplot
gg <- df %>% ggplot(aes(x = mirca, y = ggcmi)) + geom_point()
gg <- gg + theme_bw() + xlab("Mirca yields (kg/ha)") + ylab("GGCMI yields (kg/ha)")
gg <- gg + geom_abline(intercept = 0, slope = 1, colour = "red") + facet_wrap(~ Run_type)
ggsave(filename = 'Scatterplot_wheat_mirca_vs_ggcmi.png', plot = gg, width = 10, height = 5, units = 'in')

# Scatterplot for last year
gg <- df[df$Year == 1998 & df$Cultivar == "Seri82BA",] %>% ggplot(aes(x = mirca, y = ggcmi)) + geom_point(aes(colour = factor(Cultivar)))
gg <- gg + theme_bw() + xlab("Mirca yields (kg/ha)") + ylab("GGCMI yields (kg/ha)")
gg <- gg + geom_abline(intercept = 0, slope = 1, colour = "red") + facet_wrap(~ Run_type) + guides(colour = guide_legend(title = "Cultivar"))
ggsave(filename = 'Scatterplot_wheat_mirca_vs_ggcmi_last_year.png', plot = gg, width = 10, height = 5, units = 'in')

# Explore the cause of the problems
library(mgcv)
allResults$Source <- factor(allResults$Source)
allResults$Cultivar <- factor(allResults$Cultivar)
allResults$Run_type <- factor(allResults$Run_type)
gam_obj <- gam(HWAH ~ s(Pixel) + s(Year), family = gaussian(link = identity), data = allResults)

png(filename = 'importance_GAM.png', width = 1000, height = 600, units = 'px', pointsize = 30)
par(mfrow = c(1, 2))
plot(gam_obj)
dev.off()

library(randomForest)
set.seed(1235)
rf_obj <- randomForest(HWAH ~ Pixel + Cultivar + Run_type + Source, data=allResults[allResults$Year == 1998,], importance=TRUE, proximity=TRUE)
plot(rf_obj)

png(filename = 'importance_rf.png', width = 1000, height = 1000, units = 'px', pointsize = 30)
par(mfrow = c(2, 2))
partialPlot(rf_obj, allResults[allResults$Year == 1998, c("Pixel", "Cultivar", "Run_type", "Source", "HWAH")], x.var = "Pixel")
partialPlot(rf_obj, allResults[allResults$Year == 1998, c("Pixel", "Cultivar", "Run_type", "Source", "HWAH")], x.var = "Cultivar")
partialPlot(rf_obj, allResults[allResults$Year == 1998, c("Pixel", "Cultivar", "Run_type", "Source", "HWAH")], x.var = "Run_type")
partialPlot(rf_obj, allResults[allResults$Year == 1998, c("Pixel", "Cultivar", "Run_type", "Source", "HWAH")], x.var = "Source")
dev.off()

# ------------------------------------------------------------------------------------------------------- #
# Quantile plots
# ------------------------------------------------------------------------------------------------------- #
quantileFun <- function(x){z <- stats::quantile(x, probs=0.05); return(z)}
aux <- as.data.frame(dplyr::summarise(group_by(allResults, Pixel, Cultivar, Run_type, Source), quantileFun(HWAH)))
names(aux)[5] <- "p05"

quantileFun <- function(x){z <- stats::quantile(x, probs=0.50); return(z)}
aux1 <- as.data.frame(dplyr::summarise(group_by(allResults, Pixel, Cultivar, Run_type, Source), quantileFun(HWAH)))
names(aux1)[5] <- "p50"

quantileFun <- function(x){z <- stats::quantile(x, probs=0.90); return(z)}
aux2 <- as.data.frame(dplyr::summarise(group_by(allResults, Pixel, Cultivar, Run_type, Source), quantileFun(HWAH)))
names(aux2)[5] <- "p90"

aux <- merge(aux, aux1, by = c("Pixel", "Cultivar", "Run_type", "Source")); rm(aux1)
aux <- merge(aux, aux2, by = c("Pixel", "Cultivar", "Run_type", "Source")); rm(aux2)

# ------------------------------------------------------------------------------------------------------- #
# Diagnostic
# ------------------------------------------------------------------------------------------------------- #
gg <- aux[aux$Run_type == "diagnostic", ] %>% ggplot(aes(x = Pixel, y = p50, ymin = p05, ymax = p90, colour = Source))
gg <- gg + geom_linerange(position = position_dodge(width = c(0.6, 0.4)), size = 1, alpha = 0.5)
gg <- gg + geom_point(aes(color = Source, shape = Source), position = position_dodge(width = c(0.6, 0.4)), size = 3)
gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
ggsave(filename = 'Wheat_mirca_vs_ggcmi_90interval_diagnostic.png', plot = gg, width = 15, height = 10, units = 'in')

# ------------------------------------------------------------------------------------------------------- #
# Final
# ------------------------------------------------------------------------------------------------------- #
gg <- aux[aux$Run_type == "final", ] %>% ggplot(aes(x = Pixel, y = p50, ymin = p05, ymax = p90, colour = Source))
gg <- gg + geom_linerange(position = position_dodge(width = c(0.6, 0.4)), size = 1, alpha = 0.5)
gg <- gg + geom_point(aes(color = Source, shape = Source), position = position_dodge(width = c(0.6, 0.4)), size = 3)
gg <- gg + facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
ggsave(filename = 'Wheat_mirca_vs_ggcmi_90interval_final.png', plot = gg, width = 15, height = 10, units = 'in')

# ------------------------------------------------------------------------------------------------------- #
# Mean yield map
# ------------------------------------------------------------------------------------------------------- #

library(dplyr)
library(ncdf4)
library(raster)
library(rgdal)
library(ggplot2)
library(scales)

wd_dir <- 'D:/ToBackup/Modelling/bid-cc-agricultural-sector/GGCMI_data/GGCMI'
ggcmi <- brick(paste(wd_dir, "/Wheat_ir_growing_season_dates_v1.25.nc4", sep = ""), varname="planting day")
ggcmi <- ggcmi[[1]]; rm(wd_dir)

latam <- readOGR("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/03-Map_LatinAmerica/Latino_America.shp", layer="Latino_America")
ggcmi <- raster::crop(x = ggcmi, y = extent(latam))
ggcmi[] <- NA

Run <- read.csv("D:/ToBackup/Modelling/bid-cc-agricultural-sector/Results/Historical/Rainfed/wheat_rainfed_results_ggcmi.csv")
Yld <- as.data.frame(dplyr::summarise(group_by(Run, Pixel, Cultivar, Run_type), mean(HWAH)))
names(Yld)[4] <- "Mean_yield"

Yld <- merge(x = Yld, y = crop_mgmt, by = "Pixel")

cultivarList <- c("Seri82BA", "TajanBA", "DonErnestoBA", "Gerek79BA", "HalconsnaBA", "BrigadierBA")
run_typeList <- c("diagnostic", "final")
lapply(1:length(cultivarList), function(i){
  
  lapply(1:length(run_typeList), function(j){
    
    cells <- cellFromXY(object = ggcmi, xy = cbind(Yld$x[Yld$Cultivar == cultivarList[i] & Yld$Run_type == run_typeList[j]], Yld$y[Yld$Cultivar == cultivarList[i] & Yld$Run_type == run_typeList[j]]))
    ggcmi[cells] <- Yld$Mean_yield[Yld$Cultivar == cultivarList[i] & Yld$Run_type == run_typeList[j]]
    
    ggcmi.p <- data.frame(rasterToPoints(ggcmi))
    
    latam_map <- ggplot() + geom_polygon(data = latam, aes(x = long, y = lat, group = group), fill = "black", colour = "grey90", alpha = 1)
    latam_map <- latam_map + theme_dark()
    latam_map <- latam_map + geom_raster(data = ggcmi.p, aes(x = x, y = y, fill = layer))
    latam_map <- latam_map + coord_equal(ratio = 1) + scale_fill_gradientn(colours = c("darkgoldenrod4", "yellow", "forestgreen"), values = rescale(c(0, 5000, 10000, 15000, 20000)))
    latam_map <- latam_map + guides(fill = guide_legend(title = "Yield (kg/ha)"))
    ggsave(filename = paste("mean_yield_map_", cultivarList[i], "_", run_typeList[j], ".png", sep = ""), plot = latam_map, width = 10, height = 10, units = "in")
    
  })
  
})
