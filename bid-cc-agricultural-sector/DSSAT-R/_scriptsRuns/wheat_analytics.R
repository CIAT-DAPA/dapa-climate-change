
setwd('/home/jmesa/bid_reruns/')
run_type <- c('diagnostic', 'final')
cultivar <- c("Seri82BA", "TajanBA", "DonErnestoBA", "Gerek79BA", "HalconsnaBA", "BrigadierBA")

library(dplyr)

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
  
  Run <- do.call(rbind_all, Run)
  return(Run)
  
})

Run <- do.call(rbind_all, Run)
Run$Year <- as.numeric(substr(Run$SDAT, start = 1, stop = 4))

write.csv(Run, '/home/jmesa/wheat_results.csv', row.names = F)

# 3. Do some graphics that allow to figure out and explore differences produced by changes in second application date of fertilizer

options(warn = -1); options(scipen = 999)

library(ggplot2)
library(dplyr)
library(tidyr)

Run <- read.csv('wheat_results.csv')


Run %>% ggplot(aes(x = Year, y = HWAH, group = as.factor(Year), colour = as.factor(Pixel))) + geom_point() +
  facet_grid(Run_type ~ Cultivar) + theme_bw()

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
gg
ggsave(filename = 'Wheat_diagnostic_vs_final_90interval.pdf', plot = gg, width = 15, height = 10, units = 'in')
system(paste("convert -verbose -density 300 Wheat_diagnostic_vs_final_90interval.pdf -quality 100 -sharpen 0x1.0 -alpha off Wheat_diagnostic_vs_final_90interval.png", sep=""), wait=TRUE)

ggplot(dat, aes(x = Treatment, y = Meas, ymin = Meas - SD/2, ymax = Meas + SD/2)) +
  geom_linerange(aes(color = Temp), position=position_dodge(width=c(0.6,0.4)), size = 1, alpha = 0.5) +
  geom_point(aes(color = Temp, shape = Temp), position=position_dodge(width=c(0.6,0.4)), size = 3) +
  theme_bw()

plot(x = Run$Year[Run$Pixel == 1 & Run$Cultivar == "Seri82BA" & Run$Run_type == "diagnostic"],
     y = Run$HWAH[Run$Pixel == 1 & Run$Cultivar == "Seri82BA" & Run$Run_type == "diagnostic"], ty = 'l')

hist(Run$Year[Run$Pixel == 1 & Run$Cultivar == "Seri82BA" & Run$Run_type == "diagnostic"])
median(Run$Year[Run$Pixel == 1 & Run$Cultivar == "Seri82BA" & Run$Run_type == "diagnostic"])

# To fix
gg <- Run %>% ggplot(aes(x = as.numeric(Pixel), y = as.numeric(HWAH), group = as.factor(Year), colour = Run_type)) + geom_point() +
  facet_wrap(~ Cultivar) + theme_bw() + xlab('Pixel') + ylab('Yield (kg/ha)')
gg
ggsave(filename = 'Wheat_diagnostic_vs_final.pdf', plot = gg, width = 12, height = 8, units = 'in')
system(paste("convert -verbose -density 300 Wheat_diagnostic_vs_final.pdf -quality 100 -sharpen 0x1.0 -alpha off Wheat_diagnostic_vs_final.png", sep=""), wait=TRUE)

dat1<-cbind(dat,aux=rep(1,length(dat[,1]))) 
dat1<-within(dat1, {aux = unlist(by(aux,Treatment,cumsum))})
dat1$aux<-dat1$aux+as.numeric(dat1$Treatment)*10
ggplot(dat1, aes( x=aux, y = Meas, ymin = Meas - SD/2, ymax = Meas + SD/2)) +
  geom_linerange(aes(color = Temp), size = 1, alpha = 0.5) +geom_point(aes(color = Temp, shape = Temp))+
  scale_x_continuous("Treatment",breaks=c(13.5,23.5), labels=c("A","B")) + # here you define coordinates for A and B 
  theme_bw()



gg <- ggplot(YieldMatrix, aes(x = year, y = yield, group = run_type, colour = run_type)) + geom_line()
gg <- gg + xlab('Year') + ylab('Yield') + theme_bw()
gg


# --------------------------------------------------------------------------------------------------------------------- #
# Plotting Mirca dates
# --------------------------------------------------------------------------------------------------------------------- #
cultivar <- 1
cul_list <- data.frame(CID=1:6,dsid=c("IB0010","IB0013","IB0016","IB0028","IB0022","IB0026"),
                       culname=c("Seri82BA","TajanBA","DonErnestoBA","Gerek79BA","HalconsnaBA","BrigadierBA"))
run_type <- "final"
sys_type <- "riego"
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

# Load GGCMI data
library(ncdf4)
library(raster)

wd_dir <- 'D:/ToBackup/Modelling/bid-cc-agricultural-sector/GGCMI_data/GGCMI'
ggcmi <- raster::raster(paste(wd_dir, "/Wheat_ir_growing_season_dates_v1.25.nc4", sep = ""))
ggcmi[which(ggcmi[] == -99)] <- NA

ggcmi_dates <- raster::extract(x = ggcmi, y = crop_mgmt[, c('x', 'y')])
plot(x = crop_mgmt$mirca.start, y = ggcmi_dates, ty = "p", pch = 20, xlab = "Mirca start date", ylab = "GGCMI start date")
abline(0, 1)

crop_mgmt2 <- crop_mgmt
crop_mgmt$mirca.start <- ggcmi_dates

saveRDS(object = crop_mgmt, file = 'D:/ToBackup/Modelling/bid-cc-agricultural-sector/crop_mgmt.rds')


library(class)

train <- crop_mgmt[!is.na(crop_mgmt$mirca.start), c("x", "y")]
test <- crop_mgmt[is.na(crop_mgmt$mirca.start), c("x", "y")]

train_labels <- crop_mgmt$mirca.start[!is.na(crop_mgmt$mirca.start)]
test_labels <- crop_mgmt$mirca.start[is.na(crop_mgmt$mirca.start)]

knn(train = train, test = test, cl = train_labels, k = 50)

library(rgdal)
latam <- readOGR("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/03-Map_LatinAmerica/Latino_America.shp", layer="Latino_America")

# GGCMI start dates
latam_map <- ggplot() + geom_polygon(data = latam, aes(x = long, y = lat, group = group), fill = "black", colour = "grey90", alpha = 1)
latam_map <- latam_map + theme_dark()
latam_map <- latam_map + geom_point(data = crop_mgmt, aes(x = x, y = y, color = as.factor(mirca.start)), alpha = 1, size = 3)
latam_map <- latam_map + xlab('Longitude') + ylab('Latitude') + guides(color = guide_legend(title = 'Mirca start'))
latam_map <- latam_map + coord_equal(ratio=1)
latam_map
ggsave(filename = 'ggcmi_start.png', plot = latam_map, width = 10, height = 10, units = "in")

# MIRCA start dates
latam_map <- ggplot() + geom_polygon(data = latam, aes(x = long, y = lat, group = group), fill = "black", colour = "grey90", alpha = 1)
latam_map <- latam_map + theme_dark()
latam_map <- latam_map + geom_point(data = crop_mgmt, aes(x = x, y = y, color = as.factor(mirca.start)), alpha = 1, size = 3)
latam_map <- latam_map + xlab('Longitude') + ylab('Latitude') + guides(color = guide_legend(title = 'Mirca start'))
latam_map <- latam_map + coord_equal(ratio=1)
ggsave(filename = 'mirca_start.png', plot = latam_map, width = 10, height = 10, units = "in")

latam_map <- ggplot() + geom_polygon(data = latam, aes(x = long, y = lat, group = group), fill = "black", colour = "grey90", alpha = 1)
latam_map <- latam_map + theme_dark()
latam_map <- latam_map + geom_point(data = crop_mgmt, aes(x = x, y = y, color = as.factor(mirca.end)), alpha = 1, size = 3)
latam_map <- latam_map + xlab('Longitude') + ylab('Latitude') + guides(color = guide_legend(title = 'Mirca end'))
latam_map <- latam_map + coord_equal(ratio=1)
ggsave(filename = 'mirca_end.png', plot = latam_map, width = 10, height = 10, units = "in")

# Relation: MIRCA dates vs yield|temperature

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
