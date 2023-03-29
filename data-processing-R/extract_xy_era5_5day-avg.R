library(stringr)
library(gtools)
coordinates <- cbind(-89.37278, 14.82056)  
siteId <- "camotan"
# coordinates <- cbind(-91.50278, 14.52833)
# siteId <- "mazatenango" 
# coordinates <- cbind(-91.82333, 14.96715)
# siteId <- "sanmarcos"

iDirTc <- "U:/observed/gridded_products/era5/sis-agromet/nc/precipitation_flux"
yi <- 2012
yf <- 2022

## List files by years
dtsLsC <-  list.files(path=iDirTc, pattern=paste0("Precipitation-Flux_C3S-glob-agric.*.nc"),full.names = T,ignore.case=F)
prefix <- "Prec"
varLn <- "Precipitation"
unit <- "mm"

years <- paste(yi:yf, sep="", collapse="|")
dtsLsC_yrs <- dtsLsC[grepl(years,dtsLsC)]
dtsRs <- stack(dtsLsC_yrs)

## Extract values
value <- extract(dtsRs, coordinates)
value_t <- t(value)
write.table(cbind(basename(dtsLsC_yrs), value_t), paste0("D:/cenavarro/Workspace/maga/era5_daily_", siteId, "_", prefix, ".txt"), 
            sep="\t", row.names=T, quote=F)

## Calc pentads
txt <- read.table(paste0("D:/cenavarro/Workspace/maga/era5_daily_", siteId, "_", prefix, ".txt"))
date <- unlist(lapply(str_split(txt[,2], "_"), `[[`, 4))
prec_daily <- txt[,3]

prec_5dayavg <- running(prec_daily, width = 5, by = 5, fun = mean, trim = 0, na.rm = TRUE)

inidate_pentad <- date[seq(1, length(date), 5)]
enddate_pentad <- date[seq(5, length(date), 5)]

prec_daily_out <- cbind(date, prec_daily)
colnames(prec_daily_out) <- c("date", "prec")
write.csv(prec_daily_out, paste0("D:/cenavarro/Workspace/maga/era5_daily_", siteId, "_", prefix, ".csv"), 
          row.names = F, col.names = T)

prec_pentad_out <- cbind(paste0(inidate_pentad, "-", enddate_pentad), rep(1:73, length(yi:yf)), as.vector(prec_5dayavg))
colnames(prec_pentad_out) <- c("inidate-enddate", "pentad", "prec")

write.csv(prec_pentad_out, paste0("D:/cenavarro/Workspace/maga/era5_pentad_", siteId, "_", prefix, ".csv"), 
          row.names = F, col.names = T)