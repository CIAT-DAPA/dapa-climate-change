require(raster)
iDir <- "Y:/Product_6_resilient_coffee/02-monthly-interpolations/stations-averages/1976_1985"
fname <- "tmin_ris.csv"

st <- read.csv(paste0(iDir, "/", fname))

coords <- cbind(st$LONG, st$LAT)

srtm <- raster("S:/observed/gridded_products/srtm/Colombia_90m/srtm90m_v41")
alt <- extract(srtm, coords)
alt[is.na(alt)]  <- -9999.9

st_mod <- cbind.data.frame(st$ID, st$SOURCE, st$OLD_ID, st$NAME, st$COUNTRY, st$LONG, st$LAT, round(alt, digits = 1), st$JAN, st$FEB, st$MAR, st$APR, st$MAY, st$JUN, st$JUL, st$AUG, st$SEP, st$OCT, st$NOV, st$DEC, st$NYEARS)
colnames(st_mod) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")

write.csv(st_mod, paste0(iDir, "/", fname), row.names=F)

