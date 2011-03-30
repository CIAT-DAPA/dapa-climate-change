#Julian Ramirez
#CIAT / University of Leeds

#Calculates a station density map based on a set of stations at 30arc-second spatial resolution

require(raster)
require(foreign)
rm(list=ls());gc(T);gc()

stDir <- "F:/PhD-work/climate-data-assessment/wcl-uncertainties/input-data"
rDir <- "F:/PhD-work/climate-data-assessment/wcl-uncertainties/mask-srtm/ETH"

vn <- "rain"
st <- read.dbf(paste(stDir, "/wc_", vn, "_stations.dbf", sep=""))

rs <- raster(paste(rDir, "/altitude.asc", sep=""))
st <- st[which(st$LONG >= rs@extent@xmin & st$LONG <= rs@extent@xmax & st$LAT >= rs@extent@ymin & st$LAT <= rs@extent@ymax),]

plot(rs)
points(st$LONG, st$LAT, col="red", pch=20, cex=0.5)

xy <- xyFromCell(rs, which(!is.na(rs[])



