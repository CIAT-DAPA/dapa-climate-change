#Julian Ramirez
#eejarv@leeds.ac.uk
#Nov 2010

#Calculate suitability based on the assumption that the areas are splitted:
#This will use the suitability as calibrated with tmax (max. temp) and tmin (min. temp) to obtain a general suitability rating
#And another grid stating which suitability rating has been taken (0 for any, 1 for tmin, 2 for tmax, 3 for both)

require(rgdal)
require(raster)

suitMerge <- function(rn, rx) {
	rs <- raster(rn); pd <- raster(rn)
	rs[which(rx[] == rn[])] <- rn[which(rn[] == rx[])]; pd[which(rx[] == rn[])] <- 0
	rs[which(rx[] == 0 & rn[] != 0)] <- rn[which(rx[] == 0 & rn[] != 0)]; pd[which(rx[] == 0 & rn[] != 0)] <- 1
	rs[which(rx[] != 0 & rn[] == 0)] <- rx[which(rx[] != 0 & rn[] == 0)]; pd[which(rx[] != 0 & rn[] == 0)]  <- 2
	rs[which(rx[] != 0 & rn[] != 0)] <- (rn[which(rx[] != 0 & rn[] != 0)] * (rn[which(rx[] != 0 & rn[] != 0)] * 0.01)) + (rx[which(rx[] != 0 & rn[] != 0)] * (rx[which(rx[] != 0 & rn[] != 0)] * 0.01)); pd[which(rx[] != 0 & rn[] != 0)] <- 3
	rs[which(rs[] > 100)] <- 100
	return(stack(rs, pd))
}
par(mfrow=c(1,2))
plot(rs, col=colorRampPalette(c("yellow","red"))(100))
points(d$Longitude, d$Latitude, pch=20, cex=0.7, col="blue")
plot(pd)
points(d$Longitude, d$Latitude, pch=20, cex=0.7, col="blue")

