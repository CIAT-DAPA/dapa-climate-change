#Julian Ramirez
#eejarv@leeds.ac.uk
#Nov 2010

#Calculate suitability based on the assumption that the areas are splitted:
#This will use the suitability as calibrated with tmax (max. temp) and tmin (min. temp) to obtain a general suitability rating
#And another grid stating which suitability rating has been taken (0 for any, 1 for tmin, 2 for tmax, 3 for both)

#if future=T then the rc must be that of the current conditions

require(rgdal)
require(raster)

suitMerge <- function(rn, rx, rc, future=F) {
	if (!future) {
		rs <- raster(rn); pd <- raster(rn)
		rs[which(rx[] == rn[])] <- rn[which(rn[] == rx[])]; pd[which(rx[] == rn[])] <- 0
		rs[which(rx[] == 0 & rn[] != 0)] <- rn[which(rx[] == 0 & rn[] != 0)]; pd[which(rx[] == 0 & rn[] != 0)] <- 1
		rs[which(rx[] != 0 & rn[] == 0)] <- rx[which(rx[] != 0 & rn[] == 0)]; pd[which(rx[] != 0 & rn[] == 0)]  <- 2
		rs[which(rx[] != 0 & rn[] != 0)] <- ((rn[which(rx[] != 0 & rn[] != 0)] * (rn[which(rx[] != 0 & rn[] != 0)] * 0.01)) + (rx[which(rx[] != 0 & rn[] != 0)] * (rx[which(rx[] != 0 & rn[] != 0)] * 0.01))) / ((rx[which(rx[] != 0 & rn[] != 0)] * 0.01) + (rn[which(rx[] != 0 & rn[] != 0)] * 0.01)); 	pd[which(rx[] != 0 & rn[] != 0)] <- 3
		rs[which(rs[] > 100)] <- 100
		return(stack(rs, pd))
	} else {
		rs <- raster(rn)
		rs[which(rc[] == 0)] <- rn[which(rc[] == 0)]
		rs[which(rc[] == 1)] <- rn[which(rc[] == 1)]
		rs[which(rc[] == 2)] <- rx[which(rc[] == 2)]
		rs[which(rc[] == 3)] <- ((rn[which(rc[] == 3)] * rn[which(rc[] == 3)] * 0.01) + (rx[which(rc[] == 3)] * rx[which(rc[] == 3)] * 0.01)) / ((rn[which(rc[] == 3)] * 0.01) + (rx[which(rc[] == 3)] * 0.01))
		rs[which(rs[] > 100)] <- 100
		return(rs)
	}
}
