#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

#Function to create a raster with a desired resolution from a shapefile

createMask <- function(shp, res) { #Function to create a mask
	xn <- shp@bbox[1,1] - 2*res
	xx <- shp@bbox[1,2] + 2*res
	yn <- shp@bbox[2,1] - 2*res
	yx <- shp@bbox[2,2] + 2*res
	nc <- round((xx - xn) / res); nr <- round((yx - yn) / res)
	xr <- (xx-xn)/nc; yr <- (yx-yn)/nr
	if (xr != yr) {yx <- xx-xn+yn} #readjust yx if horizontal and vertical grids are not equal
	rs <- raster(xmn=xn, xmx=xx, ymn=yn, ymx=yx, ncol=nc, nrow=nr); rs[] <- 1
	rs <- polygonsToRaster(shp, rs, silent=T, getCover=T)
	rs[which(rs[] == 0)] <- NA; rs[which(!is.na(rs[]))] <- 1
	return(rs)
}
