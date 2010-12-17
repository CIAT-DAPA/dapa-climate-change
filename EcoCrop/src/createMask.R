#Julian Ramirez
#eejarv@leeds.ac.uk
#Dec 2010

#Script to create a mask raster from a shapefile

createMask <- function(shp, res) { #Function to create a mask from the shapefile
	xn <- shp@bbox[1,1] - 2*res; if (xn < -180) {xn <- -180}
	xx <- shp@bbox[1,2] + 2*res; if (xx > 180) {xx <- 180}
	yn <- shp@bbox[2,1] - 2*res; if (yn < -90) {yn <- -90}
	yx <- shp@bbox[2,2] + 2*res; if (yx > 90) {yx <- 90}
	nc <- round((xx - xn) / res); xx <- (nc * res) + xn #calculating and readjusting ncols and xmax
	nr <- round((yx - yn) / res); yx <- (nr * res) + yn #calculating and readjusting nrows and ymax
	rs <- raster(xmn=xn, xmx=xx, ymn=yn, ymx=yx, ncol=nc, nrow=nr); rs[] <- 1
	rs <- rasterize(shp, rs, silent=T, getCover=T) #getCover is TRUE to get the percent each cell is occupied by the polygon
	rs[which(rs[] == 0)] <- NA; rs[which(!is.na(rs[]))] <- 1
	return(rs)
}
