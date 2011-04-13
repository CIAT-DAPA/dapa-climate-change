#Julian Ramirez-Villegas
#University of Leeds
#October 22, 2010

#Function to create a raster with a desired resolution from a shapefile

createMask <- function(shp, resol) { #Function to create a mask
  #Original bounding box
	xn <- shp@bbox[1,1] - 2*resol
	xx <- shp@bbox[1,2] + 2*resol
	yn <- shp@bbox[2,1] - 2*resol
	yx <- shp@bbox[2,2] + 2*resol
  
  #Modified bounding box to match resolution
  disx <- round(xx - xn)
  nc <- round(disx/resol)
  disx <- nc*resol
  xx <- xn + disx
  
  disy <- round(yx-yn)
  nr <- round(disy/resol)
  disy <- nr*resol
  yx <- yn + disy
  
  #Create and rasterize the shapefile
	rs <- raster(xmn=xn, xmx=xx, ymn=yn, ymx=yx, ncol=nc, nrow=nr); rs[] <- 1
	rs <- rasterize(shp, rs, silent=T, getCover=T)
	rs[which(rs[] == 0)] <- NA; rs[which(!is.na(rs[]))] <- 1
	return(rs)
}
