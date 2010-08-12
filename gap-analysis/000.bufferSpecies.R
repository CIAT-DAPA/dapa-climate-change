#Creates a buffer area around a set of sampling points (lat/lon). As it is from points, the areas are circular. You should define a buffer distance (bDist) in meters (do not worry whether your data is in lat/lon), the raster package will handle everything with transformations between these distances. 

#Resolution (resol) should be in degree. spFile is the input occurrences file (ID, Lon, Lat), and spOutFile is the name of your ASCIIGrid file (raster).

require(rgdal)
require(raster)

createBuffers <- function(spFile, spOutFile, bDist, msk) {
  
  rs <- raster(msk)
  #rs[which(!is.na(rs[]))] <- 1
  
	#bDist must be in meters (300000) for 300km
	
	if (file.exists(spFile)) {
		
		cat('Buffering...', "\n")
		
		if (!file.exists(spOutFile)) {
			spData <- read.csv(spFile)
      
			rsd <- distanceFromPoints(rs, spData[,2:3])
			
			rsdf <- rsd
			rsdf[which(rsdf[] > bDist)] <- 0
			rsdf[which(rsdf[] != 0)] <- 1
			
			dataType(rsdf) <- 'INT1U'
			
			rsdf <- writeRaster(rsdf, spOutFile, format='ascii', datatype='INT1U', overwrite=T)
			rm(rsd)
		} else {
			rsdf <- raster(spOutFile)
		}
	} else {
	 stop("The occurrence file does not exist")
	}
	return(rsdf)
}