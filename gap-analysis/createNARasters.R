require(raster)
require(maptools)
gpclibPermit()

source(zipWrite.R)

createNARaster <- function(spID, {
	pol <- readShapePoly(shpName)
	rs <- raster("mask.asc")

	pa <- polygonsToRaster(pol, rs)

	pa[which(!is.na(pa[]))] <- 1

	pa <- zipWrite(pa, outFolder, outName)
}
