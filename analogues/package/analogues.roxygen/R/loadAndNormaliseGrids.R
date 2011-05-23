#' Load a raster an normalises it
#'
#' This functions loads raster grids
#' and normlizes them to a mean of 0 and sd of 1
#'
#' @param path to a raster
#' @return a RasterLayer
#' @export
#' @examples
#' r <- loadAndNormaliseGrids("weather.asc")


loadAndNormaliseGrids <-
function(x) {
    r <- raster(x)
    r <- (r - cellStats(r,mean))/cellStats(r,sd)
    return(r)
  }

