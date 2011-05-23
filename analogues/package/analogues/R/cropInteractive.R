#' Crop raster to a region of intrest
#'
#' @param rast RasterLayer or RasterStack
#' @return raster
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

cropInteractive  <- function(r) {
  dev.new()
  plot(r)
  map("world", add=T)
  cat("select region of interest on the plot now, by clicking two corners of a bbx \n")
  nextent <- drawExtent()
  dev.off()
  return(crop(r,nextent))
}

#cropInteractive <-
#function(object, ...) {
#   UseMethod("cropInteractive")
#}

#' Extract reference from weights grid
#'
#' @param params an object of the class AnalogueParameters
#' @param weights a list with all the weights
#' @return an object of AnalogueResults
#’ @method cropInteractie RasterLayer
#’ @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )



#' Extract reference from weights grid
#'
#' @param params an object of the class AnalogueParameters
#' @param weights a list with all the weights
#' @return an object of AnalogueResults
#’ @method cropInteractie RasterStack
#’ @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )


#cropInteractive.RasterStack  <- function(r) {
#  dev.new()
#  plot(r[[1]])
#  map("world", add=T)
#  cat("select region of interest on the plot now, by clicking two corners of a bbx \n")
#  nextent <- drawExtent()
#  dev.off()
#  return(crop(r,nextent))
#}