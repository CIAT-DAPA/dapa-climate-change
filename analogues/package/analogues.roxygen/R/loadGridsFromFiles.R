#' Load seperate grids 
#'
#' This functions seperate grids and returns a raster stack
#'
#' @param params an object of the class AnalogueParameters
#' @param path path to the file
#' @keywords manip
#' @return an object of class RasterStack
#' @export


loadGridsFromFiles <-
function(path,params) {

  
  if(!params$normalise) {
    cat(str_c("loading ",path," \n"))
    grid <- do.call(stack,lapply(str_c(path, 1:params$ndivisions, ".",params$ext),raster))
  } else {
    cat(str_c("loading ",path," and normalising\n"))
    grid <- do.call(stack,lapply(str_c(path, 1:params$ndivisions, ".",params$ext),loadAndNormaliseGrids))
  }
  
  return(grid)
}

