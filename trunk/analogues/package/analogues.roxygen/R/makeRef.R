#' Extract reference values from rasters
#'
#' @param params an object of the class AnalogueParameters
#' @param ... an object of the class AnalogueTraining, AnalogueTraining
#' @return an object of AnalogueResults
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

makeRef <-
function(object, ...) {
   UseMethod("makeRef")
}

#' Extract reference from data grid
#'
#' @param params an object of the class AnalogueParameters
#' @param weights a list with all the weights
#' @return an object of AnalogueResults
#’ @method makeRef ATraining
#’ @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

makeRef.ATraining <- function(data,params) {
  refs <- lapply(data,function(x){ 
      cat("extracting ref values ... \n")
      as.vector(extract(x,cbind(params$x,params$y)))})
  return(refs)
}



#' Extract reference from weights grid
#'
#' @param params an object of the class AnalogueParameters
#' @param weights a list with all the weights
#' @return an object of AnalogueResults
#’ @method makeRef AnalogueWeights
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

makeRef.AnalogueWeights <- function(data, params) {
 f_ref <- lapply(data,function(x) {
  cat("extracting ref values ... \n")
  if (class(x) == "RasterStack" | class(x) == "RasterLayer"){
    return(as.vector(extract(x,cbind(params$x,params$y))))
  } else {
    return(rep(1,params$ndivisions))
  }
  
 })
}
