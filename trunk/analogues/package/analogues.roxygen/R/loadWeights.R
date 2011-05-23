#' Load weights for grids
#'
#' This functions loads weight grids, if they exist. Otherwise
#' weights are set to 1 or the value specified.
#'
#' @param params an object of the class AnalogueParameters
#' @return an object of class AnalogueWeights
#' @export


loadWeights <-
function(params) {
  
  # list with the return values
  rweights <- list()
  
  # path to grids, only division (e.g. month is added later)
  paths <- with(params, as.vector(t(outer(str_c(climate_data,"/",gcms,"_"),str_c(weights,"_"), FUN="str_c"))))
  for (i in 1:length(paths)) {
    if (file.exists(str_c(paths[i],"1.asc"))) {
      rweights[[i]] <- loadGridsFromFiles(paths[i],params)
    } else {
      rweights[[i]] <- 1
    }
  }
 
 
  class(rweights) <- "AnalogueWeights"
  
  return(rweights)
}

