#' Load data for the dissimilarity calculations
#'
#' This functions loads the dissimilarity data.
#'
#' @param params an object of the class AnalogueParameters
#' @keywords manip
#' @return an object of AnalogueTraining
#' @export
#' @examples
#' ccafs_params <- createParams(x, z, )

loadData <- function(params) {

  # check if params if the right class

  if (class(params) != "AnalogueParameters")
     return("params must be of class AnalogueParameters")
  else {
  
  # list to hold all the data
  training <- list() 
  
  params$use.grass <- 'FASLE'

  if (params$use.grass==TRUE) {
    # here comes the grass connection 


  # --- from here code is up to date
  }else {
      
    # get paths for rasters, if there are gcms as well
    # add the ndivisions in the next (level loadGridsFromFiles function, so 
    # that its easy to build stacks
    
    paths <- with(params, as.vector(t(outer(str_c(climate_data,"/",gcms,"_"),str_c(vars,"_"), FUN="str_c")))) 
      
    # load data
    training <- lapply(paths, function(x) loadGridsFromFiles(x,params))
        
  }
  class(training) <- "AnalogueWeights"
  return(training)
}
}

