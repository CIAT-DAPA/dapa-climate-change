#' Calculate dissimilarity
#'
#' This functions calculate dissimilarities
#'
#' @param params an object of the class AnalogueParameters
#' @param training an object of the class AnalogueTraining
#' @param weights an object of the class AnalogueWeiths
#' @keywords manip
#' @return an object of AnalogueResults
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )


dissimilarity <-
function(params, training, weights) {
  
  # create roll
  roll.v <- c()
  months <- 1:params$ndivisions
  for (i in 1:length(months)) {
    roll.v <- c(roll.v,(months[c(i:length(months),0:(i-1))]))  
  }
  roll <- matrix(data=roll.v, ncol=length(months), byrow=T)
  
  # cut roll to the actual growin period
  roll <- roll[ , params$growing_season]
  
  # only keep first row, if accross the years is false
  if (!params$across_year) roll <- roll[1, , drop=FALSE]
  
  
  if (params$direction=="backwd" | params$direction=="backward") {
    # projecting all future gcms back to the first one, which is current
    results <- lapply(2:length(params$gcms),function(x) calcDis(roll=roll,params=params,training,weights,base=x,project=1))

  } else if (params$direction=="forwd" | params$direction=="forward"){
    # projecting from the current (first grid) to all futur gcms 
    results <- lapply(2:length(params$gcms),function(x) calcDis(roll=roll,params=params,training,weights,base=1,project=x)) 
    
  } else if (params$direction=="current") {
    results <- calcDis(roll=roll,params=params,training,weights,base=1,project=1) 
    
  } else { 
    stop("no directions was chosen") 
  }
  
  # if there is only gcm and a current, unlis the the list
  if (length(params$gcms==2)) results <- unlist(results)
}

