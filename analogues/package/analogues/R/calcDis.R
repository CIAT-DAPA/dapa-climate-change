#' Call the right funciton to calculate distance 
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

calcDis <-
function(roll, params,training,weights,base,project) {

  # for each combination (lag) loop calc dissimilartiy
  if (params$method=="ccafs") {
    
    if (length(roll) > 1)
      res_all <- apply(roll, 1, function(x) calcCcafs(x,params,training, weights,base,project))
    
    if (length(roll) <= 1)
       res_all <- calcCcafs(roll,params,training, weights,base,project)
    
    class(res_all) <- "CcafsResults"
  
  } else if (params$method=="hallegate" | params$method=="hal") {
    
    if (length(roll) > 1)
      res_all <- apply(roll,1,function(x) calcHal(x,params,training,weights, base,project))
    
    if (length(roll) <= 1)
      res_all <- calcHal(x,params,training,weights, base,project)
    
    class(res_all) <- "HalResults"
 
 } else {
     stop("sorry, i dont know the method you chose")
  }
 
  res_all <- summarizeResults(res_all,params)
  
  return(res_all)
}

