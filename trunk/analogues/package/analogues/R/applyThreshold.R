#' Apply thresholding to araster
#'
#' @param r RasterLayer or RasterStack
#' @param range if threshold should be applied to a range
#' @param best probablity values at which threshold should be cut
#' @return RasterLayer
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )


applyThreshold <- function(results,range=NA,best=NA){
  
  if (!is.na(range) & !is.na(best))
    exit("range and best are mutually exclusive, one needs to be NA")
  
  results.v <- results.n <- getValues(results)
  
  if (!is.na(range)) {
    results.n <- ifelse(results.v >= range[1] & results.v <= range[2], results.v,NA)
  }
  
  if (!is.na(best)) {
    best <- best[order(best)]
    probs <- quantile(results.v,probs=best,na.rm=T)
    results.n <- ifelse(results.v >= 0 & results.v <= probs[1], results.v,NA)
  }
  
  results <- setValues(results,results.n)
  
  return(results)
  
}

