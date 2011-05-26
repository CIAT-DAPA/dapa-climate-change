#' Create a report
#'
#' @param results a list with rasters
#' @param do_mean calculate the mean over all rasters in the list
#' @param do_std calcualte the sd over all rasters in the list
#' @param do_cv calculate the coefficient of variance over all rasters in the list
#' @return prints a report and save it as pdf
#' @value a list
#' @export
#' @examples
#' summary(list(restuls[[1]], results[[3]], results[[2]]), TRUE,TRUE,TRUE)


summary <- function(results, do_mean=TRUE,do_std=TRUE,do_cv=TRUE) {
  
  # get predicted dissimilarities
  results_sum <- list()
  
  results <- do.call(stack, results)
        
  if (do_mean | do_cv) {
    cat("calculating mean dissimilarity \n")
    results_sum$mean <- stackApply(results, indices=rep(1,nlayers(results)),fun=mean)
  }
     
  if (do_std | do_cv) {
    cat("calculating sd of dissimilarity \n")
    results_sum$std <- stackApply(results,indices=rep(1,nlayers(results)),fun=sd)
  }
  
  if (do.cv) {
    cat("calculating coefficient of variance \n")
    results_sum$cv <- results_sum$std / results_sum$mean * 100
  }
  
  return(results_sum)
}
