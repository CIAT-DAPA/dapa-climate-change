# apply thresholds
ccafs.apply.threshold <- function(results,from=0,to=10){
  
  results.v <- getValues(results)
  results.v <- ifelse(results.v >= from & results.v <= to, results.v,NA)
  results <- setValues(results,results.v)
  
  return(results)
  
  # different methods for threshold could be implemented e.g. quartiles,
  
}

# function to summarize results

ccafs.summary <- function(results, do.mean=T,do.std=T,do.cv=T) {
  
  # get predicted dissimilarities
  results.sum <- list()
        
  if (do.mean) {
    cat("calculating mean dissimilarity \n")
    results.sum$mean <- stackApply(results, indices=rep(1,nlayers(results)),fun=mean)
  }
     
  if (do.std) {
    cat("calculating sd of dissimilarity \n")
    results.sum$std <- stackApply(results,indices=rep(1,nlayers(results)),fun=sd)
  }
  
  if (do.cv) {
    cat("calculating coefficient of variance \n")
    results.sum$cv <- results.sum$std / results.sum$mean * 100
  }
  
  return(results.sum)
}
