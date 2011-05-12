# apply thresholds
ccafs.apply.threshold <- function(results,range=NA,best=NA){
  
  if (!is.na(range) & !is.na(best))
    exit("range and best are mutually exclusive, one needs to be NA")
  
  results.v <- getValues(results[[1]])
  
  if (!is.na(range))
    results.v <- ifelse(results.v >= range[1] & results.v <= range[2], results.v,NA)
    
  if (!is.na(best)) {
    probs <- quantile(results.v,probs=best,na.rm=T)
    for (i in 1:length(probs)) 
      results.v <- ifelse(results.v <= probs[i],best[i],results.v)
    results.v <- ifelse(results.v > probs[length(probs)],NA,results.v)
  }
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
