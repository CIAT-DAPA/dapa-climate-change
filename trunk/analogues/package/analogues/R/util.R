makeRoll <- function(parmas) {
    
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
}



summarizeResults <- function(object, ...) {
  UseMethod("summarizeResults", object)
}

summarizeResults.CcafsResults <- function(res_all, params) {
  if (is.list(res_all)) {
    if (params$keep_lag & params$across_year) {
        # create stack with lagged 
        res_return <- do.call(stack,res_all)
    
    } else if (!params$keep_lag & params$across_year) {
        # take the minimum of each each month
        res_sum <- do.call(stack,res_all)
        res_return <- stackApply(res_sum,rep(1,nlayers(res_sum)),min)
    } else if(!params$across_year) {
        res_return <- res_all[[1]]
    }
  } else {
     if (params$keep_lag & params$across_year) {
        # create stack with lagged 
        res_return <- res_all
    
    } else if (!params$keep_lag & params$across_year) {
        # take the minimum of each each month
        res_return <- apply(res_all,1,min)
    } else if(!params$across_year) {
        res_return <- res_all
    }
  }
  return(res_return)
}

summarizeResults.HalResults <- function(res_all, params) {
  if (is.list(res_all)) {
    if (params$keep_lag & params$across_year) {
        # create stack with lagged 
        res_return <- do.call(stack,res_all)
    
    } else if (!params$keep_lag & params$across_year) {
        # take the minimum of each each month
        res_sum <- do.call(stack,res_all)
        res_return <- stackApply(res_sum,rep(1,nlayers(res_sum)),max)
    } else if(!params$across_year) {
        res_return <- res_all[[1]]
    }
  } else {
     if (params$keep_lag & params$across_year) {
        # create stack with lagged 
        res_return <- res_all
    
    } else if (!params$keep_lag & params$across_year) {
        # take the minimum of each each month
        res_return <- apply(res_all,1,function(x) any(x==TRUE))
    } else if(!params$across_year) {
        res_return <- res_all
    }
  }
  return(res_return)
}