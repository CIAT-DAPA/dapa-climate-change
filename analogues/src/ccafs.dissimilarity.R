ccafs.dissimilarity <- function(cdata=cdata, params=params, new.direction=NA) {
  
  # overwrite direction
  if (!is.na(new.direction)) {
   params$direction <- new.direction
  
  }
  
  # create roll
  roll.v <- c()
  months <- params$growing.season
  for (i in 1:length(months)) roll.v <- c(roll.v,(months[c(i:length(months),0:(i-1))]))  
  roll <- matrix(data=roll.v,ncol=length(months),byrow=T)
  
  if (params$direction=="backwd" | params$direction=="backward")
  {
    results <- list()
  
    # get values baseline
    base.v <- list()
    base.v[['tmp_b.v']] <- getValues(cdata$tmean_b)
    base.v[['pre_b.v']] <- getValues(cdata$prec_b)
    base.v[['dtr_b.v']] <- getValues(cdata$dtr_b)
  
    for (gcm in params$gcms)
    {
      # create result 
      cat(str_c("calculating dissimilarity for: ",gcm,"\n"))
      res_all <- apply(roll, 1, function(x) ccafs.function(x,params,base.v,gcm))
      
      res_sum <- apply(res_all, 1, function(x) {
        if (sum(is.na(x))==length(months)) {
          return(c(NA, NA)) }
        else {
          r1 <- which.min(x)
          r2 <- x[r1]
          return(c(r1, r2)) } } )
      
      results[[gcm]]<- setValues(cdata$tmean_b[[1]],res_sum[2,]) 
    }
  } else if (params$direction=="forwd" | params$direction=="forward"){
    
    results <- list()
    base.v <- list()
    
    for (gcm in params$gcms) {
      
      # get base value, but this time for every scenario in the futur
      
      cat(str_c("converting raster to matrix for ", gcm,"\n"))
      base.v[['tmp_b.v']] <- getValues(cdata[[str_c("tmean_",gcm)]])
      base.v[['pre_b.v']] <- getValues(cdata[[str_c("prec_",gcm)]])
      base.v[['dtr_b.v']] <- getValues(cdata[[str_c("dtr_",gcm)]])
      
      cat(str_c("calculating dissimilarity for: ",gcm,"\n"))
      res_all <- apply(roll, 1, function(x) ccafs.function(x,params,base.v,"current"))
      
      res_sum <- apply(res_all, 1, function(x) {
        if (sum(is.na(x))==length(months)) {
          return(c(NA, NA)) }
        else {
          r1 <- which.min(x)
          r2 <- x[r1]
          return(c(r1, r2)) } } )
      
      results[[gcm]]<- setValues(cdata$tmean_b[[1]],res_sum[2,]) 
    }
    
  } else if (params$direction=="current") {
    
    # create result 
    cat("calculating dissimilarity for: current \n")
    res_all <- apply(roll, 1, function(x) ccafs.function(x,params,base.v,"current"))
    
    res_sum <- apply(res_all, 1, function(x) {
      if (sum(is.na(x))==length(months)) {
        return(c(NA, NA)) }
      else {
        r1 <- which.min(x)
        r2 <- x[r1]
        return(c(r1, r2)) } } )
    
    results[[gcm]]<- setValues(cdata$tmean_b[[1]],res_sum[2,])
    
  } else { stop("no directions was chosen") }
  
  results <- stack(results)
  
  return(results)
}