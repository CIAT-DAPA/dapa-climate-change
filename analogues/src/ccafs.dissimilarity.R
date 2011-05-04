ccafs.dissimilarity <- function(cdata=cdata, params=params, new.direction=NA) {
  
  results <- list()
  base.v <- list()
  
  # overwrite direction
  if (!is.na(new.direction)) {
   params$direction <- new.direction
  }
  
  # create roll
  roll.v <- c()
  months <- 1:params$ndivisions
  for (i in 1:length(months)) roll.v <- c(roll.v,(months[c(i:length(months),0:(i-1))]))  
  roll <- matrix(data=roll.v,ncol=length(months),byrow=T)
  
  # cut roll to the actual growin period
  roll <- roll[,params$growing.season]
  
  if (!params$across.year) roll <- roll[1,,drop=FALSE]

  if (params$direction=="backwd" | params$direction=="backward") {
  
    # get values baseline
    for (i in params$vars)
      base.v[[str_c(i,"_b.v")]] <- getValues(cdata[[str_c(i,"_b")]])
  
    for (gcm in params$gcms)
    {
      # create result 
      cat(str_c("calculating dissimilarity for: ",gcm,"\n"))
      # for each combination (lag) loop calc dissimilartiy
      results[[gcm]] <- calc.dis(roll=roll,params=params,base.v=base.v,delta=gcm,cdata=cdata) # problem with no default value for delta
    
    }
  } else if (params$direction=="forwd" | params$direction=="forward"){
    
    for (gcm in params$gcms) {
      
      # get base value, but this time for every scenario in the futur  
      cat(str_c("converting raster to matrix for ", gcm,"\n"))
      
      for (i in params$vars)
        base.v[[str_c(i,"_b.v")]] <- getValues(cdata[[str_c(i,"_",gcm)]])
      
      #base.v[['tmp_b.v']] <- getValues(cdata[[str_c("tmean_",gcm)]])
      #base.v[['pre_b.v']] <- getValues(cdata[[str_c("prec_",gcm)]])
      #base.v[['dtr_b.v']] <- getValues(cdata[[str_c("dtr_",gcm)]])
      
      results[[gcm]] <- calc.dis(roll,params,base.v, delta=gcm,cdata)
    }
    
  } else if (params$direction=="current") {
    
    # get base values
    for (i in params$vars)
      base.v[[str_c(i,"_b.v")]] <- getValues(cdata[[str_c(i,"_b")]])
    
    #base.v[['tmp_b.v']] <- getValues(cdata$tmean_b)
    #base.v[['pre_b.v']] <- getValues(cdata$prec_b)
    #base.v[['dtr_b.v']] <- getValues(cdata$dtr_b)
   
    results[["current_dissimilarity"]] <- calc.dis(roll,params,base.v, delta="current",cdata)
    
  } else { stop("no directions was chosen") }
  
  return(results)
}
