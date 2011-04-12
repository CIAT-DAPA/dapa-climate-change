ccafs.make.ref <- function(cdata=cdata, params=params, new.x=NA, new.y=NA, new.growing.season=NA) {
  
  # get grass params
  grass.params <- params$grass.params
  
  # overwrite default x and y
  
  if (!is.na(new.x) & !is.na(new.y)) {
   params$x <- new.x
   params$y <- new.y
  }
 
  if (!is.na(new.growing.season)) {
   params$growing.season <- new.growing.season
  }
  
  # f_ref: list to store the futur reference 
  f_ref <- list()
  
  # extract reference value for current
  for (var in params$vars) {
    cat(str_c("extracting reference of current conditions for: ",var,"\n"))
    f_ref[[str_c("current_",var)]] <- with(params,as.vector(extract(cdata[[str_c(var,"_b")]],cbind(x,y))))
  }
  
  # extract reference for the futur, if model direction is backwrd
  if (params$direction=="backwd"){
   
   # do it for all desired scenarios
   for (gcm in params$gcms) {
    
    # assuming connection to grass remains open (needs double checking)
    if (params$use.grass==T) {
      for (var in params$vars)
      {
        cat(str_c("extracting reference of future (",gcm,") conditions for: ",var,"\n"))
        q_str <- with(params, str_c(scenario,"_",year,"s_",gcms,"_",var,"_",growing.season,"_10min", collapse=",")) # correct naming needed
        q_res <- execGRASS("r.what", parameters=list(input=q_str,east_north=with(params,c(x,y)),fs=","),intern=T)
        f_ref[[str_c(gcm,"_",params$year,"_",var)]] <- str_split(q_res,",")[[1]][4:(length(params$growing.season)+3)]
      }
   } else {
      for (var in params$vars)
      {
        cat(str_c("extracting reference of future (",gcm,") conditions for: ",var,"\n"))
        f_ref[[str_c(gcm,"_",var)]] <- with(params,extract(cdata[[str_c(var,"_",gcm)]],cbind(x,y)))
      }
    }    
   }
  }
  
    return(f_ref)
}