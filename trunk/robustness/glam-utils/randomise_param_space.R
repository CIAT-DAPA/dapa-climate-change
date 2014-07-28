#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014

#function to randomise (1) order of parameters, and (2) starting point of parameter space
randomise_param_space <- function(params, plist_in, seed) {
  #params <- this_params
  #plist_in <- param_orig
  #seed <- seed_list[1]
  
  #first randomise param list
  set.seed(seed)
  reord <- sample(1:nrow(plist_in),replace=F)
  plist_out <- plist_in[reord,]
  row.names(plist_out) <- 1:nrow(plist_out)
  
  #put starting value of parameter space into params
  for (i in 1:nrow(plist_out)) {
    #i <- 1
    param <- paste(plist_out$PARAM[i])
    sect <- paste(plist_out$WHERE[i])
    set.seed(seed)
    tval <- runif(1, plist_out$MIN[i], plist_out$MAX[i])
    if (param %in% c("SLA_INI","NDSLA")) {
      params[[sect]][[param]] <- tval
    } else {
      params[[sect]][[param]][,"Value"] <- tval
    }
  }
  
  #return object
  r_obj <- list(PARAM_LIST=plist_out, PARAMS=params)
}
