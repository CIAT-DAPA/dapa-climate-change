#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014

#put all given parameter values into (coming from a latin hypercube sample) a given parameter set

#function to put values into parameter set
put_param_hyp <- function(hyp_sample, params, p_list, all_param) {
  #hyp_sample <- as.numeric(out_df[1,])
  #p_list <- names(out_df)
  #params <- base_params
  #all_param <- param_list
  for (i in 1:length(p_list)) {
    #i <- 1
    param <- p_list[i]
    sect <- paste(all_param$WHERE[which(all_param$PARAM == param)])
    
    #put parameter value into parameter set
    if (param %in% c("SLA_INI","NDSLA")) {
      params[[sect]][[param]] <- hyp_sample[i]
    } else {
      params[[sect]][[param]][,"Value"] <- hyp_sample[i]
    }
  }
  #return object
  return(params)
}
