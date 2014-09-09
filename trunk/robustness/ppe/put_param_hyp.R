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


#function to randomise starting point of parameter space (.CUL parameters)
put_culpar <- function(plist_in, model) {
  #model <- csmodel
  #plist_in <- out_df[1,]
  
  if (model == "MZCER045") {
    culparams <- data.frame(P1=plist_in$P1[1],P2=plist_in$P2[1],P5=plist_in$P5[1],
                            G2=plist_in$G2[1],G3=plist_in$G3[1],PHINT=plist_in$PHINT[1])
  } else if (model == "MZIXM045") {
    culparams <- data.frame(P1=plist_in$P1[1],P2=plist_in$P2[1],P5=plist_in$P5[1],
                            G2=plist_in$G2[1],G3=plist_in$G3[1],PHINT=plist_in$PHINT[1],
                            AX=plist_in$AX[1],LX=plist_in$LX[1])
  }
  
  #return object
  return(culparams)
}


#function to randomise starting point of parameter space (.CUL parameters)
put_ecopar <- function(plist_in, model) {
  #model <- csmodel
  #plist_in <- out_df[1,]
  
  #opt_data$ECO <- data.frame(DSGFT=170,RUE=4.2,KCAN=0.85,TSEN=6.0,CDAY=15.0)
  if (model == "MZCER045") {
    ecoparams <- data.frame(DSGFT=plist_in$DSGFT[1],RUE=plist_in$RUE[1],KCAN=plist_in$KCAN[1],
                            TSEN=plist_in$TSEN[1],CDAY=15)
  } else if (model == "MZIXM045") {
    ecoparams <- data.frame(DSGFT=plist_in$DSGFT[1],RUE=plist_in$RUE[1],KCAN=plist_in$KCAN[1],
                            TSEN=plist_in$TSEN[1],PSTM=plist_in$PSTM[1],PEAR=plist_in$PEAR[1],
                            CDAY=15)
  }
  
  #return object
  return(ecoparams)
}


#function to randomise starting point of parameter space (.CUL parameters)
put_spepar <- function(params, plist_in, param_list) {
  #params <- get_spepar(paste(bin_dir,"/MZCER045.SPE",sep=""))
  #plist_in <- out_df[1,]
  #param_list
  
  #select .SPE params
  tlist <- param_list[param_list$FILE == "SPE",]
  
  #set values in file
  for (param in paste(tlist$PARAM)) {
    #param <- paste(tlist$PARAM)[1]
    sect <- paste(tlist$WHERE[which(tlist$PARAM == param)])
    tval <- plist_in[1,param]
    if (param %in% c("SDSZ","RSGRT")) {
      params[[sect]][which(gsub(" ","",params[[sect]][["PARAM"]]) == param),"VALUE"] <- tval
    } else {
      params[[sect]][[param]] <- tval
    }
  }
  
  #return object
  return(params)
}

#function to randomise starting point of parameter space (.CUL parameters)
put_xfile <- function(params, plist_in, param_list) {
  #params <- get_xfile_dummy()
  #plist_in <- out_df[1,]
  #param_list <- param_list
  
  #select XFILE params
  tlist <- param_list[param_list$FILE == "XFILE",]
  
  #set values in file
  for (param in paste(tlist$PARAM)) {
    #param <- paste(plist_in$PARAM)[1]
    sect <- paste(tlist$WHERE[which(tlist$PARAM == param)])
    tval <- plist_in[1,param]
    params[[sect]][[param]] <- tval
    if (param == "PPOP") {params[[sect]][["PPOE"]] <- tval}
  }
  
  #return object
  return(params)
}



