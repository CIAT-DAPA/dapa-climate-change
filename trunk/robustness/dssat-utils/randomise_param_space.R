#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jul 2014

#function to randomise order of parameters
randomise_order <- function(plist_in, seed) {
  #plist_in <- param_orig
  #seed <- seed_list[1]
  
  #randomise param list
  set.seed(seed)
  reord <- sample(1:nrow(plist_in),replace=F)
  plist_out <- plist_in[reord,]
  row.names(plist_out) <- 1:nrow(plist_out)
  
  #return object
  return(plist_out)
}


#function to randomise starting point of parameter space (.CUL parameters)
randomise_culpar <- function(plist_in, seed, model) {
  #model <- csmodel
  #plist_in <- param_orig
  #seed <- seed_list[1]
  
  if (model == "MZCER045") {
    set.seed(seed)
    culparams <- data.frame(P1=runif(1, plist_in$MIN[which(plist_in$PARAM=="P1")], plist_in$MAX[which(plist_in$PARAM=="P1")]),
                            P2=runif(1, plist_in$MIN[which(plist_in$PARAM=="P2")], plist_in$MAX[which(plist_in$PARAM=="P2")]),
                            P5=runif(1, plist_in$MIN[which(plist_in$PARAM=="P5")], plist_in$MAX[which(plist_in$PARAM=="P5")]),
                            G2=runif(1, plist_in$MIN[which(plist_in$PARAM=="G2")], plist_in$MAX[which(plist_in$PARAM=="G2")]),
                            G3=runif(1, plist_in$MIN[which(plist_in$PARAM=="G3")], plist_in$MAX[which(plist_in$PARAM=="G3")]),
                            PHINT=runif(1, plist_in$MIN[which(plist_in$PARAM=="PHINT")], plist_in$MAX[which(plist_in$PARAM=="PHINT")]))
  } else if (model == "MZIXM045") {
    set.seed(seed)
    culparams <- data.frame(P1=runif(1, plist_in$MIN[which(plist_in$PARAM=="P1")], plist_in$MAX[which(plist_in$PARAM=="P1")]),
                            P2=runif(1, plist_in$MIN[which(plist_in$PARAM=="P2")], plist_in$MAX[which(plist_in$PARAM=="P2")]),
                            P5=runif(1, plist_in$MIN[which(plist_in$PARAM=="P5")], plist_in$MAX[which(plist_in$PARAM=="P5")]),
                            G2=runif(1, plist_in$MIN[which(plist_in$PARAM=="G2")], plist_in$MAX[which(plist_in$PARAM=="G2")]),
                            G3=runif(1, plist_in$MIN[which(plist_in$PARAM=="G3")], plist_in$MAX[which(plist_in$PARAM=="G3")]),
                            PHINT=runif(1, plist_in$MIN[which(plist_in$PARAM=="PHINT")], plist_in$MAX[which(plist_in$PARAM=="PHINT")]),
                            AX=runif(1, plist_in$MIN[which(plist_in$PARAM=="AX")], plist_in$MAX[which(plist_in$PARAM=="AX")]),
                            LX=runif(1, plist_in$MIN[which(plist_in$PARAM=="LX")], plist_in$MAX[which(plist_in$PARAM=="LX")]))
  }
  
  #return object
  return(culparams)
}


#function to randomise starting point of parameter space (.CUL parameters)
randomise_ecopar <- function(plist_in, seed, model) {
  #model <- csmodel
  #plist_in <- param_orig
  #seed <- seed_list[1]
  
  #opt_data$ECO <- data.frame(DSGFT=170,RUE=4.2,KCAN=0.85,TSEN=6.0,CDAY=15.0)
  if (model == "MZCER045") {
    set.seed(seed)
    ecoparams <- data.frame(DSGFT=runif(1, plist_in$MIN[which(plist_in$PARAM=="DSGFT")], plist_in$MAX[which(plist_in$PARAM=="DSGFT")]),
                            RUE=runif(1, plist_in$MIN[which(plist_in$PARAM=="RUE")], plist_in$MAX[which(plist_in$PARAM=="RUE")]),
                            KCAN=runif(1, plist_in$MIN[which(plist_in$PARAM=="KCAN")], plist_in$MAX[which(plist_in$PARAM=="KCAN")]),
                            TSEN=runif(1, plist_in$MIN[which(plist_in$PARAM=="TSEN")], plist_in$MAX[which(plist_in$PARAM=="TSEN")]),
                            CDAY=15)
  } else if (model == "MZIXM045") {
    set.seed(seed)
    ecoparams <- data.frame(DSGFT=runif(1, plist_in$MIN[which(plist_in$PARAM=="DSGFT")], plist_in$MAX[which(plist_in$PARAM=="DSGFT")]),
                            RUE=runif(1, plist_in$MIN[which(plist_in$PARAM=="RUE")], plist_in$MAX[which(plist_in$PARAM=="RUE")]),
                            KCAN=runif(1, plist_in$MIN[which(plist_in$PARAM=="KCAN")], plist_in$MAX[which(plist_in$PARAM=="KCAN")]),
                            TSEN=runif(1, plist_in$MIN[which(plist_in$PARAM=="TSEN")], plist_in$MAX[which(plist_in$PARAM=="TSEN")]),
                            PSTM=runif(1, plist_in$MIN[which(plist_in$PARAM=="PSTM")], plist_in$MAX[which(plist_in$PARAM=="PSTM")]),
                            PEAR=runif(1, plist_in$MIN[which(plist_in$PARAM=="PEAR")], plist_in$MAX[which(plist_in$PARAM=="PEAR")]),
                            CDAY=15)
  }
  
  #return object
  return(ecoparams)
}


#function to randomise starting point of parameter space (.CUL parameters)
randomise_spepar <- function(params, plist_in, seed) {
  #params <- get_spepar(paste(bin_dir,"/MZCER045.SPE",sep=""))
  #plist_in <- param_orig
  #seed <- seed_list[1]
  
  #select .SPE params
  plist_in <- plist_in[plist_in$FILE == "SPE",]
  
  #set values in file
  for (param in paste(plist_in$PARAM)) {
    #param <- paste(plist_in$PARAM)[1]
    sect <- paste(plist_in$WHERE[which(plist_in$PARAM == param)])
    set.seed(seed)
    tval <- runif(1, plist_in$MIN[which(plist_in$PARAM==param)], plist_in$MAX[which(plist_in$PARAM==param)])
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
randomise_xfile <- function(params, plist_in, seed) {
  #params <- get_xfile_dummy()
  #plist_in <- param_orig
  #seed <- seed_list[1]
  
  #select XFILE params
  plist_in <- plist_in[plist_in$FILE == "XFILE",]
  
  #set values in file
  for (param in paste(plist_in$PARAM)) {
    #param <- paste(plist_in$PARAM)[1]
    sect <- paste(plist_in$WHERE[which(plist_in$PARAM == param)])
    set.seed(seed)
    tval <- runif(1, plist_in$MIN[which(plist_in$PARAM==param)], plist_in$MAX[which(plist_in$PARAM==param)])
    params[[sect]][[param]] <- tval
    if (param == "PPOP") {params[[sect]][["PPOE"]] <- tval}
  }
  
  #return object
  return(params)
}

