#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Dec 2012

#update parameter set based upon adaptation run configuration
update_params_adap <- function(run_data,params) {
  #grab season, remove from data.frame and update params
  season <- paste(this_run$SEASON); this_run$SEASON <- NULL
  params$glam_param.mod_mgt$SEASON <- season
  
  for (param in names(this_run)) {
    #param <- names(this_run)[1]
    p_val <- this_run[,param]
    where <- paste(adap_run$TRAITS$section[which(adap_run$TRAITS$parameter == param)])
    if (length(where) > 1) {where <- unique(where)}
    
    if (!is.na(p_val)) {
      if (param == "TE") {
        #adjusting according to CO2
        rtio <- params$glam_param.hts_fut$B_TE$Value/params[[where]][[param]]$Value
        params[[where]][[param]]$Value <- p_val
        if (p_val > params[[where]][[param]]$Max)  {params[[where]][[param]]$Max <- p_val}
        
        params$glam_param.hts_fut$B_TE$Value <- p_val*rtio
        if (p_val > params$glam_param.hts_fut$B_TE$Max)  {params$glam_param.hts_fut$B_TE$Max <- p_val}
      } else if (param == "TEN_MAX") {
        #updating both the baseline and future one
        params[[where]][[param]]$Value <- p_val
        if (p_val > params[[where]][[param]]$Max) {params[[where]][[param]]$Max <- p_val}
        
        params$glam_param.hts_fut$B_TEN_MAX$Value <- p_val
        if (p_val > params$glam_param.hts_fut$B_TEN_MAX$Max) {params$glam_param.hts_fut$B_TEN_MAX$Max <- p_val}
      } else if (param == "SLA_INI") {
        params[[where]][[param]] <- p_val
      } else {
        params[[where]][[param]]$Value <- p_val
        if (p_val > params[[where]][[param]]$Max) {params[[where]][[param]]$Max <- p_val}
        if (p_val < params[[where]][[param]]$Min) {params[[where]][[param]]$Min <- p_val}
      }
    }
  }
  return(params)
}



#create two data frames for adaptation runs
cfg_adap_runs <- function(runs_data,rcp_data="output.RData") {
  
  #load output data
  load(rcp_data)
  params <- run_data$PARAMS
  rm(run_data)
  
  #loop through parameters
  for (i in 1:nrow(runs_data)) {
    #i <- 15
    param <- paste(runs_data$parameter[i]) #paramList[i]
    where <- paste(runs_data$section[i])
    chgt <- paste(runs_data$chg[i])
    grp <- paste(runs_data$group[i])
    
    if (param == "SLA_INI") {
      minval <- max(c(as.numeric(paste(runs_data$min[which(runs_data$parameter == param)])),params[[where]][[param]]))
      maxval <- as.numeric(paste(runs_data$max[which(runs_data$parameter == param)]))
      ranval <- maxval-minval
    } else {
      if (chgt == "a") {
        minval <- max(c(as.numeric(paste(runs_data$min[i])),params[[where]][[param]]$Value))
        maxval <- as.numeric(paste(runs_data$max[i]))
      } else {
        minval <- params[[where]][[param]]$Value + params[[where]][[param]]$Value*as.numeric(paste(runs_data$min[i]))*0.01
        maxval <- params[[where]][[param]]$Value + params[[where]][[param]]$Value*as.numeric(paste(runs_data$max[i]))*0.01
      }
      ranval <- maxval-minval
    }
    
    #define modification values
    advals <- c(minval+ranval*0.25,minval+ranval*0.5,minval+ranval)
    
    if (i==1) {
      gen_df <- data.frame(parameter=param,section=where,group=grp,low=advals[1],
                           mid=advals[2],top=advals[3])
    } else {
      gen_df <- rbind(gen_df,data.frame(parameter=param,section=where,group=grp,
                                        low=advals[1],mid=advals[2],top=advals[3]))
    }
  }
  
  #######################################################
  ####
  #configure all runs: create a data frame with id of run and values of
  #all parameters (parameters being columns)
  #combined adaptation runs
  levs <- c("low","top")
  adap_exp <- expand.grid(BMASS=levs,TT=levs,THR=levs) #matrix of runs
  
  #output data frame
  parList <- unique(paste(runs_data$parameter))
  out_df <- as.data.frame(matrix(nrow=1000,ncol=(length(parList)+1)))
  names(out_df) <- c("RUNID",parList)
  
  #genotypic changes
  rowc <- 1
  for (i in 1:nrow(gen_df)) {
    #i <- 1
    param <- paste(gen_df$parameter[i])
    for (bnd in c("low","mid","top")) {
      #bnd <- "low"
      out_df[rowc,param] <- c(gen_df[i,bnd])
      out_df$RUNID[rowc] <- rowc
      rowc <- rowc+1
    }
  }
  
  #combined runs (with decreased vegetative TT)
  for (i in 1:nrow(adap_exp)) {
    #i <- 1
    for (grp in names(adap_exp)) {
      #grp <- "BMASS"
      grp_ad <- gen_df[which(gen_df$group==grp),]
      if (grp == "TT") {grp_ad <- grp_ad[c(1,3:nrow(grp_ad)),]}
      for (j in 1:nrow(grp_ad)) {
        #j <- 1
        param <- paste(grp_ad$parameter[j])
        out_df[rowc,param] <- grp_ad[j,paste(adap_exp[i,grp])]
      }
    }
    out_df$RUNID[rowc] <- rowc
    rowc <- rowc+1
  }
  
  #combined runs (with increased vegetative TT)
  for (i in 1:nrow(adap_exp)) {
    #i <- 1
    for (grp in names(adap_exp)) {
      #grp <- "BMASS"
      grp_ad <- gen_df[which(gen_df$group==grp),]
      if (grp == "TT") {grp_ad <- grp_ad[c(2:nrow(grp_ad)),]}
      for (j in 1:nrow(grp_ad)) {
        #j <- 1
        param <- paste(grp_ad$parameter[j])
        out_df[rowc,param] <- grp_ad[j,paste(adap_exp[i,grp])]
      }
    }
    out_df$RUNID[rowc] <- rowc
    rowc <- rowc+1
  }
  
  out_df <- out_df[which(!is.na(out_df$RUNID)),]
  
  #all above with:
  #RFD: rainfed
  #IRR: whole GS irrigation
  #using other types of irrigation (i.e. during i=2, or during i=3,4 results in too many runs)
  out_df <- rbind(cbind(out_df,SEASON="RFD"),cbind(out_df,SEASON="IRR"))
  out_df$RUNID <- paste("ADAP_",10000+(1:nrow(out_df)),sep="")
  
  #return objects
  out_list <- list(TRAITS=gen_df,RUNS=out_df)
  return(out_list)
}


