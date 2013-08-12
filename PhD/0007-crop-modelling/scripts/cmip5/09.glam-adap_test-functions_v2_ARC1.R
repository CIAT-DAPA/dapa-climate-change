#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Dec 2012

run_group_adap <- function(j) {
  #source functions of interest
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
  source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
  source(paste(src.dir,"/cmip5/09.glam-adap_test-functions_v2.R",sep=""))
  
  this_loc <- groupingList$LOC[j]
  this_pst <- groupingList$PARSET[j]
  this_gcm <- paste(groupingList$GCM[j])
  
  ids <- which(all_proc$LOC == this_loc & all_proc$PARSET == this_pst & all_proc$GCM == this_gcm)
  
  cat("\nrunning",length(ids),"ids \n")
  cat("LOC:",this_loc,"\n")
  cat("PARSET:",this_pst,"\n")
  cat("GCM:",this_gcm,"\n")
  
  #loop ids in this group
  timeall <- c()
  for (i in ids) {
    #here get initial model configuration
    RUN_CFG <- get_cfg_adap(i,all_proc)
    #if historical then run one wrapper else run the other
    runtime <- system.time(glam_adap_run_wrapper(RUN_CFG))
    timeall <- c(timeall,as.numeric(runtime)[3])
  }
  return(timeall)
}


#rcp wrapper
glam_adap_run_wrapper <- function(RUN_CFG) {
  #check the existence of the environment set up, before running
  if (class(try(get("ENV_CFG"),silent=T)) == "try-error") {
    stop("src.dir needs to be set")
  }
  
  #source functions of interest
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
  source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
  source(paste(src.dir,"/cmip5/09.glam-adap_test-functions_v2.R",sep=""))
  
  #input directories and model
  cropName <- ENV_CFG$CROP_NAME
  cDir <- paste(ENV_CFG$BDIR,"/model-runs/",toupper(ENV_CFG$CROP_NAME),sep="")
  
  #here construct a control file folder
  #out_bdir <- paste(bDir,"/model-runs/",toupper(cropName),"/runs/cmip5_hist",sep="")
  ctrl_dir <- paste(ENV_CFG$OUT_BDIR,"/_process/exp-",RUN_CFG$PARSET,"_",RUN_CFG$SCE,sep="")
  if (!file.exists(ctrl_dir)) {dir.create(ctrl_dir,recursive=T)}
  ctrl_fil <- paste(ctrl_dir,"/",RUN_CFG$PERIOD,"_loc-",RUN_CFG$LOC,"_",RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,".proc",sep="")
  
  if (!file.exists(ctrl_fil)) {
    #create run setup
    #files that were generated
    setup_rcp <- list()
    setup_rcp$BDIR <- ENV_CFG$BDIR
    setup_rcp$SCRATCH <- ENV_CFG$SCRATCH
    setup_rcp$USE_SCRATCH <- ENV_CFG$USE_SCRATCH
    setup_rcp$CELL <- RUN_CFG$LOC
    setup_rcp$ZONE <- ENV_CFG$CELLS$ZONE[which(ENV_CFG$CELLS$CELL == RUN_CFG$LOC)]
    setup_rcp$METHOD <- NA #not needed for future climate runs
    setup_rcp$CROPNAME <- ENV_CFG$CROP_NAME
    setup_rcp$RCP_DIR <- paste(setup_rcp$BDIR,"/model-runs/",toupper(setup_rcp$CROPNAME),"/runs/",ENV_CFG$RUNS_NAME,"/exp-",RUN_CFG$PARSET,"_outputs/",RUN_CFG$SCE,sep="")
    setup_rcp$CAL_DIR <- paste(setup_rcp$BDIR,"/model-runs/",toupper(setup_rcp$CROPNAME),"/adapt/",ENV_CFG$ADAP_NAME,"/exp-",RUN_CFG$PARSET,"_outputs/",RUN_CFG$SCE,sep="")
    setup_rcp$PRE_DIR <- paste(setup_rcp$BDIR,"/model-runs/",toupper(setup_rcp$CROPNAME),"/calib/exp-",RUN_CFG$PARSET,"_outputs",sep="")
    setup_rcp$YIELD_FILE <- NA #not needed for future climate runs
    setup_rcp$YGP_FILE <- NA #not needed for future climate runs
    setup_rcp$SOW_FILE_RFD <- paste(setup_rcp$PRE_DIR,"/gridcells/fcal_",setup_rcp$CELL,"/opt_fcal_",setup_rcp$CELL,".txt",sep="")
    setup_rcp$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup_rcp$CELL,"_irr.txt",sep="")
    setup_rcp$WTH_DIR_RFD <- NA #temporary
    setup_rcp$WTH_DIR_IRR <- NA #temporary
    setup_rcp$WTH_ROOT <- "ingc"
    setup_rcp$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",setup_rcp$CELL,".txt",sep="")
    setup_rcp$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",setup_rcp$CELL,".txt",sep="")
    setup_rcp$SIM_NAME <- NA # temporary
    setup_rcp$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
    setup_rcp$OPT_METHOD <- NA #not needed for future climate run
    if (!is.null(setup_rcp$ALT_BIN)) {setup_rcp$ALT_BIN <- NULL} else {setup_rcp$ALT_BIN <- ENV_CFG$ALT_BIN}
    
    #if using scratch directory instead of nfs
    if (setup_rcp$USE_SCRATCH) {setup_rcp$SCRATCH <- paste(setup_rcp$SCRATCH,"/exp-",RUN_CFG$PARSET,"_",RUN_CFG$SCE,sep="")}
    
    cat("\nprocessing cell",setup_rcp$CELL,"run",RUN_CFG$RUNID,"\n")
    
    #type of input
    inputType <- gsub("rcp_","",RUN_CFG$WTYPE)
    setup_rcp$SIM_NAME <- paste(RUN_CFG$WTYPE,"_",RUN_CFG$CO2_P,"_",setup_rcp$CELL,sep="")
    
    #output file of his and rcp run
    saveFile <- paste(setup_rcp$CAL_DIR,"/",setup_rcp$SIM_NAME,"/output.RData",sep="")
    rcp_dir <- paste(setup_rcp$RCP_DIR,"/",setup_rcp$SIM_NAME,sep="")
    his_dir <- paste(setup_rcp$RCP_DIR,"/his_",inputType,"_",RUN_CFG$LOC,sep="")
    
    #     #baseline run output stuff that is needed to get the optimal ygp value
    #     if (inputType == "del") {
    #       his_dir <- paste(setup_rcp$RCP_DIR,"/his_allin_",RUN_CFG$LOC,sep="")
    #     }
    
    saveFile_his <- paste(his_dir,"/output.RData",sep="")
    saveFile_rcp <- paste(rcp_dir,"/output.RData",sep="")
    
    #here check if respective future climate run has been done
    if (file.exists(saveFile_rcp)) {
      if (!file.exists(saveFile)) {
        
        #######################################################
        ### get parameter set
        #######################################################
        #load baseline
        #load(saveFile_his)
        #rm(optimal); rm(optimised); rm(params); rm(setup)
        #ybas <- out_data[[2]]$DATA$YIELD
        #rm(out_data)
        
        #load future
        load(saveFile_rcp)
        #yfut <- run_data$RUNS[[8]]$DATA$RFD$YIELD
        
        #get parameter set and irrigated ratio
        base_params <- run_data$PARAMS
        ir_vls <- run_data$IRATIO
        
        #ygp value and years
        base_params$glam_param.ygp$YGP$Value <- run_data$YGP
        cal_ygp <- run_data$YGP
        ir_vls$YEAR <- base_params$glam_param.mod_mgt$ISYR:base_params$glam_param.mod_mgt$IEYR
        rm(run_data)
        
        #assign directories according to input type
        if (inputType == "allin") {
          setup_rcp$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45/",RUN_CFG$SCE,"/rfd_",setup_rcp$CELL,sep="")
          setup_rcp$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45/",RUN_CFG$SCE,"/irr_",setup_rcp$CELL,sep="")
        } else if (inputType == "bcrain") {
          setup_rcp$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_bc/",RUN_CFG$SCE,"/rfd_",setup_rcp$CELL,sep="")
          setup_rcp$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_bc/",RUN_CFG$SCE,"/irr_",setup_rcp$CELL,sep="")
        } else if (inputType == "del") {
          setup_rcp$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_del/",RUN_CFG$SCE,"/rfd_",setup_rcp$CELL,sep="")
          setup_rcp$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_del/",RUN_CFG$SCE,"/irr_",setup_rcp$CELL,sep="")
        } else if (inputType == "sh") {
          setup_rcp$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_sh/",RUN_CFG$SCE,"/rfd_",setup_rcp$CELL,sep="")
          setup_rcp$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth-cmip5_rcp45_sh/",RUN_CFG$SCE,"/irr_",setup_rcp$CELL,sep="")
        }
        
        #configuration of adaptation
        adap_run <- cfg_adap_runs(runs_data=ENV_CFG$ADAP_RUNS,rcp_data=saveFile_rcp)
        
        #here run the model. Loop the adaptation scenarios
        adap_data <- list()
        for (run_i in 1:nrow(adap_run$RUNS)) {
          #get adaptation runs configured
          #run_i <- 43
          this_run <- adap_run$RUNS[run_i,]
          run_id <- this_run$RUNID
          this_run$RUNID <- NULL
          
          cat(paste("adap run _adap ",run_i," ",sep=""))
          
          #call updater
          params <- base_params
          params <- update_params_adap(run_data=this_run,params=params,traits=adap_run$TRAITS)
          
          #run the model!!!
          run_data <- GLAM_adap_run_loc(GLAM_params=params,RUN_setup=setup_rcp,
                                        iratio=ir_vls,subdir=paste("adap_",run_i,sep=""))
          
          #put results into object
          adap_data[[run_i]] <- list()
          adap_data[[run_i]]$DATA <- run_data$DATA
          adap_data[[run_i]]$PARAMS <- params
          adap_data[[run_i]]$RUN_ID <- run_id
          adap_data[[run_i]]$CONFIG <- this_run
        }
        arun_data <- list()
        arun_data$RUNS <- adap_data
        arun_data$ADAP_RUNS <- adap_run
        arun_data$SETUP <- setup_rcp
        arun_data$IRATIO <- ir_vls
        arun_data$YGP <- cal_ygp
        
        save(list=c("arun_data"),file=saveFile)
        
        #here remove everything
        setwd(setup_rcp$BDIR)
        #system(paste("rm -rf ",paste(setup_rcp$CAL_DIR,"/",setup_rcp$SIM_NAME,"/",setup_rcp$SIM_NAME,sep="")))
      }
      #write control file
      ff <- file(ctrl_fil,"w")
      cat("Processed on",date(),"\n",file=ff)
      close(ff)
    } else {
      cat("rcp experiment for this run has not yet been run \n")
    }
  }
  return(ctrl_fil)
}



#get initial model configuration
get_cfg_adap <- function(i,all_proc) {
  this_run <- all_proc[i,]
  RUN_CFG <- list()
  RUN_CFG$RUNID <- paste(this_run$RUNID)
  RUN_CFG$PERIOD <- unlist(strsplit(RUN_CFG$RUNID,"_",fixed=T))[1]
  RUN_CFG$LOC <- this_run$LOC
  RUN_CFG$SCE <- paste(this_run$GCM)
  RUN_CFG$GCM <- unlist(strsplit(RUN_CFG$SCE,"_ENS_"))[1]
  RUN_CFG$ENS <- unlist(strsplit(RUN_CFG$SCE,"_ENS_"))[2]
  RUN_CFG$PARSET <- this_run$PARSET
  RUN_CFG$WTYPE <- paste("rcp_",this_run$WTH_TYPE,sep="")
  RUN_CFG$CO2_P <- paste(this_run$CO2_P)
  return(RUN_CFG)
}


#update parameter set based upon adaptation run configuration
#change min/max in range of parameters in parameter set before running if value
#being tested is larger than in pset
####
update_params_adap <- function(run_data,params,traits) {
  #grab season, remove from data.frame and update params
  season <- paste(run_data$SEASON); run_data$SEASON <- NULL
  params$glam_param.mod_mgt$SEASON <- season
  
  for (param in names(run_data)) {
    #param <- names(run_data)[1]
    p_val <- run_data[,param]
    where <- paste(traits$section[which(traits$parameter == param)])
    if (length(where) > 1) {where <- unique(where)}
    
    if (!is.na(p_val)) {
      if (param == "TE") {
        #adjusting according to CO2. Remember that ICO2=1 modifies TEN_MAX and TE
        #so just
        #1. modify B_TE and B_TEN_MAX according to adapt
        #2. 'bmass' TEN_MAX value = B_TEN_MAX
        #3. modify TE from B_TE using the CO2 parameterisation rule
        
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
    if (param == "GCPLFL" & as.numeric(paste(runs_data$min[i])) < 0) {
      advals <- c(maxval-ranval*0.25,maxval-ranval*0.5,maxval-ranval)
    } else {
      advals <- c(minval+ranval*0.25,minval+ranval*0.5,minval+ranval)
    }
    
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
      #for (bnd in c("low","top")) {
      #for (bnd in c("top")) {
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
      #grp_ad[1,4:6] <- rev(grp_ad[1,4:6])
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
  out_df <- cbind(out_df,SEASON="RFD")
  out_df$RUNID <- paste("ADAP_",10000+(1:nrow(out_df)),sep="")
  
  #return objects
  out_list <- list(TRAITS=gen_df,RUNS=out_df)
  return(out_list)
}



############################################################
####### function to perform a GLAM run, 
############################################################
GLAM_adap_run_loc <- function(GLAM_params,RUN_setup,iratio=0,subdir="r1") {
  simset <- RUN_setup$SIM_NAME
  cell <- RUN_setup$CELL
  cropName <- RUN_setup$CROPNAME
  bDir <- RUN_setup$BDIR
  isyr <- GLAM_params$glam_param.mod_mgt$ISYR
  ieyr <- GLAM_params$glam_param.mod_mgt$IEYR
  adap_seas <- GLAM_params$glam_param.mod_mgt$SEASON
  
  #input directories and model
  execName <- paste("glam-",tolower(cropName),sep="")
  
  #determine operating system and bin folder
  machine <- as.data.frame(t(Sys.info()))
  machine <- paste(machine$sysname)
  
  if (is.null(RUN_setup$ALT_BIN)) {
    binDir <- paste(bDir,"/model-runs/bin/glam-",tolower(machine),sep="")
  } else {
    binDir <- paste(RUN_setup$ALT_BIN,"/glam-",tolower(machine),sep="")
  }
  
  if (tolower(machine) == "windows") {
    glam_cmd <- paste(paste(execName,".exe",sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
    execName <- paste(execName,".exe",sep="")
  } else if (tolower(machine) == "linux") {
    glam_cmd <- paste(paste("./",execName,sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
  }
  
  #output directories
  if (RUN_setup$USE_SCRATCH) {
    cal_dir <- RUN_setup$SCRATCH #run directory
    nfs_dir <- paste(RUN_setup$CAL_DIR,"/",simset,sep="")
    if (!file.exists(nfs_dir)) {dir.create(nfs_dir,recursive=T)}
  } else {
    cal_dir <- RUN_setup$CAL_DIR #run directory
  }
  if (!file.exists(cal_dir)) {dir.create(cal_dir,recursive=T)}
  
  cal_dir <- paste(cal_dir,"/",simset,sep="") #run directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #cal_dir <- paste(cal_dir,"/run-",subdir,sep="")
  #if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #files that were generated
  yFile <- "nofile"
  ygpFile <- "nofile"
  sowFile_rfd <- RUN_setup$SOW_FILE_RFD
  sowFile_irr <- RUN_setup$SOW_FILE_IRR
  wthDir_rfd <- RUN_setup$WTH_DIR_RFD
  wthDir_irr <- RUN_setup$WTH_DIR_IRR
  solFile <- RUN_setup$SOL_FILE
  solGrid <- RUN_setup$SOL_GRID
  
  #irrigated/rainfed/mix variable
  if (length(which(iratio$IRATIO == 0)) == nrow(iratio)) {
    run.type <- "RFD"
  } else if (length(which(iratio$IRATIO == 1)) == nrow(iratio)) {
    run.type <- "IRR"
  } else {
    run.type <- "MIX"
  }
  
  #loop through sequence of values
  cat("performing run",run.type,"\n")
  
  ##############here irrigation rate
  if (run.type == "RFD") {
    GLAM_params$glam_param.mod_mgt$SEASON <- adap_seas #"RFD"
    
    #check if the planting date is well configured
    if (sowFile_rfd == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/",run.type,"_run-",subdir,sep=""))
    
    #write the model params
    opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
    opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      
      ######################################################
      #write filenames file
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      wth_row <- paste("inputs/ascii/wth/",RUN_setup$WTH_ROOT,sep="")
      soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(solFile,"/",fixed=T))[length(unlist(strsplit(solFile,"/",fixed=T)))],sep="")
      soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(solGrid,"/",fixed=T))[length(unlist(strsplit(solGrid,"/",fixed=T)))],sep="")
      yield_row <- yFile
      
      if (sowFile_rfd == "nofile") {
        sow_row <- sowFile_rfd
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_rfd,"/",fixed=T))[length(unlist(strsplit(sowFile_rfd,"/",fixed=T)))],sep="")
      }
      if (ygpFile == "nofile") {
        ygp_row <- "nofile"
        if (GLAM_params$glam_param.ygp$YGP$Value < -90) {
          stop("YGP needs to be specified either by value or by a file")
        }
      } else {
        ygp_row <- paste("inputs/",unlist(strsplit(ygpFile,"/",fixed=T))[length(unlist(strsplit(ygpFile,"/",fixed=T)))],sep="")
        GLAM_params$glam_param.ygp$YGP$Value <- -99.0
      }
      
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      
      if (sowFile_rfd != "nofile") {
        x <- file.copy(sowFile_rfd,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_rfd),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_rfd,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file, .inf file and compress the daily files
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #if daily files were produced then compress and remove
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      rfd_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    }
    
  } else if (run.type == "IRR") {
    GLAM_params$glam_param.mod_mgt$SEASON <- "IRR"
    
    #check if the planting date is well configured
    if (sowFile_irr == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/",run.type,"_run-",subdir,sep=""))
    
    #write the model params
    opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
    opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      ######################################################
      #write filenames file
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      wth_row <- paste("inputs/ascii/wth/",RUN_setup$WTH_ROOT,sep="")
      soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(solFile,"/",fixed=T))[length(unlist(strsplit(solFile,"/",fixed=T)))],sep="")
      soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(solGrid,"/",fixed=T))[length(unlist(strsplit(solGrid,"/",fixed=T)))],sep="")
      yield_row <- yFile
      
      if (sowFile_irr == "nofile") {
        sow_row <- sowFile_irr
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_irr,"/",fixed=T))[length(unlist(strsplit(sowFile_irr,"/",fixed=T)))],sep="")
      }
      if (ygpFile == "nofile") {
        ygp_row <- "nofile"
        if (GLAM_params$glam_param.ygp$YGP$Value < -90) {
          stop("YGP needs to be specified either by value or by a file")
        }
      } else {
        ygp_row <- paste("inputs/",unlist(strsplit(ygpFile,"/",fixed=T))[length(unlist(strsplit(ygpFile,"/",fixed=T)))],sep="")
        GLAM_params$glam_param.ygp$YGP$Value <- -99.0
      }
      
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      if (sowFile_irr != "nofile") {
        x <- file.copy(sowFile_irr,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_irr),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_irr,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #compress & remove daily files should they exist
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      irr_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    }
  } else if (run.type == "MIX") {
    GLAM_params$glam_param.mod_mgt$SEASON <- adap_seas #"RFD"
    
    #check if the planting date is well configured
    if (sowFile_rfd == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/RFD_run-",subdir,sep=""))
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      ######################################################
      #write filenames file
      wth_row <- paste("inputs/ascii/wth/",RUN_setup$WTH_ROOT,sep="")
      soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(solFile,"/",fixed=T))[length(unlist(strsplit(solFile,"/",fixed=T)))],sep="")
      soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(solGrid,"/",fixed=T))[length(unlist(strsplit(solGrid,"/",fixed=T)))],sep="")
      yield_row <- yFile
      
      if (sowFile_rfd == "nofile") {
        sow_row <- sowFile_rfd
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_rfd,"/",fixed=T))[length(unlist(strsplit(sowFile_rfd,"/",fixed=T)))],sep="")
      }
      if (ygpFile == "nofile") {
        ygp_row <- "nofile"
        if (GLAM_params$glam_param.ygp$YGP$Value < -90) {
          stop("YGP needs to be specified either by value or by a file")
        }
      } else {
        ygp_row <- paste("inputs/",unlist(strsplit(ygpFile,"/",fixed=T))[length(unlist(strsplit(ygpFile,"/",fixed=T)))],sep="")
        GLAM_params$glam_param.ygp$YGP$Value <- -99.0
      }
      
      #write the model params
      opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep="")
      opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      
      #write filenames file
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      if (sowFile_rfd != "nofile") {
        x <- file.copy(sowFile_rfd,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_rfd),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_rfd,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #compress & remove daily files
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      rfd_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      rfd_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
    }
    
    ##!
    #Now the irrigated run
    GLAM_params$glam_param.mod_mgt$SEASON <- "IRR"
    
    #check if the planting date is well configured
    if (sowFile_irr == "nofile") {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
        stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
      }
    } else {
      if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
        GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/IRR_run-",subdir,sep=""))
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      ######################################################
      #write filenames file
      if (sowFile_irr == "nofile") {
        sow_row <- sowFile_irr
      } else {
        sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_irr,"/",fixed=T))[length(unlist(strsplit(sowFile_irr,"/",fixed=T)))],sep="")
      }
      
      #write the model params
      opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run-irr.txt",sep="")
      opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
      parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
      
      #write filenames file
      ofnames <- paste(run_dir,"/filenames-",tolower(cropName),"-run.txt",sep="")
      fn <- file(ofnames,"w")
      cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
      cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
      cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
      close(fn)
      
      #copy all inputs to run directory and write filenames
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      if (sowFile_irr != "nofile") {
        x <- file.copy(sowFile_irr,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      }
      if (ygpFile != "nofile") {
        x <- file.copy(ygpFile,paste(run_dir,"/inputs",sep=""),overwrite=T)
      }
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir_irr),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_irr,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file
      x <- file.remove(execName)
      x <- file.remove("glam.inf")
      
      #remove all input files
      setwd("./inputs/ascii/wth")
      #system(paste("7z a daily.7z -tzip *.wth"))
      x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/obs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/soil")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs/ascii/sow")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir); setwd("./inputs")
      x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
      setwd(run_dir)
      
      #compress & remove daily files, should they exist (IASCII = 2 or 3)
      if (GLAM_params$glam_param.mod_mgt$IASCII >= 2) {
        setwd("./output/daily")
        system(paste("7z a daily.7z -tzip *.out"))
        x <- sapply(list.files(".",pattern="\\.out"),FUN= function(x) {s <- file.remove(x)})
        setwd(run_dir)
      }
    } else {
      setwd(run_dir)
    }
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    if (length(outfile) == 0) {
      irr_data <- as.data.frame(matrix(NA,nrow=28,ncol=42))
    } else {
      irr_data <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
    }
  }
  
  names(rfd_data) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                       "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
  names(irr_data) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS","T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
  
  out_all <- list(); out_all$DATA <- list()
  out_all$DATA$RFD <- rfd_data
  out_all$DATA$IRR <- irr_data
  
  if (RUN_setup$USE_SCRATCH) {
    #system(paste("cp -rf ",cal_dir," ",paste(nfs_dir,"/.",sep=""),sep=""))
    setwd(nfs_dir)
    system(paste("rm -rf ",cal_dir,sep=""))
  }
  return(out_all)
}

