#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012


##### run the whole process
glam_ygp_half_half <- function(this_run) {
  #get the run details
  sid <- runs_ref$SID[this_run]
  zone <- runs_ref$RUN[this_run]
  seed <- runs_ref$SEED[this_run]
  expID <- runs_ref$EXPID[this_run]
  
  #check the existence of three parameters needed for sourcing this script
  if (class(try(get("src.dir"),silent=T)) == "try-error") {
    stop("src.dir needs to be set")
  }
  
  if (class(try(get("bDir"),silent=T)) == "try-error") {
    stop("bDir needs to be set")
  }
  
  if (class(try(get("maxiter"),silent=T)) == "try-error") {
    stop("maxiter (max. num. iterations) needs to be set")
  }
  
  if (class(try(get("zone"),silent=T)) == "try-error") {
    stop("zone to be calibrated needs to be set")
  }
  
  #Read in a dummy GLAM parameter file and create a new one based on a new parameter for
  #running and optimising GLAM
  
  #source all needed functions
  source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-run-functions.R",sep=""))
  source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
  
  #input directories and model
  cropName <- "gnut"
  cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
  pDir <- paste(cDir,"/params",sep="") #parameter files
  
  #load cell details
  #cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
  cells <- read.csv(paste(cDir,"/inputs/calib-cells-selection-",selection,".csv",sep=""))
  
  #ci <- 1
  ciList <- which(cells$ZONE == zone)
  for (ci in ciList) {
    #get run setup
    #files that were generated
    CAL_setup <- list()
    CAL_setup$BDIR <- bDir
    CAL_setup$SCRATCH <- scratch
    CAL_setup$USE_SCRATCH <- use_scratch
    CAL_setup$CELL <- cells$CELL[ci]
    CAL_setup$ZONE <- cells$ZONE[ci]
    CAL_setup$METHOD <- "lin"
    CAL_setup$CROPNAME <- "gnut"
    CAL_setup$CAL_DIR <- paste(CAL_setup$BDIR,"/model-runs/",toupper(CAL_setup$CROPNAME),"/calib/exp-",expID,"_outputs",sep="")
    CAL_setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",CAL_setup$CELL,"_",CAL_setup$METHOD,".txt",sep="")
    CAL_setup$YGP_FILE <- "nofile"
    CAL_setup$SOW_FILE_RFD <- paste(CAL_setup$CAL_DIR,"/gridcells/fcal_",CAL_setup$CELL,"/opt_fcal_",CAL_setup$CELL,".txt",sep="")
    CAL_setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",CAL_setup$CELL,"_irr.txt",sep="")
    CAL_setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth/rfd_",CAL_setup$CELL,sep="")
    CAL_setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth/irr_",CAL_setup$CELL,sep="")
    CAL_setup$WTH_ROOT <- "ingc"
    CAL_setup$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",CAL_setup$CELL,".txt",sep="")
    CAL_setup$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",CAL_setup$CELL,".txt",sep="")
    CAL_setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
    
    #create the run setup
    RUN_setup <- list()
    RUN_setup$B_DIR <- paste(CAL_setup$BDIR,"/model-runs/",toupper(cropName),sep="")
    RUN_setup$BIN_DIR <- paste(RUN_setup$B_DIR,"/./../bin",sep="")
    RUN_setup$CAL_DIR <- paste(RUN_setup$B_DIR,"/calib",sep="")
    RUN_setup$INPUTS_DIR <- paste(RUN_setup$B_DIR,"/inputs",sep="")
    RUN_setup$ASC_DIR <- paste(RUN_setup$INPUTS_DIR,"/ascii",sep="")
    RUN_setup$RUNS_DIR <- CAL_setup$SCRATCH
    RUN_setup$CROP <- cropName
    RUN_setup$YEARS <- 1966:1993
    RUN_setup$EXP_DIR <- paste("exp-",expID,"_outputs",sep="")
    RUN_setup$GRID <- paste(RUN_setup$INPUTS_DIR,"/calib-cells-selection-",selection,".csv",sep="")
    RUN_setup$PREFIX <- "fcal_"
    RUN_setup$GRIDCELL <- CAL_setup$CELL
    RUN_setup$YGP <- "opt"
    RUN_setup$CODES_PREFIX <- "soilcodes_"
    RUN_setup$TYPES_PREFIX <- "soiltypes_"
    RUN_setup$WTH_ROOT <- "ingc"
    RUN_setup$IRR_RS_DIR <- paste(RUN_setup$B_DIR,"/irrigated_ratio",sep="")
    RUN_setup$IRR_RS_PREFIX <- "raw-"
    RUN_setup$IRR_RS_EXT <- ".asc"
    
    base_RUN_setup <- RUN_setup
    
    cat("\nprocessing cell",CAL_setup$CELL,"\n")
    
    #get defaults (parameter set)
    params <- GLAM_get_default(x=cells,cell=CAL_setup$CELL,parDir=pDir)
    params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
    params$glam_param.sim_ctr$NDSLA <- 1
    
    #load list of parameters to optimise, ranges, and number of steps
    opt_rules <- read.table(paste(pDir,"/optimisation-rules.txt",sep=""),sep="\t",header=T)
    #reorder optim rules
    if (!is.na(seed)) {
      set.seed(seed)
      reord <- sample(1:nrow(opt_rules),replace=F)
      opt_rules <- opt_rules[reord,]
      row.names(opt_rules) <- 1:nrow(opt_rules)
    }
    
    #extract irrigation rates
    irDir <- paste(cDir,"/irrigated_ratio",sep="")
    library(raster)
    ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
    ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==CAL_setup$CELL)],Y=cells$Y[which(cells$CELL==CAL_setup$CELL)]))
    ir_vls <- as.numeric(ir_vls)
    ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
    ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
    
    ###############################################
    # final calibration of YGP in parts for assessing GLAM
    ###############################################
    #part 1: calib and test
    #part 2: test and calib
    #part 3: compare YGP values from part1-calib and part2-calib
    
    ###############################################
    #load the calib.csv, last iteration
    cal_data <- read.csv(paste(CAL_setup$CAL_DIR,"/optimisation/z",CAL_setup$ZONE,"_rfd_irr/calib.csv",sep=""))
    optimal <- cal_data[which(cal_data$iter==maxiter),]
    
    #update the parameter set
    for (rw in 1:nrow(optimal)) {
      pname <- paste(optimal$param[rw])
      where <- paste(optimal$sect[rw])
      
      if (pname == "TB" | pname == "TO" | pname == "TM") {
        params[[where]][[paste(pname,"FLWR",sep="")]][,"Value"] <- optimal$opt_val[rw]
        params[[where]][[paste(pname,"PODF",sep="")]][,"Value"] <- optimal$opt_val[rw]
        params[[where]][[paste(pname,"LMAX",sep="")]][,"Value"] <- optimal$opt_val[rw]
        params[[where]][[paste(pname,"HARV",sep="")]][,"Value"] <- optimal$opt_val[rw]
      } else {
        params[[where]][[pname]][,"Value"] <- optimal$opt_val[rw]
      }
    }
    
    ######################################################################################
    ######################################################################################
    ######################################################################################
    #part 1: calib and test
    ######################################################################################
    ######################################################################################
    ######################################################################################
    ##
    #inner cal dir
    CAL_setup$CAL_DIR <- paste(CAL_setup$CAL_DIR,"/half_half_ygp",sep="")
    
    #calib for the first 14 years of the time series
    #and then run with that YGP value the latter part of the time series
    params$glam_param.mod_mgt$ISYR <- 1966 #start year
    params$glam_param.mod_mgt$IEYR <- 1979 #end year
    
    #correct ir_vls for years i'm interested in
    #ir_vls_sel <- ir_vls[which(ir_vls$YEAR >= params$glam_param.mod_mgt$ISYR & ir_vls$YEAR <= params$glam_param.mod_mgt$IEYR),]
    
    #if using scratch directory instead of nfs
    if (use_scratch) {CAL_setup$SCRATCH <- paste(scratch,"/exp-",expID,"_hhp1",sep="")}
    
    #this particular simulation name (half-half-part-1 = hhp1)
    CAL_setup$SIM_NAME <- paste("hhp1_",CAL_setup$CELL,sep="")
    
    #################################################################################
    #run the optimiser for YGP, 20 steps
    parname <- "YGP"
    where <- "glam_param.ygp"
    nstep <- 100
    params[[where]][[parname]][,"Min"] <- 0.01
    params[[where]][[parname]][,"Max"] <- 1.00
    
    if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/ygp.RData",sep=""))) {
      # reset lists of output parameters
      optimal <- list(); optimised <- list()
      
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=CAL_setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))) {
        dir.create(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""),recursive=T)
      }
      save(list=c("optimised","optimal"),file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/ygp.RData",sep=""))
      save(list=c("CAL_setup"),file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/CAL_setup.RData",sep=""))
      
      #copy outputs from each run
      best_run <- which(optimised[[parname]][["VALUE"]] == optimal[[parname]])
      #if irrigated run exists then copy everything from it
      rd_irr <- paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/iter-ygp/ygp/IRR_run-",best_run,"_",optimal[[parname]],sep="")
      rd_rfd <- paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/iter-ygp/ygp/RFD_run-",best_run,"_",optimal[[parname]],sep="")
      
      if (file.exists(rd_irr)) {
        x <- file.copy(from=paste(rd_irr,"/filenames-",tolower(cropName),"-run.txt",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))
        x <- file.copy(from=paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run.txt",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))
        x <- file.copy(from=paste(rd_irr,"/output/",tolower(cropLong),".out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/",tolower(cropLong),"_IRR.out",sep=""))
      }
      
      if (file.exists(rd_rfd)) {
        x <- file.copy(from=paste(rd_irr,"/filenames-",tolower(cropName),"-run.txt",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""),overwrite=T)
        
        if (file.exists(paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep=""))) {
          x <- file.copy(from=paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep=""),
                         to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))
        } else {
          x <- file.copy(from=paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run.txt",sep=""),
                         to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep=""))
        }
        
        x <- file.copy(from=paste(rd_irr,"/output/",tolower(cropLong),".out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/",tolower(cropLong),"_RFD.out",sep=""))
      }
      system(paste("rm -rf ",CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/iter-ygp",sep=""))
    } else {
      load(file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/ygp.RData",sep=""))
      load(file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/CAL_setup.RData",sep=""))
    }
    
    #now perform model run with that value of ygp
    #params$glam_param.ygp$YGP$Value <- optimal$YGP
    if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test/RUN_setup.RData",sep=""))) {
      #grab required run data and perform the run
      #initial run configuration
      RUN_setup <- base_RUN_setup
      RUN_setup$RUNS_DIR <- CAL_setup$SCRATCH
      RUN_setup$YGP <- optimal$YGP
      RUN_setup$YEARS <- 1980:1993
      
      #load irrigation data
      cell_xy <- read.csv(RUN_setup$GRID)
      RUN_setup$IDATA <- load_irr_data(rs_dir=RUN_setup$IRR_RS_DIR,
                                       rs_prefix=RUN_setup$IRR_RS_PREFIX,yi=min(RUN_setup$YEARS),
                                       yf=max(RUN_setup$YEARS),xy=cbind(x=cell_xy$X,y=cell_xy$Y),
                                       ext=RUN_setup$IRR_RS_EXT,cell_ids=cell_xy$CELL)
      
      #configure GLAM run
      RUN_setup <- GLAM_config(RUN_setup,force="no")
      RUN_setup$PARAM_IRR$glam_param.mod_mgt$ISYR <- 1980
      RUN_setup$PARAM_IRR$glam_param.mod_mgt$IEYR <- 1993
      RUN_setup$PARAM_RFD$glam_param.mod_mgt$ISYR <- 1980
      RUN_setup$PARAM_RFD$glam_param.mod_mgt$IEYR <- 1993
      
      #make this particular glam run
      RUN_setup <- GLAM_run(RUN_setup)
      
      #copy the outputs
      if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))) {
        dir.create(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""),recursive=T)
      }
      
      #copying output files
      if (RUN_setup$RUN_TYPE=="RFD") {
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-rfd.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
      } else if (RUN_setup$RUN_TYPE=="RFD") {
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-irr.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
      } else {
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-rfd.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-irr.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
      }
      
      #remove scratch folder
      system(paste("rm -rf ",RUN_setup$RUN_DIR,sep=""))
      
      #save run config
      save(list=c("RUN_setup"),file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test/RUN_setup.RData",sep=""))
    } else {
      load(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test/RUN_setup.RData",sep=""))
    }
    
    
    ######################################################################################
    ######################################################################################
    ######################################################################################
    #part 2: test and calib
    ######################################################################################
    ######################################################################################
    ######################################################################################
    ##
    #calib for the first 14 years of the time series
    #and then run with that YGP value the latter part of the time series
    params$glam_param.mod_mgt$ISYR <- 1980 #start year
    params$glam_param.mod_mgt$IEYR <- 1993 #end year
    
    #correct ir_vls for years i'm interested in
    #ir_vls_sel <- ir_vls[which(ir_vls$YEAR >= params$glam_param.mod_mgt$ISYR & ir_vls$YEAR <= params$glam_param.mod_mgt$IEYR),]
    
    #if using scratch directory instead of nfs
    if (use_scratch) {CAL_setup$SCRATCH <- paste(scratch,"/exp-",expID,"_hhp2",sep="")}
    
    #this particular simulation name (half-half-part-1 = hhp1)
    CAL_setup$SIM_NAME <- paste("hhp2_",CAL_setup$CELL,sep="")
    
    #################################################################################
    #run the optimiser for YGP, 20 steps
    parname <- "YGP"
    where <- "glam_param.ygp"
    nstep <- 100
    params[[where]][[parname]][,"Min"] <- 0.01
    params[[where]][[parname]][,"Max"] <- 1.00
    
    if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/ygp.RData",sep=""))) {
      # reset lists of output parameters
      optimal <- list(); optimised <- list()
      
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=CAL_setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))) {
        dir.create(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""),recursive=T)
      }
      save(list=c("optimised","optimal"),file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/ygp.RData",sep=""))
      save(list=c("CAL_setup"),file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/CAL_setup.RData",sep=""))
      
      #copy outputs from each run
      best_run <- which(optimised[[parname]][["VALUE"]] == optimal[[parname]])
      #if irrigated run exists then copy everything from it
      rd_irr <- paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/iter-ygp/ygp/IRR_run-",best_run,"_",optimal[[parname]],sep="")
      rd_rfd <- paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/iter-ygp/ygp/RFD_run-",best_run,"_",optimal[[parname]],sep="")
      
      if (file.exists(rd_irr)) {
        x <- file.copy(from=paste(rd_irr,"/filenames-",tolower(cropName),"-run.txt",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))
        x <- file.copy(from=paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run.txt",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))
        x <- file.copy(from=paste(rd_irr,"/output/",tolower(cropLong),".out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/",tolower(cropLong),"_IRR.out",sep=""))
      }
      
      if (file.exists(rd_rfd)) {
        x <- file.copy(from=paste(rd_irr,"/filenames-",tolower(cropName),"-run.txt",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""),overwrite=T)
        
        if (file.exists(paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep=""))) {
          x <- file.copy(from=paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep=""),
                         to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train",sep=""))
        } else {
          x <- file.copy(from=paste(rd_irr,"/glam-r2-param-",tolower(cropName),"-run.txt",sep=""),
                         to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep=""))
        }
        
        x <- file.copy(from=paste(rd_irr,"/output/",tolower(cropLong),".out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/",tolower(cropLong),"_RFD.out",sep=""))
      }
      system(paste("rm -rf ",CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/iter-ygp",sep=""))
    } else {
      load(file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/ygp.RData",sep=""))
      load(file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/train/CAL_setup.RData",sep=""))
    }
    
    #now perform model run with that value of ygp
    #params$glam_param.ygp$YGP$Value <- optimal$YGP
    if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test/RUN_setup.RData",sep=""))) {
      #grab required run data and perform the run
      #initial run configuration
      RUN_setup <- base_RUN_setup
      RUN_setup$RUNS_DIR <- CAL_setup$SCRATCH
      RUN_setup$YGP <- optimal$YGP
      RUN_setup$YEARS <- 1966:1979
      
      #load irrigation data
      cell_xy <- read.csv(RUN_setup$GRID)
      RUN_setup$IDATA <- load_irr_data(rs_dir=RUN_setup$IRR_RS_DIR,
                                       rs_prefix=RUN_setup$IRR_RS_PREFIX,yi=min(RUN_setup$YEARS),
                                       yf=max(RUN_setup$YEARS),xy=cbind(x=cell_xy$X,y=cell_xy$Y),
                                       ext=RUN_setup$IRR_RS_EXT,cell_ids=cell_xy$CELL)
      
      #configure GLAM run
      RUN_setup <- GLAM_config(RUN_setup,force="no")
      RUN_setup$PARAM_IRR$glam_param.mod_mgt$ISYR <- 1966
      RUN_setup$PARAM_IRR$glam_param.mod_mgt$IEYR <- 1979
      RUN_setup$PARAM_RFD$glam_param.mod_mgt$ISYR <- 1966
      RUN_setup$PARAM_RFD$glam_param.mod_mgt$IEYR <- 1979
      
      #make this particular glam run
      RUN_setup <- GLAM_run(RUN_setup)
      
      #copy the outputs
      if (!file.exists(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))) {
        dir.create(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""),recursive=T)
      }
      
      #copying output files
      if (RUN_setup$RUN_TYPE=="RFD") {
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-rfd.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
      } else if (RUN_setup$RUN_TYPE=="RFD") {
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-irr.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
      } else {
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-rfd.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
        x <- file.copy(from=paste(RUN_setup$RUN_DIR,"/output/",tolower(cropLong),"-irr.out",sep=""),
                       to=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test",sep=""))
      }
      
      #remove scratch folder
      system(paste("rm -rf ",RUN_setup$RUN_DIR,sep=""))
      
      #save run config
      save(list=c("RUN_setup"),file=paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test/RUN_setup.RData",sep=""))
    } else {
      load(paste(CAL_setup$CAL_DIR,"/",CAL_setup$SIM_NAME,"/test/RUN_setup.RData",sep=""))
    }
  }
}



