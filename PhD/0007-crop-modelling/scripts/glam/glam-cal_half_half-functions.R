#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012


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
    setup <- list()
    setup$BDIR <- bDir
    setup$SCRATCH <- scratch
    setup$USE_SCRATCH <- use_scratch
    setup$CELL <- cells$CELL[ci]
    setup$ZONE <- cells$ZONE[ci]
    setup$METHOD <- "lin"
    setup$CROPNAME <- "gnut"
    setup$CAL_DIR <- paste(setup$BDIR,"/model-runs/",toupper(setup$CROPNAME),"/calib/exp-",expID,"_outputs",sep="")
    setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
    setup$YGP_FILE <- "nofile"
    setup$SOW_FILE_RFD <- paste(setup$CAL_DIR,"/gridcells/fcal_",setup$CELL,"/opt_fcal_",setup$CELL,".txt",sep="")
    setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
    setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
    setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
    setup$WTH_ROOT <- "ingc"
    setup$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",setup$CELL,".txt",sep="")
    setup$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
    setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
    
    #if using scratch directory instead of nfs
    if (use_scratch) {setup$SCRATCH <- paste(setup$SCRATCH,"/exp-",expID,"_hhp1",sep="")}
    
    cat("\nprocessing cell",setup$CELL,"\n")
    
    #get defaults (parameter set)
    params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
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
    ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
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
    cal_data <- read.csv(paste(setup$CAL_DIR,"/optimisation/z",setup$ZONE,"_rfd_irr/calib.csv",sep=""))
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
    
    ###############################################
    #part 1: calib and test
    ##
    #calib for the first 14 years of the time series
    #and then run with that YGP value the latter part of the time series
    params$glam_param.mod_mgt$ISYR <- 1966 #start year
    params$glam_param.mod_mgt$IEYR <- 1979 #end year
    
    #where this specific run will be put
    setup$CAL_DIR <- paste(setup$CAL_DIR,"/half_half_ygp",sep="")
    setup$SIM_NAME <- paste("hhp1_",setup$CELL,sep="")
    
    #################################################################################
    #run the optimiser for YGP, 20 steps
    parname <- "YGP"
    where <- "glam_param.ygp"
    nstep <- 100
    params[[where]][[parname]][,"Min"] <- 0.01
    params[[where]][[parname]][,"Max"] <- 1.00
    
    if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/ygp.RData",sep=""))) {
      # reset lists of output parameters
      optimal <- list(); optimised <- list()
      
      optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                param=parname,n.steps=nstep,iter=tolower(parname),
                                                iratio=ir_vls)
      
      optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
      cat(parname,":",optimal[[parname]],"\n")
      if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
      
      save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
      
      if (plot_all) {
        tiff(paste(plotsDir,"/",tolower(parname),".tif",sep=""),res=300,compression="lzw",height=1000,
             width=1250,pointsize=8)
        par(mar=c(3,3,2,1))
        plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",
             main=paste(parname," :: ",optimal[[parname]],sep=""),
             xlab="Parameter value",ylab="RMSE (kg/ha)")
        grid(nx=10,ny=10)
        abline(v=optimal[[parname]],col="red",lty=2,lwd=0.8)
        dev.off()
      }
      
    }
  }
}


