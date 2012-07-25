#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2012

#get parameter set from a given GLAM run
#this is gridcell specific
GLAM_get_run_param <- function(asc_dir,cal_dir,exp_dir,crop="gnut",prefix="fcal_",gridcell,ygp="opt",ipdate="rfd") {
  #if yield gap parameter is =="opt" then grab the optimal ygp from ygp.RData
  if (!is.numeric(ipdate)) {
    if (!ipdate %in% c("rfd","irr")) {
      warning("ipdate was mis-specified, optimal rainfed sowing dates will be retrieved")
      ipdate <- "rfd"
    }
  } else {
    if (ipdate > 365) {
      warning("ipdate is above 365, using 160 as default")
      ipdate <- 160
    }
  }
  
  #directory with run data
  run_dir <- paste(cal_dir,"/",exp_dir,"/gridcells/",prefix,gridcell,sep="")
  
  #grab ygp value
  if (ygp == "opt") {
    ygp_file <- paste(run_dir,"/ygp.RData",sep="")
    load(ygp_file)
    ygp_value <- optimal$YGP
    rm(optimal); rm(optimised); g=gc(); rm(g)
  } else {
    ygp_value <- ygp
  }
  
  #grab ipdate value
  if (ipdate == "rfd") {
    ipd_file <- paste(run_dir,"/ipdate.RData",sep="")
    load(ipd_file)
    ipd_value <- optimal$IPDATE
    rm(optimal); rm(optimised); g=gc(); rm(g)
  } else if (ipdate == "irr") {
    ipd_file <- paste(asc_dir,"/sow/sowing_",gridcell,"_irr.txt",sep="")
    ipd_value <- read.fortran(ipd_file,format=c("2I4","I6"))
    ipd_value <- as.numeric(ipd_value)[3]
  } else {
    ipd_value <- ipdate
  }
  
  #name of parameter file
  par_file <- paste(run_dir,"/glam-r2-param-",tolower(crop),"-run.txt",sep="")
  if (!file.exists(par_file)) {
    par_file <- paste(run_dir,"/glam-r2-param-",tolower(crop),"-run-rfd.txt",sep="")
  }
  
  #update parameter file
  params <- GLAM_get_par(par_file,retain="all") #get run parameter set
  params$glam_param.ygp$YGP$Value <- ygp_value #replace ygp
  params$glam_param.spt_mgt$IPDATE$Value <- ipd_value #replace ipdate
  
  return(params)
}


#get soil data files for a given gridcell
get_soil_files <- function(run_setup,asc_dir,gridcell,codes_prefix="soilcodes_",types_prefix="soiltypes_") {
  if (!is.list(run_setup)) {
    warning("run_setup has to be a list, so an empty one was created")
    run_setup <- list()
  }
  soil_dir <- paste(asc_dir,"/soil",sep="") #soil data folder
  scod_file <- paste(soil_dir,"/",codes_prefix,gridcell,".txt",sep="") #soil codes file
  styp_file <- paste(soil_dir,"/",types_prefix,gridcell,".txt",sep="") #soil types file
  
  #soil codes filename into list
  if (file.exists(scod_file)) {
    run_setup$SOILCODES <- scod_file
  } else {
    run_setup$SOILCODES <- "nofile"
  }
  
  #soil types filename into list
  if (file.exists(scod_file)) {
    run_setup$SOILTYPES <- styp_file #assign value to list
  } else {
    run_setup$SOILCODES <- "nofile"
  }
  return(run_setup)
}


#get wth data
get_wth_data <- function(run_setup,asc_dir,gridcell,wth_root,run_type="rfd",yi=1966,yf=1993) {
  if (!is.list(run_setup)) {
    warning("run_setup has to be a list, so an empty one was created")
    run_setup <- list()
  }
  
  if (!run_type %in% c("rfd","irr")) {
    stop("run_type was mis-specified")
  }
  
  wth_dir <- paste(asc_dir,"/wth",sep="") #wth data folder
  wth_dir <- paste(wth_dir,"/",run_type,"_",gridcell,sep="")
  
  count <- 0
  for (yr in yi:yf) {
    wth_file <- paste(wth_dir,"/",wth_root,"001001",yr,".wth",sep="")
    if (file.exists(wth_file)) {count <- count+1}
  }
  
  if (count == length(yi:yf)) {
    if (run_type == "rfd") {
      run_setup$WTH_DIR_RFD <- wth_dir
    } else {
      run_setup$WTH_DIR_IRR <- wth_dir
    }
  } else {
    if (run_type == "rfd") {
      run_setup$WTH_DIR_RFD <- paste(length(yi:yf)-(count)," missing files",sep="")
    } else {
      run_setup$WTH_DIR_IRR <- paste(length(yi:yf)-(count)," missing files",sep="")
    }
  }
  run_setup$WTHROOT <- wth_root
  #run_setup$RUN_TYPE <- run_type
  return(run_setup)
}


#load irrigation data
load_irr_data <- function(rs_dir,rs_prefix="raw-",yi=1966,yf=1993,xy,ext=".asc",cell_ids=NA) {
  rs_list <- paste(rs_dir,"/",rs_prefix,yi:yf,ext,sep="")
  if (require(raster) & require(rgdal)) {rstk <- stack(rs_list)} #load raster
  irr_data <- as.data.frame(extract(rstk,xy))
  names(irr_data) <- paste("Y",yi:yf,sep="")
  irr_data <- cbind(CELL=cell_ids,X=xy[,1],Y=xy[,2],irr_data)
  return(irr_data)
}


#get irr data (data and sowing date)
get_irr_data <- function(irr_data,gridcell,yi=1966,yf=1993) {
  #check the data is in proper format
  if (!is.data.frame(irr_data)) {
    stop("irr_data must be a data frame")
  }
  irr_data$X <- NULL; irr_data$Y <- NULL
  #check the number of years is present
  if (length(grep("Y",names(irr_data))) != length(yi:yf)) {
    stop("number of years does not match")
  }
  
  #grab irrigated fraction data
  iratio <- as.numeric(irr_data[which(irr_data$CELL==gridcell),paste("Y",yi:yf,sep="")])
  iratio <- data.frame(YEAR=yi:yf,IRR_RATIO=iratio)
  iratio$IRR_RATIO[which(iratio$IRR_RATIO > 1)] <- 1
  return(iratio)
}


#configure a given model run
################################################################################
#force is a variable that will assume the gridcell you're simulating
#is either rainfed (rfd), irrigated (irr), or the data-defined one (no)
GLAM_config <- function(run_setup,force="no") {
  cat("configuring the GLAM run...\n")
  #creating output base folder
  if (!file.exists(run_setup$RUNS_DIR)) {dir.create(run_setup$RUNS_DIR,recursive=T)}
  
  #load grid
  g_xy <- read.csv(run_setup$GRID)
  g_ids <- g_xy$CELL
  g_xy <- cbind(x=g_xy$X,y=g_xy$Y)
  
  #get irrigation data
  #if this is a forced rainfall run then set all irrigation ratios to zero
  if (force == "rfd") {
    run_setup$IRATIO <- data.frame(YEAR=run_setup$YEARS,IRATIO=0)
  } else if (force == "irr") {
    run_setup$IRATIO <- data.frame(YEAR=run_setup$YEARS,IRATIO=1)
  } else {
    idata <- run_setup$IDATA
    idata <- get_irr_data(irr_data=idata,gridcell=run_setup$GRIDCELL,yi=min(run_setup$YEARS),
                          yf=max(run_setup$YEARS))
    run_setup$IRATIO <- idata
  }
  
  #create run dir
  run_setup$RUN_DIR <- create_dirs(paste(run_setup$RUNS_DIR,"/run_",run_setup$GRIDCELL,sep=""))
  
  #get parameter set(s): if there is some irrigated fraction then grab
  #the two parameter sets run_type <- "rfd" or "irr", accordingly
  if (length(which(run_setup$IRATIO$IRR_RATIO == 0)) == nrow(run_setup$IRATIO)) {
    #no irrigation, so only rainfed
    run_setup$PARAM_RFD <- GLAM_get_run_param(asc_dir=run_setup$ASC_DIR,cal_dir=run_setup$CAL_DIR,
                                   exp_dir=run_setup$EXP_DIR,crop=run_setup$CROP,
                                   prefix=run_setup$PREFIX,gridcell=run_setup$GRIDCELL,
                                   ygp=run_setup$YGP,ipdate="rfd")
    run_setup$RUN_TYPE <- "RFD"
  } else if (length(which(run_setup$IRATIO$IRR_RATIO == 1)) == nrow(run_setup$IRATIO)) {
    #get only irrigated parameter set
    run_setup$PARAM_IRR <- GLAM_get_run_param(asc_dir=run_setup$ASC_DIR,cal_dir=run_setup$CAL_DIR,
                                   exp_dir=run_setup$EXP_DIR,crop=run_setup$CROP,
                                   prefix=run_setup$PREFIX,gridcell=run_setup$GRIDCELL,
                                   ygp=run_setup$YGP,ipdate="irr")
    run_setup$RUN_TYPE <- "IRR"
  } else {
    #get both parameter sets
    run_setup$PARAM_RFD <- GLAM_get_run_param(asc_dir=run_setup$ASC_DIR,cal_dir=run_setup$CAL_DIR,
                                   exp_dir=run_setup$EXP_DIR,crop=run_setup$CROP,
                                   prefix=run_setup$PREFIX,gridcell=run_setup$GRIDCELL,
                                   ygp=run_setup$YGP,ipdate="rfd")
    run_setup$PARAM_IRR <- GLAM_get_run_param(asc_dir=run_setup$ASC_DIR,cal_dir=run_setup$CAL_DIR,
                                   exp_dir=run_setup$EXP_DIR,crop=run_setup$CROP,
                                   prefix=run_setup$PREFIX,gridcell=run_setup$GRIDCELL,
                                   ygp=run_setup$YGP,ipdate="irr")
    run_setup$RUN_TYPE <- "MIX"
  }
  
  #get soil files
  run_setup <- get_soil_files(run_setup,asc_dir=run_setup$ASC_DIR,gridcell=run_setup$GRIDCELL,
                              codes_prefix=run_setup$CODES_PREFIX,types_prefix=run_setup$TYPES_PREFIX)
  
  #get wth files (locations and wthroot)
  if (run_setup$RUN_TYPE == "RFD") {
    run_setup <- get_wth_data(run_setup,asc_dir=run_setup$ASC_DIR,gridcell=run_setup$GRIDCELL,
                              wth_root=run_setup$WTH_ROOT,run_type="rfd",yi=1966,yf=1993)
  } else if (run_setup$RUN_TYPE == "IRR") {
    run_setup <- get_wth_data(run_setup,asc_dir=run_setup$ASC_DIR,gridcell=run_setup$GRIDCELL,
                              wth_root=run_setup$WTH_ROOT,run_type="irr",yi=1966,yf=1993)
  } else {
    run_setup <- get_wth_data(run_setup,asc_dir=run_setup$ASC_DIR,gridcell=run_setup$GRIDCELL,
                              wth_root=run_setup$WTH_ROOT,run_type="rfd",yi=1966,yf=1993)
    
    run_setup <- get_wth_data(run_setup,asc_dir=run_setup$ASC_DIR,gridcell=run_setup$GRIDCELL,
                              wth_root=run_setup$WTH_ROOT,run_type="irr",yi=1966,yf=1993)
  }
  
  
  #which executable to copy
  machine <- as.data.frame(t(Sys.info())) #determine operating system and bin folder
  machine <- paste(machine$sysname)
  run_setup$EXEC_NAME <- paste("glam-",tolower(run_setup$CROP),sep="")
  run_setup$BIN_DIR <- paste(run_setup$BIN_DIR,"/glam-",tolower(machine),sep="")
  if (tolower(machine) == "windows") {
    run_setup$EXEC_NAME <- paste(run_setup$EXEC_NAME,".exe",sep="")
  } else if (tolower(machine) == "linux") {
    run_setup$EXEC_NAME <- paste("./",run_setup$EXEC_NAME,sep="")
  }
  
  #copy files that are necessary for this run
  #executable
  x <- file.copy(paste(run_setup$BIN_DIR,"/",run_setup$EXEC_NAME,sep=""),run_setup$RUN_DIR,overwrite=T)
  #soil codes
  x <- file.copy(run_setup$SOILCODES,paste(run_setup$RUN_DIR,"/inputs/ascii/soil",sep=""),overwrite=T)
  #soil types
  x <- file.copy(run_setup$SOILTYPES,paste(run_setup$RUN_DIR,"/inputs/ascii/soil",sep=""),overwrite=T)
  
  #copy weather. If both things (RFD & irr) have to be run then create extra folder
  if (run_setup$RUN_TYPE == "RFD") {
    x <- sapply(list.files(run_setup$WTH_DIR_RFD),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},run_setup$WTH_DIR_RFD,paste(run_setup$RUN_DIR,"/inputs/ascii/wth",sep=""))
  } else if (run_setup$RUN_TYPE == "IRR") {
    x <- sapply(list.files(run_setup$WTH_DIR_IRR),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},run_setup$WTH_DIR_IRR,paste(run_setup$RUN_DIR,"/inputs/ascii/wth",sep=""))
  } else {
    if (!file.exists(paste(run_setup$RUN_DIR,"/inputs/ascii/wth_irr",sep=""))) {dir.create(paste(run_setup$RUN_DIR,"/inputs/ascii/wth_irr",sep=""))}
    x <- sapply(list.files(run_setup$WTH_DIR_RFD),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},run_setup$WTH_DIR_RFD,paste(run_setup$RUN_DIR,"/inputs/ascii/wth",sep=""))
    x <- sapply(list.files(run_setup$WTH_DIR_IRR),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},run_setup$WTH_DIR_IRR,paste(run_setup$RUN_DIR,"/inputs/ascii/wth_irr",sep=""))
  }
  
  #return run_setup
  return(run_setup)
}


# ####function to run glam based on some particular configuration
GLAM_run <- function(run_setup) {
  cat("running the model for the selected setup...\n")
  #write parameter file
  if (run_setup$RUN_TYPE == "RFD") {
    run_setup$PARFILE_RFD <- paste(run_setup$RUN_DIR,"/glam-r2-param-",tolower(run_setup$CROP),"-run-rfd.txt",sep="")
    run_setup$PARFILE_RFD <- GLAM_create_parfile(params=run_setup$PARAM_RFD,outfile=run_setup$PARFILE_RFD,base_file=NA,overwrite=T)
  } else if (run_setup$RUN_TYPE == "IRR") {
    run_setup$PARFILE_IRR <- paste(run_setup$RUN_DIR,"/glam-r2-param-",tolower(run_setup$CROP),"-run-irr.txt",sep="")
    run_setup$PARFILE_IRR <- GLAM_create_parfile(params=run_setup$PARAM_IRR,outfile=run_setup$PARFILE_IRR,base_file=NA,overwrite=T)
  } else {
    run_setup$PARFILE_RFD <- paste(run_setup$RUN_DIR,"/glam-r2-param-",tolower(run_setup$CROP),"-run-rfd.txt",sep="")
    run_setup$PARFILE_RFD <- GLAM_create_parfile(params=run_setup$PARAM_RFD,outfile=run_setup$PARFILE_RFD,base_file=NA,overwrite=T)
    
    run_setup$PARFILE_IRR <- paste(run_setup$RUN_DIR,"/glam-r2-param-",tolower(run_setup$CROP),"-run-irr.txt",sep="")
    run_setup$PARFILE_IRR <- GLAM_create_parfile(params=run_setup$PARAM_IRR,outfile=run_setup$PARFILE_IRR,base_file=NA,overwrite=T)
  }
  
  #write filenames file
  if (run_setup$RUN_TYPE == "RFD") {
    run_setup$FNFILE_RFD <- paste("filenames-",tolower(run_setup$CROP),"-run-rfd.txt",sep="")
    parfile <- unlist(strsplit(run_setup$PARFILE_RFD,"/",fixed=T))[length(unlist(strsplit(run_setup$PARFILE_RFD,"/",fixed=T)))]
    wth_row <- paste("inputs/ascii/wth/",run_setup$WTH_ROOT,sep="")
    soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T)))],sep="")
    soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILCODES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILCODES,"/",fixed=T)))],sep="")
    sow_row <- "nofile"
    yield_row <- "nofile"
    ygp_row <- "nofile"
    
    fn <- file(paste(run_setup$RUN_DIR,"/",run_setup$FNFILE_RFD,sep=""),"w")
    cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
    cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
    close(fn)
  } else if (run_setup$RUN_TYPE == "IRR") {
    run_setup$FNFILE_IRR <- paste("filenames-",tolower(run_setup$CROP),"-run-irr.txt",sep="")
    parfile <- unlist(strsplit(run_setup$PARFILE_IRR,"/",fixed=T))[length(unlist(strsplit(run_setup$PARFILE_IRR,"/",fixed=T)))]
    wth_row <- paste("inputs/ascii/wth/",run_setup$WTH_ROOT,sep="")
    soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T)))],sep="")
    soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILCODES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILCODES,"/",fixed=T)))],sep="")
    sow_row <- "nofile"
    yield_row <- "nofile"
    ygp_row <- "nofile"
    
    fn <- file(paste(run_setup$RUN_DIR,"/",run_setup$FNFILE_IRR,sep=""),"w")
    cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
    cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
    close(fn)
  } else {
    #write rainfed file first
    run_setup$FNFILE_RFD <- paste("filenames-",tolower(run_setup$CROP),"-run-rfd.txt",sep="")
    parfile <- unlist(strsplit(run_setup$PARFILE_RFD,"/",fixed=T))[length(unlist(strsplit(run_setup$PARFILE_RFD,"/",fixed=T)))]
    wth_row <- paste("inputs/ascii/wth/",run_setup$WTH_ROOT,sep="")
    soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T)))],sep="")
    soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILCODES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILCODES,"/",fixed=T)))],sep="")
    sow_row <- "nofile"
    yield_row <- "nofile"
    ygp_row <- "nofile"
    
    fn <- file(paste(run_setup$RUN_DIR,"/",run_setup$FNFILE_RFD,sep=""),"w")
    cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
    cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
    close(fn)
    
    ###
    #write irrigated file now
    run_setup$FNFILE_IRR <- paste("filenames-",tolower(run_setup$CROP),"-run-irr.txt",sep="")
    parfile <- unlist(strsplit(run_setup$PARFILE_IRR,"/",fixed=T))[length(unlist(strsplit(run_setup$PARFILE_IRR,"/",fixed=T)))]
    wth_row <- paste("inputs/ascii/wth_irr/",run_setup$WTH_ROOT,sep="")
    soilty_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILTYPES,"/",fixed=T)))],sep="")
    soilco_row <- paste("inputs/ascii/soil/",unlist(strsplit(run_setup$SOILCODES,"/",fixed=T))[length(unlist(strsplit(run_setup$SOILCODES,"/",fixed=T)))],sep="")
    sow_row <- "nofile"
    yield_row <- "nofile"
    ygp_row <- "nofile"
    
    fn <- file(paste(run_setup$RUN_DIR,"/",run_setup$FNFILE_IRR,sep=""),"w")
    cat(sprintf("%-41s",parfile),"\n",sep="",file=fn)
    cat(sprintf("%-41s",wth_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilty_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",soilco_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",sow_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",yield_row),"\n",sep="",file=fn)
    cat(sprintf("%-41s",ygp_row),"\n",sep="",file=fn)
    close(fn)
  }
  
  ###
  #run model under given configuration
  if (run_setup$RUN_TYPE == "RFD") {
    #rainfed run
    run_setup$GLAM_CMD <- paste(run_setup$EXEC_NAME,run_setup$FNFILE_RFD)
    setwd(run_setup$RUN_DIR)
    system(run_setup$GLAM_CMD)
    
    #delete the exec file, .inf file and compress the daily files
    x <- file.remove(run_setup$EXEC_NAME)
    x <- file.remove("glam.inf")
    x <- file.copy(from=paste("./output/",run_setup$PARAM_RFD$glam_param.sim_ctr$RunID,".out",sep=""),
                   to=paste("./output/",run_setup$PARAM_RFD$glam_param.sim_ctr$RunID,"-rfd.out",sep=""))
    x <- file.remove(paste("./output/",run_setup$PARAM_RFD$glam_param.sim_ctr$RunID,".out",sep=""))
    
    #remove all input files
    setwd("./inputs/ascii/wth")
    x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/obs")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/soil")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/sow")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR)
    
    #output file name for easy read
    run_setup$OUT_FILE_RFD <- paste(run_setup$RUN_DIR,"/output/",run_setup$PARAM_RFD$glam_param.sim_ctr$RunID,"-rfd.out",sep="")
    
  } else if (run_setup$RUN_TYPE == "IRR") {
    #fully irrigated run
    run_setup$GLAM_CMD <- paste(run_setup$EXEC_NAME,run_setup$FNFILE_IRR)
    setwd(run_setup$RUN_DIR)
    system(run_setup$GLAM_CMD)
    
    #delete the exec file, .inf file and compress the daily files
    x <- file.remove(run_setup$EXEC_NAME)
    x <- file.remove("glam.inf")
    x <- file.copy(from=paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,".out",sep=""),
                   to=paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,"-irr.out",sep=""))
    x <- file.remove(paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,".out",sep=""))
    
    #remove all input files
    setwd("./inputs/ascii/wth")
    x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/obs")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/soil")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/sow")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR)
    
    #output file name for easy read
    run_setup$OUT_FILE_IRR <- paste(run_setup$RUN_DIR,"/output/",run_setup$PARAM_RFD$glam_param.sim_ctr$RunID,"-irr.out",sep="")
    
  } else {
    #first the rainfed run
    run_setup$GLAM_CMD <- paste(run_setup$EXEC_NAME,run_setup$FNFILE_RFD)
    setwd(run_setup$RUN_DIR)
    system(run_setup$GLAM_CMD)
    
    #delete the exec file, .inf file and compress the daily files
    x <- file.remove("glam.inf")
    x <- file.copy(from=paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,".out",sep=""),
                   to=paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,"-rfd.out",sep=""))
    x <- file.remove(paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,".out",sep=""))
    
    #output file name for easy read
    run_setup$OUT_FILE_RFD <- paste(run_setup$RUN_DIR,"/output/",run_setup$PARAM_RFD$glam_param.sim_ctr$RunID,"-rfd.out",sep="")
    
    ###
    #now the irrigated run
    run_setup$GLAM_CMD <- paste(run_setup$EXEC_NAME,run_setup$FNFILE_IRR)
    setwd(run_setup$RUN_DIR)
    system(run_setup$GLAM_CMD)
    
    #delete the exec file, .inf file and compress the daily files
    x <- file.remove(run_setup$EXEC_NAME)
    x <- file.remove("glam.inf")
    x <- file.copy(from=paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,".out",sep=""),
                   to=paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,"-irr.out",sep=""))
    x <- file.remove(paste("./output/",run_setup$PARAM_IRR$glam_param.sim_ctr$RunID,".out",sep=""))
    
    #remove all input files
    setwd("./inputs/ascii/wth")
    x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/wth_irr")
    x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/obs")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/soil")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs/ascii/sow")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR); setwd("./inputs")
    x <- sapply(list.files(".",pattern="\\.txt"),FUN= function(x) {s <- file.remove(x)})
    setwd(run_setup$RUN_DIR)
    
    #output file name for easy read
    run_setup$OUT_FILE_IRR <- paste(run_setup$RUN_DIR,"/output/",run_setup$PARAM_RFD$glam_param.sim_ctr$RunID,"-irr.out",sep="")
  }
  return(run_setup)
}


