#Julian Ramirez-Villegas
#UoL / CCAFS
#Jul 2014

#single run of DSSAT CSM for a given set of inputs. This run will
#1. create directory (a single directory per run is needed)
#2. copy all .CDE, .SDA, .WDA, DSSATPRO.L45, MODEL.ERR, DSCSM045.EXE
#3. write meteorology and soil files
#4. write parameter files (.CUL, .ECO, .SPE)
#5. write DSSBatch.v45

#this function requires other functions to be loaded into memory (see repo dir ./robustness/)

#an object of type _list_ is the only input to this function. such object has the following 
#elements:
#run_data$MODEL: name of model within CSM e.g. "MZCER045", "MZIXM045"
#run_data$BASE_DIR: directory where all runs are stored --a directory with run will be created inside
#run_data$BIN_DIR: path where compiled executable and all related files are
#run_data$WTH_DIR: path where meteorology file (meteo_cell-$LOC$.met) is
#run_data$LOC: id of location (e.g. 1792)
#run_data$LON: longitude of location, extracted as xy_main$x[which(xy_main$LOC == run_data$LOC)]
#run_data$LAT: latitude of location extracted as xy_main$y[which(xy_main$LOC == run_data$LOC)]
#run_data$RUN_ID: id of run, constructed using run_data$LOC
#run_data$ME: <- xy_main$ME[which(xy_main$LOC == run_data$LOC)]
#run_data$ISYR: first year of simulation
#run_data$IEYR: last year of simulation
#run_data$SOW_DATE: sowing date of run, extracted as xy_main$SOW_DATE1[which(xy_main$LOC == run_data$LOC)]
#run_data$SOW_WINDOW: sowing window (to set PLAST in *AUTOMATIC MANAGEMENT)
#run_data$SOILS: a list with all soilfile info (see ./robustness/dssat-utils/make_soilfile.R)
#run_data$CUL: cultivar parameters
#run_data$ECO: ecotype parameters
#run_data$SPE: species parameters
#run_data$XFILE: list with experimental details (see ./robustness/dssat-utils/make_xfile.R)

#start of function
run_dssat <- function(run_data) {
  #set variable names
  base_dir <- run_data$BASE_DIR
  run_id <- run_data$RUN_ID
  model <- run_data$MODEL
  basename <- run_data$BASENAME
  years <- run_data$ISYR:run_data$IEYR
  
  #create directory (and structure) for run
  run_dir <- paste(base_dir,"/",run_id,sep="")
  if (!file.exists(run_dir)) {dir.create(run_dir,recursive=T)}
  
  #verify model and parameters match, and write param files
  if (model == "MZCER045") {
    #cultivars
    if (length(which(names(run_data$CUL) %in% c("P1","P2","P5","G2","G3","PHINT")) == 6)) {
      run_data$CUL$CUL_ID <- "GE0001"; run_data$CUL$CUL_NAME <- "generic"
      run_data$CUL$EXPNO <- "."; run_data$CUL$ECO_ID <- "IB0001"
      culfile <- make_culpar(run_data$CUL, paste(run_dir,"/MZCER045.CUL",sep=""),overwrite=T)
    } else {
      stop("missing / wrong CUL parameters for selected model (CER)")
    }
    
    #ecotype
    if (length(which(names(run_data$ECO) %in% c("DSGFT","RUE","KCAN","TSEN","CDAY")) == 5)) {
      run_data$ECO$ECO_ID <- "IB0001"; run_data$ECO$ECO_NAME <- "generic"
      run_data$ECO$TBASE <- 8; run_data$ECO$TOPT <- 34; run_data$ECO$ROPT <- 34
      run_data$ECO$P20 <- 12.5; run_data$ECO$DJTI <- 4; run_data$ECO$GDDE <- 6
      ecofile <- make_ecopar(run_data$ECO, paste(run_dir,"/MZCER045.ECO",sep=""),overwrite=T)
    } else {
      stop("missing / wrong ECO parameters for selected model (CER)")
    }
    
    #species
    if (length(run_data$SPE) == 12) {
      spefile <- make_spepar(run_data$SPE, paste(run_dir,"/MZCER045.SPE",sep=""),overwrite=T)
    } else {
      stop("missing / wrong SPE parameters for selected model (CER)")
    }
  } else if (model == "MZIXM045") {
    #cultivars
    if (length(which(names(run_data$CUL) %in% c("P1","P2","P5","G2","G3","PHINT","AX","LX")) == 8)) {
      run_data$CUL$CUL_ID <- "GE0001"; run_data$CUL$CUL_NAME <- "generic"
      run_data$CUL$EXPNO <- "."; run_data$CUL$ECO_ID <- "IB0001"
      culfile <- make_culpar(run_data$CUL, paste(run_dir,"/MZIXM045.CUL",sep=""),overwrite=T)
    } else {
      stop("missing / wrong CUL parameters for selected model (IXM)")
    }
    
    #ecotype
    if (length(which(names(run_data$ECO) %in% c("DSGFT","RUE","KCAN","PSTM","PEAR","TSEN","CDAY")) == 7)) {
      run_data$ECO$ECO_ID <- "IB0001"; run_data$ECO$ECO_NAME <- "generic"
      run_data$ECO$TBASE <- 8; run_data$ECO$TOPT <- 34; run_data$ECO$ROPT <- 34
      run_data$ECO$P20 <- 12.5; run_data$ECO$DJTI <- 4; run_data$ECO$GDDE <- 6
      ecofile <- make_ecopar(run_data$ECO, paste(run_dir,"/MZIXM045.ECO",sep=""),overwrite=T)
    } else {
      stop("missing / wrong ECO parameters for selected model (IXM)")
    }
    
    #species
    if (length(run_data$SPE) == 22) {
      spefile <- make_spepar(run_data$SPE, paste(run_dir,"/MZIXM045.SPE",sep=""),overwrite=T)
    } else {
      stop("missing / wrong SPE parameters for selected model (IXM)")
    }
  } else {
    stop("run_data$MODEL has to be MZCER045 or MZIXM045")
  }
  
  #copy (if file exist) or create weather files 
  #set weather filename, check existence, copy if exists, else create using .met file
  nyears <- length(years)
  wthfil <- paste(basename,substr(paste(run_data$ISYR),3,4),nyears,".WTH",sep="")
  wthfil_raw <- paste(basename,substr(paste(run_data$ISYR),3,4),nyears,"_loc-",run_data$LOC,".WTH",sep="")
  if (file.exists(paste(run_data$WTH_DIR,"/loc-",run_data$LOC,"/",wthfil_raw,sep=""))) {
    cpfil <- file.copy(paste(run_data$WTH_DIR,"/loc-",run_data$LOC,"/",wthfil_raw,sep=""),paste(run_dir,"/",wthfil,sep=""))
    wthfil <- list(WTH_DIR=paste(run_dir,"/inputs/ascii/wth",sep=""))
  } else {
    fildir <- make_wth(x=data.frame(CELL=run_data$LOC,X=run_data$LON,Y=run_data$LAT,ELEV=run_data$ELEV),
                       wthDir_in=run_data$WTH_DIR,
                       wthDir_out=run_dir,
                       years=years,fields=list(CELL="CELL",X="X",Y="Y",ELEV="ELEV"),
                       out_file=wthfil)
    
  }
  
  #write soil file
  soilfil <- make_soilfile(run_data$SOILS, paste(run_dir,"/SOIL.SOL",sep=""), overwrite=T)
  
  #write xfile
  run_data$XFILE$sim_ctrl$VBOSE <- "0" #write only Summary.OUT outputs (as needed)
  xfil <- make_xfile(run_data$XFILE, paste(run_dir,"/",basename,substr(paste(run_data$ISYR),3,4),"01.MZX",sep=""),overwrite=T)
  
  #write DSSBatch.v45
  dssfil <- file(paste(run_dir,"/DSSBatch.v45",sep=""),open="w")
  cat("$BATCH(MAIZE)\n",file=dssfil)
  cat("@FILEX                                                                                        TRTNO     RP     SQ     OP     CO\n",file=dssfil)
  for (i in 1:nrow(run_data$XFILE$treatments)) {
    cat(paste(sprintf("%-92s",paste(basename,substr(paste(run_data$ISYR),3,4),"01.MZX",sep=""))," ",
              sprintf("%6d",as.integer(run_data$XFILE$treatments$N[i]))," ",
              sprintf("%6d",as.integer(run_data$XFILE$treatments$R[i]))," ",
              sprintf("%6d",as.integer(0))," ", #assume this is for sequential runs (i.e. irrelevant here)
              sprintf("%6d",as.integer(run_data$XFILE$treatments$O[i]))," ",
              sprintf("%6d",as.integer(run_data$XFILE$treatments$C[i]))," ",sep=""),file=dssfil)
  }
  close(dssfil)
  
  #copy model (-fp to *force and *preserve_attributes)
  system(paste("cp -fp ",run_data$BIN_DIR,"/DSCSM045.EXE ",run_dir,"/.",sep=""))
  
  #copy needed files (.CDE, .WDA, .SDA, DSSATPRO.L45, MODEL.ERR)
  system(paste("cp -fp ",run_data$BIN_DIR,"/*.CDE ",run_dir,"/.",sep=""))
  system(paste("cp -fp ",run_data$BIN_DIR,"/*.WDA ",run_dir,"/.",sep=""))
  system(paste("cp -fp ",run_data$BIN_DIR,"/*.SDA ",run_dir,"/.",sep=""))
  system(paste("cp -fp ",run_data$BIN_DIR,"/DSSATPRO.L45 ",run_dir,"/.",sep=""))
  system(paste("cp -fp ",run_data$BIN_DIR,"/MODEL.ERR ",run_dir,"/.",sep=""))
  
  #go to dir, run model, return to where i am
  thisdir <- getwd(); setwd(run_dir); system(paste("rm -f *.OUT && ./DSCSM045.EXE ",model," B DSSBatch.v45",sep="")); setwd(thisdir)
  
  #return run_dir, and out_file for copying of file
  run_data$OUT_FILES <- list.files(run_dir,pattern="\\.OUT")
  run_data$RUN_DIR <- run_dir
  
  #return object
  return(run_data)
}


