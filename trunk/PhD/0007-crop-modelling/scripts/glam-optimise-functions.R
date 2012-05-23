#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

############################################################
####### function to optimise a given parameter in GLAM
############################################################
GLAM_optimise_rfd <- function(GLAM_params,sect="glam_param.ygp",param="YGP",n.steps=20,
                          cell=636,method="lin",cropName,bDir,iter=1,simset="test") {
  
  param <- toupper(param)
  
  #input directories and model
  cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
  binDir <- paste(bDir,"/model-runs/bin/glam",sep="")
  execName <- paste("glam-",tolower(cropName),sep="")
  
  #determine operating system
  machine <- as.data.frame(t(Sys.info()))
  machine <- paste(machine$sysname)
  
  if (tolower(machine) == "windows") {
    glam_cmd <- paste(paste(execName,".exe",sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
    execName <- paste(execName,".exe",sep="")
  } else if (tolower(machine) == "linux") {
    glam_cmd <- paste(paste("./",execName,sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
  }
  
  #output directories
  parDir <- paste(cropDir,"/params",sep="") #parameter files
  cal_dir <- paste(cropDir,"/calib",sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  cal_dir <- paste(cal_dir,"/",simset,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  cal_dir <- paste(cal_dir,"/iter-",iter,sep="")
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #files that were generated
  yFile <- paste(cropDir,"/inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
  sowFile <- paste(cropDir,"/inputs/ascii/sow/sowing_",cell,"_start.txt",sep="")
  wthDir <- paste(cropDir,"/inputs/ascii/wth",sep="")
  solFile <- paste(cropDir,"/inputs/ascii/soil/soiltypes_",cell,".txt",sep="")
  solGrid <- paste(cropDir,"/inputs/ascii/soil/soilcodes_",cell,".txt",sep="")
  
  #create optimisation folder if it does not exist
  optDir <- paste(cal_dir,"/",tolower(param),sep="")
  if (!file.exists(optDir)) {dir.create(optDir)}
  
  #create sequence of values
  if (param == "TB" | param == "TO" | param == "TM") {
    vals <- seq(GLAM_params[[sect]][[paste(param,"FLWR",sep="")]][,"Min"],GLAM_params[[sect]][[paste(param,"FLWR",sep="")]][,"Max"],length.out=n.steps)
  } else {
    vals <- seq(GLAM_params[[sect]][[param]][,"Min"],GLAM_params[[sect]][[param]][,"Max"],length.out=n.steps)
  }
  
  #loop through sequence of values
  for (i in 1:length(vals)) {
    cat("performing run",i,"value =",vals[i],"(",param,")","\n")
    
    if (param == "TB" | param == "TO" | param == "TM") {
      GLAM_params[[sect]][[paste(param,"FLWR",sep="")]][,"Value"] <- vals[i]
      GLAM_params[[sect]][[paste(param,"PODF",sep="")]][,"Value"] <- vals[i]
      GLAM_params[[sect]][[paste(param,"LMAX",sep="")]][,"Value"] <- vals[i]
      GLAM_params[[sect]][[paste(param,"HARV",sep="")]][,"Value"] <- vals[i]
    } else {
      GLAM_params[[sect]][[param]][,"Value"] <- vals[i]
    }
    
    #write the model params
    opfil <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
    opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
    
    #output folder
    run_dir <- create_dirs(paste(optDir,"/run-",i,"_",vals[i],sep=""))
    
    #check whether the *.out already exists
    outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
    if (length(outfile) == 0) {
      ######################################################
      #write filenames file
      parfile <- paste("glam-r2-param-",tolower(cropName),"-run.txt",sep="")
      wth_row <- "inputs/ascii/wth/ingc"
      soilty_row <- paste("inputs/ascii/soil/soiltypes_",cell,".txt",sep="")
      soilco_row <- paste("inputs/ascii/soil/soilcodes_",cell,".txt",sep="")
      sow_row <- paste("inputs/ascii/sow/sowing_",cell,"_start.txt",sep="")
      yield_row <- paste("inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
      ygp_row <- "nofile"
      
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
      x <- file.copy(paste(parDir,"/",parfile,sep=""),run_dir,overwrite=T)
      x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
      x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
      x <- file.copy(sowFile,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
      x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
      x <- sapply(list.files(wthDir),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir,paste(run_dir,"/inputs/ascii/wth",sep=""))
      
      #now run!
      setwd(run_dir)
      system(glam_cmd)
      
      #delete the exec file
      x <- file.remove(execName)
    } else {
      setwd(run_dir)
    }
    #read in the predicted yield
    outfile <- list.files("./output/",pattern="\\.out")
    pred <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
    names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                     "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                     "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                     "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                     "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
    y_p <- pred$YIELD
    y_o <- read.fortran(yFile,format=c("A12","F8"),n=28)
    y_o <- y_o$V2
    
    #calc rmse
    odf <- data.frame(YEAR=GLAM_params$glam_param.mod_mgt$ISYR:GLAM_params$glam_param.mod_mgt$IEYR,
                      OBS=y_o,PRED=y_p)
    rmse <- sqrt(sum((odf$OBS-odf$PRED)^2) / nrow(odf))
    out_row <- data.frame(VALUE=vals[i],RMSE=rmse)
    
    if (i == 1) {
      out_all <- out_row
    } else {
      out_all <- rbind(out_all,out_row)
    }
    
  }
  write.table(out_all,sep="\t",quote=F,file=paste(optDir,"/optim.txt",sep=""))
  return(out_all)
}



############################################################
####### function to optimise a given parameter in GLAM
############################################################
GLAM_optimise <- function(GLAM_params,sect="glam_param.ygp",param="YGP",n.steps=20,
                              cell=636,method="lin",cropName,bDir,iter=1,iratio=0,simset) {
  param <- toupper(param)
  
  #input directories and model
  cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
  binDir <- paste(bDir,"/model-runs/bin/glam",sep="")
  execName <- paste("glam-",tolower(cropName),sep="")
  
  #determine operating system
  machine <- as.data.frame(t(Sys.info()))
  machine <- paste(machine$sysname)
  
  if (tolower(machine) == "windows") {
    glam_cmd <- paste(paste(execName,".exe",sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
    execName <- paste(execName,".exe",sep="")
  } else if (tolower(machine) == "linux") {
    glam_cmd <- paste(paste("./",execName,sep=""),paste("filenames-",tolower(cropName),"-run.txt",sep=""))
  }
  
  #output directories
  parDir <- paste(cropDir,"/params",sep="") #parameter files
  cal_dir <- paste(cropDir,"/calib",sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  cal_dir <- paste(cal_dir,"/",simset,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  cal_dir <- paste(cal_dir,"/iter-",iter,sep="")
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #files that were generated
  yFile <- paste(cropDir,"/inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
  sowFile <- paste(cropDir,"/inputs/ascii/sow/sowing_",cell,"_start.txt",sep="")
  wthDir <- paste(cropDir,"/inputs/ascii/wth",sep="")
  solFile <- paste(cropDir,"/inputs/ascii/soil/soiltypes_",cell,".txt",sep="")
  solGrid <- paste(cropDir,"/inputs/ascii/soil/soilcodes_",cell,".txt",sep="")
  
  #create optimisation folder if it does not exist
  optDir <- paste(cal_dir,"/",tolower(param),sep="")
  if (!file.exists(optDir)) {dir.create(optDir)}
  
  #create sequence of values
  if (param == "TB" | param == "TO" | param == "TM") {
    vals <- seq(GLAM_params[[sect]][[paste(param,"FLWR",sep="")]][,"Min"],GLAM_params[[sect]][[paste(param,"FLWR",sep="")]][,"Max"],length.out=n.steps)
  } else {
    vals <- seq(GLAM_params[[sect]][[param]][,"Min"],GLAM_params[[sect]][[param]][,"Max"],length.out=n.steps)
  }
  
  #irrigated/rainfed/mix variable
  if (length(which(iratio$IRATIO == 0)) == nrow(iratio)) {
    run.type <- "RFD"
  } else if (length(which(iratio$IRATIO == 1)) == nrow(iratio)) {
    run.type <- "IRR"
  } else {
    run.type <- "MIX"
  }
  
  #loop through sequence of values
  for (i in 1:length(vals)) {
    cat("performing run",run.type,i,"value =",vals[i],"(",param,")","\n")
    
    if (param == "TB" | param == "TO" | param == "TM") {
      GLAM_params[[sect]][[paste(param,"FLWR",sep="")]][,"Value"] <- vals[i]
      GLAM_params[[sect]][[paste(param,"PODF",sep="")]][,"Value"] <- vals[i]
      GLAM_params[[sect]][[paste(param,"LMAX",sep="")]][,"Value"] <- vals[i]
      GLAM_params[[sect]][[paste(param,"HARV",sep="")]][,"Value"] <- vals[i]
    } else {
      GLAM_params[[sect]][[param]][,"Value"] <- vals[i]
    }
    
    ##############here irrigation rate
    if (run.type == "RFD") {
      GLAM_params$glam_param.mod_mgt$SEASON <- "RFD"
      #write the model params
      opfil <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
      opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/",run.type,"_run-",i,"_",vals[i],sep=""))
      
      #check whether the *.out already exists
      outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
      if (length(outfile) == 0) {
        
        ######################################################
        #write filenames file
        parfile <- paste("glam-r2-param-",tolower(cropName),"-run.txt",sep="")
        wth_row <- "inputs/ascii/wth/ingc"
        soilty_row <- paste("inputs/ascii/soil/soiltypes_",cell,".txt",sep="")
        soilco_row <- paste("inputs/ascii/soil/soilcodes_",cell,".txt",sep="")
        sow_row <- paste("inputs/ascii/sow/sowing_",cell,"_start.txt",sep="")
        yield_row <- paste("inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
        ygp_row <- "nofile"
        
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
        x <- file.copy(paste(parDir,"/",parfile,sep=""),run_dir,overwrite=T)
        x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        x <- file.copy(sowFile,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file
        x <- file.remove(execName)
      } else {
        setwd(run_dir)
      }
      #read in the predicted yield
      outfile <- list.files("./output/",pattern="\\.out")
      pred <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                       "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
      y_p <- pred$YIELD
      y_o <- read.fortran(yFile,format=c("A12","F8"),n=28)
      y_o <- y_o$V2
      
      #calc rmse
      odf <- data.frame(YEAR=GLAM_params$glam_param.mod_mgt$ISYR:GLAM_params$glam_param.mod_mgt$IEYR,
                        OBS=y_o,PRED=y_p)
      rmse <- sqrt(sum((odf$OBS-odf$PRED)^2) / nrow(odf))
    } else if (run.type == "IRR") {
      GLAM_params$glam_param.mod_mgt$SEASON <- "IRR"
      #write the model params
      opfil <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
      opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/",run.type,"_run-",i,"_",vals[i],sep=""))
      
      #check whether the *.out already exists
      outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
      if (length(outfile) == 0) {
        ######################################################
        #write filenames file
        parfile <- paste("glam-r2-param-",tolower(cropName),"-run.txt",sep="")
        wth_row <- "inputs/ascii/wth/ingc"
        soilty_row <- paste("inputs/ascii/soil/soiltypes_",cell,".txt",sep="")
        soilco_row <- paste("inputs/ascii/soil/soilcodes_",cell,".txt",sep="")
        sow_row <- paste("inputs/ascii/sow/sowing_",cell,"_start.txt",sep="")
        yield_row <- paste("inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
        ygp_row <- "nofile"
        
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
        x <- file.copy(paste(parDir,"/",parfile,sep=""),run_dir,overwrite=T)
        x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        x <- file.copy(sowFile,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file
        x <- file.remove(execName)
      } else {
        setwd(run_dir)
      }
      #read in the predicted yield
      outfile <- list.files("./output/",pattern="\\.out")
      pred <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                       "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
      y_p <- pred$YIELD
      y_o <- read.fortran(yFile,format=c("A12","F8"),n=28)
      y_o <- y_o$V2
      
      #calc rmse
      odf <- data.frame(YEAR=GLAM_params$glam_param.mod_mgt$ISYR:GLAM_params$glam_param.mod_mgt$IEYR,
                        OBS=y_o,PRED=y_p)
      rmse <- sqrt(sum((odf$OBS-odf$PRED)^2) / nrow(odf))
    } else if (run.type == "MIX") {
      GLAM_params$glam_param.mod_mgt$SEASON <- "RFD"
      #write the model params
      opfil <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep="")
      opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/RFD_run-",i,"_",vals[i],sep=""))
      
      #check whether the *.out already exists
      outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
      if (length(outfile) == 0) {
        ######################################################
        #write filenames file
        parfile <- paste("glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep="")
        wth_row <- "inputs/ascii/wth/ingc"
        soilty_row <- paste("inputs/ascii/soil/soiltypes_",cell,".txt",sep="")
        soilco_row <- paste("inputs/ascii/soil/soilcodes_",cell,".txt",sep="")
        sow_row <- paste("inputs/ascii/sow/sowing_",cell,"_start.txt",sep="")
        yield_row <- paste("inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
        ygp_row <- "nofile"
        
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
        x <- file.copy(paste(parDir,"/",parfile,sep=""),run_dir,overwrite=T)
        x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        x <- file.copy(sowFile,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file
        x <- file.remove(execName)
      } else {
        setwd(run_dir)
      }
      #read in the predicted yield
      outfile <- list.files("./output/",pattern="\\.out")
      pred <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                       "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
      y_p <- pred$YIELD
      y_o <- read.fortran(yFile,format=c("A12","F8"),n=28)
      y_o <- y_o$V2
      odf <- data.frame(YEAR=GLAM_params$glam_param.mod_mgt$ISYR:GLAM_params$glam_param.mod_mgt$IEYR,
                        OBS=y_o,RFD=y_p)
      
      ##!
      #Now the irrigated run
      GLAM_params$glam_param.mod_mgt$SEASON <- "IRR"
      #write the model params
      opfil <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-run-irr.txt",sep="")
      opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/IRR_run-",i,"_",vals[i],sep=""))
      
      #check whether the *.out already exists
      outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
      if (length(outfile) == 0) {
        ######################################################
        #write filenames file
        parfile <- paste("glam-r2-param-",tolower(cropName),"-run-irr.txt",sep="")
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
        x <- file.copy(paste(parDir,"/",parfile,sep=""),run_dir,overwrite=T)
        x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        x <- file.copy(sowFile,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file
        x <- file.remove(execName)
      } else {
        setwd(run_dir)
      }
      #read in the predicted yield
      outfile <- list.files("./output/",pattern="\\.out")
      pred <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                       "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
      y_p <- pred$YIELD
      odf$IRR <- y_p
      odf$IRATIO <- iratio$IRATIO
      odf$PRED <- (1-odf$IRATIO)*odf$RFD + (odf$IRATIO)*odf$IRR
      
      #calc rmse
      rmse <- sqrt(sum((odf$OBS-odf$PRED)^2) / nrow(odf))
    }
    
    out_row <- data.frame(VALUE=vals[i],RMSE=rmse)
    
    if (i == 1) {
      out_all <- out_row
    } else {
      out_all <- rbind(out_all,out_row)
    }
    
  }
  write.table(out_all,sep="\t",quote=F,file=paste(optDir,"/optim.txt",sep=""))
  return(out_all)
}







