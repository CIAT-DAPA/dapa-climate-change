#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

############################################################
####### function to optimise a given parameter in GLAM, 
####### except sowing date, RLL, DUL, SAT
############################################################
GLAM_optimise <- function(GLAM_params,RUN_setup,sect="glam_param.ygp",param="YGP",n.steps=20,
                              iter=1,iratio=0) {
  param <- toupper(param)
  simset <- RUN_setup$SIM_NAME
  cell <- RUN_setup$CELL
  method <- RUN_setup$METHOD
  cropName <- RUN_setup$CROPNAME
  bDir <- RUN_setup$BDIR
  
  #input directories and model
  cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
  execName <- paste("glam-",tolower(cropName),sep="")
  
  #determine operating system and bin folder
  machine <- as.data.frame(t(Sys.info()))
  machine <- paste(machine$sysname)
  binDir <- paste(bDir,"/model-runs/bin/glam-",tolower(machine),sep="")
  
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
  yFile <- RUN_setup$YIELD_FILE
  sowFile_rfd <- RUN_setup$SOW_FILE_RFD
  sowFile_irr <- RUN_setup$SOW_FILE_IRR
  wthDir_rfd <- RUN_setup$WTH_DIR_RFD
  wthDir_irr <- RUN_setup$WTH_DIR_IRR
  solFile <- RUN_setup$SOL_FILE
  solGrid <- RUN_setup$SOL_GRID
  
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
      
      #check if the planting date is well configured
      if (sowFile_rfd == "nofile") {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
          stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
        }
      } else {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
          GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Min <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Max <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$NVAL <- 1
        }
      }
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/",run.type,"_run-",i,"_",vals[i],sep=""))
      
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
        
        if (sowFile_rfd == "nofile") {
          sow_row <- sowFile_rfd
        } else {
          sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_rfd,"/",fixed=T))[length(unlist(strsplit(sowFile_rfd,"/",fixed=T)))],sep="")
        }
        yield_row <- paste("inputs/ascii/obs/",unlist(strsplit(yFile,"/",fixed=T))[length(unlist(strsplit(yFile,"/",fixed=T)))],sep="")
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
        x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        
        if (sowFile_rfd != "nofile") {
          x <- file.copy(sowFile_rfd,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        }
        
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir_rfd),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_rfd,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file and compress the daily files
        x <- file.remove(execName)
        
        #compress and remove wth files
        setwd("./inputs/ascii/wth")
        system(paste("7z a daily.7z -tzip *.wth"))
        x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
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
      
      #check if the planting date is well configured
      if (sowFile_irr == "nofile") {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
          stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
        }
      } else {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
          GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Min <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Max <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$NVAL <- 1
        }
      }
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/",run.type,"_run-",i,"_",vals[i],sep=""))
      
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
        
        if (sowFile_irr == "nofile") {
          sow_row <- sowFile_irr
        } else {
          sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_irr,"/",fixed=T))[length(unlist(strsplit(sowFile_irr,"/",fixed=T)))],sep="")
        }
        
        yield_row <- paste("inputs/ascii/obs/",unlist(strsplit(yFile,"/",fixed=T))[length(unlist(strsplit(yFile,"/",fixed=T)))],sep="")
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
        x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        if (sowFile_irr != "nofile") {
          x <- file.copy(sowFile_irr,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        }
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir_irr),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_irr,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file
        x <- file.remove(execName)
        
        #compress and remove wth files
        setwd("./inputs/ascii/wth")
        system(paste("7z a daily.7z -tzip *.wth"))
        x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
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
      
      #check if the planting date is well configured
      if (sowFile_rfd == "nofile") {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
          stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
        }
      } else {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
          GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Min <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Max <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$NVAL <- 1
        }
      }
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/RFD_run-",i,"_",vals[i],sep=""))
      
      #write the model params
      opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run-rfd.txt",sep="")
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
        if (sowFile_rfd == "nofile") {
          sow_row <- sowFile_rfd
        } else {
          sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_rfd,"/",fixed=T))[length(unlist(strsplit(sowFile_rfd,"/",fixed=T)))],sep="")
        }
        yield_row <- paste("inputs/ascii/obs/",unlist(strsplit(yFile,"/",fixed=T))[length(unlist(strsplit(yFile,"/",fixed=T)))],sep="")
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
        x <- file.copy(paste(binDir,"/",execName,sep=""),run_dir,overwrite=T)
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        if (sowFile_rfd != "nofile") {
          x <- file.copy(sowFile_rfd,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        }
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir_rfd),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_rfd,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file
        x <- file.remove(execName)
        
        #compress and remove wth files
        setwd("./inputs/ascii/wth")
        system(paste("7z a daily.7z -tzip *.wth"))
        x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
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
      
      #check if the planting date is well configured
      if (sowFile_irr == "nofile") {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value < -90) {
          stop("in a rainfed run you need either a sow dates file or a value for IPDATE")
        }
      } else {
        if (GLAM_params$glam_param.spt_mgt$IPDATE$Value > -90) {
          GLAM_params$glam_param.spt_mgt$IPDATE$Value <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Min <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$Max <- -99
          #GLAM_params$glam_param.spt_mgt$IPDATE$NVAL <- 1
        }
      }
      
      #output folder
      run_dir <- create_dirs(paste(optDir,"/IRR_run-",i,"_",vals[i],sep=""))
      
      #write the model params
      opfil <- paste(run_dir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
      opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)
      
      #check whether the *.out already exists
      outfile <- list.files(paste(run_dir,"/output",sep=""),pattern="\\.out")
      if (length(outfile) == 0) {
        ######################################################
        #write filenames file
        parfile <- unlist(strsplit(opfil,"/",fixed=T))[length(unlist(strsplit(opfil,"/",fixed=T)))]
        
        if (sowFile_irr == "nofile") {
          sow_row <- sowFile_irr
        } else {
          sow_row <- paste("inputs/ascii/sow/",unlist(strsplit(sowFile_irr,"/",fixed=T))[length(unlist(strsplit(sowFile_irr,"/",fixed=T)))],sep="")
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
        x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
        if (sowFile_irr != "nofile") {
          x <- file.copy(sowFile_irr,paste(run_dir,"/inputs/ascii/sow",sep=""),overwrite=T)
        }
        x <- file.copy(solFile,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- file.copy(solGrid,paste(run_dir,"/inputs/ascii/soil",sep=""),overwrite=T)
        x <- sapply(list.files(wthDir_irr),FUN= function(x,idir,odir) {s <- file.copy(paste(idir,"/",x,sep=""),odir,overwrite=T)},wthDir_irr,paste(run_dir,"/inputs/ascii/wth",sep=""))
        
        #now run!
        setwd(run_dir)
        system(glam_cmd)
        
        #delete the exec file
        x <- file.remove(execName)
        
        #compress and remove wth files
        setwd("./inputs/ascii/wth")
        system(paste("7z a daily.7z -tzip *.wth"))
        x <- sapply(list.files(".",pattern="\\.wth"),FUN= function(x) {s <- file.remove(x)})
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







