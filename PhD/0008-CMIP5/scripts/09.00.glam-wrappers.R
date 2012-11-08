#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012


### wrapper function to summarise the YGP optimisation
wrapper_summarise_biol <- function(this_proc) {
  library(raster)
  
  #sourcing functions
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir,"/climateSignals-functions.R",sep=""))
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",procList[this_proc],"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  cal_dir <- paste(runs_odir,"/",procList[this_proc],sep="")
  
  if (file.exists(cal_dir)) {
    if (!file.exists(paste(cal_dir,"/calib_all_cells.csv",sep=""))) {
      #select cell
      #cell <- cells$CELL[1]
      # loop through gridcells
      for (cell in cells$CELL) {
        cat("\nprocessing gridcell",paste(cell),"\n")
        #get the required metrics:
        #1. mean predicted yield (taken from best value of YGP)
        #2. mean standard deviation of yield (taken from best value of YGP)
        #3. yield gap parameter
        #4. R pearson and pvalue
        #5. RMSE
        #6. RMSE / mean obs. yield * 100
        #7. initial planting date
        
        run_dir <- paste(cal_dir,"/calib_",cell,sep="")
        if (!file.exists(run_dir)) {
          r_val <- NA
          p_val <- NA
          rmse <- NA
          prmse <- NA
          yp_mean <- NA
          yp_stdv <- NA
          yo_mean <- NA
          yo_stdv <- NA
          n <- NA
        } else {
          #load rainfed yields
          cat("load rainfed yields\n")
          data_dir <- paste(run_dir,"/RFD_run/output",sep="")
          outfile <- list.files(data_dir,pattern="\\.out")
          pred <- read.table(paste(data_dir,"/",outfile,sep=""),header=F,sep="\t")
          names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                           "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                           "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                           "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                           "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
          y_rfd <- pred$YIELD
          
          #load irrigated yields
          cat("load irrigated yields\n")
          data_dir <- paste(run_dir,"/IRR_run/output",sep="")
          if (file.exists(data_dir)) {
            outfile <- list.files(data_dir,pattern="\\.out")
            pred <- read.table(paste(data_dir,"/",outfile,sep=""),header=F,sep="\t")
            names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                             "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                             "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                             "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                             "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
            y_irr <- pred$YIELD
          } else {
            y_irr <- rep(0,times=length(y_rfd))
          }
          
          #get irrigation ratio
          #extract irrigation rates
          cat("get irrigation rates\n")
          ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==cell)],Y=cells$Y[which(cells$CELL==cell)]))
          ir_vls <- as.numeric(ir_vls)
          ir_vls[which(ir_vls > 1)] <- 1
          
          #put all this into a data.frame
          cat("calculate 'true' predicted yield\n")
          y_pred <- data.frame(YEAR=1966:1993,RFD=y_rfd,IRR=y_irr,IRATIO=ir_vls)
          y_pred$PRED <- y_pred$RFD*(1-y_pred$IRATIO) + y_pred$IRR*y_pred$IRATIO
          
          #load observed yields
          cat("load observed yields\n")
          yfil <- paste(cDir,"/inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
          y_o <- read.fortran(yfil,format=c("A12","F8"),n=28)
          y_pred$OBS <- y_o$V2
          
          #get anything -99 to NA
          y_pred$OBS[which(y_pred$OBS < -90)] <- NA
          
          #remove all lines that are NA in OBS and calculate 'n' (number of observations)
          y_pred <- y_pred[which(!is.na(y_pred$OBS)),]
          n <- nrow(y_pred)
          
          #4. R pearson and pvalue
          if (n >= 2) {
            cat("calculate final metrics\n")
            r_val <- cor.test(y_pred$PRED,y_pred$OBS)$estimate
            p_val <- cor.test(y_pred$PRED,y_pred$OBS)$p.value
          } else {
            r_val <- NA
            p_val <- NA
          }
          
          #5. RMSE
          rmse <- sqrt(sum((y_pred$OBS-y_pred$PRED)^2) / nrow(y_pred))
          
          #6. RMSE / mean obs. yield * 100
          prmse <- rmse/mean(y_pred$OBS)*100
          
          #mean and standard deviation of predicted and observed yield
          yp_mean <- mean(y_pred$PRED)
          yp_stdv <- sd(y_pred$PRED)
          yo_mean <- mean(y_pred$OBS)
          yo_stdv <- sd(y_pred$OBS)
          
          #remove optimal and optimised
          rm(optimal); rm(optimised)
        }
        
        #output data frame
        out_row <- data.frame(CELL=cell,X=cells$X[which(cells$CELL == cell)],
                              Y=cells$Y[which(cells$CELL == cell)],YGP=NA,Y_OBS=yo_mean,YSD_OBS=yo_stdv,
                              Y_PRED=yp_mean,YSD_PRED=yp_stdv,CCOEF=r_val,PVAL=p_val,RMSE=rmse,P_RMSE=prmse,
                              SOW_DATE=NA,N=n)
        
        if (cell == cells$CELL[1]) {
          out_all <- out_row
        } else {
          out_all <- rbind(out_all,out_row)
        }
      }
      write.csv(out_all,paste(cal_dir,"/calib_all_cells.csv",sep=""),quote=T,row.names=F)
    } else {
      out_all <- read.csv(paste(cal_dir,"/calib_all_cells.csv",sep=""))
    }
    
    #now create the rasters
    out_rs_dir <- paste(cal_dir,"/calib_results_spat",sep="")
    if (!file.exists(out_rs_dir)) {dir.create(out_rs_dir)}
    
    rnames <- names(out_all)[4:13]
    for (rn in rnames) {
      if (!file.exists(paste(out_rs_dir,"/",tolower(rn),".asc",sep=""))) {
        cat("output of",rn,"\n")
        rs <- raster(msk)
        rs[out_all$CELL] <- out_all[,rn]
        rs <- writeRaster(rs,paste(out_rs_dir,"/",tolower(rn),".asc",sep=""),format="ascii",overwrite=T)
        rm(rs); g=gc(); rm(g)
      } else {
        cat(rn,"already exists\n")
      }
    }
    
    #ratio of observed to predicted mean yields
    if (!file.exists(paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""))) {
      yo <- raster(paste(out_rs_dir,"/y_obs.asc",sep=""))
      yp <- raster(paste(out_rs_dir,"/y_pred.asc",sep=""))
      yo_yp <- yo/yp
      yo_yp <- writeRaster(yo_yp,paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""),format="ascii",overwrite=T)
    } else {
      yo_yp <- raster(paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""))
    }
    
    #ratio of observed to predicted sd yields
    if (!file.exists(paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""))) {
      sdo <- raster(paste(out_rs_dir,"/ysd_obs.asc",sep=""))
      sdp <- raster(paste(out_rs_dir,"/ysd_pred.asc",sep=""))
      sdo_sdp <- sdo/sdp
      sdo_sdp <- writeRaster(sdo_sdp,paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""),format="ascii",overwrite=T)
    } else {
      sdo_sdp <- raster(paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""))
    }
  }
}



###wrapper to run a given perturbed crop parameter experiment
wrapper_perturbed_biol <- function(this_pt) {
  #library
  library(raster)
  
  #source all needed functions
  source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/climateSignals-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/09.00.glam-wrappers.R",sep=""))
  
  parname <- paste(pert_runs$Parameter[this_pt])
  where <- paste(pert_runs$Where[this_pt])
  
  if (parname!="HTS" & parname!="TDS") {
    parlow <- as.numeric(paste(pert_runs$Unperturbed[this_pt])) - as.numeric(paste(pert_runs$Perturbation[this_pt]))
    if (parlow < as.numeric(paste(pert_runs$Minimum.realistic[this_pt]))) {parlow <- as.numeric(paste(pert_runs$Minimum.realistic[this_pt]))}
    parhigh <- as.numeric(paste(pert_runs$Unperturbed[this_pt])) + as.numeric(paste(pert_runs$Perturbation[this_pt]))
  }
  
  #ci <- 1
  ciList <- 1:nrow(cells)
  for (ci in ciList) {
    #get run setup
    #files that were generated
    setup <- list()
    setup$BDIR <- bDir
    setup$CELL <- cells$CELL[ci]
    setup$METHOD <- "lin"
    setup$CROPNAME <- "gnut"
    setup$CAL_DIR <- paste(runs_odir,"/p-",tolower(parname),sep="")
    setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
    setup$YGP_FILE <- "nofile"
    setup$SOW_FILE_RFD <- paste(sow_dir,"/opt_calib_",setup$CELL,".txt",sep="")
    setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
    setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
    setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
    setup$WTH_ROOT <- "ingc"
    setup$SOL_FILE <- paste(input_dir,"/ascii/soil/soiltypes2.txt",sep="")
    setup$SOL_GRID <- paste(input_dir,"/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
    setup$SIM_NAME <- paste("calib_",setup$CELL,sep="")
    setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
    
    #update wth dir rainfed if there is a modification in weather files
    if (file.exists(paste(wth_dir,"/rfd_",setup$CELL,sep=""))) {
      setup$WTH_DIR_RFD <- paste(wth_dir,"/rfd_",setup$CELL,sep="")
    }
    
    cat("\nprocessing cell",setup$CELL,"\n")
    
    #get defaults (parameter set)
    params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
    params$glam_param.mod_mgt$ISYR <- 1966 #start year
    params$glam_param.mod_mgt$IEYR <- 1993 #end year
    params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
    
    #extract irrigation rates
    ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
    ir_vls <- as.numeric(ir_vls)
    ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
    ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
    
    #get optimal YGP from unperturbed model run
    load(paste(unp_rundir,"/calib_",setup$CELL,"/iter-ygp/output.RData",sep=""))
    params$glam_param.ygp$YGP$Value <- optimal$YGP
    rm(optimal); rm(optimised); g=gc(); rm(g)
    
    if (parname == "HTS") {
      params$glam_param.mod_mgt$HTS <- "+1" #turn off HTS
      params$glam_param.sparer$RSPARE1$Value <- -99 #turn off TDS
      params$glam_param.sparer$RSPARE2$Value <- -99 #turn off TDS
      
      #run with all types
      for (type in unique(hts_types$TYPE)) {
        #type <- unique(hts_types$TYPE[1])
        params[[where]][["TCRITMIN"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"TCRITMIN"]
        params[[where]][["PPCRIT"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"PPCRIT"]
        params[[where]][["TLINT"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"TLINT"]
        params[[where]][["TCSLOPE"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"TCSLOPE"]
        params[[where]][["TLSLOPE"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"TLSLOPE"]
        params[[where]][["FDWIDTH"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"FDWIDTH"]
        params[[where]][["FDOFFSET"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"FDOFFSET"]
        params[[where]][["TLIMMIN"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"TLIMMIN"]
        params[[where]][["IDURMAX"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"IDURMAX"]
        params[[where]][["IBAMAX"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"IBAMAX"]
        params[[where]][["IAAMAX"]]["Value"] <- hts_types[which(hts_types$TYPE==type),"IAAMAX"]
        
        setup$CAL_DIR <- paste(runs_odir,"/p-",tolower(parname),"_",type,sep="") #update setup
        orcdir <- GLAM_run_loc(GLAM_params=params,RUN_setup=setup,iratio=ir_vls) #run glam with this configuration
      }
      
    } else if (parname == "TDS") {
      params$glam_param.mod_mgt$HTS <- "-1" #turn off HTS
      
      #run with all types
      for (type in unique(tds_types$TYPE)) {
        #type <- unique(tds_types$TYPE[1])
        params[[where]][["RSPARE1"]]["Value"] <- tds_types[which(tds_types$TYPE==type),"HIMIN"]
        params[[where]][["RSPARE2"]]["Value"] <- tds_types[which(tds_types$TYPE==type),"FSW"]
        
        setup$CAL_DIR <- paste(runs_odir,"/p-",tolower(parname),"_",type,sep="") #update setup
        orcdir <- GLAM_run_loc(GLAM_params=params,RUN_setup=setup,iratio=ir_vls) #run glam with this configuration
      }
      
    } else {
      params$glam_param.mod_mgt$HTS <- "-1" #turn off HTS
      params$glam_param.sparer$RSPARE1$Value <- -99 #turn off TDS
      params$glam_param.sparer$RSPARE2$Value <- -99 #turn off TDS
      
      #run with low perturbation
      params[[where]][[parname]]["Value"] <- parlow #update parameter set
      setup$CAL_DIR <- paste(runs_odir,"/p-",tolower(parname),"_",parlow,sep="") #update setup
      orcdir <- GLAM_run_loc(GLAM_params=params,RUN_setup=setup,iratio=ir_vls) #run glam with this configuration
      
      #run with high perturbation
      params[[where]][[parname]]["Value"] <- parhigh #update parameter set
      setup$CAL_DIR <- paste(runs_odir,"/p-",tolower(parname),"_",parhigh,sep="") #update setup
      orcdir <- GLAM_run_loc(GLAM_params=params,RUN_setup=setup,iratio=ir_vls) #run glam with this configuration
    }
    
  }
}



############################################################
####### function to run a given parameter set in GLAM, 
############################################################
GLAM_run_loc <- function(GLAM_params,RUN_setup,iratio=0) {
  simset <- RUN_setup$SIM_NAME
  cell <- RUN_setup$CELL
  method <- RUN_setup$METHOD
  cropName <- RUN_setup$CROPNAME
  bDir <- RUN_setup$BDIR
  
  #input directories and model
  #cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
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
  #parDir <- paste(cropDir,"/params",sep="") #parameter files
  #cal_dir <- paste(cropDir,"/calib",sep="") #calibration directory
  cal_dir <- RUN_setup$CAL_DIR #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  cal_dir <- paste(cal_dir,"/",simset,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #files that were generated
  yFile <- RUN_setup$YIELD_FILE
  ygpFile <- RUN_setup$YGP_FILE
  sowFile_rfd <- RUN_setup$SOW_FILE_RFD
  sowFile_irr <- RUN_setup$SOW_FILE_IRR
  wthDir_rfd <- RUN_setup$WTH_DIR_RFD
  wthDir_irr <- RUN_setup$WTH_DIR_IRR
  solFile <- RUN_setup$SOL_FILE
  solGrid <- RUN_setup$SOL_GRID
  
  #check consistency of yield file
  ydum <- read.fortran(yFile,format=c("A12","F8"),n=28)
  if (length(which(ydum$V2 < -90)) > 0) {
    y_has_na <- T
  } else {
    y_has_na <- F
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
  cat("performing run",run.type,"gridcell =",cell,"\n")
  
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
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/",run.type,"_run",sep=""))
    
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
      if (y_has_na) {
        yield_row <- "nofile"
      } else {
        yield_row <- paste("inputs/ascii/obs/",unlist(strsplit(yFile,"/",fixed=T))[length(unlist(strsplit(yFile,"/",fixed=T)))],sep="")
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
      x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
      
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
      
      #delete the exec file and compress the daily files
      x <- file.remove(execName)
      
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
    run_dir <- create_dirs(paste(cal_dir,"/",run.type,"_run",sep=""))
    
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
      if (y_has_na) {
        yield_row <- "nofile"
      } else {
        yield_row <- paste("inputs/ascii/obs/",unlist(strsplit(yFile,"/",fixed=T))[length(unlist(strsplit(yFile,"/",fixed=T)))],sep="")
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
      x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
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
      }
    }
    
    #output folder
    run_dir <- create_dirs(paste(cal_dir,"/RFD_run",sep=""))
    
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
      if (y_has_na) {
        yield_row <- "nofile"
      } else {
        yield_row <- paste("inputs/ascii/obs/",unlist(strsplit(yFile,"/",fixed=T))[length(unlist(strsplit(yFile,"/",fixed=T)))],sep="")
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
      x <- file.copy(yFile,paste(run_dir,"/inputs/ascii/obs",sep=""),overwrite=T)
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
    run_dir <- create_dirs(paste(cal_dir,"/IRR_run",sep=""))
    
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
  }
  return(run_dir)
}





### wrapper function to summarise the YGP optimisation
wrapper_summarise_GCM_cal <- function(this_proc) {
  library(raster)
  
  #sourcing functions
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
  
  #get gcm and ensemble member names
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  cal_dir <- paste(runs_odir,"/",gcm,"_",ens,sep="")
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",gcm,"-",ens,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  if (file.exists(cal_dir)) {
    if (!file.exists(paste(cal_dir,"/calib_all_cells.csv",sep=""))) {
      #select cell
      #cell <- cells$CELL[1]
      # loop through gridcells
      for (cell in cells$CELL) {
        cat("\nprocessing gridcell",paste(cell),"\n")
        #get the required metrics:
        #1. mean predicted yield (taken from best value of YGP)
        #2. mean standard deviation of yield (taken from best value of YGP)
        #3. yield gap parameter
        #4. R pearson and pvalue
        #5. RMSE
        #6. RMSE / mean obs. yield * 100
        #7. initial planting date
        
        run_dir <- paste(cal_dir,"/calib_",cell,sep="")
        if (!file.exists(run_dir)) {
          ygp <- NA
          r_val <- NA
          p_val <- NA
          rmse <- NA
          prmse <- NA
          yp_mean <- NA
          yp_stdv <- NA
          yo_mean <- NA
          yo_stdv <- NA
          n <- NA
        } else {
          ygpDir <- paste(run_dir,"/iter-ygp",sep="")
          
          #get best ygp, and mean predicted yield from that run
          #you need to read irrigated and rainfed, and also iratios
          cat("getting optimal ygp\n")
          load(paste(ygpDir,"/output.RData",sep=""))
          ygp <- optimal$YGP
          opt_pos <- which(optimised$YGP$VALUE == ygp)
          
          #load rainfed yields
          cat("load rainfed yields\n")
          data_dir <- paste(run_dir,"/iter-ygp/ygp/RFD_run-",opt_pos,"_",ygp,"/output",sep="")
          outfile <- list.files(data_dir,pattern="\\.out")
          pred <- read.table(paste(data_dir,"/",outfile,sep=""),header=F,sep="\t")
          names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                           "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                           "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                           "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                           "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
          y_rfd <- pred$YIELD
          
          #load irrigated yields
          cat("load irrigated yields\n")
          data_dir <- paste(run_dir,"/iter-ygp/ygp/IRR_run-",opt_pos,"_",ygp,"/output",sep="")
          if (file.exists(data_dir)) {
            outfile <- list.files(data_dir,pattern="\\.out")
            pred <- read.table(paste(data_dir,"/",outfile,sep=""),header=F,sep="\t")
            names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                             "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                             "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                             "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                             "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
            y_irr <- pred$YIELD
          } else {
            y_irr <- rep(0,times=length(y_rfd))
          }
          
          #get irrigation ratio
          #extract irrigation rates
          cat("get irrigation rates\n")
          ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==cell)],Y=cells$Y[which(cells$CELL==cell)]))
          ir_vls <- as.numeric(ir_vls)
          ir_vls[which(ir_vls > 1)] <- 1
          
          #put all this into a data.frame
          cat("calculate 'true' predicted yield\n")
          y_pred <- data.frame(YEAR=1966:1993,RFD=y_rfd,IRR=y_irr,IRATIO=ir_vls)
          y_pred$PRED <- y_pred$RFD*(1-y_pred$IRATIO) + y_pred$IRR*y_pred$IRATIO
          
          #load observed yields
          cat("load observed yields\n")
          yfil <- paste(cDir,"/inputs/ascii/obs/yield_",cell,"_",method,".txt",sep="")
          y_o <- read.fortran(yfil,format=c("A12","F8"),n=28)
          y_pred$OBS <- y_o$V2
          
          #get anything -99 to NA
          y_pred$OBS[which(y_pred$OBS < -90)] <- NA
          
          #remove all lines that are NA in OBS and calculate 'n' (number of observations)
          y_pred <- y_pred[which(!is.na(y_pred$OBS)),]
          n <- nrow(y_pred)
          
          #4. R pearson and pvalue
          if (n >= 2) {
            cat("calculate final metrics\n")
            r_val <- cor.test(y_pred$PRED,y_pred$OBS)$estimate
            p_val <- cor.test(y_pred$PRED,y_pred$OBS)$p.value
          } else {
            r_val <- NA
            p_val <- NA
          }
          
          #5. RMSE
          rmse <- sqrt(sum((y_pred$OBS-y_pred$PRED)^2) / nrow(y_pred))
          
          #6. RMSE / mean obs. yield * 100
          prmse <- rmse/mean(y_pred$OBS)*100
          
          #mean and standard deviation of predicted and observed yield
          yp_mean <- mean(y_pred$PRED)
          yp_stdv <- sd(y_pred$PRED)
          yo_mean <- mean(y_pred$OBS)
          yo_stdv <- sd(y_pred$OBS)
          
          #remove optimal and optimised
          rm(optimal); rm(optimised)
        }
        
        #output data frame
        out_row <- data.frame(CELL=cell,X=cells$X[which(cells$CELL == cell)],
                              Y=cells$Y[which(cells$CELL == cell)],YGP=ygp,Y_OBS=yo_mean,YSD_OBS=yo_stdv,
                              Y_PRED=yp_mean,YSD_PRED=yp_stdv,CCOEF=r_val,PVAL=p_val,RMSE=rmse,P_RMSE=prmse,
                              SOW_DATE=NA,N=n)
        
        if (cell == cells$CELL[1]) {
          out_all <- out_row
        } else {
          out_all <- rbind(out_all,out_row)
        }
      }
      write.csv(out_all,paste(cal_dir,"/calib_all_cells.csv",sep=""),quote=T,row.names=F)
    } else {
      out_all <- read.csv(paste(cal_dir,"/calib_all_cells.csv",sep=""))
    }
    
    #now create the rasters
    out_rs_dir <- paste(cal_dir,"/calib_results_spat",sep="")
    if (!file.exists(out_rs_dir)) {dir.create(out_rs_dir)}
    
    rnames <- names(out_all)[4:13]
    for (rn in rnames) {
      if (!file.exists(paste(out_rs_dir,"/",tolower(rn),".asc",sep=""))) {
        cat("output of",rn,"\n")
        rs <- raster(msk)
        rs[out_all$CELL] <- out_all[,rn]
        rs <- writeRaster(rs,paste(out_rs_dir,"/",tolower(rn),".asc",sep=""),format="ascii",overwrite=T)
        rm(rs); g=gc(); rm(g)
      } else {
        cat(rn,"already exists\n")
      }
    }
    
    #ratio of observed to predicted mean yields
    if (!file.exists(paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""))) {
      yo <- raster(paste(out_rs_dir,"/y_obs.asc",sep=""))
      yp <- raster(paste(out_rs_dir,"/y_pred.asc",sep=""))
      yo_yp <- yo/yp
      yo_yp <- writeRaster(yo_yp,paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""),format="ascii",overwrite=T)
    } else {
      yo_yp <- raster(paste(out_rs_dir,"/yobs_by_ypred.asc",sep=""))
    }
    
    #ratio of observed to predicted sd yields
    if (!file.exists(paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""))) {
      sdo <- raster(paste(out_rs_dir,"/ysd_obs.asc",sep=""))
      sdp <- raster(paste(out_rs_dir,"/ysd_pred.asc",sep=""))
      sdo_sdp <- sdo/sdp
      sdo_sdp <- writeRaster(sdo_sdp,paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""),format="ascii",overwrite=T)
    } else {
      sdo_sdp <- raster(paste(out_rs_dir,"/sdobs_by_sdpred.asc",sep=""))
    }
  }
}


### wrapper function to optimize the ygp using the GCM data
wrapper_GCM_glam_optimise_ygp <- function(this_proc) {
  
  #source all needed functions
  source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/signals//climateSignals-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  
  #get gcm and ensemble member names
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",gcm,"-",ens,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  
  #create an output control directory if it does not exist yet
  octr_dir <- paste(runs_odir,"/x.proc",sep="")
  if (!file.exists(octr_dir)) {dir.create(octr_dir)}
  
  cFile <- paste(octr_dir,"/",gcm,"_",ens,".proc",sep="")
  
  if (!file.exists(cFile)) {
    #ci <- 1
    ciList <- 1:nrow(cells)
    for (ci in ciList) {
      #get run setup
      #files that were generated
      setup <- list()
      setup$BDIR <- bDir
      setup$SCRATCH <- "/scratch/eejarv/cmip5_paper"
      setup$USE_SCRATCH <- T
      setup$CELL <- cells$CELL[ci]
      setup$METHOD <- "lin"
      setup$CROPNAME <- "gnut"
      setup$CAL_DIR <- paste(runs_odir,"/",gcm,"_",ens,sep="")
      setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
      setup$YGP_FILE <- "nofile"
      setup$SOW_FILE_RFD <- paste(input_dir,"/ascii/sow/opt_calib_",setup$CELL,".txt",sep="")
      setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
      setup$WTH_DIR_RFD <- paste(input_dir,"/ascii/wth_fut/",gcm,"_ENS_",ens,"/rfd_",setup$CELL,sep="")
      setup$WTH_DIR_IRR <- paste(input_dir,"/ascii/wth_fut/",gcm,"_ENS_",ens,"/irr_",setup$CELL,sep="")
      setup$WTH_ROOT <- "ingc"
      setup$SOL_FILE <- paste(input_dir,"/ascii/soil/soiltypes2.txt",sep="")
      setup$SOL_GRID <- paste(input_dir,"/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
      setup$SIM_NAME <- paste("calib_",setup$CELL,sep="")
      setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
      setup$OPT_METHOD <- "CH07"
      
      cat("\nprocessing cell",setup$CELL,"\n")
      
      if (setup$USE_SCRATCH) {setup$SCRATCH <- paste(setup$SCRATCH,"/",gcm,"_",ens,sep="")}
      
      nw_irr <- length(list.files(setup$WTH_DIR_IRR,pattern="\\.wth"))
      nw_rfd <- length(list.files(setup$WTH_DIR_RFD,pattern="\\.wth"))
      
      if (nw_rfd == 28 & nw_irr == 28) {
        #get defaults (parameter set)
        params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
        params$glam_param.mod_mgt$ISYR <- 1966 #start year
        params$glam_param.mod_mgt$IEYR <- 1993 #end year
        params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
        params$glam_param.mod_mgt$HTS <- "-1" #turn off HTS
        params$glam_param.sparer$RSPARE1$Value <- -99 #turn off TDS
        params$glam_param.sparer$RSPARE2$Value <- -99 #turn off TDS
        
        #extract irrigation rates
        irDir <- paste(cDir,"/irrigated_ratio",sep="")
        library(raster)
        ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
        ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
        ir_vls <- as.numeric(ir_vls)
        ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
        ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
        
        ######################################################
        # final calibration of YGP
        ######################################################
        #run the optimiser for YGP, 20 steps
        parname <- "YGP"
        where <- "glam_param.ygp"
        nstep <- 20
        
        if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
          # reset lists of output parameters
          optimal <- list(); optimised <- list()
          
          optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                    param=parname,n.steps=20,iter=tolower(parname),
                                                    iratio=ir_vls)
          
          optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
          cat(parname,":",optimal[[parname]],"\n")
          if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
          
          save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
          
          #now make the plot
          #plotsDir <- paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/plots",sep="")
          #if (!file.exists(plotsDir)) {dir.create(plotsDir)}
          
          #tiff(paste(plotsDir,"/",tolower(parname),".tif",sep=""),res=300,compression="lzw",height=1000,
          #     width=1250,pointsize=8)
          #par(mar=c(3,3,2,1))
          #plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",
          #     main=paste(parname," :: ",optimal[[parname]],sep=""),
          #     xlab="Parameter value",ylab="RMSE (kg/ha)")
          #grid(nx=10,ny=10)
          #abline(v=optimal[[parname]],col="red",lty=2,lwd=0.8)
          #dev.off()
        }
      } else {
        warning("incomplete set of weather files for ",gcm," - ",ens,"\n")
      }
    }
    
    #write the control file
    ff <- file(cFile,"w")
    cat("this model run was finished on",date(),"\n",file=ff)
    close(ff)
  } else {
    cat("process is already done\n")
  }
}

