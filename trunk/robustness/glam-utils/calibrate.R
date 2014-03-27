#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2014 #borrows from PhD script called "glam-optimise-functions.R"

#############################################################################################
####### function to optimise a given parameter in GLAM (for as many grid cells as provided), 
####### except sowing date, RLL, DUL, SAT
#############################################################################################

#this (first) function should
#1. use number of steps to choose the values to iterate
#2. run model for all grid cells with selected value
#3. calibrate (i.e. find optimum YGP) for each grid cell individually
#4. return table of parameter value * ygp values, and RMSE value

#note: a second function will deal with YGP

src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_dirs.R",sep=""))
source(paste(src.dir,"/glam-utils/make_soilfiles.R",sep=""))
source(paste(src.dir,"/glam-utils/make_sowfile.R",sep=""))
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
source(paste(src.dir,"/glam-utils/make_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/get_parameterset.R",sep=""))
source(paste(src.dir,"/glam-utils/run_glam.R",sep=""))

wd <- "~/Leeds-work/quest-for-robustness"
runsDir <- paste(wd,"/crop_model_runs",sep="")
calibDir <- paste(runsDir,"/ppe_optimisation",sep="")
mdataDir <- paste(wd,"/data/model_data",sep="")
metDir <- paste(wd,"/data/meteorology",sep="")
binDir <- paste(wd,"/bin/glam-maize-osx",sep="")

#load objects
load(paste(mdataDir,"/initial_conditions_major.RData",sep=""))
load(paste(mdataDir,"/yield_major.RData",sep=""))

#arguments
opt_data <- list()
opt_data$CROP <- "maize"
opt_data$MODEL <- "glam-maiz"
opt_data$BASE_DIR <- calibDir
opt_data$BIN_DIR <- binDir
opt_data$PAR_DIR <- mdataDir
opt_data$WTH_DIR <- paste(metDir,"/extract_temp",sep="") #for reading meteo_cell-$LOC$.met
opt_data$LOC <- c(1792,1793,1794)
opt_data$ISYR <- 1950
opt_data$IEYR <- 1950
opt_data$INI_COND <- xy_main
opt_data$YLD_DATA <- xy_main_yield
opt_data$PARAMS <- GLAM_get_default(opt_data$PAR_DIR)
opt_data$SIM_NAME <- "optim1"
#opt_data$PARAM <- "YGP"
#opt_data$SECT <- "glam_param.ygp"
opt_data$NSTEPS <- 100
opt_data$RUN_TYPE <- "RFD"
opt_data$METHOD <- "RMSE"
opt_data$USE_SCRATCH <- F
opt_data$SCRATCH <- NA

#calibrate ygp

GLAM_calibrate <- function(opt_data) {
  param <- "YGP" #toupper(opt_data$PARAM)
  sect <- "glam_param.ygp" #tolower(opt_data$SECT)
  simset <- opt_data$SIM_NAME
  cell <- opt_data$CELL
  method <- opt_data$METHOD
  crop_name <- opt_data$CROP
  b_dir <- opt_data$BASE_DIR
  isyr <- opt_data$ISYR
  ieyr <- opt_data$IEYR
  params <- opt_data$PARAMS
  run_type <- opt_data$RUN_TYPE
  
  #put years into parameter set
  params$glam_param.mod_mgt$ISYR <- isyr
  params$glam_param.mod_mgt$IEYR <- ieyr
  
  #here is the optimisation method
  #RMSE: is yearly root mean square error (classical)
  #CH07: is the MSE method proposed in Challinor et al. (2007) AGEE, that optimises based on
  #      the differences between mean and standard deviations of the simulated time series
  #CH10: is the MSE method proposed in Challinor et al. (2010) ERL, that optimises based
  #      on the difference between mean yields only. I guess this method is only valid when
  #      an insufficiently large observed yield + weather time series is available.
  if (is.null(opt_data$METHOD)) {
    opt_meth <- "RMSE" #defaulting to RMSE if missing in input list
  } else {
    opt_meth <- toupper(opt_data$METHOD)
  }
  
  if (!opt_meth %in% c("RMSE","CH07","CH10")) {
    opt_meth <- "RMSE" #defaulting the RMSE
  }
  
  #input directories and model
  exec_name <- opt_data$MODEL
  
  #running command
  glam_cmd <- paste("./",exec_name,sep="")
  
  #output directories
  if (opt_data$USE_SCRATCH) {
    cal_dir <- opt_data$SCRATCH #calibration directory
    nfs_dir <- paste(opt_data$BASE_DIR,"/",simset,sep="")
    if (!file.exists(nfs_dir)) {dir.create(nfs_dir,recursive=T)}
  } else {
    cal_dir <- opt_data$BASE_DIR #calibration directory
  }
  if (!file.exists(cal_dir)) {dir.create(cal_dir,recursive=T)}
  
  cal_dir <- paste(cal_dir,"/",simset,sep="") #calibration directory
  if (!file.exists(cal_dir)) {dir.create(cal_dir)}
  
  #create optimisation folder if it does not exist
  opt_dir <- paste(cal_dir,"/",tolower(param),sep="")
  if (!file.exists(opt_dir)) {dir.create(opt_dir)}
  
  #create sequence of values
  vals <- seq(params[[sect]][[param]][,"Min"],params[[sect]][[param]][,"Max"],length.out=opt_data$NSTEPS)
  
  #loop through sequence of values
  for (i in 1:length(vals)) {
    #i <- 1
    cat("performing run ",run_type," ",i," value = ",vals[i]," (",param,")",sep="","\n")
    
    #assign values to parameter set
    params[[sect]][[param]][,"Value"] <- vals[i]
    
    ##############here irrigation rate
    params$glam_param.mod_mgt$SEASON <- run_type
    
    for (loc in opt_data$LOC) { #here i am
      #loc <- opt_data$LOC[1]
      lon <- opt_data$INI_COND$x[which(opt_data$INI_COND$LOC == loc)]
      cat("...loc= ",loc,sep="","\n")
      
    }
    #process the planting date
    sow_date <- opt_data$INI_COND$SOW_DATE1[which(opt_data$INI_COND$LOC == )]
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
      rmse <- NA
    } else {
      pred <- read.table(paste("./output/",outfile,sep=""),header=F,sep="\t")
      names(pred) <- c("YEAR","LAT","LON","PLANTING_DATE","STG","RLV_M","LAI","YIELD","BMASS","SLA",
                       "HI","T_RAIN","SRAD_END","PESW","TRANS","ET","P_TRANS+P_EVAP","SWFAC","EVAP+TRANS",
                       "RUNOFF","T_RUNOFF","DTPUPTK","TP_UP","DRAIN","T_DRAIN","P_TRANS","TP_TRANS",
                       "T_EVAP","TP_EVAP","T_TRANS","RLA","RLA_NORM","RAIN_END","DSW","TRADABS",
                       "DUR","VPDTOT","TRADNET","TOTPP","TOTPP_HIT","TOTPP_WAT","TBARTOT")
      y_p <- pred$YIELD
      y_o <- read.fortran(yFile,format=c("3I4","F8"),n=28)
      y_o <- y_o[which(y_o$V1 >= isyr & y_o$V1 <= ieyr),]
      y_o <- y_o$V4
      
      #calc rmse
      odf <- data.frame(YEAR=GLAM_params$glam_param.mod_mgt$ISYR:GLAM_params$glam_param.mod_mgt$IEYR,
                        OBS=y_o,PRED=y_p)
      odf$OBS[which(odf$OBS < -90)] <- NA
      
      if (optMeth == "RMSE") {
        rmse <- sqrt(sum((odf$OBS-odf$PRED)^2,na.rm=T) / (length(which(!is.na(odf$OBS)))))
      } else if (optMeth == "CH07") {
        rmse <- (mean(odf$OBS,na.rm=T)-mean(odf$PRED,na.rm=T))^2 + (sd(odf$OBS,na.rm=T)-sd(odf$PRED,na.rm=T))^2
      } else if (optMeth == "CH10") {
        rmse <- (mean(odf$OBS,na.rm=T)-mean(odf$PRED,na.rm=T))^2
      }
    }
    
    out_row <- data.frame(VALUE=vals[i],RMSE=rmse)
    
    if (i == 1) {
      out_all <- out_row
    } else {
      out_all <- rbind(out_all,out_row)
    }
    
  }
  write.table(out_all,sep="\t",quote=F,file=paste(optDir,"/optim.txt",sep=""))
  if (RUN_setup$USE_SCRATCH) {
    system(paste("cp -rf ",cal_dir," ",paste(nfs_dir,"/.",sep=""),sep=""))
    setwd(nfs_dir)
    system(paste("rm -rf ",cal_dir,sep=""))
  }
  return(out_all)
}
