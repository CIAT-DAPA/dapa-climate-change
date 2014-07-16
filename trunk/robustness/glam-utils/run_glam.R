#Julian Ramirez-Villegas
#UoL / CCAFS
#Feb 2014

#single run of GLAM for a given set of inputs. This run will
#1. create directories
#2. write meteorology, soil and sowing file
#3. write parameter file
#4. write filenames file

#this function requires other functions to be loaded into memory (see ./robustness/)

#an object of type _list_ is the only input to this function. such object has the following 
#characteristics:
#run_data$CROP: name of crop, e.g. "maize"
#run_data$MODEL: name of executable e.g. "glam-maiz"
#run_data$BASE_DIR: directory where all runs are stored --a directory with run will be created inside
#run_data$BIN_DIR: path where executable is
#run_data$PAR_DIR: path where parameter file (base_param_maize.txt) is
#run_data$WTH_DIR: path where meteorology file (meteo_cell-$LOC$.met) is
#run_data$LOC: id of location (e.g. 1792)
#run_data$LON: longitude of location, extracted as xy_main$x[which(xy_main$LOC == run_data$LOC)]
#run_data$LAT: latitude of location extracted as xy_main$y[which(xy_main$LOC == run_data$LOC)]
#run_data$RUN_ID: id of run, constructed using run_data$LOC
#run_data$ME: <- xy_main$ME[which(xy_main$LOC == run_data$LOC)]
#run_data$SOW_DATE: sowing date of run, extracted as xy_main$SOW_DATE1[which(xy_main$LOC == run_data$LOC)]
#run_data$RLL: lower moisture limit, extracted as xy_main$RLL[which(xy_main$LOC == run_data$LOC)]
#run_data$DUL: upper moisture limit, extracted as xy_main$DUL[which(xy_main$LOC == run_data$LOC)]
#run_data$SAT: saturation limit, extracted as xy_main$SAT[which(xy_main$LOC == run_data$LOC)]
#run_data$ISYR: first year of simulation
#run_data$IEYR: last year of simulation
#run_data$PARAMS: GLAM parameter set

#start of function
run_glam <- function(run_data) {
  #set variable names
  base_dir <- run_data$BASE_DIR
  run_id <- run_data$RUN_ID
  
  #create directory (and structure) for run
  run_dir <- paste(base_dir,"/",run_id,sep="")
  run_dir <- create_dirs(run_dir)
  
  #load and write parameter file
  if (is.null(run_data$PARAMS)) {
    params <- GLAM_get_default(run_data$PAR_DIR)
    params$glam_param.mod_mgt$IASCII <- 1 #output only to season file
    params$glam_param.mod_mgt$ISYR <- run_data$ISYR
    params$glam_param.mod_mgt$IEYR <- run_data$IEYR
    params$glam_param.sim_ctr$SMLON <- run_data$LON
    params$glam_param.sim_ctr$CROP <- run_data$CROP
  } else {
    if (is.na(run_data$PARAMS)[1]) {
      params <- GLAM_get_default(run_data$PAR_DIR)
      params$glam_param.mod_mgt$IASCII <- 1 #output only to season file
      params$glam_param.mod_mgt$ISYR <- run_data$ISYR
      params$glam_param.mod_mgt$IEYR <- run_data$IEYR
      params$glam_param.sim_ctr$SMLON <- run_data$LON
      params$glam_param.sim_ctr$CROP <- run_data$CROP
    } else {
      params <- run_data$PARAMS
      params$glam_param.sim_ctr$SMLON <- run_data$LON
    }
  }
  parfile <- GLAM_create_parfile(params, paste(run_dir,"/",run_data$CROP,"_param_run.txt",sep=""))
  
  #copy (if these exist) or create weather files
  #always one extra year to avoid running out of weather data
  wthfil <- list.files(paste(run_data$WTH_DIR,"/loc-",run_data$LOC,sep=""),pattern="\\.wth")
  nwthfil <- unlist(lapply(params$glam_param.mod_mgt$ISYR:(params$glam_param.mod_mgt$IEYR+1),function(x) grep(x,wthfil)))
  nyears <- length(params$glam_param.mod_mgt$ISYR:(params$glam_param.mod_mgt$IEYR+1))
  if (length(nwthfil) == nyears) {
    wthfil <- wthfil[nwthfil]
    cpfil <- lapply(wthfil, function(x) file.copy(paste(run_data$WTH_DIR,"/loc-",run_data$LOC,"/",x,sep=""),paste(run_dir,"/inputs/ascii/wth",sep="")))
    wthfil <- list(WTH_DIR=paste(run_dir,"/inputs/ascii/wth",sep=""))
  } else {
    wthfil <- make_wth(data.frame(CELL=run_data$LOC,X=run_data$LON,Y=run_data$LAT),
                       wthDir_in=run_data$WTH_DIR,wthDir_out=paste(run_dir,"/inputs/ascii/wth",sep=""),
                       years=params$glam_param.mod_mgt$ISYR:(params$glam_param.mod_mgt$IEYR+1))
  }
  
  #create soil files
  solfil <- make_soilcodes(outfile=paste(run_dir,"/inputs/ascii/soil/soilcodes.txt",sep=""))
  solfil <- make_soiltypes(data.frame(CELL=run_data$LOC,RLL=run_data$RLL,DUL=run_data$DUL,SAT=run_data$SAT),
                           outfile=paste(run_dir,"/inputs/ascii/soil/soiltypes.txt",sep=""))
  
  #create sowing date file
  sowfil <- make_sowdates(data.frame(CELL=run_data$LOC,SOW_DATE=run_data$SOW_DATE),
                          outfile=paste(run_dir,"/inputs/ascii/sow/sowing.txt",sep=""))
  
  #copy model
  system(paste("cp -fp ",run_data$BIN_DIR,"/",run_data$MODEL," ",run_dir,"/",run_data$MODEL,sep=""))
  
  #write filenames file
  fn <- file(paste(run_dir,"/filenames.txt",sep=""),"w")
  cat(sprintf("%-41s",gsub(paste(run_dir,"/",sep=""),"",parfile)),"\n",sep="",file=fn)
  cat(sprintf("%-41s",paste(gsub(paste(run_dir,"/",sep=""),"",wthfil$WTH_DIR),"/afrb",sep="")),"\n",sep="",file=fn)
  cat(sprintf("%-41s",gsub(paste(run_dir,"/",sep=""),"",solfil)),"\n",sep="",file=fn)
  cat(sprintf("%-41s","inputs/ascii/soil/soilcodes.txt"),"\n",sep="",file=fn)
  cat(sprintf("%-41s",gsub(paste(run_dir,"/",sep=""),"",sowfil)),"\n",sep="",file=fn)
  cat(sprintf("%-41s","nofile"),"\n",sep="",file=fn)
  cat(sprintf("%-41s","nofile"),"\n",sep="",file=fn)
  close(fn)
  
  #go to dir, run model, return to where i am
  thisdir <- getwd(); setwd(run_dir); system(paste("./",run_data$MODEL,sep="")); setwd(thisdir)
  
  #return run_dir, and out_file for copying of file
  run_data$SEAS_FILES <- list.files(paste(run_dir,"/output/",sep=""),pattern="\\.out")
  run_data$DAILY_FILES <- list.files(paste(run_dir,"/output/daily",sep=""),pattern="\\.out")
  run_data$RUN_DIR <- run_dir
  
  #return object
  return(run_data)
}


