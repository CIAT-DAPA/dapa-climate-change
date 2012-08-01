#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2012


#change the names of the columns to real names before the
#stacked plot
fix_names <- function(in_data,real_names) {
  for (i in 2:length(names(in_data))) {
    nam <- names(in_data)[i]
    if (as.numeric(nam)==0) {
      rnam <- "NONE"
    } else {
      if (nchar(nam) == 1) {
        rnam <- real_names[as.numeric(nam)]
      } else {
        jc <- c()
        for (j in 1:nchar(nam)) {
          jc <- c(jc,as.numeric(substr(nam,j,j)))
          }
        rnam <- paste(real_names[jc],collapse="_")
      }
    }
    #cat(rnam,"\n")
    names(in_data)[i] <- rnam
  }
  return(in_data)
}



#### function to calculate proportion of each constraint in the dominant
#### constraint raster
calc_portion_dominant <- function(x,vals) {
  x <- raster(x)
  stacked <- data.frame(CONSTRAINT=vals,RATIO=0)
  #x <- constraints1[[1]]
  for (i in 1:nrow(stacked)) {
    v <- stacked$CONSTRAINT[i]
    pc <- length(which(x[] == v))/length(which(!is.na(x[])))
    stacked$RATIO[i] <- pc
  }
  s2 <- data.frame()
  s2 <- rbind(s2,t(stacked$RATIO))
  names(s2) <- stacked$CONSTRAINT
  #s2 <- cbind(LAYER=layerNames(x),s2)
  return(s2)
}



#function to map the constraints of a given year. This function will produce
#a map in which a number is assigned to each constraint. Such number corresponds
#to the constraint that when removed will produce the largest yield gain.
#
#the total number of constraints that have been evaluated is 8 for the first map
#(i.e. constraints.tif) and 7 for the second map (i.e. constraints_no_irr.tif). Since
#the impact of whole-growing season irrigation is high and includes all 
#water-stress-related sub-seasonal processes it was removed for the second map.
#
#a value of zero in the map indicates that no removal produced an increase in
#predicted yields. A value above 8 (first map) or 7 (second map) indicates that
#more than one process produced the same yield increase. For instance, a value of
#35 would indicate process 3 and 5 were limiting yield at the same extent.
#
#the list of processes is: 
#(1) drought during the growing season (i.e. irrigated run),
#(2) terminal drought stress, 
#(3) drought stress during flowering, 
#(4) mean temperature during the gs (i.e. data were set to optimal value), 
#(5) mean temperature (i.e. modification of thermal time equation so that maximum 
#    TEFF was obtained in each day,
#(6) high temperature effects on transpiration efficiency (TETRS)
#(7) high temperature stress during flowering
#(8) effects of limited radiation (assumed the crop could capture 100% radiation, EXTC=0)
#
map_constraint_year <- function(yr,run_setup,cons_data,base_rs) {
  #for (yr in min(GLAM_setup$YEARS):max(GLAM_setup$YEARS)) {}
  cat("processing year",yr,"\n")
  
  #output yearly directory
  out_dir <- paste(GLAM_setup$OUT_RS_DIR,"/",yr,sep="")
  if (!file.exists(out_dir)) {dir.create(out_dir)}
  
  yr_data <- cons_data[which(cons_data$YEAR==yr),]
  
  if (!file.exists(paste(out_dir,"/control.tif",sep=""))) {
    rs_control <- raster(base_rs)
    rs_control[yr_data$GRIDCELL] <- yr_data$CONTROL
    rs_control <- writeRaster(rs_control,paste(out_dir,"/control.tif",sep=""),format="GTiff")
  } else {
    rs_control <- raster(paste(out_dir,"/control.tif",sep=""))
  }
  
  ratios <- c()
  for (i in 4:ncol(yr_data)) {
    cname <- paste(names(yr_data)[i])
    #get the yield data in
    if (!file.exists(paste(out_dir,"/",tolower(cname),".tif",sep=""))) {
      rs <- raster(base_rs)
      rs[yr_data$GRIDCELL] <- yr_data[,i]
      rs <- writeRaster(rs,paste(out_dir,"/",tolower(cname),".tif",sep=""),format="GTiff")
    } else {
      rs <- raster(paste(out_dir,"/",tolower(cname),".tif",sep=""))
    }
    
    #calculate ratio of change
    if (!file.exists(paste(out_dir,"/ratio-",tolower(cname),".tif",sep=""))) {
      rs_ratio <- (rs - rs_control) / rs_control * 100
      rs_ratio <- writeRaster(rs_ratio,paste(out_dir,"/ratio-",tolower(cname),".tif",sep=""),format="GTiff")
    } else {
      rs_ratio <- raster(paste(out_dir,"/ratio-",tolower(cname),".tif",sep=""))
    }
    ratios <- c(ratios,rs_ratio)
  }
  
  #create raster stack
  ratios <- stack(ratios)
  
  #with a calc function get which position is the most constrained, including
  #the drought growth one
  find_max <- function(x) {
    if (length(which(is.na(x))) == length(x)) {
      ro <- NA
    } else if (length(which(x==0)) == length(x)) {
      ro <- 0
    } else if (length(which(x>0)) == 0) {
      ro <- 0
    } else {
      ro <- which(x == max(x))
      if (length(ro)>1) {ro <- as.numeric(paste(ro,collapse=""))}
    }
    return(ro)
  }
  
  #2. per year create a raster that shows from 1 to n the dominating process
  #   dominating process is hereby referred to as that which when removed
  #   causes the largest increase in crop yield
  #
  if (!file.exists(paste(out_dir,"/constraints.tif",sep=""))) {
    constraint <- calc(ratios,fun=find_max)
    constraint <- writeRaster(constraint,paste(out_dir,"/constraints.tif",sep=""),format="GTiff")
    #plot(constraint,col=rev(terrain.colors(9)))
    #text(x=xFromCell(rs,yr_data$GRIDCELL),y=yFromCell(rs,yr_data$GRIDCELL),cex=0.4,labels=yr_data$GRIDCELL)
  } else {
    constraint <- raster(paste(out_dir,"/constraints.tif",sep=""))
  }
  
  #with a calc function get which position is the most constrained, excluding
  #the drought one
  if (!file.exists(paste(out_dir,"/constraints_no_irr.tif",sep=""))) {
    ratios2 <- ratios
    ratios2 <- dropLayer(ratios2,1)
    constraint2 <- calc(ratios2,fun=find_max)
    constraint2 <- writeRaster(constraint2,paste(out_dir,"/constraints_no_irr.tif",sep=""),format="GTiff")
  } else {
    constraint2 <- raster(paste(out_dir,"/constraints_no_irr.tif",sep=""))
  }
  return(out_dir)
}



#get all constraints data for a given experiment
get_constraint_data <- function(run_setup) {
  #get results of control run
  cat("\ngetting results of control run\n")
  run_setup$RUN_DIR <- paste(run_setup$RUNS_DIR,"/",run_setup$EXP_DIR,"/control",sep="")
  
  #get the yield data
  yield_data <- lapply(X=gc_list,FUN=get_yearly_gridcell,run_setup,vlist,"rfd")
  yield_data <- do.call("rbind",yield_data)
  
  #output object
  constraint_data <- yield_data
  names(constraint_data)[3] <- c("CONTROL")
  
  #loop through processes tested
  for (proc in unique(constraints$process)) {
    #proc <- paste(unique(constraints$process)[1])
    proc <- paste(proc)
    cons <- constraints[which(constraints$process == proc),]
    cnam <- paste(cons$constraint[1])
    
    cat("getting results of",cnam,"---",proc,"\n")
    run_setup$RUN_DIR <- paste(run_setup$RUNS_DIR,"/",run_setup$EXP_DIR,"/",cnam,"_",proc,sep="")
    yield_data <- lapply(X=gc_list,FUN=get_yearly_gridcell,run_setup,vlist,"rfd")
    yield_data <- do.call("rbind",yield_data)
    
    constraint_data <- cbind(constraint_data,NEWVAL=yield_data$YIELD)
    names(constraint_data)[ncol(constraint_data)] <- toupper(paste(cnam,"_",proc,sep=""))
  }
  return(constraint_data)
}



#wrap the gridcells
get_yearly_gridcell <- function(gridcell,run_setup,var_list,season="rfd") {
  run_setup$GRIDCELL <- gridcell
  output_dir <- paste(run_setup$RUN_DIR,"/run_",run_setup$GRIDCELL,"/output",sep="")
  
  #check if rainfed file exists
  if (season == "rfd") {
    glam_file <- paste(run_setup$CROP_LONG,"-",season,".out",sep="")
    if (file.exists(paste(output_dir,"/",glam_file,sep=""))) {
      run_setup$OUT_FILE <- glam_file
      yield <- get_glam_data_year(run_setup=run_setup,var_list=var_list,varname=run_setup$TARGET_VAR)
    } else {
      stop("the file for rainfed season does not exist")
    }
  } else if (season == "irr") {
    glam_file <- paste(run_setup$CROP_LONG,"-",season,".out",sep="")
    if (file.exists(paste(output_dir,"/",glam_file,sep=""))) {
      run_setup$OUT_FILE <- glam_file
      yield <- get_glam_data_year(run_setup=run_setup,var_list=var_list,varname=run_setup$TARGET_VAR)
    } else {
      stop("the file for irrigated season does not exist")
    }
  } else {
    #determine whether both seasons were run, if so, then
    #rainfed data
    glam_file <- paste(run_setup$CROP_LONG,"-rfd.out",sep="")
    if (file.exists(paste(output_dir,"/",glam_file,sep=""))) {
      run_setup$OUT_FILE <- glam_file
      yield_rfd <- get_glam_data_year(run_setup=run_setup,var_list=var_list,varname=run_setup$TARGET_VAR)
    } else {
      stop("the file for rainfed season does not exist")
    }
    #irrigated data
    glam_file <- paste(run_setup$CROP_LONG,"-irr.out",sep="")
    if (file.exists(paste(output_dir,"/",glam_file,sep=""))) {
      run_setup$OUT_FILE <- glam_file
      yield_irr <- get_glam_data_year(run_setup=run_setup,var_list=var_list,varname=run_setup$TARGET_VAR)
    } else {
      stop("the file for rainfed season does not exist")
    }
    
    #get irrigation data
    iratio <- as.numeric(run_setup$IDATA[which(run_setup$IDATA$CELL==run_setup$GRIDCELL),paste("Y",min(run_setup$YEARS):max(run_setup$YEARS),sep="")])
    
    #final yield data frame
    yield <- data.frame(YEAR=yield_rfd$YEAR,YIELD=(yield_irr$YIELD*iratio + yield_rfd$YIELD*(1-iratio)))
  }
  yield <- cbind(GRIDCELL=run_setup$GRIDCELL,yield)
  return(yield)
}


### function to get data of a given variable of a given run
get_glam_data_year <- function(run_setup,var_list,varname="YIELD") {
  #check whether the variable is in the list
  if (!toupper(varname) %in% toupper(var_list)) {
    warning("varname is not in var_list, assuming varname=YIELD")
  }
  
  if (!"YEAR" %in% toupper(var_list)) {
    stop("variable YEAR needs to be present in var_list")
  }
  
  out_file <- paste(run_setup$RUN_DIR,"/run_",run_setup$GRIDCELL,"/output/",run_setup$OUT_FILE,sep="")
  if (!file.exists(out_file)) {
    stop("run_setup$OUT_FILE does not exist in the file system")
  } else {
    glam_data <- read.table(out_file,header=F,sep="\t")
  }
  
  #check whether the variable list has appropriate length
  if (ncol(glam_data) != length(var_list)) {
    stop("var_list needs to have an appropriate number of terms")
  } else {
    names(glam_data) <- toupper(paste(var_list))
  }
  
  #get the variable of interest
  var_data <- data.frame(YEAR=glam_data[,"YEAR"],VALUES=as.numeric(glam_data[,toupper(varname)]))
  names(var_data)[2] <- toupper(varname)
  return(var_data)
}



### function to copy the results from the scratch folder to some
### other folder (typically in the NFS)
copy_results <- function(run_setup,o_dir,dump_scratch=F) {
  if (run_setup$RUNS_DIR == o_dir) {
    stop("scratch and output directories cannot be the same")
  }
  
  con_file <- paste(o_dir,"/",run_setup$EXP_DIR,"/run.info",sep="")
  run_setup$RUNS_DIR <- paste(run_setup$RUNS_DIR,"/",run_setup$EXP_DIR,sep="")
  if (file.exists(con_file)) {
    if (file.exists(run_setup$RUNS_DIR) & dump_scratch) {
      cat("already there, removing...")
      system(paste("rm -rf ",run_setup$RUNS_DIR,sep=""))
      run_setup$COPY_STATUS <- "DUMPED"
    }
  } else {
    if (!file.exists(o_dir)) {dir.create(o_dir,recursive=T)}
    cat("copying...\n")
    system(paste("cp -rf ",run_setup$RUNS_DIR," ",paste(o_dir,"/.",sep=""),sep=""))
    #write status file
    fc <- file(con_file,"w")
    cat("copied on",date(),"by",paste(as.data.frame(t(Sys.info()))$login),"@",
        paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=fc)
    close(fc)
    run_setup$COPY_STATUS <- "COPIED"
  }
  
  if (file.exists(paste(o_dir,"/",run_setup$EXP_dir,sep="")) & dump_scratch) {
    cat("removing...\n")
    system(paste("rm -rf ",run_setup$RUNS_DIR,sep=""))
    run_setup$COPY_STATUS <- "COPIED AND DUMPED"
  }
  run_setup$NFS_OUT_DIR <- paste(o_dir,"/",run_setup$EXP_dir,sep="")
  return(run_setup)
}



#### function to wrap the constraints analysis of a given gridcell
glam_constraint_wrapper <- function(cell) {
  #sourcing required functions
  source(paste(src.dir,"/scripts/glam/glam-run-functions.R",sep=""))
  source(paste(src.dir,"/scripts/glam/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/scripts/glam/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/scripts/glam/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/scripts/glam/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/scripts/glam/glam-constraints-functions.R",sep=""))
  
  
  cat("\n########################################################\n")
  cat("##############gridcell",cell,"###############################\n")
  cat("########################################################\n")
  
  #object to keep initial configuration
  GLAM_setup_base$GRIDCELL <- cell
  
  #perform the "control" run
  GLAM_setup <- GLAM_setup_base
  GLAM_setup$RUNS_DIR <- paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/control",sep="")
  GLAM_setup$YGP <- 1
  GLAM_setup <- GLAM_config(GLAM_setup,force="rfd")
  if (GLAM_setup$STATUS == "READY") {
    GLAM_setup <- GLAM_run(GLAM_setup)
  }
  
  #loop through processes
  for (proc in unique(constraints$process)) {
    #proc <- paste(unique(constraints$process)[1])
    proc <- paste(proc)
    cons <- constraints[which(constraints$process == proc),]
    cnam <- paste(cons$constraint[1])
    
    cat("\ncrop sensitivity to",cnam,"---",proc,"\n")
    
    #configure the model run
    GLAM_setup <- GLAM_setup_base
    GLAM_setup$YGP <- 1
    GLAM_setup$RUNS_DIR <- paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/",cnam,"_",proc,sep="")
    GLAM_setup <- GLAM_config(GLAM_setup,force="rfd")
    
    if (GLAM_setup$STATUS == "READY") {
      #if the tested constraint is in the parameter set
      if (paste(cons$where[1]) == "PARAM") {
        #update the parameter set
        for (i in 1:nrow(cons)) {
          #i=1
          sec <- paste(cons$section[i])
          par <- paste(cons$parameter[i])
          val <- paste(cons$value[i])
          uni <- cons$unique[i]
          typ <- paste(cons$type[i])
          if (typ == "num") {val <- as.numeric(val)}
          
          #which type of run will be performed
          if (GLAM_setup$RUN_TYPE == "RFD") {
            if (uni) {
              GLAM_setup$PARAM_RFD[[sec]][[par]] <- val
            } else {
              GLAM_setup$PARAM_RFD[[sec]][[par]]["Value"] <- val
            }
          } else if (GLAM_setup$RUN_TYPE == "IRR") {
            if (uni) {
              GLAM_setup$PARAM_IRR[[sec]][[par]] <- val
            } else {
              GLAM_setup$PARAM_IRR[[sec]][[par]]["Value"] <- val
            }
          } else {
            if (uni) {
              GLAM_setup$PARAM_RFD[[sec]][[par]] <- val
              GLAM_setup$PARAM_IRR[[sec]][[par]] <- val
            } else {
              GLAM_setup$PARAM_RFD[[sec]][[par]]["Value"] <- val
              GLAM_setup$PARAM_IRR[[sec]][[par]]["Value"] <- val
            }
          }
        }
        #now run the model
        GLAM_setup <- GLAM_run(GLAM_setup)
        
      } else if (paste(cons$where[1]) == "WTH_DIR") {
        #if the tested constraint is in the wth dir, then alter the mean temperature data
        if (GLAM_setup$RUN_TYPE == "RFD") {
          GLAM_setup$WTH_DIR_RFD <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMIN",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value-0.5,times=365))
          
          GLAM_setup$WTH_DIR_RFD <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMAX",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value+0.5,times=365))
        } else if (GLAM_setup$RUN_TYPE == "IRR") {
          GLAM_setup$WTH_DIR_IRR <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMIN",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value-0.5,times=365))
          
          GLAM_setup$WTH_DIR_IRR <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMAX",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value+0.5,times=365))
          
        } else {
          GLAM_setup$WTH_DIR_RFD <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMIN",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value-0.5,times=365))
          
          GLAM_setup$WTH_DIR_RFD <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMAX",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value+0.5,times=365))
          
          GLAM_setup$WTH_DIR_IRR <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth_irr",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMIN",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value-0.5,times=365))
          
          GLAM_setup$WTH_DIR_IRR <- GLAM_chg_wth(wth_dir=paste(GLAM_setup$RUN_DIR,"/inputs/ascii/wth_irr",sep=""),
                                                 wth_root=GLAM_setup$WTH_ROOT,yi=min(GLAM_setup$YEARS),
                                                 yf=max(GLAM_setup$YEARS),target_var="TMAX",
                                                 values=rep(GLAM_setup$PARAM_RFD$glam_param.phenol$TOFLWR$Value+0.5,times=365))
        }
        #perform the model run
        GLAM_setup <- GLAM_run(GLAM_setup)
        
      } else if (paste(cons$where[1]) == "BIN_DIR") {
        #if the tested constraint is a change in the binary
        val <- paste(cons$value)
        GLAM_setup$BIN_DIR <- paste(GLAM_setup$BIN_DIR,val,sep="")
        
        #replacing the executable
        x <- file.remove(paste(GLAM_setup$RUN_DIR,"/",GLAM_setup$EXEC_NAME,sep=""))
        x <- file.copy(from=paste(GLAM_setup$BIN_DIR,"/",GLAM_setup$EXEC_NAME,sep=""),
                       to=GLAM_setup$RUN_DIR)
        GLAM_setup <- GLAM_run(GLAM_setup)
      }
    }
    setwd(GLAM_setup$B_DIR)
  }
}



#function to replace the weather files using a set of prescribed values
GLAM_chg_wth <- function(wth_dir,wth_root,yi,yf,target_var="TMIN",values=NA) {
  cat("transforming",target_var,"...\n")
  for (yr in yi:yf) {
    #yr <- yi
    #open the file
    wth_file <- paste(wth_dir,"/",wth_root,"001001",yr,".wth",sep="")
    wth_data <- read.fortran(wth_file,format=c("I5","F6","3F7"),skip=4)
    names(wth_data) <- c("DATE","SRAD","TMAX","TMIN","RAIN")
    wth_data$YEAR <- as.numeric(substr(wth_data$DATE,1,2))
    wth_data$JDAY <- as.numeric(substr(wth_data$DATE,3,5))
    
    sdet_2 <- as.character(read.fortran(wth_file,n=1,format=c("A50")))
    sdet_2 <- strsplit(sdet_2," : ",fixed=T)[[1]][2]
    sdet_1 <- read.fortran(wth_file,skip=2,n=1,format=c("A6","2F9","5F6"))
    sdet_1$V1 <- gsub(" ","",sdet_1$V1)
    s_details <- data.frame(NAME=sdet_2,INSI=sdet_1$V1,LAT=sdet_1$V2,LONG=sdet_1$V3,
                            ELEV=sdet_1$V4,TAV=sdet_1$V5,AMP=sdet_1$V6,
                            REFHT=sdet_1$V7,WNDHT=sdet_1$V8)
    
    wth_data[,toupper(target_var)] <- values
    wth_file <- write_wth(wth_data,outfile=wth_file,site.details=s_details)
  }
  return(wth_dir)
}


