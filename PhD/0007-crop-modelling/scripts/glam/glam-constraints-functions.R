#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2012


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


