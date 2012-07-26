#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2012

##### script to run GLAM based on a particular configuration

#local
# b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
# src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
# scratch <- paste(b_dir,"/runs/constraints",sep="")

#eljefe
b_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
scratch <- "/scratch/eejarv/constraints"

#sourcing required functions
source(paste(src.dir,"/scripts/glam/glam-run-functions.R",sep=""))
source(paste(src.dir,"/scripts/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/scripts/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/scripts/glam/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/scripts/glam/glam-make_wth.R",sep=""))
source(paste(src.dir,"/scripts/glam/glam-constraints-functions.R",sep=""))

#load list of constraints to apply
constraints <- read.table(paste(b_dir,"/runs/constraints/constraints.tab",sep=""),header=T,sep="\t")


### to do: when a run is done already, do not repeat
### to do: convert into function
### to do: parallelise
### to do: collate data

gc_list <- read.csv(paste(GLAM_setup$B_DIR,"/inputs/calib-cells-selection-v6.csv",sep=""))$CELL

### to do: this for is to be replaced by a parallel process
for (cell in gc_list) {
  
  cat("\n########################################################\n")
  cat("##############gridcell",cell,"###############################\n")
  cat("########################################################\n")
  
  #initial run configuration
  GLAM_setup <- list()
  GLAM_setup$B_DIR <- b_dir
  GLAM_setup$BIN_DIR <- paste(GLAM_setup$B_DIR,"/./../bin",sep="")
  GLAM_setup$CAL_DIR <- paste(GLAM_setup$B_DIR,"/calib",sep="")
  GLAM_setup$INPUTS_DIR <- paste(GLAM_setup$B_DIR,"/inputs",sep="")
  GLAM_setup$ASC_DIR <- paste(GLAM_setup$INPUTS_DIR,"/ascii",sep="")
  GLAM_setup$RUNS_DIR <- scratch
  GLAM_setup$CROP <- "gnut"
  GLAM_setup$YEARS <- 1966:1993
  GLAM_setup$EXP_DIR <- "exp-10_outputs"
  GLAM_setup$GRID <- paste(GLAM_setup$INPUTS_DIR,"/calib-cells-selection-v6.csv",sep="")
  GLAM_setup$PREFIX <- "fcal_"
  GLAM_setup$GRIDCELL <- cell
  GLAM_setup$YGP <- "opt"
  GLAM_setup$CODES_PREFIX <- "soilcodes_"
  GLAM_setup$TYPES_PREFIX <- "soiltypes_"
  GLAM_setup$WTH_ROOT <- "ingc"
  GLAM_setup$IRR_RS_DIR <- paste(GLAM_setup$B_DIR,"/irrigated_ratio",sep="")
  GLAM_setup$IRR_RS_PREFIX <- "raw-"
  GLAM_setup$IRR_RS_EXT <- ".asc"
  
  #load irrigation data
  cell_xy <- read.csv(GLAM_setup$GRID)
  GLAM_setup$IDATA <- load_irr_data(rs_dir=GLAM_setup$IRR_RS_DIR,
                                    rs_prefix=GLAM_setup$IRR_RS_PREFIX,yi=min(GLAM_setup$YEARS),
                                    yf=max(GLAM_setup$YEARS),xy=cbind(x=cell_xy$X,y=cell_xy$Y),
                                    ext=GLAM_setup$IRR_RS_EXT,cell_ids=cell_xy$CELL)
  
  #object to keep initial configuration
  GLAM_setup_base <- GLAM_setup
  
  #perform the "control" run
  GLAM_setup <- GLAM_setup_base
  GLAM_setup$RUNS_DIR <- paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/control",sep="")
  GLAM_setup$YGP <- 1
  GLAM_setup <- GLAM_config(GLAM_setup,force="rfd")
  GLAM_setup <- GLAM_run(GLAM_setup)
  
  
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
    setwd(GLAM_setup$B_DIR)
  }
}





