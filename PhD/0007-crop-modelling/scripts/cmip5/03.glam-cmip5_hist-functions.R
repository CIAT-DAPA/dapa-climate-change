#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Sept 2012

##### script to run GLAM based on a particular configuration

#local
# b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
# src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
# scratch <- paste(b_dir,"/runs/cmip5_hist",sep="")

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


###read the experiments that will be used
parset_list <- read.csv(paste(b_dir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expid_list <- parset_list$EXPID[which(parset_list$ISSEL==1)]

#loop through experiments
for (expid in expid_list) {
  cat("....\n")
  cat("running experiment",expid,"\n")
  cat("....\n")
  #################################################################################
  #################################################################################
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
  GLAM_setup$EXP_DIR <- paste("exp-",expid,"_outputs",sep="")
  GLAM_setup$GRID <- paste(GLAM_setup$INPUTS_DIR,"/calib-cells-selection-v6.csv",sep="")
  GLAM_setup$PREFIX <- "fcal_"
  GLAM_setup$GRIDCELL <- NA
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
  GLAM_setup_base <- GLAM_setup
  #################################################################################
  #################################################################################
  
  
  #################################################################################
  #################################################################################
  #perform the model runs
  gc_list <- read.csv(GLAM_setup$GRID)$CELL
  
  #number of cpus to use
  if (length(gc_list) > 12) {ncpus <- 12} else {ncpus <- length(gc_list)}
  
  #here do the parallelisation
  #load library and create cluster
  library(snowfall)
  sfInit(parallel=T,cpus=ncpus)
  
  #export variables
  sfExport("src.dir")
  sfExport("b_dir")
  sfExport("scratch")
  sfExport("cell_xy")
  sfExport("GLAM_setup_base")
  sfExport("gc_list")
  sfExport("constraints")
  
  #run the function in parallel
  system.time(sfSapply(as.vector(gc_list),glam_constraint_wrapper))
  
  #stop the cluster
  sfStop()
  
  #################################################################################
  #################################################################################
  #collate data
  ### 1. copy the results to the nfs
  odir <- paste(b_dir,"/runs/constraints",sep="")
  rsetup <- copy_results(run_setup=GLAM_setup,o_dir=odir,dump_scratch=F)
}



