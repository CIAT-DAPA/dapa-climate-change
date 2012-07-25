#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2012

##### script to run GLAM based on a particular configuration

#local
b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#eljefe
# b_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#sourcing required functions
source(paste(src.dir,"/scripts/glam/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/scripts/glam/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/scripts/glam/glam-parFile-functions.R",sep=""))


#initial run configuration
GLAM_setup <- list()
GLAM_setup$B_DIR <- b_dir
GLAM_setup$BIN_DIR <- paste(GLAM_setup$B_DIR,"/./../bin",sep="")
GLAM_setup$CAL_DIR <- paste(GLAM_setup$B_DIR,"/calib",sep="")
GLAM_setup$INPUTS_DIR <- paste(GLAM_setup$B_DIR,"/inputs",sep="")
GLAM_setup$ASC_DIR <- paste(GLAM_setup$INPUTS_DIR,"/ascii",sep="")
GLAM_setup$RUNS_DIR <- paste(GLAM_setup$B_DIR,"/runs/testing",sep="")
GLAM_setup$CROP <- "gnut"
GLAM_setup$YEARS <- 1966:1993
GLAM_setup$EXP_DIR <- "exp-02_outputs"
GLAM_setup$GRID <- paste(GLAM_setup$INPUTS_DIR,"/calib-cells-selection-v6.csv",sep="")
GLAM_setup$PREFIX <- "fcal_"
GLAM_setup$GRIDCELL <- 636
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

#configure GLAM run
GLAM_setup <- GLAM_config(GLAM_setup,force="no")

#make this particular glam run
GLAM_setup <- GLAM_run(GLAM_setup)


