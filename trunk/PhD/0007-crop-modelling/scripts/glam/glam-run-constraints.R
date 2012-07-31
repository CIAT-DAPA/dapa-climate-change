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
GLAM_setup$EXP_DIR <- "exp-10_outputs"
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



#################################################################################
#################################################################################
#collate data

### 1. copy the results to the nfs
odir <- paste(b_dir,"/runs/constraints",sep="")
rsetup <- copy_results(run_setup=GLAM_setup,o_dir=odir,dump_scratch=F)

### 2. grab yield data into the form of a table with each
###    constraint being a column.  Save data into file also
if (!file.exists(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/yield_data.csv",sep=""))) {
  vlist <- paste(read.table(paste(src.dir,"/data/GLAM-varnames.tab",sep=""),header=T,sep="\t")$EOS)
  
  #initial configuration
  GLAM_setup <- GLAM_setup_base
  GLAM_setup$CROP_LONG <- "groundnut"
  GLAM_setup$TARGET_VAR <- "YIELD"
  GLAM_setup$RUNS_DIR <- paste(GLAM_setup$B_DIR,"/runs/constraints",sep="")
  
  #get constraints data
  cons_data <- get_constraint_data(run_setup=GLAM_setup)
  
  #write these data
  write.csv(cons_data,paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/yield_data.csv",sep=""),quote=T,row.names=F)
} else {
  cons_data <- read.csv(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/yield_data.csv",sep=""))
}


#################################################################################
#################################################################################
# map the constraints
GLAM_setup$RUNS_DIR <- paste(GLAM_setup$B_DIR,"/runs/constraints",sep="")
GLAM_setup$OUT_RS_DIR <- paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/rasters",sep="")
if (!file.exists(GLAM_setup$OUT_RS_DIR)) {dir.create(GLAM_setup$OUT_RS_DIR)}

#1. for a given year put the percent change in yield caused by that given
#   process into a raster
if (require(raster)) {
  base_rs <- raster(paste(GLAM_setup$CAL_DIR,"/",GLAM_setup$EXP_DIR,"/general/calib_results_spat/y_obs.asc",sep=""))
  base_rs[] <- NA
}

#loop the years
for (yr in min(GLAM_setup$YEARS):max(GLAM_setup$YEARS)) {
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
}



#3. per constraint, calculate percent of cells over each year that is
#   subjected to that particular constraint. Plot all constraints in
#   the chart. Maybe as an stacked barplot.








