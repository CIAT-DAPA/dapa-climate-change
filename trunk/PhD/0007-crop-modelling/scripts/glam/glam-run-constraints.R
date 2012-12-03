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
  
  if (!file.exists(paste(b_dir,"/runs/constraints/exp-",expid,"_outputs/run.info",sep=""))) {
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
  
  #################################################################################
  #################################################################################
  #################################################################################
  
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
  
  #map the constraints for ech year
  yr_odir <- lapply(min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),FUN=map_constraint_year,GLAM_setup,cons_data,base_rs)
  
  
  #################################################################################
  #################################################################################
  #################################################################################
  
  #3. per constraint, calculate percent of gridcells over each year that is
  #   subjected to that particular constraint. Plot all constraints in
  #   the chart. Maybe as an stacked barplot with x-axis being years
  
  cons_list <- names(cons_data)
  cons_list <- cons_list[4:length(cons_list)]
  
  control_rs <- paste(GLAM_setup$OUT_RS_DIR,"/",min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),"/control.tif",sep="")
  control_rs <- as.list(control_rs)
  
  #get the proportion of gridcells for each constraint into a table
  if (!file.exists(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/affected-areas.csv",sep=""))) {
    for (cons in cons_list) {
      #cons <- cons_list[1]
      cat("processing constraint",tolower(cons),"...\n")
      
      #load all data for that constraint
      cons_rs <- paste(GLAM_setup$OUT_RS_DIR,"/",min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),"/",tolower(cons),".tif",sep="")
      cons_rs <- as.list(cons_rs)
      
      funapp <- function(x,y) {
        x <- raster(x) #control
        y <- raster(y) #other one
        div <- (y - x) / x * 100
        nps <- length(which(!is.na(x[])))
        ips <- length(which(div[] > 0))
        rat <- ips/nps
        return(rat)
      }
      p_aff <- data.frame(YEAR=min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),RATIO=mapply(funapp,control_rs,cons_rs))
      names(p_aff)[2] <- cons
      if (cons == cons_list[1]) {
        pa_aff <- p_aff
      } else {
        pa_aff <- merge(pa_aff,p_aff,by="YEAR",sort=F)
      }
    }
    write.csv(pa_aff,paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/affected-areas.csv",sep=""),quote=T,row.names=F)
  } else {
    pa_aff <- read.csv(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/affected-areas.csv",sep=""))
  }
}







# ##create a figure with these curves
# img_dir <- paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/figures",sep="")
# if (!file.exists(img_dir)) {dir.create(img_dir)}
# tiff(paste(img_dir,"/affected_areas.tiff",sep=""),res=300,pointsize=10,
#      width=2000,height=1300,units="px",compression="lzw")
# par(mar=c(5,5,1,1),cex=1)
# par(xpd=T, mar=par()$mar+c(0,0,0,9))
# plot(pa_aff$YEAR,pa_aff$DROUGHT_GROWTH,ty="l",ylim=c(0,1),lty=1,col="purple",cex=1.2,
#      xlab="Year",ylab="Fraction area affected by constraint")
# lines(pa_aff$YEAR,pa_aff$DROUGHT_TDS,ty="l",ylim=c(0,1),lty=1,col="blue",cex=1.2)
# lines(pa_aff$YEAR,pa_aff$DROUGHT_REPRODUCTION,ty="l",ylim=c(0,1),lty=1,col="darkblue",cex=1.2)
# lines(pa_aff$YEAR,pa_aff$TEMPERATURE_MEAN,ty="l",ylim=c(0,1),lty=2,col="darkred",cex=1.2)
# lines(pa_aff$YEAR,pa_aff$TEMPERATURE_TTCALC,ty="l",ylim=c(0,1),lty=2,col="red",cex=1.2)
# lines(pa_aff$YEAR,pa_aff$TEMPERATURE_TETRS,ty="l",ylim=c(0,1),lty=2,col="darkorange",cex=1.2)
# lines(pa_aff$YEAR,pa_aff$TEMPERATURE_HTS,ty="l",ylim=c(0,1),lty=2,col="darkorange4",cex=1.2)
# lines(pa_aff$YEAR,pa_aff$RADIATION_PHOTOSYNTHESIS,ty="l",ylim=c(0,1),lty=3,col="black",cex=1.2)
# legend(1995, 0.8,c("drought (GS)","drought (TDS)","drought (SWFF)","mean temp. (GS)","mean temp. (TT)","high temp. (TETRS)","high temp. (HTS)","light extinction (RN)"),
#        cex=0.8,col=c("purple","blue","darkblue","darkred","red","darkorange","darkorange4","black"),
#        lty=c(1,1,1,2,2,2,2,3))
# par(xpd=F)
# grid()
# dev.off()
# 
# 
# ########
# #for each of the dominant constraint maps calculate the proportion of each constraint
# #else set to zero, if two or more then create new category with both mixed
# 
# if (!file.exists(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/dominant-process-stacked.csv",sep=""))) {
#   constraints1 <- paste(GLAM_setup$OUT_RS_DIR,"/",min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),"/constraints.tif",sep="")
#   vals <- unique(as.numeric(values(stack(constraints1))))
#   vals <- vals[which(!is.na(vals))]
#   stacked1 <- lapply(constraints1,FUN=calc_portion_dominant,vals)
#   stacked1 <- do.call("rbind",stacked1)
#   stacked1 <- cbind(YEAR=min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),stacked1)
#   write.csv(stacked1,paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/dominant-process-stacked.csv",sep=""),quote=T,row.names=F)
# } else {
#   stacked1 <- read.csv(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/dominant-process-stacked.csv",sep=""))
#   names(stacked1) <- gsub("X","",names(stacked1))
#   stacked1 <- fix_names(stacked1,cons_list)
# }
# 
# if (!file.exists(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/dominant-process-stacked-no_irr.csv",sep=""))) {
#   constraints2 <- paste(GLAM_setup$OUT_RS_DIR,"/",min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),"/constraints_no_irr.tif",sep="")
#   vals2 <- unique(as.numeric(values(stack(constraints2))))
#   vals2 <- vals2[which(!is.na(vals2))]
#   stacked2 <- lapply(constraints2,FUN=calc_portion_dominant,vals2)
#   stacked2 <- do.call("rbind",stacked2)
#   stacked2 <- cbind(YEAR=min(GLAM_setup$YEARS):max(GLAM_setup$YEARS),stacked2)
#   write.csv(stacked2,paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/dominant-process-stacked-no_irr.csv",sep=""),quote=T,row.names=F)
# } else {
#   stacked2 <- read.csv(paste(GLAM_setup$RUNS_DIR,"/",GLAM_setup$EXP_DIR,"/dominant-process-stacked-no_irr.csv",sep=""))
#   names(stacked2) <- gsub("X","",names(stacked2))
#   red_list <- cons_list[2:length(cons_list)]
#   stacked2 <- fix_names(stacked2,red_list)
# }
# 
# 
# #do the stacked barplot here
# library(ggplot2)
# library(reshape2)
# 
# 
# #with irrigated run
# x <- stacked1
# mx <- melt(x, id.vars=1)
# mx$variable <- paste(mx$variable)
# mx$variable[which(mx$variable=="DROUGHT_GROWTH")] <- "drought (GS)"
# mx$variable[which(mx$variable=="NONE")] <- "none"
# mx$variable[which(mx$variable=="RADIATION_PHOTOSYNTHESIS")] <- "light extinction (RN)"
# mx$variable[which(mx$variable=="DROUGHT_TDS")] <- "drought (TDS)"
# mx$variable[which(mx$variable=="DROUGHT_GROWTH_RADIATION_PHOTOSYNTHESIS")] <- "drought (GS) + light extinction (RN)"
# mx$variable[which(mx$variable=="TEMPERATURE_MEAN")] <- "mean temp. (GS)"
# mx$variable[which(mx$variable=="DROUGHT_REPRODUCTION_TEMPERATURE_HTS")] <- "drought (SWFF) + high temp. (HTS)"
# mx$variable[which(mx$variable=="DROUGHT_GROWTH_TEMPERATURE_MEAN")] <- "drought (GS) + mean temp. (GS)"
# 
# 
# tiff(paste(img_dir,"/stacked_constraints.tiff",sep=""),res=300,pointsize=10,
#      width=2000,height=1000,units="px",compression="lzw")
# ggplot(mx, aes(x=YEAR, y=value, fill=variable)) + 
#   geom_bar(stat="identity") +
#   scale_x_continuous('Year') + 
#   scale_y_continuous('Fraction area affected by constraint') +
#   scale_fill_discrete(name = "Constraint") +
#   opts(axis.title.x = theme_text(face="bold", size=8),
#        axis.title.y = theme_text(face="bold", size=8, angle=90),
#        legend.text = theme_text(size=5))
# dev.off()
# 
# #without irrigated run
# x <- stacked2
# mx <- melt(x, id.vars=1)
# mx$variable <- paste(mx$variable)
# mx$variable[which(mx$variable=="RADIATION_PHOTOSYNTHESIS")] <- "light extinction (RN)"
# mx$variable[which(mx$variable=="DROUGHT_TDS")] <- "drought (TDS)"
# mx$variable[which(mx$variable=="NONE")] <- "none"
# mx$variable[which(mx$variable=="TEMPERATURE_MEAN")] <- "mean temp. (GS)"
# mx$variable[which(mx$variable=="TEMPERATURE_MEAN_RADIATION_PHOTOSYNTHESIS")] <- "drought (GS) + light extinction (RN)"
# mx$variable[which(mx$variable=="DROUGHT_REPRODUCTION_TEMPERATURE_HTS")] <- "drought (SWFF) + high temp. (HTS)"
# mx$variable[which(mx$variable=="DROUGHT_TDS_TEMPERATURE_MEAN")] <- "drought (GS) + mean temp. (GS)"
# mx$variable[which(mx$variable=="TEMPERATURE_TTCALC")] <- "mean temp. (TT)"
# 
# 
# tiff(paste(img_dir,"/stacked_constraints-no_irr.tiff",sep=""),res=300,pointsize=10,
#      width=2000,height=1000,units="px",compression="lzw")
# ggplot(mx, aes(x=YEAR, y=value, fill=variable)) + 
#   geom_bar(stat="identity") +
#   scale_x_continuous('Year') + 
#   scale_y_continuous('Fraction area affected by constraint') +
#   scale_fill_discrete(name = "Constraint") +
#   opts(axis.title.x = theme_text(face="bold", size=8),
#        axis.title.y = theme_text(face="bold", size=8, angle=90),
#        legend.text = theme_text(size=5))
# dev.off()



