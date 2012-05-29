#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "F:/PhD-work/crop-modelling/GLAM"
#maxiter <- 20


#eljefe
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
#maxiter <- 20

#source(paste(src.dir,"/glam-optimise-ygp_ipdate.R",sep=""))

#check the existence of three parameters needed for sourcing this script
if (class(try(get("src.dir"),silent=T)) == "try-error") {
  stop("src.dir needs to be set")
}

if (class(try(get("bDir"),silent=T)) == "try-error") {
  stop("bDir needs to be set")
}

if (class(try(get("maxiter"),silent=T)) == "try-error") {
  stop("maxiter (max. num. iterations) needs to be set")
}


#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM

#source all needed functions
source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))


#input directories and model
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
pDir <- paste(cDir,"/params",sep="") #parameter files

#load cell details
#cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
cells <- read.csv(paste(cDir,"/inputs/calib-cells-selection.csv",sep=""))

#ci <- 1

#get run setup
#files that were generated
setup <- list()
setup$BDIR <- bDir
setup$CELL <- cells$CELL[ci]
setup$ZONE <- cells$ZONE[ci]
setup$METHOD <- "lin"
setup$CROPNAME <- "gnut"
setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
setup$SOW_FILE_RFD <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_start.txt",sep="")
setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
setup$WTH_ROOT <- "ingc"
setup$SOL_FILE <- paste(cDir,"/inputs/ascii/soil/soiltypes_",setup$CELL,".txt",sep="")
setup$SOL_GRID <- paste(cDir,"/inputs/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
setup$SIM_NAME <- paste("fcal_",setup$CELL,sep="")
setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default


#get defaults (parameter set)
params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
params$glam_param.mod_mgt$ISYR <- 1966 #start year
params$glam_param.mod_mgt$IEYR <- 1993 #end year
params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file


#load list of parameters to optimise, ranges, and number of steps
opt_rules <- read.table(paste(pDir,"/optimisation-rules.txt",sep=""),sep="\t",header=T)


#extract irrigation rates
irDir <- paste(cDir,"/irrigated_ratio",sep="")
library(raster)
ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
ir_vls <- as.numeric(ir_vls)
ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1


###############################################
# final calibration of IPDATE and YGP
###############################################

###############################################
#load the calib.csv, last iteration
cal_data <- read.csv(paste(cDir,"/calib/z",setup$ZONE,"_rfd_irr/calib.csv",sep=""))
optimal <- cal_data[which(cal_data$iter==maxiter),]

#update the parameter set
for (rw in 1:nrow(optimal)) {
  pname <- paste(optimal$param[rw])
  where <- paste(optimal$sect[rw])
  
  if (pname == "TB" | pname == "TO" | pname == "TM") {
    params[[where]][[paste(pname,"FLWR",sep="")]][,"Value"] <- optimal$opt_val[rw]
    params[[where]][[paste(pname,"PODF",sep="")]][,"Value"] <- optimal$opt_val[rw]
    params[[where]][[paste(pname,"LMAX",sep="")]][,"Value"] <- optimal$opt_val[rw]
    params[[where]][[paste(pname,"HARV",sep="")]][,"Value"] <- optimal$opt_val[rw]
  } else {
    params[[where]][[pname]][,"Value"] <- optimal$opt_val[rw]
  }
}


######################################################
#now optimise the planting date
######################################################
#which and where is the param
parname <- "IPDATE"
where <- "glam_param.spt_mgt"

if (!file.exists(paste(cDir,"/calib/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
  #reset lists of output parameters
  optimal <- list(); optimised <- list()
  
  # get the planting date from Sacks et al. (2010)
  rs <- raster(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/0_base_grids/igp_dummy.tif",sep=""))
  
  # get longitude and latitude (row and column)
  cells$COL <- colFromX(rs,cells$X)
  cells$ROW <- rowFromY(rs,cells$Y)
  
  #load the planting date rasters
  sow_rs <- raster(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/calendar/",tolower(cropName),"/plant_start_lr.tif",sep=""))
  sow_re <- raster(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/calendar/",tolower(cropName),"/plant_end_lr.tif",sep=""))
  
  #grab the planting data into the cells matrix
  cells$SOW_START <- round(extract(sow_rs,cbind(X=cells$X,Y=cells$Y)),0)
  cells$SOW_END <- round(extract(sow_re,cbind(X=cells$X,Y=cells$Y)),0)
  
  #get the initial and final reported sowing dates
  sow_i <- cells$SOW_START[which(cells$CELL == setup$CELL)]
  sow_f <- cells$SOW_END[which(cells$CELL == setup$CELL)]
  sow_seq <- seq(sow_i,sow_f,by=1)
  nstep <- length(sow_seq)
  
  #set the planting date file to NA, so to pass the configuration check
  setup$SOW_FILE_RFD <- "nofile"
  
  #put these data in the parameter file
  params[[where]][[parname]][,"Value"] <- sow_i
  params[[where]][[parname]][,"Min"] <- sow_i
  params[[where]][[parname]][,"Max"] <- sow_f
  
  optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                        param=parname,n.steps=nstep,iter=tolower(parname),
                                        iratio=ir_vls)
  
  optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
  cat(parname,":",optimal[[parname]],"\n")
  if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
  
  #save the two outputs
  save(list=c("optimised","optimal"),file=paste(cDir,"/calib/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
  
  #update the parameter set to -99 and replace the planting date file
  cells$SOW_DATE <- optimal$IPDATE
  osowFile <- paste(cDir,"/inputs/ascii/sow/opt_",setup$SIM_NAME,".txt",sep="")
  osowFile <- write_sowdates(x=cells,outfile=osowFile,cell=c(setup$CELL),
                             fields=list(CELL="CELL",COL="COL",ROW="ROW",SOW_DATE="SOW_DATE"))
  
  #update setup list
  setup$SOW_FILE_RFD <- osowFile
  
  #set IPDATE again to file input
  params[[where]][[parname]][,"Value"] <- -99
  params[[where]][[parname]][,"Min"] <- -99
  params[[where]][[parname]][,"Max"] <- -99
}




#################################################################################
#run the optimiser for YGP, 20 steps
parname <- "YGP"
where <- "glam_param.ygp"
nstep <- 20

if (!file.exists(paste(cDir,"/calib/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
  # reset lists of output parameters
  optimal <- list(); optimised <- list()
  
  optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                        param=parname,n.steps=20,iter=tolower(parname),
                                        iratio=ir_vls)
  
  optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
  cat(parname,":",optimal[[parname]],"\n")
  if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
  
  save(list=c("optimised","optimal"),file=paste(cDir,"/calib/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
}








