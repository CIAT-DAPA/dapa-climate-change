#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
#cmipDir <- "V:/eejarv/CMIP5"


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
cmipDir <- "/nfs/a102/eejarv/CMIP5"


#check the existence of three parameters needed for sourcing this script
if (class(try(get("src.dir"),silent=T)) == "try-error") {
  stop("src.dir needs to be set")
}

if (class(try(get("bDir"),silent=T)) == "try-error") {
  stop("bDir needs to be set")
}

if (class(try(get("src.dir2"),silent=T)) == "try-error") {
  stop("src.dir2 needs to be set")
}

if (class(try(get("cmipDir"),silent=T)) == "try-error") {
  stop("cmip5 dir needs to be set")
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
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
input_dir <- paste(glam_dir,"/inputs",sep="")
pDir <- paste(input_dir,"/params",sep="") #parameter files
unp_rundir <- paste(glam_dir,"/model-runs/unperturbed_calib_ygp_ipdate",sep="")

#data folders
asc_dir <- paste(input_dir,"/ascii",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")
wth_dir <- paste(asc_dir,"/wth",sep="")

#output directory
runs_odir <- paste(glam_dir,"/model-runs/perturbed_biological",sep="")
if (!file.exists(runs_odir)) {dir.create(runs_odir)}

#load cell details
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

####perturbations and hts types
pert_runs <- read.table(paste(src.dir2,"/data/GLAMperturb.tab",sep=""),header=T,sep="\t")
hts_types <- read.table(paste(src.dir2,"/data/HTSTypes.tab",sep=""),header=T,sep="\t")
tds_types <- data.frame(TYPE=c("TDS1","TDS2"),HIMIN=c(0.1,0.25),FSW=c(0.1,0.01))

this_pt <- 1
parname <- paste(pert_runs$Parameter[this_pt])
where <- paste(pert_runs$Where[this_pt])

if (parname=="HTS") {
  #do something here
} else if (parname == "TDS") {
  #do something here
} else {
  parlow <- as.numeric(paste(pert_runs$Unperturbed[this_pt])) - as.numeric(paste(pert_runs$Perturbation[this_pt]))
  if (parlow < as.numeric(paste(pert_runs$Minimum.realistic[this_pt]))) {parlow <- as.numeric(paste(pert_runs$Minimum.realistic[this_pt]))}
  parhigh <- as.numeric(paste(pert_runs$Unperturbed[this_pt])) + as.numeric(paste(pert_runs$Perturbation[this_pt]))
}

#ci <- 1
ciList <- 1:nrow(cells)
for (ci in ciList) {
  #get run setup
  #files that were generated
  setup <- list()
  setup$BDIR <- bDir
  setup$CELL <- cells$CELL[ci]
  setup$METHOD <- "lin"
  setup$CROPNAME <- "gnut"
  setup$CAL_DIR <- paste(runs_odir,"/p-",tolower(parname),"_",parlow,sep="")
  setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
  setup$YGP_FILE <- "nofile"
  setup$SOW_FILE_RFD <- paste(sow_dir,"/opt_calib_",setup$CELL,".txt",sep="")
  setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
  setup$WTH_DIR_RFD <- paste(cDir,"/inputs/ascii/wth/rfd_",setup$CELL,sep="")
  setup$WTH_DIR_IRR <- paste(cDir,"/inputs/ascii/wth/irr_",setup$CELL,sep="")
  setup$WTH_ROOT <- "ingc"
  setup$SOL_FILE <- paste(input_dir,"/ascii/soil/soiltypes2.txt",sep="")
  setup$SOL_GRID <- paste(input_dir,"/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
  setup$SIM_NAME <- paste("calib_",setup$CELL,sep="")
  setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
  
  #update wth dir rainfed if there is a modification in weather files
  if (file.exists(paste(wth_dir,"/rfd_",setup$CELL,sep=""))) {
    setup$WTH_DIR_RFD <- paste(wth_dir,"/rfd_",setup$CELL,sep="")
  }
  
  cat("\nprocessing cell",setup$CELL,"\n")
  
  #get defaults (parameter set)
  params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
  params$glam_param.mod_mgt$ISYR <- 1966 #start year
  params$glam_param.mod_mgt$IEYR <- 1993 #end year
  params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
  
  #get the specific configuration of HTS and TDS
  if (parname=="HTS") {
    params$glam_param.mod_mgt$HTS <- "+1" #turn off HTS
    params$glam_param.sparer$RSPARE1$Value <- -99 #turn off TDS
    params$glam_param.sparer$RSPARE2$Value <- -99 #turn off TDS
  } else if (parname == "TDS") {
    params$glam_param.mod_mgt$HTS <- "-1" #turn off HTS
  } else {
    params$glam_param.mod_mgt$HTS <- "-1" #turn off HTS
    params$glam_param.sparer$RSPARE1$Value <- -99 #turn off TDS
    params$glam_param.sparer$RSPARE2$Value <- -99 #turn off TDS
  }
  
  #extract irrigation rates
  irDir <- paste(cDir,"/irrigated_ratio",sep="")
  library(raster)
  ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
  ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
  ir_vls <- as.numeric(ir_vls)
  ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
  ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
  
  #get optimal YGP from unperturbed model run
  load(paste(unp_rundir,"/calib_",setup$CELL,"/iter-ygp/output.RData",sep=""))
  params$glam_param.ygp$YGP$Value <- optimal$YGP
  rm(optimal); rm(optimised); g=gc(); rm(g)
  
  #develop a function to run glam
  params[[where]][[parname]]["Value"] <- parlow
  
  
}





