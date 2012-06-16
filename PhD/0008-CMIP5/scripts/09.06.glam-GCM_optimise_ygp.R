#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5/scripts"
#bDir <- "F:/PhD-work/crop-modelling/GLAM"
#cmipDir <- "V:/eejarv/CMIP5"


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
cmipDir <- "/nfs/a102/eejarv/CMIP5"

#source(paste(src.dir2,"/09.06.glam-GCM_optimise-ygp.R",sep=""))

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
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#input directories and model
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
glam_dir <- paste(cmipDir,"/analysis_glam",sep="")
input_dir <- paste(glam_dir,"/inputs",sep="")
pDir <- paste(input_dir,"/params",sep="") #parameter files

#load cell details
cells <- read.csv(paste(input_dir,"/calib-cells-selection.csv",sep=""))

#output folders
asc_dir <- paste(input_dir,"/ascii",sep="")
sow_dir <- paste(asc_dir,"/sow",sep="")
wth_dir <- paste(asc_dir,"/wth_fut",sep="")

#list of gcms
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
procList <- data.frame(GCM=gcmList)

#this_proc <- 1

runs_odir <- paste(glam_dir,"/model-runs/gcm_runs",sep="")
if (!file.exists(runs_odir)) {dir.create(runs_odir)}

### wrapper function to optimize the ygp using the GCM data
wrapper_GCM_glam_optimise_ygp <- function(this_proc) {
  
  #source all needed functions
  source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
  source(paste(src.dir,"/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
  source(paste(src.dir,"/glam-soil-functions.R",sep=""))
  source(paste(src.dir,"/glam-make_wth.R",sep=""))
  source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
  source(paste(src.dir,"/climateSignals-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  
  #get gcm and ensemble member names
  gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",gcm,"-",ens,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  
  #create an output control directory if it does not exist yet
  octr_dir <- paste(runs_odir,"/x.proc",sep="")
  if (!file.exists(octr_dir)) {dir.create(octr_dir)}
  
  cFile <- paste(octr_dir,"/",gcm,"_",ens,".proc",sep="")
  
  if (!file.exists(cFile)) {
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
      setup$CAL_DIR <- paste(runs_odir,"/",gcm,"_",ens,sep="")
      setup$YIELD_FILE <- paste(cDir,"/inputs/ascii/obs/yield_",setup$CELL,"_",setup$METHOD,".txt",sep="")
      setup$YGP_FILE <- "nofile"
      setup$SOW_FILE_RFD <- paste(input_dir,"/ascii/sow/opt_calib_",setup$CELL,".txt",sep="")
      setup$SOW_FILE_IRR <- paste(cDir,"/inputs/ascii/sow/sowing_",setup$CELL,"_irr.txt",sep="")
      setup$WTH_DIR_RFD <- paste(input_dir,"/ascii/wth_fut/",gcm,"_ENS_",ens,"/rfd_",setup$CELL,sep="")
      setup$WTH_DIR_IRR <- paste(input_dir,"/ascii/wth_fut/",gcm,"_ENS_",ens,"/irr_",setup$CELL,sep="")
      setup$WTH_ROOT <- "ingc"
      setup$SOL_FILE <- paste(input_dir,"/ascii/soil/soiltypes2.txt",sep="")
      setup$SOL_GRID <- paste(input_dir,"/ascii/soil/soilcodes_",setup$CELL,".txt",sep="")
      setup$SIM_NAME <- paste("calib_",setup$CELL,sep="")
      setup$PRE_SEAS <- "OR" #OR: original input data, RF: rainfed by default, IR: irrigated by default
      
      cat("\nprocessing cell",setup$CELL,"\n")
      
      #get defaults (parameter set)
      params <- GLAM_get_default(x=cells,cell=setup$CELL,parDir=pDir)
      params$glam_param.mod_mgt$ISYR <- 1966 #start year
      params$glam_param.mod_mgt$IEYR <- 1993 #end year
      params$glam_param.mod_mgt$IASCII <- 1 #output only to .out file
      params$glam_param.mod_mgt$HTS <- "-1" #turn off HTS
      params$glam_param.sparer$RSPARE1$Value <- -99 #turn off TDS
      params$glam_param.sparer$RSPARE2$Value <- -99 #turn off TDS
      
      #extract irrigation rates
      irDir <- paste(cDir,"/irrigated_ratio",sep="")
      library(raster)
      ir_stk <- stack(paste(irDir,"/raw-",1966:1993,".asc",sep=""))
      ir_vls <- extract(ir_stk,cbind(X=cells$X[which(cells$CELL==setup$CELL)],Y=cells$Y[which(cells$CELL==setup$CELL)]))
      ir_vls <- as.numeric(ir_vls)
      ir_vls <- data.frame(YEAR=1966:1993,IRATIO=ir_vls)
      ir_vls$IRATIO[which(ir_vls$IRATIO > 1)] <- 1
      
      ######################################################
      # final calibration of YGP
      ######################################################
      #run the optimiser for YGP, 20 steps
      parname <- "YGP"
      where <- "glam_param.ygp"
      nstep <- 20
      
      if (!file.exists(paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))) {
        # reset lists of output parameters
        optimal <- list(); optimised <- list()
        
        optimised[[parname]] <- GLAM_optimise_loc(GLAM_params=params,RUN_setup=setup,sect=where,
                                                  param=parname,n.steps=20,iter=tolower(parname),
                                                  iratio=ir_vls)
        
        optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
        cat(parname,":",optimal[[parname]],"\n")
        if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
        
        save(list=c("optimised","optimal"),file=paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/iter-",tolower(parname),"/output.RData",sep=""))
        
        #now make the plot
        plotsDir <- paste(setup$CAL_DIR,"/",setup$SIM_NAME,"/plots",sep="")
        if (!file.exists(plotsDir)) {dir.create(plotsDir)}
        
        tiff(paste(plotsDir,"/",tolower(parname),".tif",sep=""),res=300,compression="lzw",height=1000,
             width=1250,pointsize=8)
        par(mar=c(3,3,2,1))
        plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",
             main=paste(parname," :: ",optimal[[parname]],sep=""),
             xlab="Parameter value",ylab="RMSE (kg/ha)")
        grid(nx=10,ny=10)
        abline(v=optimal[[parname]],col="red",lty=2,lwd=0.8)
        dev.off()
        
      }
    }
    
    #write the control file
    ff <- file(cFile,"w")
    cat("this model run was finished on",date(),"\n",file=ff)
    close(ff)
  } else {
    cat("process is already done\n")
  }
}


#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("bDir")
sfExport("cmipDir")
sfExport("cropName")
sfExport("cDir")
sfExport("glam_dir")
sfExport("input_dir")
sfExport("pDir")
sfExport("cells")
sfExport("asc_dir")
sfExport("sow_dir")
sfExport("wth_dir")
sfExport("gcmChars")
sfExport("gcmList")
sfExport("procList")
sfExport("runs_odir")

#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),wrapper_GCM_glam_optimise_ygp))

#stop the cluster
sfStop()











