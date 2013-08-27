#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Aug 2013

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"
adap_name <- "cmip5_adapt"

#base and data directories
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
runsDir <- paste(cropDir,"/runs/",runs_name,sep="")
adapDir <- paste(cropDir,"/adapt",sep="")
arc1Dir <- paste(adapDir,"/cmip5_adapt_arc1",sep="")

#load grid cells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#experimental set up
inList <- c("allin","bcrain","sh","del")
CO2ExpList <- c("CO2_p1","CO2_p2","CO2_p3","CO2_p4")

#load list of parameter sets
expList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
expSel <- expList$EXPID[which(expList$ISSEL == 1)]

#list of GCMs
#gcmList <- list.files(paste(runsDir,"/exp-33_outputs",sep=""),pattern="_ENS_")
gcmList <- c("mohc_hadgem2_cc_ENS_r1i1p1",
             "mohc_hadgem2_es_ENS_r1i1p1",
             "mpi_m_mpi_esm_lr_ENS_r1i1p1",
             "mpi_m_mpi_esm_mr_ENS_r1i1p1",
             "mri_mri_cgcm3_ENS_r1i1p1")

##########################
#all processes
all_proc <- expand.grid(PARSET=expSel,GCM=gcmList)
copy_proc <- expand.grid(LOC=cells$CELL,WTH_TYPE=inList,CO2_P=CO2ExpList)

for (i in 1:nrow(all_proc)) {
  #i <- 1
  expid <- all_proc$PARSET[i]
  gcmid <- paste(all_proc$GCM[i])
  cat("copying",expid,"/",gcmid,"\n")
  
  #copying output data
  tindir <- paste(arc1Dir,"/",gcmid,"_",expid,sep="")
  toutdir <- paste(adapDir,"/cmip5_adapt/exp-",expid,"_outputs/",gcmid,sep="")
  if (!file.exists(toutdir)) {dir.create(toutdir)}
  
  for (j in 1:nrow(copy_proc)) {
    #j <- 1
    loc <- copy_proc$LOC[j]
    wth <- copy_proc$WTH_TYPE[j]
    co2p <- copy_proc$CO2_P[j]
    
    if (gcmid == "mohc_hadgem2_cc_ENS_r1i1p1" & expid <= 38) {
      tdir <- paste(tindir,"/process_mbp_",gcmid,"_",expid,"_",loc,"/rcp_",wth,"_",co2p,"_",loc,sep="")
    } else {
      tdir <- paste(tindir,"/process_arc1_",gcmid,"_",expid,"_",loc,"/rcp_",wth,"_",co2p,"_",loc,sep="")
    }
    system(paste("mv -f ",tdir," ",paste(toutdir,"/.",sep="")))
  }
}


