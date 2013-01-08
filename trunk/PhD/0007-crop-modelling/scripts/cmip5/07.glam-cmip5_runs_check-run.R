#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Oct 2012

library(raster)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

# #source functions of interest
# source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
# source(paste(src.dir,"/glam/glam-optimise-ygp_ipdate_wrapper.R",sep=""))
# source(paste(src.dir,"/glam/glam-parFile-functions.R",sep=""))
# source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
# source(paste(src.dir,"/glam/glam-runfiles-functions.R",sep=""))
# source(paste(src.dir,"/glam/glam-soil-functions.R",sep=""))
# source(paste(src.dir,"/glam/glam-make_wth.R",sep=""))
# source(paste(src.dir,"/glam/glam-optimise-functions.R",sep=""))
# source(paste(src.dir,"/signals/climateSignals-functions.R",sep=""))
source(paste(src.dir,"/cmip5/07.glam-cmip5_runs-functions.R",sep=""))

#configuration details
cropName <- "gnut"
ver <- "v6"
runs_name <- "cmip5_all"

#base and data directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")
ascDir <- paste(glamInDir,"/ascii",sep="")
sowDir <- paste(ascDir,"/sow",sep="")

#weather data directories
wthDir_his_rw <- paste(glamInDir,"/ascii/wth-cmip5_hist",sep="")
wthDir_rcp_rw <- paste(glamInDir,"/ascii/wth-cmip5_rcp45",sep="")
wthDir_his_bc <- paste(ascDir,"/wth-cmip5_hist_bc",sep="")
wthDir_rcp_bc <- paste(ascDir,"/wth-cmip5_rcp45_bc",sep="")

#Factor 1: Gridcells
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))

#Factor 2: GCMs
gcmList_his <- list.files(wthDir_his_rw,pattern="_ENS_")
gcmList_rcp <- list.files(wthDir_rcp_rw,pattern="_ENS_")
gcmList <- gcmList_his[gcmList_his %in% gcmList_rcp]

#Factor 3: parameter sets
parsetList <- read.csv(paste(cropDir,"/calib/results_exp/summary_exp_33-82/runs_discard.csv",sep=""))
parsetList <- parsetList[which(parsetList$ISSEL==1),]
parsetList <- parsetList$EXPID

#Factor 4: experiments
#conditionals may be needed inside the final function
expList_his <- c("his_allin","his_norain","his_notemp","his_nosrad","his_bcrain", "his_sh") #combined one
expList_rcp <- c("rcp_allin","rcp_bcrain","rcp_del","rcp_sh") #combined one

#Factor 6: values of parameters for increased CO2
#TENFAC=0.0, SLA=SLA_B*0.9633, TE=TE_B*1.0880, TTMAX=TTMAX_B*.9377
#TENFAC=0.0, SLA=SLA_B*0.9633, TE=TE_B*1.1467, TTMAX=TTMAX_B*.9377
#TENFAC=0.4, SLA=SLA_B*0.9633, TE=TE_B*1.0880, TTMAX=TTMAX_B*.9377
#TENFAC=0.4, SLA=SLA_B*0.9633, TE=TE_B*1.1467, TTMAX=TTMAX_B*.9377
CO2Exp <- data.frame(EXP_NAME=c("CO2_p1","CO2_p2","CO2_p3","CO2_p4"),
                     TENFAC=c(0,0,0.4,0.4),SLA_INI=c(0.9633,0.9633,0.9633,0.9633),
                     TE=c(1.0880,1.1467,1.0880,1.1467),
                     P_TRANS_MAX=c(0.9377,0.9377,0.9377,0.9377))
CO2ExpList <- CO2Exp$EXP_NAME


####
#baseline experiments
all_proc_his <- expand.grid(LOC=cells$CELL,GCM=gcmList,PARSET=parsetList,WTH_TYPE=expList_his)
all_proc_his$CO2_P <- "1xCO2"
all_proc_his <- cbind(RUNID=1:nrow(all_proc_his),all_proc_his)
all_proc_his$RUNID <- paste("HIS_",all_proc_his$RUNID+1e8,sep="")

#rcp experiments
all_proc_rcp <- expand.grid(LOC=cells$CELL,GCM=gcmList,PARSET=parsetList,WTH_TYPE=expList_rcp,
                            CO2_P=CO2ExpList)
all_proc_rcp <- cbind(RUNID=1:nrow(all_proc_rcp),all_proc_rcp)
all_proc_rcp$RUNID <- paste("RCP_",all_proc_rcp$RUNID+1e8,sep="")

#all runs together
all_proc <- rbind(all_proc_his,all_proc_rcp)


#do some type of grouping. This is done by gridcell, parameter set and GCM
groupingList <- expand.grid(LOC=cells$CELL,PARSET=parsetList,GCM=gcmList)
#this is 125970 processes

#output directory
out_bdir <- paste(glamDir,"/model-runs/",toupper(cropName),"/runs/",runs_name,sep="")
out_chkdir <- paste(out_bdir,"/_outputs/checks",sep="")
if (!file.exists(out_chkdir)) {dir.create(out_chkdir)}

##########################################################
#loop gcms for given exp
expid <- parsetList[1] #33
for (gcm in gcmList) {
  #gcm <- gcmList[1] #"bcc_csm1_1_ENS_r1i1p1"
  cat("\nProcessing GCM:",gcm,"\n")
  
  #output file
  rfile <- paste(out_chkdir,"/exp-",expid,"_",gcm,".RData",sep="")
  
  if (!file.exists(rfile)) {
    #output of group checking
    grp_out <- check_group(this_gcm=gcm,this_pst=expid,all_proc,groupingList)
    
    #summarise
    grp_sum <- summary_check(grp_out,all_proc)
    
    #write outputs on a RData file
    save(list=c("grp_sum","grp_out"),file=rfile)
  } else {
    load(rfile)
  }
  
  if (gcm==gcmList[1]) {
    sum_exp <- grp_sum
  } else {
    sum_exp <- rbind(sum_exp,grp_sum)
  }
}

write.csv(sum_exp,paste(out_chkdir,"/exp-",expid,"_summary.csv",sep=""),row.names=F,quote=T)


