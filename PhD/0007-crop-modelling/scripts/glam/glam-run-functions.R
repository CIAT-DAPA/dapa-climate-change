#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
cal_dir <- paste(b_dir,"/calib",sep="")
exp_dir <- "exp-02_outputs"
prefix <- "fcal_"
gridcell <- 636
ygp <- "opt"
ipdate <- "opt"
crop <- "gnut"

#get parameter set from a given GLAM run
#this is gridcell specific
GLAM_get_run_param <- function(cal_dir,exp_dir,crop="gnut",prefix="fcal_",gridcell,ygp="opt",ipdate="opt") {
  #if yield gap parameter is =="opt" then grab the optimal ygp from ygp.RData
  
  #directory with run data
  run_dir <- paste(cal_dir,"/",exp_dir,"/gridcells/",prefix,gridcell,sep="")
  
  #grab ygp value
  if (ygp == "opt") {
    ygp_file <- paste(run_dir,"/ygp.RData",sep="")
    load(ygp_file)
    ygp_value <- optimal$YGP
    rm(optimal); rm(optimised); g=gc(); rm(g)
  } else {
    ygp_value <- ygp
  }
  
  #grab ipdate value
  if (ipdate == "opt") {
    ipd_file <- paste(run_dir,"/ipdate.RData",sep="")
    load(ipd_file)
    ipd_value <- optimal$IPDATE
    rm(optimal); rm(optimised); g=gc(); rm(g)
  } else {
    ipd_value <- ipdate
  }
  
  #name of parameter file
  par_file <- paste(run_dir,"/glam-r2-param-",tolower(crop),"-run.txt",sep="")
  if (!file.exists(par_file)) {
    par_file <- paste(run_dir,"/glam-r2-param-",tolower(crop),"-run-rfd.txt",sep="")
  }
  
  params <- GLAM_get_par(par_file,retain="all") #get run parameter set
  params$glam_param.ygp$YGP$Value <- ygp_value #replace ygp
  params$glam_param.spt_mgt$IPDATE$Value <- ipd_value #replace ipdate
}


#get soil data for a given gridcell










