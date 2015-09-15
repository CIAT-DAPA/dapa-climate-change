#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2015
stop("!")

########################################################################
#This script will cleanup all outputs of the script bias_correct_met.R
########################################################################

#directories
wd <- "/nfs/a101/earjr/rice-future-tpe"
#wd <- "~/Leeds-work/rice-future-tpe"
gcm_idir <- "/mnt/data_cluster_2/gcm/cmip5/raw/daily"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
#gcm_odir <- "~/scratch/gcm_meteorology"
#if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
gcm_fdir <- paste(wd,"/gcm_meteorology",sep="")

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  cat("...cleaning up wst=",wst_name,"\n")
  
  wst_odir <- paste(gcm_fdir,"/loc_",wst_name,sep="")
  if (file.exists(paste(wst_odir,"/obs",sep=""))) {system(paste("rm -rf ",wst_odir,"/obs",sep=""))}
  if (file.exists(paste(wst_odir,"/stats",sep=""))) {system(paste("rm -rf ",wst_odir,"/stats",sep=""))}
  
  tabflist <- list.files(wst_odir,pattern="\\.tab")
  for (tfil in tabflist) {
    if (file.exists(paste(wst_odir,"/",tfil,sep=""))) {system(paste("rm -f ",wst_odir,"/",tfil,sep=""))}
  }
}




