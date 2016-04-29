#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#April 2016
stop("!")

########################################################################
#This script will cleanup all outputs of the script bias_correct_met.R
########################################################################

#directories
wd <- "/nfs/a101/earjr/drybean-future-tpe"
#wd <- "~/Leeds-work/drybean-future-tpe"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_fdir <- paste(wd,"/gcm_meteorology",sep="")

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  cat("...cleaning up wst=",wst_name,"\n")
  
  wst_odir <- paste(gcm_fdir,"/loc_",wst_name,sep="")
  if (file.exists(paste(wst_odir,"/obs",sep=""))) {system(paste("rm -rf ",wst_odir,"/obs",sep=""))}
  if (file.exists(paste(wst_odir,"/raw_merge",sep=""))) {system(paste("rm -rf ",wst_odir,"/raw_merge",sep=""))}
  if (file.exists(paste(wst_odir,"/Change_Factor_no_variability",sep=""))) {system(paste("rm -rf ",wst_odir,"/Change_Factor_no_variability",sep=""))}
  if (file.exists(paste(wst_odir,"/Change_Factor_variability",sep=""))) {system(paste("rm -rf ",wst_odir,"/Change_Factor_variability",sep=""))}
  if (file.exists(paste(wst_odir,"/statistics",sep=""))) {system(paste("rm -rf ",wst_odir,"/statistics",sep=""))}
  if (file.exists(paste(wst_odir,"/stats",sep=""))) {system(paste("rm -rf ",wst_odir,"/stats",sep=""))}
  
  tabflist <- list.files(wst_odir,pattern="\\.tab")
  for (tfil in tabflist) {
    if (file.exists(paste(wst_odir,"/",tfil,sep=""))) {system(paste("rm -f ",wst_odir,"/",tfil,sep=""))}
  }
}





