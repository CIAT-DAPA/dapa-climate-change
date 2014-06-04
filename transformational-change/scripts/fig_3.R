#Julian Ramirez-Villegas and Ulrike Rippke
#CIAT / CCAFS / UoL
#May 2014
stop("!")

#load needed libraries
library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

####
#maps: of time of crossing
#1. loop running decades to calculate frequency of crossing each threshold
#2. from these calculate:
#   *time1: 0-2 years below threshold
#   *time2: 3-5 years below threshold
#   *time3: more than 5 years below threshold
#3. use these to determine date of crossing
#4. calculate minimum and maximum crossing amongst all GCMs
#5. plot minimum, maximum and ensemble mean

#i/o directories
#b_dir <- "Y:/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_modelling"
b_dir <- "/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_modelling"
base_run <- paste(b_dir,"/CRU_30min_1971-2000_af/analyses/cru_select_corNames",sep="")
#out_dir <- paste("D:/transformational-adaptation") #lenovo
out_dir <- paste("~/Google Drive/papers/transformational-adaptation") #mbp
fig_dir <- paste(out_dir,"/figures",sep="")
dfil_dir <- paste(out_dir,"/data_files",sep="")
if (!file.exists(dfil_dir)) {dir.create(dfil_dir)}

#rcp input dir
rcp <- "RCP_60_new" #RCP_60_new | RCP_85
rcp_run <- paste(b_dir,"/FUTURE_af/",rcp,"/analyses/runs-future",sep="")

#read in thresholds and crop names
thresh_val <- read.csv(paste(b_dir,"/thres_ov_short.csv",sep=""))

#load baseline suitability rasters
base_stk <- stack(paste(base_run,"/",thresh_val$crops,sep=""))
names(base_stk) <- paste(thresh_val$crops)

#list of GCMs
gcm_list <- list.files(rcp_run)

#list of years and decades
yr_list <- c(2006:2098)
dc_list <- c((min(yr_list)+10):(max(yr_list)-9))

#loop through crops
ctime <- 2
cross_2_all <- list(earliest=c(),mean=c(),latest=c())
for (i in 1:nrow(thresh_val)) {
  #i <- 1
  crop_name <- paste(thresh_val$crops[i])
  cat("\n...processing crop=",crop_name,"\n")
  
  #folder of dfil_dir per crop
  dfil_crop <- paste(dfil_dir,"/",gsub("\\.tif","",crop_name),sep="")
  
  if (exists("cross_stk")) rm(list=c("cross_stk","dc_out_stk"))
  cross_all <- list(cross1=c(),cross2=c())
  for (gcm in gcm_list[-grep("eco_ensemble",gcm_list)]) {
    #gcm <- gcm_list[1]
    
    #load processed output
    load(file=paste(dfil_crop,"/crossing_",rcp,"_",gcm,".RData",sep=""))
    
    #put into raster stack
    cross_all$cross2 <- c(cross_all$cross2, cross_stk$layer.2)
    
    #remove any previous objects and load data for this GCM
    rm(list=c("cross_stk","dc_out_stk"))
  }
  
  cross_all$cross2 <- stack(cross_all$cross2)
  
  crosstime <- c(calc(cross_all[[paste("cross",ctime,sep="")]], fun=function(x){if (length(which(is.na(x))) == length(x)) {return(NA)} else {return(min(x,na.rm=T))}}),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=function(x){if (length(which(is.na(x))) == length(x)) {return(NA)} else {return(round(mean(x,na.rm=T)))}}),
                 calc(cross_all[[paste("cross",ctime,sep="")]], fun=function(x){if (length(which(is.na(x))) == length(x)) {return(NA)} else {return(max(x,na.rm=T))}}))
  crosstime <- stack(crosstime); names(crosstime) <- c("Earliest","Mean","Latest")
  crosstime[which(crosstime[] < 0)] <- NA
  crosstime[which(crosstime[] > 2089)] <- 2095
  
  #append into multi-crop list
  cross_2_all$earliest <- c(cross_2_all$earliest, crosstime[["Earliest"]])
  cross_2_all$mean <- c(cross_2_all$mean, crosstime[["Mean"]])
  cross_2_all$latest <- c(cross_2_all$latest, crosstime[["Latest"]])
}
cross_2_all$earliest <- stack(cross_2_all$earliest)
cross_2_all$mean <- stack(cross_2_all$mean)
cross_2_all$latest <- stack(cross_2_all$latest)

names(cross_2_all$earliest) <- paste(thresh_val$crops)
names(cross_2_all$mean) <- paste(thresh_val$crops)
names(cross_2_all$latest) <- paste(thresh_val$crops)

#extract xy data
rs_stk <- cross_2_all$mean
xy_all <- as.data.frame(xyFromCell(cross_2_all$earliest, 1:ncell(cross_2_all$earliest)))
xy_all <- cbind(xy_all, extract(cross_2_all$earliest, xy_all[,c("x","y")]))

#function to calculate substitute for each crop
calc_subs <- function(x, posit=1) {
  #x <- as.numeric(xy_all[9941,])
  x <- round(x,0)[3:length(x)]
  x_ref <- x[posit]
  if (is.na(x_ref)) {
    y <- NA
  } else {
    which(x[-posit] == max(x[-posit],na.rm=T))
  }
  
}




