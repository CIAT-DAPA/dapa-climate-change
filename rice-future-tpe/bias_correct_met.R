#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015

#1. Extract daily GCM data (HIS and all RCPs) for Brazilian locations for future TPE study
#2. Get observations into format needed by CN/JT scripts
#3. Run bias correction, summary stats / plots
#4. Create oryza2000 files as needed

library(lubridate); library(qmap); library(ggplot2)
library(tools); library(reshape); require(grid)

#source dir
src.dir <- "~/Repositories/dapa-climate-change/rice-future-tpe"
bc.dir <- "~/Repositories/dapa-climate-change/IPCC-CMIP5/bias_correction"

#directories
wd <- "/nfs/a101/earjr/rice-future-tpe"
#wd <- "~/Leeds-work/rice-future-tpe"
gcm_idir <- "/mnt/data_cluster_2/gcm/cmip5/raw/daily"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
#gcm_odir <- "~/scratch/gcm_meteorology"
#if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
gcm_fdir <- paste(wd,"/gcm_meteorology",sep="")

#source functions (from CN / JT)
source(paste(bc.dir,"/gcm_calibration_bc_local.R",sep=""))

# #create list of GCMs that have all scenarios
# gcmlist_his <- data.frame(gcm=list.files(paste(gcm_idir,"/historical",sep="")), present.his=1)
# gcmlist_r26 <- data.frame(gcm=list.files(paste(gcm_idir,"/rcp26",sep="")), present.r26=1)
# gcmlist_r45 <- data.frame(gcm=list.files(paste(gcm_idir,"/rcp45",sep="")), present.r45=1)
# gcmlist_r60 <- data.frame(gcm=list.files(paste(gcm_idir,"/rcp60",sep="")), present.r60=1)
# gcmlist_r85 <- data.frame(gcm=list.files(paste(gcm_idir,"/rcp85",sep="")), present.r85=1)
# 
# gcmlist <- gcmlist_his
# for (sce in c("r26","r45","r60","r85")) {gcmlist <- merge(gcmlist, get(paste("gcmlist_",sce,sep="")), by="gcm", all.x=T, all.y=T)}
# for (sce in c("his","r26","r45","r60","r85")) {gcmlist[which(is.na(gcmlist[,paste("present.",sce,sep="")])),paste("present.",sce,sep="")] <- 0}
# gcmlist$present.all <- rowSums(gcmlist[,2:ncol(gcmlist)])
# gcmlist <- gcmlist[which(gcmlist$present.all == 5),]
# gcmlist <- paste(gcmlist$gcm)

## variable list
syinfo <- Sys.info()
driver <- syinfo["nodename"]
if (driver == "climate.ciat.cgiarad.org") {
  varlist <- "pr"
} else if (driver == "flora.ciat.cgiar.org") {
  varlist <- "tasmax"
} else if (driver == "eclipse.ciat.cgiarad.org") {
  varlist <- "tasmin"
} else if (driver == "fauna.ciat.cgiarad.org") {
  varlist <- "rsds"
} else {
  varlist <- c("pr", "tasmax", "tasmin", "rsds")
}

## scenario list
rcplist <- c("rcp26","rcp45","rcp60","rcp85")

#historical and future periods to extract
ts_his <- "1970_2005"
ts_fut <- "2040_2069"

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))
#loc_list <- loc_list[which(loc_list$id == ".INMET.00303" | loc_list$id == ".INMET.00304" | loc_list$id == ".INMET.511"),]

# ####
# # 1. extract historical and future GCM data
# for (loc in 1:nrow(loc_list)) {
#   #loc <- 1
#   locid <- gsub(".","",paste(loc_list$id[loc]),fixed=T)
#   lon <- loc_list$lon[loc]; lat <- loc_list$lat[loc]
#   cat("...processing loc=",locid,"\n")
#   
#   #output dir for location
#   loc_odir <- paste(gcm_odir,"/loc_",locid, sep="")
#   if (!file.exists(loc_odir)) {dir.create(loc_odir)}
#   
#   #historical
#   for (var in varlist) {
#     #var <- varlist[1]
#     if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
#     gcm_extraction(var, "historical", ts=ts_his, gcmlist, lon, lat, dirgcm=gcm_idir, dirout=loc_odir)
#   }
#   
#   #rcps
#   for (rcp in rcplist) {
#     for (var in varlist) {
#       #var <- varlist[1]
#       if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
#       gcm_extraction(var, rcp, ts_fut, gcmlist, lon, lat, dirgcm=gcm_idir, dirout=loc_odir)
#     }
#   }
# }
# 
# #copy stuff back
# #system(paste("cp -urf ~/scratch/gcm_meteorology ",wd,"/.",sep=""))
# #system("rm -rf ~/scratch/gcm_meteorology")


#2. Get observations into format needed by CN/JT scripts
#step 1: load all obs meteorology data
cat("...loading observed weather data\n")
all_wdata <- data.frame()
for (st in c("GO","MT","RO","TO")) {
  #st <- "GO"
  #for TO, load from bias corrected source (in obs_meteorology), for others from Alex' files
  if (st == "TO") {
    st_data <- data.frame()
    for (wst in c(".INMET.00303",".INMET.00304",".INMET.511")) {
      #wst <- ".INMET.00303"
      #gather details
      wst_name <- gsub(".","",wst,fixed=T)
      lon <- loc_list$lon[which(loc_list$id == wst)]; lat <- loc_list$lat[which(loc_list$id == wst)]
      elev <- loc_list$elev[which(loc_list$id == wst)]; munip <- paste(loc_list$municipio[which(loc_list$id == wst)])
      
      #load and format data
      ws_data <- read.table(paste(obs_dir,"/INMET_stations/loc_",wst_name,"/final_dataset_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
      ws_data$id <- wst; ws_data$lat <- lat; ws_data$lon <- lon; ws_data$elev <- elev
      ws_data$state <- st; ws_data$municipality <- munip
      ws_data <- ws_data[,c("id","date","srad","tmax","tmin","prec","lat","lon","elev","state","municipality")]
      
      #append data
      st_data <- rbind(st_data, ws_data)
    }
  } else {
    st_data <- read.csv(paste(obs_dir,"/Arquivao",st,".csv",sep=""),header=F,skip=1)
    names(st_data) <- c("id","date","srad","tmax","tmin","prec","lat","lon","elev","state","municipality")
  }
  all_wdata <- rbind(all_wdata, st_data)
}

#step 2: writing formatted weather data for further merge
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  
  #select data
  cat("...filtering observed weather data for wst=",wst_name,"\n")
  ws_data <- all_wdata[which(all_wdata$id == wst),]
  ws_data <- unique(ws_data)
  
  #if data is incomplete
  cat("   ...n=",nrow(ws_data),"of 12419\n")
  if (nrow(ws_data) != 12419 & !wst %in% c(".INMET.00303",".INMET.00304",".INMET.511")) {
    warning("not allowed different number of rows to expected on wst=",wst_name,", first date=",paste(ws_data$date[1]),"\n")
  } else {
    #output directory
    wst_odir <- paste(gcm_fdir,"/loc_",wst_name,"/obs/wst",sep="")
    if (!file.exists(wst_odir)) {dir.create(wst_odir,recursive=T)}
    
    cat("   ...writing files\n")
    for (vname in varlist) {
      #vname <- varlist[1]
      if  (vname == "pr") {varmod <- "prec"} else if (vname == "rsds") {varmod <- "srad"} else if (vname == "tasmax") {varmod <- "tmax"} else if (vname == "tasmin") {varmod <- "tmin"}
      if (!file.exists(paste(wst_odir,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""))) {
        vardata <- ws_data[,c("date",varmod)]
        
        if (wst %in% c(".INMET.00303",".INMET.00304",".INMET.511")) {
          vardata$date <- format(seq(as.Date(paste0(1980,"/1/1")), as.Date(paste0(2005,"/12/31")), "days") ,"%Y-%m-%d")
        } else {
          vardata$date <- format(seq(as.Date(paste0(1980,"/1/1")), as.Date(paste0(2013,"/12/31")), "days") ,"%Y-%m-%d")
        }
        names(vardata) <- c("date","value")
        
        #convert srad units only if not bias corrected data (INMET data in Tocantins)
        #MJ m-2 day-1 -> w m-2
        if (vname == "rsds" & !wst %in% c(".INMET.00303",".INMET.00304",".INMET.511")) {
          vardata$value <- vardata$value * 10^6 / (24 * 60 * 60)
        }
        datobs <- write.table(vardata,paste(wst_odir,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),sep=" ",row.names=F, quote=F)
      }
    }
  }
}

#construct list of final weather stations
#stations to exclude: wst=BRAZ001, wst=CPTEC0069, wst=INMET00306, wst=INMET00308, 
#                     wst=INMET00305, wst=INMET00307

loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_fdir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]

#loop stations to merge and bias correct data
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_fdir,"/loc_",wst_name,sep="")
  gcmlist <- list.files(paste(wst_odir,"/gcm",sep=""))
  
  #remove GCMs without some of the variables
  gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
  gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
  gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
  gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]
  
  cat("...merging observed and GCM data for wst=",wst_name,"\n")
  #historical
  for (var in varlist) {
    #var <- varlist[1]
    if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
    merge_extraction(varmod, rcp="historical", yi=1981, yf=2005, gcmlist, lon, lat, dataset="wst", dirbase=wst_odir, leap=1, typeData=1)
  }
  
  #rcps
  for (rcp in rcplist) {
    #rcp <- rcplist[1]
    for (vname in varlist) {
      #vname <- varlist[1]
      if  (vname == "pr") {varmod <- "prec"} else if (vname == "rsds") {varmod <- "srad"} else if (vname == "tasmax") {varmod <- "tmax"} else if (vname == "tasmin") {varmod <- "tmin"}
      merge_extraction(varmod, rcp, yi=2041, yf=2065, gcmlist, lon, lat, dataset="wst", dirbase=wst_odir, leap=1, typeData=1)
      del_calcs(varmod, rcp, lon, lat, wst_odir, leap=1) #only to means
      cf_calcs(varmod, rcp, lon, lat, wst_odir, leap=1) #means and variability
      bc_stats(varmod, rcp, yi=2041, yf=2065, lon, lat, wst_odir) #plotting / statistics
    }
  }
  
  #historical
  for (var in varlist) {
    #var <- varlist[1]
    if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
    bc_stats(varmod, rcp="historical", yi=1981, yf=2005, lon, lat, wst_odir) #plotting / statistics
    bc_changes(varmod, rcpList=rcplist, gcmlist=gcmlist, lon, lat, wst_odir) #plotting / statistics
  }
}




