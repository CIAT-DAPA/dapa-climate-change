#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015

#1. Extract daily GCM data (HIS and all RCPs) for Brazilian locations for future TPE study
#2. Get observations into format needed by CN/JT scripts
#3. Run bias correction, summary stats / plots
#4. Create oryza2000 files as needed

#source dir
src.dir <- "~/Repositories/dapa-climate-change/rice-future-tpe"

#directories
#wd <- "/nfs/a101/earjr/drybean-future-tpe"
wd <- "~/Leeds-work/drybean-future-tpe"
#rice_wd <- "/nfs/a101/earjr/rice-future-tpe"
rice_wd <- "~/Leeds-work/rice-future-tpe"
gcm_idir <- "/mnt/data_cluster_2/gcm/cmip5/raw/daily"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_odir <- "~/scratch/gcm_meteorology"
if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
gcm_fdir <- paste(wd,"/gcm_meteorology",sep="")
if (!file.exists(gcm_fdir)) {dir.create(gcm_fdir)}

#source functions (fron CN / JT)
source(paste(src.dir,"/gcm_calibration_bc_v2.R",sep=""))

if (!file.exists(paste(gcm_fdir,"/gcm_list.csv",sep=""))) {
  gcmlist <- list.files(paste(rice_wd,"/oryza_meteorology",sep=""),pattern="_cf_rcp45_")
  gcmlist <- gsub("\\.tar.bz2","",gsub("method_cf_rcp45_","",gcmlist))
  write.csv(gcmlist, paste(gcm_fdir,"/gcm_list.csv",sep=""), row.names=F)
} else {
  gcmlist <- paste(read.csv(paste(gcm_fdir,"/gcm_list.csv",sep=""))$x)
}

## variable list
syinfo <- Sys.info()
driver <- syinfo["nodename"]
if (driver == "greenwich.ciat.cgiarad.org") {
  varlist <- "pr"
} else if (driver == "flora.ciat.cgiar.org") {
  varlist <- "tasmax"
} else if (driver == "eclipse.ciat.cgiarad.org") {
  varlist <- "tasmin"
} else if (driver == "fauna.ciat.cgiar.org") {
  varlist <- "rsds"
} else {
  varlist <- c("pr", "tasmax", "tasmin", "rsds")
}

## scenario list
rcplist <- c("rcp26","rcp45","rcp60","rcp85")

#historical and future periods to extract
ts_his <- "1970_2005"
ts_fut <- "2016_2069"

#create location list: id, lat, lon, elev, uf, municipio, file_name
if (!file.exists(paste(obs_dir,"/all_wst_locs.csv",sep=""))) {
  wst_files <- list.files(paste(obs_dir,"/estacoesGO",sep=""),pattern="\\.csv")
  loc_list <- data.frame()
  for (wsf in wst_files) {
    #wsf <- wst_files[1]
    wst_data <- read.csv(paste(obs_dir,"/estacoesGO/",wsf,sep=""))
    wst_data <- unique(wst_data[,c("id_estacao","latitude","longitude","estado","municipio")])
    names(wst_data) <- c("id","lat","lon","uf","municipio")
    wst_data$file_name <- substr(paste(gsub(" ","",substr(toupper(paste(wst_data$municipio)),1,4)),"_",sep=""),1,4)
    loc_list <- rbind(loc_list, wst_data)
  }
  alt_list <- read.csv(paste(rice_wd,"/obs_meteorology/all_wst_locs.csv",sep=""))
  loc_list <- merge(loc_list,alt_list[,c("id","elev")],by="id",all.x=T,all.y=F)
  loc_list <- loc_list[,c("id","lat","lon","elev","uf","municipio","file_name")]
  
  #get missing elevation from http://www.mapcoordinates.net/en
  loc_list$elev[which(loc_list$id == ".CPAC")] <- 527
  loc_list$elev[which(loc_list$id == ".CNPAF.1")] <- 795
  
  #write locations file
  write.csv(loc_list,paste(obs_dir,"/all_wst_locs.csv",sep=""),row.names=F)
} else {
  loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))
}

####
# 1. extract historical and future GCM data
redo <- F
for (loc in 1:nrow(loc_list)) {
  #loc <- 1
  locid <- gsub(".","",paste(loc_list$id[loc]),fixed=T)
  lon <- loc_list$lon[loc]; lat <- loc_list$lat[loc]
  cat("...processing loc=",locid,"\n")
  
  if (!file.exists(paste(gcm_fdir,"/loc_",locid,sep="")) | (redo)) {
    #output dir for location
    loc_odir <- paste(gcm_odir,"/loc_",locid, sep="")
    if (!file.exists(loc_odir)) {dir.create(loc_odir)}
    
    #historical
    for (var in varlist) {
      #var <- varlist[1]
      if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
      gcm_extraction(var, "historical", ts=ts_his, gcmlist, lon, lat, dirgcm=gcm_idir, dirout=loc_odir)
    }
    
    #rcps
    for (rcp in rcplist) {
      for (var in varlist) {
        #var <- varlist[1]
        if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
        gcm_extraction(var, rcp, ts_fut, gcmlist, lon, lat, dirgcm=gcm_idir, dirout=loc_odir)
      }
    }
    
    #copy stuff back
    system(paste("cp -urf ~/scratch/gcm_meteorology/loc_",locid," ",gcm_fdir,"/.",sep=""))
    system(paste("rm -rf ~/scratch/gcm_meteorology/loc_",locid,sep=""))
  }
}


#2. Get observations into format needed by CN/JT scripts
#step 1: load all obs meteorology data

#update variable list
varlist <- c("pr", "tasmax", "tasmin", "rsds")

cat("...loading observed weather data\n")
all_wdata <- data.frame()
for (loc in 1:nrow(loc_list)) {
  #loc <- 1
  locid <- gsub(".","",paste(loc_list$id[loc]),fixed=T)
  lon <- loc_list$lon[loc]; lat <- loc_list$lat[loc]; elev <- loc_list$elev[loc]
  cat("...reading in loc=",locid,"\n")
  
  st_data <- read.csv(paste(obs_dir,"/estacoesGO/GO_",paste(loc_list$id[loc]),"_1980_2013.csv",sep=""))
  st_data$elev <- elev
  st_data <- st_data[,c("id_estacao","data","radiacao","tempMaxima","tempMinima","precipitacao","latitude","longitude","elev","estado","municipio")]
  names(st_data) <- c("id","date","srad","tmax","tmin","prec","lat","lon","elev","state","municipality")
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
  if (nrow(ws_data) != 12419) {
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
        vardata$date <- format(seq(as.Date(paste0(1980,"/1/1")), as.Date(paste0(2013,"/12/31")), "days") ,"%Y-%m-%d")
        names(vardata) <- c("date","value")
        
        #convert srad units only if not bias corrected data (INMET data in Tocantins)
        #MJ m-2 day-1 -> W m-2
        if (vname == "rsds") {
          vardata$value <- vardata$value * 10^6 / (24 * 60 * 60)
        }
        datobs <- write.table(vardata,paste(wst_odir,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),sep=" ",row.names=F, quote=F)
      }
    }
  }
}

#construct list of final weather stations
loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_fdir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]

#load libraries
library(lubridate); library(ggplot2); library(reshape)

#loop stations to merge and bias correct data
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_fdir,"/loc_",wst_name,sep="")
  
  cat("...merging observed and GCM data for wst=",wst_name,"\n")
  #historical
  for (var in varlist) {
    #var <- varlist[1]
    if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
    merge_extraction(varmod, rcp="historical", ts="1981_2005", gcmlist, lon, lat, dataset="wst", dirbase=wst_odir)
  }
  
  #rcps
  for (rcp in rcplist) {
    #rcp <- rcplist[3]
    for (vname in varlist) {
      #vname <- varlist[1]
      if  (vname == "pr") {varmod <- "prec"} else if (vname == "rsds") {varmod <- "srad"} else if (vname == "tasmax") {varmod <- "tmax"} else if (vname == "tasmin") {varmod <- "tmin"}
      merge_extraction(varmod, rcp, ts="2021_2045", gcmlist, lon, lat, dataset="wst", dirbase=wst_odir)
      del_calcs(varmod, rcp, lon, lat, wst_odir) #only to means
      cf_calcs(varmod, rcp, lon, lat, wst_odir) #means and variability
      bc_stats(varmod, rcp, "2021_2045", lon, lat, wst_odir) #plotting / statistics
    }
  }
}




