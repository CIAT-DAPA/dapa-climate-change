#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015
stop("!")
require(qmap)

#1. Extract daily WFDEI data (HIS and all RCPs) for Tocantins locations missing 1980-1989 period
#2. Get original 1990-2010 observations into format needed by CN/JT scripts
#3. Run bias correction, summary stats / plots, as if GCM=wfdei and OBS=obs, for 1990-1999
#4. Apply bias correction on period 1980-1989, as if GCM=wfdei
#5. Create single time series 1980-2005, and merge with precipitation data

#directories
wd <- "/nfs/a101/earjr/rice-future-tpe"
#wd <- "~/Leeds-work/rice-future-tpe"
obs_idir <- "/mnt/data_cluster_4/observed/gridded_products/wfdei/daily/nc-files"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
obs_odir <- "~/scratch/obs_meteorology"
#if (!file.exists(obs_odir)) {dir.create(obs_odir)}
obs_fdir <- paste(obs_dir,"/INMET_stations",sep="")

#source functions (fron CN / JT)
source(paste(wd,"/scripts/gcm_calibration_bc_v2.R",sep=""))

## variable list
varlist <- c("tasmax", "tasmin", "rsds")

#historical and future periods to extract
ts_his <- "1979_2010"

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))
loc_list <- loc_list[which(loc_list$id == ".INMET.00303" | loc_list$id == ".INMET.00304" | loc_list$id == ".INMET.511"),]

####
#1. Extract daily WFDEI data (HIS and all RCPs) for Tocantins locations missing 1980-1989 period
for (loc in 1:nrow(loc_list)) {
  #loc <- 1
  locid <- gsub(".","",paste(loc_list$id[loc]),fixed=T)
  lon <- loc_list$lon[loc]; lat <- loc_list$lat[loc]
  cat("...processing loc=",locid,"\n")
  
  #output dir for location
  loc_odir <- paste(obs_odir,"/loc_",locid, sep="")
  if (!file.exists(loc_odir)) {dir.create(loc_odir)}
  
  #historical
  for (var in varlist) {
    #var <- varlist[1]
    if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
    obs_extraction("wfdei", var, ts=ts_his, lon, lat, dirobs=obs_idir, dirout=loc_odir)  
  }
}

#rename wfdei so it becomes a 'gcm'
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  
  cat("...moving WFDEI data for wst=",wst_name,"\n")
  wfdei_idir <- paste(obs_fdir,"/loc_",wst_name,"/obs/wfdei",sep="")
  wfdei_odir <- paste(obs_fdir,"/loc_",wst_name,"/gcm/wfdei",sep="")
  
  if (!file.exists(wfdei_odir)) {
    for (vname in varlist) {
      #vname <- varlist[1]
      if  (vname == "pr") {varmod <- "prec"} else if (vname == "rsds") {varmod <- "srad"} else if (vname == "tasmax") {varmod <- "tmax"} else if (vname == "tasmin") {varmod <- "tmin"}
      ifile <- paste("obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep="")
      ofile <- paste("raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep="")
      if (!file.exists(ofile)) {
        setwd(wfdei_idir)
        system(paste("mv -f", ifile, ofile, sep=" "))
      }
    }
    setwd(paste(obs_fdir,"/loc_",wst_name,sep=""))
    system(paste("mv -f obs gcm"))
  }
}

#2. Get original 1990-2010 observations into format needed by CN/JT scripts
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  
  #load data
  cat("...loading observed weather data for wst=",wst_name,"\n")
  ws_data <- read.csv(paste(obs_fdir,"/",tolower(wst_name),"/",toupper(wst_name),".csv",sep=""))
  ws_data <- unique(ws_data)
  names(ws_data) <- c("date","id","srad","tmax","tmin","prec")
  
  #if data is incomplete
  #output directory
  wst_odir <- paste(obs_fdir,"/loc_",wst_name,"/obs/wst",sep="")
  if (!file.exists(wst_odir)) {dir.create(wst_odir,recursive=T)}
  
  #get day, month, and year separated
  ws_data$year <- as.numeric(sapply(paste(ws_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
  ws_data$month <- as.numeric(sapply(paste(ws_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[2]}))
  ws_data$day <- as.numeric(sapply(paste(ws_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[3]}))
  ws_data$date <- as.Date(ws_data$date)
  ws_data <- ws_data[order(ws_data$date, decreasing=F),]
  
  #select only 1990-2010
  ws_data <- ws_data[which(ws_data$year >= 1990 & ws_data$year <= 2010),]
  
  #write files for each variable
  cat("   ...writing files\n")
  for (vname in varlist) {
    #vname <- varlist[1]
    if  (vname == "pr") {varmod <- "prec"} else if (vname == "rsds") {varmod <- "srad"} else if (vname == "tasmax") {varmod <- "tmax"} else if (vname == "tasmin") {varmod <- "tmin"}
    if (!file.exists(paste(wst_odir,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""))) {
      vardata <- ws_data[,c("date",varmod)]
      names(vardata) <- c("date","value")
      if (vname == "rsds") {vardata$value <- vardata$value * 10^6 / (24 * 60 * 60)}
      datobs <- write.table(vardata,paste(wst_odir,"/obs_ts_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),sep=" ",row.names=F, quote=F)
    }
  }
}

#3. Run bias correction, summary stats / plots, as if GCM=wfdei and OBS=obs, for 1990-2010
skill_mth <- data.frame()
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(obs_fdir,"/loc_",wst_name,sep="")
  
  cat("...merging and bias correcting WFDEI data for wst=",wst_name,"\n")
  #historical
  for (vname in varlist) {
    #vname <- varlist[1]
    if  (vname == "pr") {varmod <- "prec"} else if (vname == "rsds") {varmod <- "srad"} else if (vname == "tasmax") {varmod <- "tmax"} else if (vname == "tasmin") {varmod <- "tmin"}
    merge_extraction(varmod, rcp="historical", ts="1990_1999", gcmlist="wfdei", lon, lat, dataset="wst", dirbase=wst_odir)
    qm_calcs(varmod, rcp="historical", lon, lat, dirbase=wst_odir) #quantile mapping
    sh_calcs(varmod, rcp="historical", lon, lat, dirbase=wst_odir) #to means only
    bc_calcs(varmod, rcp="historical", lon, lat, dirbase=wst_odir) #with variability
    bc_stats(varmod, rcp="historical", ts="1990_1999", lon, lat, dirbase=wst_odir)
    
    #scatter plots
    rw_data <- read.table(paste(wst_odir, "/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    qm_data <- read.table(paste(wst_odir, "/qm_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    sh_data <- read.table(paste(wst_odir, "/sh_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    bc_data <- read.table(paste(wst_odir, "/bc_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    lims <- c(min(rw_data$obs,rw_data$wfdei)*0.5, max(rw_data$obs,rw_data$wfdei))
    
    #calculate skill metrics
    #correlation
    rw_cor <- cor(rw_data$obs, rw_data$wfdei, use="pairwise.complete.obs")
    qm_cor <- cor(qm_data$obs, qm_data$wfdei, use="pairwise.complete.obs")
    sh_cor <- cor(sh_data$obs, sh_data$wfdei, use="pairwise.complete.obs")
    bc_cor <- cor(bc_data$obs, bc_data$wfdei, use="pairwise.complete.obs")
    
    #rmse
    rw_rmse <- sqrt(sum(((rw_data$obs - rw_data$wfdei)^2), na.rm=T) / nrow(rw_data[which(!is.na(rw_data$wfdei)),]))
    qm_rmse <- sqrt(sum(((qm_data$obs - qm_data$wfdei)^2), na.rm=T) / nrow(qm_data[which(!is.na(qm_data$wfdei)),]))
    sh_rmse <- sqrt(sum(((sh_data$obs - sh_data$wfdei)^2), na.rm=T) / nrow(sh_data[which(!is.na(sh_data$wfdei)),]))
    bc_rmse <- sqrt(sum(((bc_data$obs - bc_data$wfdei)^2), na.rm=T) / nrow(bc_data[which(!is.na(bc_data$wfdei)),]))
    
    #relative rmse
    rw_rrmse <- rw_rmse / mean(rw_data$obs, na.rm=T)
    qm_rrmse <- qm_rmse / mean(qm_data$obs, na.rm=T)
    sh_rrmse <- qm_rmse / mean(sh_data$obs, na.rm=T)
    bc_rrmse <- bc_rmse / mean(bc_data$obs, na.rm=T)
    
    #output row and append to data.frame
    out_row <- data.frame(id=wst, variable=varmod, raw.cor=rw_cor, raw.rmse=rw_rmse, raw.rrmse=rw_rrmse,
                          qm.cor=qm_cor, qm.rmse=qm_rmse, qm.rrmse=qm_rrmse, sh.cor=sh_cor, sh.rmse=sh_rmse,
                          sh.rrmse=sh_rrmse, bc.cor=bc_cor, bc.rmse=bc_rmse, bc.rrmse=bc_rrmse)
    skill_mth <- rbind(skill_mth, out_row)
    
    pdf(paste(wst_odir,"/scatterplots_",varmod,".pdf",sep=""),height=6, width=8)
    par(mfrow=c(2,2), mar=c(5,5,1.5,1))
    plot(rw_data$obs, rw_data$wfdei, pch=21, cex=0.8, main="Raw data", xlab="observed", ylab="wfdei", xlim=lims, ylim=lims)
    grid(); abline(0,1,col="red")
    text(x=lims[1],y=lims[2]*.9,pos=4,cex=0.8,paste("r = ",round(rw_cor,3),"\n","RMSE = ",round(rw_rmse,2),"\n","RRMSE = ",round(rw_rrmse,3),sep=""))
    
    plot(qm_data$obs, qm_data$wfdei, pch=21, cex=0.8, main="Quantile mapping", xlab="observed", ylab="wfdei", xlim=lims, ylim=lims)
    grid(); abline(0,1,col="red")
    text(x=lims[1],y=lims[2]*.9,pos=4,cex=0.8,paste("r = ",round(qm_cor,3),"\n","RMSE = ",round(qm_rmse,2),"\n","RRMSE = ",round(qm_rrmse,3),sep=""))
    
    plot(sh_data$obs, sh_data$wfdei, pch=21, cex=0.8, main="Nudging", xlab="observed", ylab="wfdei", xlim=lims, ylim=lims)
    grid(); abline(0,1,col="red")
    text(x=lims[1],y=lims[2]*.9,pos=4,cex=0.8,paste("r = ",round(sh_cor,3),"\n","RMSE = ",round(sh_rmse,2),"\n","RRMSE = ",round(sh_rrmse,3),sep=""))
    
    plot(bc_data$obs, bc_data$wfdei, pch=21, cex=0.8, main="Nudging + var", xlab="observed", ylab="wfdei", xlim=lims, ylim=lims); grid(); abline(0,1,col="red")
    grid(); abline(0,1,col="red")
    text(x=lims[1],y=lims[2]*.9,pos=4,cex=0.8,paste("r = ",round(bc_cor,3),"\n","RMSE = ",round(bc_rmse,2),"\n","RRMSE = ",round(bc_rrmse,3),sep=""))
    dev.off()
  }
}
write.csv(skill_mth, paste(obs_fdir,"/bias_correction_results_1990-1999.csv",sep=""),row.names=F,quote=T)

#4. Apply bias correction on period 1980-1989, as if GCM=wfdei
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[2])
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(obs_fdir,"/loc_",wst_name,sep="")
  
  cat("\n...applying bias correction on 1980-1989 WFDEI data for wst=",wst_name,"\n")
  for (vname in varlist) {
    #vname <- varlist[2]
    if  (vname == "pr") {varmod <- "prec"} else if (vname == "rsds") {varmod <- "srad"} else if (vname == "tasmax") {varmod <- "tmax"} else if (vname == "tasmin") {varmod <- "tmin"}
    
    #read and rewrite data only for 1980-1989
    vdata <- read.table(paste(wst_odir,"/gcm/wfdei/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    vdata$year <- as.numeric(sapply(paste(vdata$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
    vdata <- vdata[which(vdata$year >= 1980 & vdata$year <= 1989),]
    vdata$year <- NULL
    write.table(vdata,paste(wst_odir,"/gcm/wfdei/raw_ts_p80s_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),sep=" ",row.names=F, quote=F)
    rm(vdata)
    
    #extract and bias correct
    merge_extraction(varmod, rcp="p80s", ts="1980_1989", gcmlist="wfdei", lon, lat, dataset="wst", dirbase=wst_odir)
    bc_calcs(varmod, rcp="p80s", lon, lat, dirbase=wst_odir) #with variability
    
    #load historical and the other to see how it looks
    bc_his <- read.table(paste(wst_odir, "/bc_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    bc_80s <- read.table(paste(wst_odir, "/bc_ts_p80s_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    
    pdf(paste(wst_odir,"/tsplot_",varmod,".pdf",sep=""),height=4, width=9)
    par(mar=c(3,5,1,1))
    plot(c(bc_80s$wfdei, bc_his$wfdei), ty="l",xlab=NA,ylab=varmod)
    iind <- length(bc_80s$wfdei) + 1; find <- length(c(bc_80s$wfdei, bc_his$wfdei))
    lines(iind:find, bc_his$obs, col="red")
    grid()
    dev.off()
  }
}

#5. Create single time series 1980-2005, and merge with precipitation data
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[2])
  lon <- loc_list$lon[which(loc_list$id == wst)]
  lat <- loc_list$lat[which(loc_list$id == wst)]
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(obs_fdir,"/loc_",wst_name,sep="")
  
  #load full precipitation time series
  pr1 <- read.csv(paste(obs_fdir,"/",tolower(wst_name),"/",toupper(wst_name),".csv",sep="")) #1983-2013
  pr2 <- read.csv(paste(obs_fdir,"/",tolower(wst_name),"/",tolower(wst_name),"_1980_1982.csv",sep="")) #1983-2013
  names(pr1) <- names(pr2) <- c("date","id","srad","tmax","tmin","prec")
  pr1 <- pr1[,c("date","id","prec")]; pr2 <- pr2[,c("date","id","prec")]
  pr_data <- rbind(pr2, pr1)
  pr_data$date <- as.Date(pr_data$date)
  pr_data <- pr_data[order(pr_data$date, decreasing=F),]
  pr_data$id <- NULL; names(pr_data)[2] <- "prec"
  pr_data$year <- as.numeric(sapply(paste(pr_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
  pr_data <- pr_data[which(pr_data$year <= 2005),]; pr_data$year <- NULL
  
  #load data for other variables
  srad1 <- read.table(paste(wst_odir,"/obs/wst/obs_ts_srad_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
  srad2 <- read.table(paste(wst_odir,"/bc_ts_p80s_srad_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
  srad2 <- srad2[,c("date","wfdei")]; names(srad2)[2] <- names(srad1)[2] <- "srad"
  srad_data <- rbind(srad2, srad1)
  srad_data$year <- as.numeric(sapply(paste(srad_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
  srad_data <- srad_data[which(srad_data$year <= 2005),]; srad_data$year <- NULL
  
  #load data for other variables
  tmin1 <- read.table(paste(wst_odir,"/obs/wst/obs_ts_tmin_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
  tmin2 <- read.table(paste(wst_odir,"/bc_ts_p80s_tmin_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
  tmin2 <- tmin2[,c("date","wfdei")]; names(tmin2)[2] <- names(tmin1)[2] <- "tmin"
  tmin_data <- rbind(tmin2, tmin1)
  tmin_data$year <- as.numeric(sapply(paste(tmin_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
  tmin_data <- tmin_data[which(tmin_data$year <= 2005),]; tmin_data$year <- NULL
  
  #load data for other variables
  tmax1 <- read.table(paste(wst_odir,"/obs/wst/obs_ts_tmax_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
  tmax2 <- read.table(paste(wst_odir,"/bc_ts_p80s_tmax_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
  tmax2 <- tmax2[,c("date","wfdei")]; names(tmax2)[2] <- names(tmax1)[2] <- "tmax"
  tmax_data <- rbind(tmax2, tmax1)
  tmax_data$year <- as.numeric(sapply(paste(tmax_data$date), FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
  tmax_data <- tmax_data[which(tmax_data$year <= 2005),]; tmax_data$year <- NULL
  
  #merge everything together
  pr_data$date <- paste(pr_data$date); srad_data$date <- paste(srad_data$date)
  tmin_data$date <- paste(tmin_data$date); tmax_data$date <- paste(tmax_data$date)
  ws_data <- merge(pr_data, srad_data, by="date")
  ws_data <- merge(ws_data, tmin_data, by="date")
  ws_data <- merge(ws_data, tmax_data, by="date")
  
  #check for NAs
  pr_nna <- nrow(ws_data[which(is.na(ws_data$prec)),]); cat("...NAs for prec=",pr_nna,"\n")
  sr_nna <- nrow(ws_data[which(is.na(ws_data$srad)),]); cat("...NAs for srad=",sr_nna,"\n")
  tn_nna <- nrow(ws_data[which(is.na(ws_data$tmin)),]); cat("...NAs for tmin=",tn_nna,"\n")
  tx_nna <- nrow(ws_data[which(is.na(ws_data$tmax)),]); cat("...NAs for tmax=",tx_nna,"\n")
  
  #write data
  if (max(c(pr_nna,sr_nna,tn_nna,tx_nna)) == 0) {
    write.table(ws_data,paste(wst_odir,"/final_dataset_lon_",lon,"_lat_",lat,".tab",sep=""),sep=" ",row.names=F, quote=F)
  } else {
    cat("NAs found, file not written\n")
  }
}

