#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#March 2016

#libraries
library(ggplot2); library(reshape)

#directories
wd <- "~/Leeds-work/rice-future-tpe"
#wd <- "/nfs/a101/earjr/rice-future-tpe"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_fdir <- paste(wd,"/gcm_meteorology",sep="")

## scenario list
rcplist <- c("rcp26","rcp45","rcp60","rcp85")

#historical and future periods to extract
ts_his <- "1970_2005"
ts_fut <- "2040_2069"

#list of variables
varlist <- c("pr", "tasmax", "tasmin", "rsds")

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

#leave in list only those stations thats were indeed processed
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
  
  #list of GCMs
  gcmlist <- list.files(paste(wst_odir,"/gcm",sep=""))
  gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
  gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
  gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
  gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]
  
  #loop variables
  for (var in varlist) {
    #var <- varlist[1]
    if  (var == "pr") {varmod <- "prec"} else if (var == "rsds") {varmod <- "srad"} else if (var == "tasmax") {varmod <- "tmax"} else if (var == "tasmin") {varmod <- "tmin"}
    
    #read baseline (observed + raw)
    obs <- read.table(paste(wst_odir,"/raw_ts_historical_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
    obs$year <- as.integer(format(as.Date(obs$date, "%Y-%m-%d"),"%Y"))
    obs$month <- as.integer(format(as.Date(obs$date, "%Y-%m-%d"),"%m"))
    
    #calculate monthly means
    if (var == "pr") {
      obs_agg <- aggregate(obs[,c("obs",gcmlist)],by=list(month=obs$month,year=obs$year),FUN=function(x) {sum(x,na.rm=T)})
    } else {
      obs_agg <- aggregate(obs[,c("obs",gcmlist)],by=list(month=obs$month,year=obs$year),FUN=function(x) {mean(x,na.rm=T)})
    }
    obs_agg <- aggregate(obs_agg[,c("obs",gcmlist)],by=list(month=obs_agg$month),FUN=function(x) {mean(x,na.rm=T)})
    
    #calculate annual total and multi-model-mean (mmm)
    if (var == "pr") {
      obs_agg <- rbind(obs_agg,c(13,colSums(obs_agg[,c("obs",gcmlist)],na.rm=T)))
    } else {
      obs_agg <- rbind(obs_agg,c(13,colMeans(obs_agg[,c("obs",gcmlist)],na.rm=T)))
    }
    obs_agg$mmm <- rowMeans(obs_agg[,gcmlist],na.rm=T)
    his_agg <- obs_agg <- obs_agg
    obs_agg[,c(gcmlist,"mmm")] <- NA; his_agg[,"obs"] <- NA
    obs_agg <- melt(obs_agg, id.vars="month"); names(obs_agg) <- c("month","model","obs")
    obs_agg <- obs_agg[which(obs_agg$model == "obs"),]; obs_agg$model <- NULL
    his_agg <- melt(his_agg, id.vars="month"); names(his_agg) <- c("month","model","his_raw")
    his_agg <- his_agg[which(his_agg$model != "obs"),]
    obs_agg <- merge(his_agg, obs_agg, by=c("month"),all=T)
    
    #bias-corrected projections
    for (rcp in rcplist) {
      #rcp <- rcplist[1]
      #bc projections
      databc <- read.table(paste(wst_odir,"/del_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
      databc$year <- as.integer(format(as.Date(databc$date, "%Y-%m-%d"),"%Y"))
      databc$month <- as.integer(format(as.Date(databc$date, "%Y-%m-%d"),"%m"))
      
      #calculate monthly means
      if (var == "pr") {
        databc_agg <- aggregate(databc[,gcmlist],by=list(month=databc$month,year=databc$year),FUN=function(x) {sum(x,na.rm=T)})
      } else {
        databc_agg <- aggregate(databc[,gcmlist],by=list(month=databc$month,year=databc$year),FUN=function(x) {mean(x,na.rm=T)})
      }
      databc_agg <- aggregate(databc_agg[,gcmlist],by=list(month=databc_agg$month),FUN=function(x) {mean(x,na.rm=T)})
      if (var == "pr") {
        databc_agg <- rbind(databc_agg,c(13,colSums(databc_agg[,gcmlist],na.rm=T)))
      } else {
        databc_agg <- rbind(databc_agg,c(13,colMeans(databc_agg[,gcmlist],na.rm=T)))
      }
      databc_agg$mmm <- rowMeans(databc_agg[,gcmlist],na.rm=T)
      databc_agg <- melt(databc_agg, id.vars="month")
      names(databc_agg) <- c("month","model","bc")
      
      #raw projections
      datarw <- read.table(paste(wst_odir,"/raw_ts_",rcp,"_",varmod,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
      datarw$year <- as.integer(format(as.Date(datarw$date, "%Y-%m-%d"),"%Y"))
      datarw$month <- as.integer(format(as.Date(datarw$date, "%Y-%m-%d"),"%m"))
      
      #calculate monthly means
      if (var == "pr") {
        datarw_agg <- aggregate(datarw[,gcmlist],by=list(month=datarw$month,year=datarw$year),FUN=function(x) {sum(x,na.rm=T)})
      } else {
        datarw_agg <- aggregate(datarw[,gcmlist],by=list(month=datarw$month,year=datarw$year),FUN=function(x) {mean(x,na.rm=T)})
      }
      datarw_agg <- aggregate(datarw_agg[,gcmlist],by=list(month=datarw_agg$month),FUN=function(x) {mean(x,na.rm=T)})
      if (var == "pr") {
        datarw_agg <- rbind(datarw_agg,c(13,colSums(datarw_agg[,gcmlist],na.rm=T)))
      } else {
        datarw_agg <- rbind(datarw_agg,c(13,colMeans(datarw_agg[,gcmlist],na.rm=T)))
      }
      datarw_agg$mmm <- rowMeans(datarw_agg[,gcmlist],na.rm=T)
      datarw_agg <- melt(datarw_agg, id.vars="month")
      names(datarw_agg) <- c("month","model","fut_raw")
      
      #merge data
      data_g2 <- merge(databc_agg, datarw_agg, by=c("model","month"))
      data_g2 <- merge(obs_agg, data_g2, by=c("model","month"),all=T)
      
      #calculate change
      if (varmod == "tasmin" | varmod == "tasmax") {
        data_g2$chg_rw <- data_g2$fut_raw - data_g2$his_raw
        data_g2$chg_bc <- data_g2$bc - data_g2$obs
      } else {
        data_g2$chg_rw <- (data_g2$fut_raw - data_g2$his_raw) / data_g2$his_raw * 100
        data_g2$chg_bc <- (data_g2$bc - data_g2$obs) / data_g2$obs * 100
      }
      
      #plot by GCM
      for (gcm in c(gcmlist,"mmm")) {
        #gcm <- "mmm" #gcmlist[1]
        plotdata <- data_g2[which(data_g2$model == gcm),]
        plotdata$his_raw <- plotdata$obs <- plotdata$bc <- plotdata$fut_raw <- plotdata$model <- NULL
        plotdata <- melt(plotdata, id.vars="month")
        p <- ggplot(plotdata, aes(x=month, y=value, fill=variable)) + 
          geom_bar(width=0.75,stat="identity",position=position_dodge(),size=0.5) + 
          scale_fill_discrete(name="Type",
                              labels=c("raw", "del")) +
          scale_x_continuous("Month",limits=c(0,14),breaks=seq(0,14,by=1)) + 
          scale_y_continuous("Projected change") + 
          theme_bw() +
          theme(axis.text.x=element_text(size=15),
                axis.text.y=element_text(size=15),
                axis.title.x=element_text(size=18),
                axis.title.y=element_text(size=18),
                legend.position="right",
                legend.title = element_text(size=15),
                legend.text = element_text(size=15),
                legend.key = element_rect(color="white"))
        print(p)
      }
      
    }
  }
}
