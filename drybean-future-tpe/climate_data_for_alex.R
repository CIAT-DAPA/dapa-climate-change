#Julian Ramirez-Villegas
#University of Leeds / CCAFS / CIAT
#August 2015
stop("!")

#Projected changes in climates for all RCPs by 2041-2065, i.e. dT (oC), and dP (%)

#library(sp); library(maptools); library(raster); library(rgeos)
#library(ggplot2); library(grid); library(gridExtra)

#source code directory
src.dir <- "~/Repositories/dapa-climate-change/drybean-future-tpe"

#directories
wd <- "/nfs/a101/earjr/drybean-future-tpe" #directory of drybean results
rice_wd <- "/nfs/a101/earjr/rice-future-tpe" #directory of rice results
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

#construct list of final weather stations
loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_dir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]

#for each weather station (and thiessen polygon), calculate change in seasonal (NDJFM)
#a. mean temperature
#b. total precipitation

#lists of RCPs, GCMs, BC method
varlist <- c("prec", "tmean")
rcplist <- c("rcp26","rcp45","rcp60","rcp85")
mthlist <- c("cf","del")
gcmlist <- paste(read.csv(paste(gcm_dir,"/gcm_list.csv",sep=""))$x)

#focus season
tseas <- "dry"

if (!file.exists(paste(wd,"/data/climate_change_data_",tseas,".RData",sep=""))) {
  futclim_data <- list()
  data_out <- loc_list #id_list
  for (wst in loc_list$id) {
    #wst <- paste(loc_list$id[1])
    wst_name <- gsub(".","",wst,fixed=T)
    lon <- loc_list$lon[which(loc_list$id == wst)]
    lat <- loc_list$lat[which(loc_list$id == wst)]
    wst_odir <- paste(gcm_dir,"/loc_",wst_name,sep="")
    
    cat("\n")
    out_rcp <- data.frame()
    for (vname in varlist) {
      #vname <- varlist[1]
      
      #load historical data (only once)
      if (vname == "prec") {
        dat_his <- read.table(paste(wst_odir,"/raw_merge/raw_ts_historical_",vname,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
        dat_his <- dat_his[,c("date","obs")]
      } else {
        dat_his1 <- read.table(paste(wst_odir,"/raw_merge/raw_ts_historical_tmin_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
        dat_his2 <- read.table(paste(wst_odir,"/raw_merge/raw_ts_historical_tmax_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
        dat_his1 <- dat_his1[,c("date","obs")]; dat_his2 <- dat_his2[,c("date","obs")]
        dat_his <- data.frame(date=dat_his1$date,obs=((dat_his1$obs+dat_his2$obs)*.5))
      }
      
      #extra details
      dat_his$year <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%Y")))
      dat_his$month <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%m")))
      dat_his$day <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%d")))
      dat_his$jday <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%j")))
      
      #only WET: NDJFM, i.e. c(11:12,1:3); and DRY: JFMAM, i.e. c(1:5)
      if (tseas == "wet") {mth_list <- c(11:12,1:3)}
      if (tseas == "dry") {mth_list <- c(1:5)}
      dat_his <- dat_his[which(dat_his$month %in% mth_list),]
      
      #calculate seasonal total precip for each year
      allvals <- c()
      for (yr in c(min(dat_his$year):(max(dat_his$year)-1))) {
        #yr <- 1981
        if (tseas == "wet") {
          yrdata1 <- dat_his[which(dat_his$year == yr & dat_his$month %in% c(11:12)),]
          yrdata2 <- dat_his[which(dat_his$year == (yr+1) & dat_his$month %in% c(1:3)),]
          yrdata <- rbind(yrdata1, yrdata2)
        } else if (tseas == "dry") {
          yrdata <- dat_his[which(dat_his$year == yr & dat_his$month %in% c(1:5)),]
        }
        if (vname == "prec") {totval <- sum(yrdata$obs, na.rm=T)} else {totval <- mean(yrdata$obs, na.rm=T)}
        allvals <- c(allvals, totval)
      }
      hisval <- mean(allvals, na.rm=T)
        
      for (rcp in rcplist) {
        #rcp <- rcplist[1]
        for (mth in mthlist) {
          #mth <- mthlist[1]
          cat("...processing all GCMs for wst=",wst_name,"/ rcp=",rcp,"/ method=",mth,"/ variable=",vname,"\n")
          if (mth == "cf") {mthstr <- "Change_Factor_variability"}
          if (mth == "del") {mthstr <- "Change_Factor_no_variability"}
          for (gcm in gcmlist) {
            #gcm <- gcmlist[1]
            
            #load future data
            if (vname == "prec") {
              dat_rcp <- read.table(paste(wst_odir,"/",mthstr,"/",mth,"_ts_",rcp,"_",vname,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
              dat_rcp <- dat_rcp[,c("date",gcm)]
              names(dat_rcp)[2] <- "gcm"
            } else {
              dat_rcp1 <- read.table(paste(wst_odir,"/",mthstr,"/",mth,"_ts_",rcp,"_tmin_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
              dat_rcp2 <- read.table(paste(wst_odir,"/",mthstr,"/",mth,"_ts_",rcp,"_tmax_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
              dat_rcp1 <- dat_rcp1[,c("date",gcm)]; names(dat_rcp1)[2] <- "gcm"
              dat_rcp2 <- dat_rcp2[,c("date",gcm)]; names(dat_rcp2)[2] <- "gcm"
              dat_rcp <- data.frame(date=dat_rcp1$date,gcm=((dat_rcp1$gcm+dat_rcp2$gcm)*.5))
            }
            
            #extra details
            dat_rcp$year <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%Y")))
            dat_rcp$month <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%m")))
            dat_rcp$day <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%d")))
            dat_rcp$jday <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%j")))
            
            #only WET or DRY
            if (tseas == "wet") {mth_list <- c(11:12,1:3)}
            if (tseas == "dry") {mth_list <- c(1:5)}
            dat_rcp <- dat_rcp[which(dat_rcp$month %in% mth_list),]
            
            #calculate seasonal total precip for each year
            allvals <- c()
            for (yr in c(min(dat_rcp$year):(max(dat_rcp$year)-1))) {
              #yr <- 1981
              if (tseas == "wet") {
                yrdata1 <- dat_rcp[which(dat_rcp$year == yr & dat_rcp$month %in% c(11:12)),]
                yrdata2 <- dat_rcp[which(dat_rcp$year == (yr+1) & dat_rcp$month %in% c(1:3)),]
                yrdata <- rbind(yrdata1, yrdata2)
              } else if (tseas == "dry") {
                yrdata <- dat_rcp[which(dat_rcp$year == yr & dat_rcp$month %in% c(1:5)),]
              }
              if (vname == "prec") {totval <- sum(yrdata$gcm, na.rm=T)} else {totval <- mean(yrdata$gcm, na.rm=T)}
              allvals <- c(allvals, totval)
            }
            rcpval <- mean(allvals, na.rm=T)
            
            #change value
            if (vname == "prec") {
              chgval <- (rcpval - hisval) / max(c(hisval,0.01)) * 100
            } else {
              chgval <- rcpval - hisval
            }
            
            #final data.frame
            out_row <- data.frame(rcp=rcp, method=mth, gcm=gcm, variable=vname, his_val=hisval, rcp_val=rcpval, change=chgval)
            out_rcp <- rbind(out_rcp, out_row)
          }
        }
      }
    }
    
    #list of data
    futclim_data[[wst]] <- out_rcp
    
    #calculate mean and agreement of change
    meanval <- aggregate(out_rcp[,c("his_val","rcp_val","change")], by=list(rcp=out_rcp$rcp,variable=out_rcp$variable),FUN=function(x) {mean(x,na.rm=T)})
    
    #agreement
    unc_func <- function(x) {
      mval <- mean(x,na.rm=T)
      if (mval < 0) {uval <- length(which(x < 0)) / length(x) * 100}
      if (mval > 0) {uval <- length(which(x > 0)) / length(x) * 100}
      if (mval == 0) {uval <- length(which(x == 0)) / length(x) * 100}
      return(uval)
    }
    uncval <- aggregate(out_rcp[,c("change")], by=list(rcp=out_rcp$rcp,variable=out_rcp$variable),FUN=unc_func)
    names(uncval)[3] <- "agreement"
    
    #final data.frame
    out_df <- merge(meanval, uncval, by=c("variable","rcp"))
    
    #put out_df numerical items into data_out
    for (rcp in rcplist) {
      for (vname in varlist) {
        for (i in 1:4) {
          cname <- paste(rcp,".",vname,".",names(out_df)[i+2],sep="")
          if (length(which(cname %in% names(data_out))) == 0) {
            data_out$value <- NA; names(data_out)[ncol(data_out)] <- cname
          }
          data_out[which(loc_list$id %in% wst),cname] <- out_df[which(out_df$variable == vname & out_df$rcp == rcp),i+2]
        }
      }
    }
  }
  save(list=c("data_out","futclim_data"),file=paste(wd,"/data/climate_change_data_",tseas,".RData",sep=""))
} else {
  load(file=paste(wd,"/data/climate_change_data_",tseas,".RData",sep=""))
}




