#Julian Ramirez-Villegas
#University of Leeds / CCAFS / CIAT
#August 2015
stop("!")

#Fig 2. projected changes in climates for all RCPs by 2041-2065, i.e. dT (oC), and dP (%)

library(sp); library(maptools); library(raster); library(rgeos)
library(ggplot2); library(grid); library(gridExtra)

#directories
wd <- "/nfs/a101/earjr/rice-future-tpe"
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")
fig_odir <- paste(wd,"/figures",sep="")

source(paste(wd,"/scripts/thiessen_polygons.R",sep=""))

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

#load Brazil shapefile
bra_shp <- readRDS(paste(wd,"/data/BRA_adm1.rds",sep=""))
bra_shp <- bra_shp[which(bra_shp$ID_1 %in% c(9,12,22,27,7)),]

#construct list of final weather stations
loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_dir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]

#number of stations per state from paper: 3 TO; 7 RO; 16 MT; 25 GO
#remove CALDAS NOVAS (.IPGO.00007), as it was not used in Heinemann et al. (2015) JXB
loc_list <- loc_list[which(loc_list$id != ".IPGO.00007"),]
row.names(loc_list) <- 1:nrow(loc_list)

#create thiessen polygons from weather stations
wst_xy <- loc_list[,c("lon","lat")]; names(wst_xy) <- c("x","y")
thiepol <- voronoipolygons(wst_xy)
proj4string(thiepol)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

#intersect thiessen polygons with state polygons
f_thiepol <- gIntersection(thiepol, bra_shp, byid=T)

#for each weather station (and thiessen polygon), calculate change in seasonal (NDJFM)
#a. mean temperature
#b. total precipitation

#get IDs from f_thiepol for further appending of data
id_list <- data.frame(ID=sapply(slot(f_thiepol,'polygons'),function(x) {slot(x,'ID')}))
id_list$WST_ID <- sapply(id_list$ID, function(x) {as.numeric(unlist(strsplit(paste(x)," ",fixed=T))[1])})
id_list$OBJECTID <- sapply(id_list$ID, function(x) {as.numeric(unlist(strsplit(paste(x)," ",fixed=T))[2])})

wst_xy$WST_ID <- row.names(wst_xy)
id_list <- merge(id_list, wst_xy, by="WST_ID")
names(id_list)[4:5] <- c("x_wst","y_wst")
row.names(id_list) <- id_list$ID; id_list <- id_list[,c("ID","WST_ID","OBJECTID","x_wst","y_wst")]

#lists of RCPs, GCMs, BC method
varlist <- c("prec", "tmean")
rcplist <- c("rcp26","rcp45","rcp60","rcp85")
mthlist <- c("cf","del")
gcmlist <- list.files(paste(gcm_dir,"/loc_",gsub(".","",loc_list$id[1],fixed=T),"/gcm",sep=""))
gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]

if (!file.exists(paste(fig_odir,"/fig2_data.RData",sep=""))) {
  futclim_data <- list()
  data_out <- id_list
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
        dat_his <- read.table(paste(wst_odir,"/raw_ts_historical_",vname,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
        dat_his <- dat_his[,c("date","obs")]
      } else {
        dat_his1 <- read.table(paste(wst_odir,"/raw_ts_historical_tmin_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
        dat_his2 <- read.table(paste(wst_odir,"/raw_ts_historical_tmax_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
        dat_his1 <- dat_his1[,c("date","obs")]; dat_his2 <- dat_his2[,c("date","obs")]
        dat_his <- data.frame(date=dat_his1$date,obs=((dat_his1$obs+dat_his2$obs)*.5))
      }
      
      #extra details
      dat_his$year <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%Y")))
      dat_his$month <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%m")))
      dat_his$day <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%d")))
      dat_his$jday <- as.integer(as.integer(format(as.Date(dat_his$date, "%Y-%m-%d"),"%j")))
      
      #only NDJFM, i.e. c(11:12,1:3)
      mth_list <- c(11:12,1:3)
      dat_his <- dat_his[which(dat_his$month %in% mth_list),]
      
      #calculate seasonal total precip for each year
      allvals <- c()
      for (yr in c(min(dat_his$year):(max(dat_his$year)-1))) {
        #yr <- 1981
        yrdata1 <- dat_his[which(dat_his$year == yr & dat_his$month %in% c(11:12)),]
        yrdata2 <- dat_his[which(dat_his$year == (yr+1) & dat_his$month %in% c(1:3)),]
        yrdata <- rbind(yrdata1, yrdata2)
        if (vname == "prec") {totval <- sum(yrdata$obs, na.rm=T)} else {totval <- mean(yrdata$obs, na.rm=T)}
        allvals <- c(allvals, totval)
      }
      hisval <- mean(allvals, na.rm=T)
      
      for (rcp in rcplist) {
        #rcp <- rcplist[1]
        for (mth in mthlist) {
          #mth <- mthlist[1]
          cat("...processing all GCMs for wst=",wst_name,"/ rcp=",rcp,"/ method=",mth,"/ variable=",vname,"\n")
          for (gcm in gcmlist) {
            #gcm <- gcmlist[1]
            
            #load future data
            if (vname == "prec") {
              dat_rcp <- read.table(paste(wst_odir,"/",mth,"_ts_",rcp,"_",vname,"_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
              dat_rcp <- dat_rcp[,c("date",gcm)]
              names(dat_rcp)[2] <- "gcm"
            } else {
              dat_rcp1 <- read.table(paste(wst_odir,"/",mth,"_ts_",rcp,"_tmin_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
              dat_rcp2 <- read.table(paste(wst_odir,"/",mth,"_ts_",rcp,"_tmax_lon_",lon,"_lat_",lat,".tab",sep=""),header=T)
              dat_rcp1 <- dat_rcp1[,c("date",gcm)]; names(dat_rcp1)[2] <- "gcm"
              dat_rcp2 <- dat_rcp2[,c("date",gcm)]; names(dat_rcp2)[2] <- "gcm"
              dat_rcp <- data.frame(date=dat_rcp1$date,gcm=((dat_rcp1$gcm+dat_rcp2$gcm)*.5))
            }
            
            #extra details
            dat_rcp$year <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%Y")))
            dat_rcp$month <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%m")))
            dat_rcp$day <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%d")))
            dat_rcp$jday <- as.integer(as.integer(format(as.Date(dat_rcp$date, "%Y-%m-%d"),"%j")))
            
            #only NDJFM c(11:12,1:3)
            mth_list <- c(11:12,1:3)
            dat_rcp <- dat_rcp[which(dat_rcp$month %in% mth_list),]
            
            #calculate seasonal total precip for each year
            allvals <- c()
            for (yr in c(min(dat_rcp$year):(max(dat_rcp$year)-1))) {
              #yr <- 1981
              yrdata1 <- dat_rcp[which(dat_rcp$year == yr & dat_rcp$month %in% c(11:12)),]
              yrdata2 <- dat_rcp[which(dat_rcp$year == (yr+1) & dat_rcp$month %in% c(1:3)),]
              yrdata <- rbind(yrdata1, yrdata2)
              if (vname == "prec") {totval <- sum(yrdata$gcm, na.rm=T)} else {totval <- mean(yrdata$gcm, na.rm=T)}
              allvals <- c(allvals, totval)
            }
            rcpval <- mean(allvals, na.rm=T)
            
            #change value
            if (vname == "prec") {
              chgval <- (rcpval - hisval) / max(c(hisval,0.001)) * 100
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
          data_out[which(data_out$WST_ID == which(loc_list$id %in% wst)),cname] <- out_df[which(out_df$variable == vname & out_df$rcp == rcp),i+2]
        }
      }
    }
  }
  save(list=c("data_out","futclim_data"),file=paste(fig_odir,"/fig2_data.RData",sep=""))
} else {
  load(file=paste(fig_odir,"/fig2_data.RData",sep=""))
}

#append these data to thiessen polygons
fpols <- data_out; names(data_out)[1] <- "id"
fpols <- SpatialPolygonsDataFrame(f_thiepol, fpols, match.ID=T)
fpols.points <- fortify(fpols, region="id")
fpols.df <- merge(fpols.points, fpols@data, by="id")

prec_min <- min(data_out[,grep("prec.change",names(data_out))])
prec_max <- max(data_out[,grep("prec.change",names(data_out))])
tmean_min <- min(data_out[,grep("tmean.change",names(data_out))])
tmean_max <- max(data_out[,grep("tmean.change",names(data_out))])

#agreement amongst GCMs
unc_vals <- data_out[,c("WST_ID","x_wst","y_wst","rcp26.prec.agreement","rcp45.prec.agreement","rcp60.prec.agreement","rcp85.prec.agreement")]
unc_vals <- unique(unc_vals)
unc_vals$rcp26.prec.agreement <- paste(round(unc_vals$rcp26.prec.agreement,1))
unc_vals$rcp45.prec.agreement <- paste(round(unc_vals$rcp45.prec.agreement,1))
unc_vals$rcp60.prec.agreement <- paste(round(unc_vals$rcp60.prec.agreement,1))
unc_vals$rcp85.prec.agreement <- paste(round(unc_vals$rcp85.prec.agreement,1))

#printing all figures
for (rcp in rcplist) {
  for (vname in varlist) {
    #vname <- "tmean"; rcp <- "rcp26"
    #details
    cat("...printing variable=",vname,"/ rcp=",rcp,"\n")
    cname <- paste(rcp,".",vname,".change",sep="")
    lims <- ceiling(c(1.4,get(paste(vname,"_max",sep="")))*10)*0.1
    
    #plot object
    viz <- ggplot() + geom_polygon(data=fpols.df, aes_string(x="long", y="lat", group="group", fill=cname), colour='grey 80')
    if (vname == "tmean") {
      brks <- c(ceiling(seq(1.4,get(paste(vname,"_max",sep="")),length.out=10)*10)*0.1)
      viz <- viz + scale_fill_gradient(low="#ffeda0",high="#e31a1c",na.value="grey50",guide="colourbar",
                                      space="Lab",breaks=brks,limits=lims)
    } else {
      exval <- ceiling(max(abs(c(get(paste(vname,"_min",sep="")),get(paste(vname,"_max",sep="")))))*10)*0.1
      brks1 <- round(seq(-1*exval,0,length.out=10),1)
      brks2 <- round(seq(0,exval,length.out=10),1)
      brks <- unique(c(brks1,brks2))
      viz <- viz + scale_fill_gradient2(low="#a6611a",mid="#f5f5f5",high="#018571",na.value="grey50",guide="colourbar",
                                       space="Lab",breaks=brks,limits=c(-1*exval,exval))
    }
    viz <- viz + labs(x="", y="", title="")
    viz <- viz + geom_polygon(data=bra_shp, aes(x=long, y=lat, group=group), fill=NA, colour="black",size=1.1)
    if (vname == "prec") {
      viz <- viz + geom_text(data=unc_vals, aes_string(x="x_wst", y="y_wst", label=paste(rcp,".",vname,".agreement",sep="")), 
                             colour="black", size=3.5, fontface="bold")
    }
    viz <- viz + theme(panel.background=element_rect(fill="white",colour="black",size=0.8),
                       legend.key=element_rect(fill='white',size=1.5),legend.title=element_blank(),
                       legend.key.height=unit(0.25,"in"),legend.key.width=unit(1.6,"in"),
                       legend.position=c("bottom"),legend.text=element_text(size=12,face="bold"),
                       axis.text.x =element_blank(),axis.text.y= element_blank(), axis.ticks=element_blank(),
                       axis.line=element_line(colour="grey 50"))
    viz <- viz + coord_equal(ratio=1)
    #print(viz)
    
    #pdf of plot
    pdf(file=paste(fig_odir,"/fig_2_",rcp,"_",vname,".pdf",sep=""),height=7,width=10)
    print(viz)
    dev.off()
    
    #convert to PNG
    setwd(fig_odir)
    system(paste("convert -verbose -density 300 fig_2_",rcp,"_",vname,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_2_",rcp,"_",vname,".png",sep=""))
    setwd("~")
  }
}


#viz <- ggplot() + geom_polygon(data=fpols.df, aes(x=long, y=lat, group=group, fill=rcp85.tmean.rcp_val), colour='grey 80')
#print(viz)



