#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2015
stop("!")

#load libraries
library(raster); library(sp); library(rgdal)

#directories
src_dir <- "~/Repositories/dapa-climate-change/p4s-csa"
wd <- "~/Leeds-work/p4s-csa"
adm_dir <- paste(wd,"/climate-impacts/adm_data",sep="")
isric_dir <- paste(wd,"/hh-analyses/ISRIC_soil",sep="")
out_dir <- paste(wd,"/climate-impacts/outputs",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

agmerra_dir <- "~/Leeds-work/datasets/AgMERRA_data"
bcsd_dir <- "~/Leeds-work/datasets/bcsd_data"
#bcsd_dir <- paste(wd,"/climate-impacts/bcsd_data",sep="")
#agmerra_dir <- paste(wd,"/hh-analyses/AgMERRA_data",sep="")

#sourcing scripts
source(paste(src_dir,"/hh-analyses/extract_AgMERRA.R",sep=""))
source(paste(src_dir,"/hh-analyses/wgen_srad.R",sep=""))
source(paste(src_dir,"/climate-impacts/extract_bcsd.R",sep=""))
source(paste(src_dir,"/hh-analyses/calc_risk_indices.R",sep=""))
source(paste(src_dir,"/hh-analyses/calc_phdate.R",sep=""))

#list of GCMs and scenarios
gcm_list <- c("bcc-csm1-1","GFDL-ESM2M","BNU-ESM","IPSL-CM5A-LR","MIROC-ESM-CHEM",
              "NorESM1-M","CCSM4")
sce_list <- c("historical","rcp45")

#1. load the admin area of interest
#2. convert to grid based on BCSD resolution
#3. make a data.frame of locations from this
#4. extract AgMERRA statistics
#5. extract pr, tasmin, tasmax for n years from BCSD data
#6. generate srad for years based on AgMERRA statistics and BCSD precip data
#7. calculate indices and make boxplots of frequency of occurrence of these indices

#1. load the admin area of interest
shp <- readRDS(paste(adm_dir,"/MLI_adm1.rds",sep=""))
shp <- shp[which(shp$NAME_1 == "Sikasso"),]

#2. convert to grid based on BCSD resolution
rs_ref <- raster(paste(bcsd_dir,"/pr_day_BCSD_historical_r1i1p1_GFDL-ESM2M_2000.nc",sep=""))
rs_ref <- rotate(rs_ref)
rs_ext <- extent(shp)
rs_ext@xmin <- rs_ext@xmin - xres(rs_ref)*2; rs_ext@xmax <- rs_ext@xmax + xres(rs_ref)*2
rs_ext@ymin <- rs_ext@ymin - yres(rs_ref)*2; rs_ext@ymax <- rs_ext@ymax + yres(rs_ref)*2
rs_ref <- crop(rs_ref, rs_ext)
rs_adm <- rasterize(shp, rs_ref, getCover=T)
rs_adm[which(rs_adm[] <= 0)] <- NA; rs_adm[which(!is.na(rs_adm[]))] <- 1
#plot(rs_adm); plot(shp,add=T)

#3. make a list of locations from this
loc_xy <- as.data.frame(xyFromCell(rs_adm, which(!is.na(rs_adm[]))))
names(loc_xy) <- c("lon","lat")

#4. extract AgMERRA statistics for locations (srad, and prate)
if (!file.exists(paste(out_dir,"/agmerra_data.RData",sep=""))) {
  agmerra_srad <- extract_AgMERRA_mult(in_dir=agmerra_dir,year=NULL,varname="srad",year_range=c(1986,2005),xy=loc_xy)
  agmerra_prate <- extract_AgMERRA_mult(in_dir=agmerra_dir,year=NULL,varname="prate",year_range=c(1986,2005),xy=loc_xy)
  save(list=c("agmerra_srad","agmerra_prate"), file=paste(out_dir,"/agmerra_data.RData",sep=""))
} else {
  load(file=paste(out_dir,"/agmerra_data.RData",sep=""))
}

#5. extract pr, tasmin, tasmax for n years from BCSD data
##############
for (sce in sce_list) {
  for (gcm in gcm_list) {
    #sce <- sce_list[1]; gcm <- gcm_list[1]
    if (sce == "historical") {years <- c(1986:2005)} else {years <- c(2006:2099)}
    ##############
    
    if (!file.exists(paste(out_dir,"/bcsd_data_",sce,"_",gcm,".RData",sep=""))) {
      bcsd_data <- extract_bcsd(sce, gcm, years, bbox=rs_ext, loc_xy, bcsd_dir)
      save(list=c("bcsd_data"),file=paste(out_dir,"/bcsd_data_",sce,"_",gcm,".RData",sep=""))
    } else {
      load(file=paste(out_dir,"/bcsd_data_",sce,"_",gcm,".RData",sep=""))
    }
    
    #6. generate srad for years based on AgMERRA statistics and BCSD precip data
    #loop locations for final calculations
    if (!file.exists(paste(out_dir,"/wth_data_",sce,"_",gcm,".RData",sep=""))) {
      wth_data <- list()
      for (iloc in 1:nrow(loc_xy)) {
        #iloc <- 1
        cat("...calculating srad for site=",iloc,"out of",nrow(loc_xy),"\n")
        
        ###merge agmerra data
        data_agmerra <- merge(agmerra_srad[[iloc]],agmerra_prate[[iloc]],by=c("date","year"))
        data_agmerra$month <- unlist(lapply(paste(data_agmerra$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[2]}))
        
        ###
        for (yr in years) {
          #yr <- years[1]
          tdates <- bcsd_data[[iloc]]
          tdates$year <- unlist(lapply(paste(tdates$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
          tdates <- tdates[which(tdates$year == paste(yr)),]
          tdates$year <- NULL
          
          #rename columns
          names(tdates) <- c("date","prec","tasmin","tasmax")
          
          #generate solar radiation for this year based on historical values and this year's BCSD precipitation
          tdates <- wgen_srad(daily_hist=data_agmerra,tdates,year=yr,lon=loc_xy$lon[iloc],lat=loc_xy$lat[iloc])
          
          if (yr == years[1]) {
            wth_data[[iloc]] <- tdates
          } else {
            wth_data[[iloc]] <- rbind(wth_data[[iloc]], tdates)
          }
          rm(tdates)
        }
      }
      #name columns and correct row numbers
      for (iloc in 1:nrow(loc_xy)) {names(wth_data[[iloc]]) <- c("DATE","RAIN","TMIN","TMAX","SRAD")}
      for (iloc in 1:nrow(loc_xy)) {row.names(wth_data[[iloc]]) <- 1:nrow(wth_data[[iloc]])}
      
      #save object
      save(list=c("wth_data"),file=paste(out_dir,"/wth_data_",sce,"_",gcm,".RData",sep=""))
    } else {
      load(file=paste(out_dir,"/wth_data_",sce,"_",gcm,".RData",sep=""))
    }
    
    #extract soil data, and calculate soil water holding capacity for selected sites
    #soilcap = sum(af_AWCh2__M_sd[1-x]_1km) * x
    if (!file.exists(paste(out_dir,"/soil_data.RData",sep=""))) {
      root_depth <- raster(paste(isric_dir,"/af_ERDICM__M_1km.tif",sep=""))
      rs_prj <- projectRaster(rs_adm, crs=root_depth@crs)
      root_depth <- crop(root_depth, extent(rs_prj))
      root_depth <- projectRaster(root_depth, crs=shp@proj4string)
      
      #calculate average of root depth for each big grid cell
      rs_res <- resample(rs_adm, root_depth, method="ngb")
      rs_ids <- rs_adm; rs_ids[which(!is.na(rs_ids[]))] <- which(!is.na(rs_ids[]))
      rs_pts <- as.data.frame(xyFromCell(rs_res, which(!is.na(rs_res[]))))
      rs_pts$id_coarse <- extract(rs_ids, cbind(x=rs_pts$x, y=rs_pts$y))
      rs_pts$rdepth <- extract(root_depth, data.frame(x=rs_pts$x, y=rs_pts$y))
      rs_pts <- aggregate(rs_pts[,c("x","y","rdepth")],by=list(id_coarse=rs_pts$id_coarse),FUN=function(x) {mean(x,na.rm=T)})
      rs_pts$x <- rs_pts$y <- NULL
      
      #put root_depth data on soil_data data.frame
      soil_data <- loc_xy
      soil_data$id_coarse <- extract(rs_ids, data.frame(x=soil_data$lon, y=soil_data$lat))
      soil_data <- merge(soil_data, rs_pts, by="id_coarse")
      rm(rs_pts)
      
      #extract soil water holding capacity data on soil_data data.frame
      depths <- c(25,100,225,450,800,1500)
      for (s_i in 1:6) {
        #s_i <- 1
        tdepth <- depths[s_i]
        cat("...extracting depth=",tdepth*.1,"cm\n")
        rs <- raster(paste(isric_dir,"/af_AWCh2__M_sd",s_i,"_1km.tif",sep=""))
        rs <- crop(rs, extent(rs_prj))
        rs <- projectRaster(rs, crs=shp@proj4string)
        rs_res <- resample(rs_adm, rs, method="ngb")
        rs_pts <- as.data.frame(xyFromCell(rs_res, which(!is.na(rs_res[]))))
        rs_pts$id_coarse <- extract(rs_ids, cbind(x=rs_pts$x, y=rs_pts$y))
        rs_pts$value <- extract(rs, data.frame(x=rs_pts$x, y=rs_pts$y))
        rs_pts <- aggregate(rs_pts[,c("x","y","value")],by=list(id_coarse=rs_pts$id_coarse),FUN=function(x) {mean(x,na.rm=T)})
        rs_pts$x <- rs_pts$y <- NULL
        soil_data <- merge(soil_data, rs_pts, by="id_coarse")
        names(soil_data)[ncol(soil_data)] <- paste("d.",tdepth,sep="")
        rm(list=c("rs","rs_pts","rs_res"))
      }
      #calculate soil water holding capacity in mm, minval and maxval taken from
      #Fatondji et al. (2012) --in: Kihara, J. et al. Improving soil fert. recommendation using DSSAT
      soil_data$soilcp <- apply(soil_data,1,FUN=soilcap_calc,minval=45,maxval=100)
      
      save(soil_data, file=paste(out_dir,"/soil_data.RData",sep=""))
    } else {
      load(file=paste(out_dir,"/soil_data.RData",sep=""))
    }
    
    
    #calculate watbal
    if (!file.exists(paste(out_dir,"/watbal_",sce,"_",gcm,".RData",sep=""))) {
      watbal_data <- list()
      for (iloc in 1:length(wth_data)) {
        cat("...calculating watbal for loc=",iloc,"out of",length(wth_data),"\n")
        ilon <- loc_xy$lon[iloc]; ilat <- loc_xy$lat[iloc]
        watbal_data[[iloc]] <- watbal_wrapper(out_all=wth_data[[iloc]], soilcp=soil_data$soilcp[which(soil_data$lon == ilon & soil_data$lat == ilat)])
        rm(list=c("ilon","ilat"))
        #plot(watbal_data[[iloc]]$ERATIO*100,ty="l"); lines(watbal_data[[iloc]]$ETMAX,col="red"); lines(watbal_data[[iloc]]$RAIN,col="blue")
      }
      save(watbal_data, file=paste(out_dir,"/watbal_",sce,"_",gcm,".RData",sep=""))
    } else {
      load(file=paste(out_dir,"/watbal_",sce,"_",gcm,".RData",sep=""))
    }
    
    #calculate indices for each year
    if (!file.exists(paste(out_dir,"/indices_data_",sce,"_",gcm,".RData",sep=""))) {
      if (sce != "historical") {
        load(file=paste(out_dir,"/indices_data_historical_",gcm,".RData",sep=""))
        seas_his <- out_indices; rm(out_indices)
      }
      
      out_seas <- data.frame()
      for (yr in years) {
        #yr <- years[1]
        cat("...calculating indices for year=",yr,"\n")
        min_ini <- as.numeric(format(as.Date(paste0(yr,"/4/15")) ,"%j"))
        max_ini <- as.numeric(format(as.Date(paste0(yr,"/5/15")) ,"%j")) + 20
        
        for (iloc in 1:length(watbal_data)) {
          #iloc <- 1
          ilon <- loc_xy$lon[iloc]; ilat <- loc_xy$lat[iloc]
          wbal_loc <- watbal_data[[iloc]]
          wbal_loc$YEAR <- unlist(lapply(paste(wbal_loc$DATE),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
          wbal_loc <- wbal_loc[which(wbal_loc$YEAR == paste(yr)),]
          wbal_loc$YEAR <- NULL
          
          #determine likely sowing dates for each site, report also estimated harvest date
          #taken from Marteau et al. (2011)
          if (sce == "historical") {
            sdate <- pdate_marteau(wbal_loc,mindate=min_ini,maxdate=max_ini,p_thresh=1)
            min_end <- sdate + 50; max_end <- sdate + 200
            hdate1 <- hdate_jones(wbal_loc,mindate=min_end,maxdate=max_end,e_thresh=0.5)
            hdate2 <- hdate_jones(wbal_loc,mindate=min_end,maxdate=max_end,e_thresh=0.6)
            hdate3 <- hdate_jones(wbal_loc,mindate=min_end,maxdate=max_end,e_thresh=0.7)
            #plot(wbal_loc$ERATIO*100,ty="l"); lines(wbal_loc$RAIN,col="blue"); abline(v=c(sdate,hdate1,hdate2,hdate3))
            
            #create growing period data.frame
            gperiod <- data.frame(gp_id=c(1:3),sdate=rep(sdate,3),hdate=c(hdate1,hdate2,hdate3))
            gperiod$anth <- round(gperiod$sdate + (gperiod$hdate - gperiod$sdate) * 0.5)
            gperiod$dur <- gperiod$hdate - gperiod$sdate
          } else {
            sdate <- pdate_marteau(wbal_loc,mindate=min_ini,maxdate=max_ini,p_thresh=1)
            gperiod <- seas_his[which(seas_his$loc_id == iloc),]
            gperiod <- aggregate(gperiod[,c("sdate","hdate","anth","dur")], by=list(gp_id=gperiod$gp_id), FUN=function(x) {round(mean(x,na.rm=T))})
            gperiod$sdate <- sdate
            gperiod$hdate <- gperiod$sdate + gperiod$dur
            gperiod$anth <- round(gperiod$sdate + (gperiod$hdate - gperiod$sdate) * 0.5)
          }
          
          #indices:
          #HTS1: high temperature stress around anthesis (30 day centered at anth), tmax >= 35, 37.5, 40
          #HTS2: high temperature stress during grainfill (5 days after anth until hdate), tmax >= 35, 37.5, 40
          #LETHAL: lethal temperature any day in cycle, tmax >= 43, 47.5, 50
          #CD: crop duration, if Tmean > (22, 23, 24) then CD=T-23, else CD=0
          #DS1: number of days Ea/Ep < 0.4, 0.5, 0.6
          #DS2: max number of consecutive days Ea/Ep < 0.4, 0.5, 0.6
          #DS3: eq. to DS1 but during anthesis (30 day centered at anth)
          #DS4: severe drought around anthesis (30 day centered at anth) cons. days Ea/Ep < 0.1, 0.15, 0.20
          #ATT: accum thermal time using capped top, Tb=7,8,9, To=30,32.5,35
          #DLOSS: duration loss (difference between No. days to reach ATT_baseline in future vs. baseline)
          #PTOT: total rainfall
          #WES: wet early season if period between sowing and anthesis is above field cap. >= 50 % time
          #BADSOW: no. days in sowing window +-15 centered at sdate with 0.05*SOILCP < AVAIL < 0.9*SOILCP
          #BADHAR: no. days in harvest window (+25 after hdate) with AVAIL < 0.85*SOILCP
          #calculate indices for all growing periods
          for (gpi in gperiod$gp_id) {
            #gpi <- gperiod$gp_id[1]
            sdate_i <- gperiod$sdate[gpi]; hdate_i <- gperiod$hdate[gpi]; anth_i <- gperiod$anth[gpi]; dur_i <- gperiod$dur[gpi]
            hts1 <- c(); for (tvi in c(35,37.5,40)) {hts1 <- c(hts1,calc_hts(wbal_loc,(anth_i-15),(anth_i+15),tvi))}
            hts2 <- c(); for (tvi in c(35,37.5,40)) {hts2 <- c(hts2,calc_hts(wbal_loc,(anth_i+5),hdate_i,tvi))}
            lethal <- c(); for (tvi in c(43,47.5,50)) {lethal <- c(lethal,calc_hts(wbal_loc,sdate_i,hdate_i,tvi))}
            cdur <- c(); for (tvi in c(22,23,24)) {cdur <- c(cdur,calc_cdur(wbal_loc,sdate_i,hdate_i,tvi))}
            ds1 <- c(); for (tvi in c(0.4,0.5,0.6)) {ds1 <- c(ds1,calc_wsdays(wbal_loc,sdate_i,hdate_i,tvi))}
            ds2 <- c(); for (tvi in c(0.4,0.5,0.6)) {ds2 <- c(ds2,calc_cons_wsdays(wbal_loc,sdate_i,hdate_i,tvi))}
            ds3 <- c(); for (tvi in c(0.4,0.5,0.6)) {ds3 <- c(ds3,calc_wsdays(wbal_loc,(anth_i-15),(anth_i+15),tvi))}
            ds4 <- c(); for (tvi in c(0.1,0.15,0.2)) {ds4 <- c(ds4,calc_cons_wsdays(wbal_loc,(anth_i-15),(anth_i+15),tvi))}
            att <- c(); for (tvi in 1:3) {att <- c(att,calc_att(wbal_loc,sdate_i,hdate_i,c(7,8,9)[tvi],c(30,32.5,35)[tvi]))}
            
            if (sce == "historical") {
              dloss <- c(); for (tvi in 1:3) {dloss <- c(dloss,calc_dloss(wbal_loc,sdate_i,dur_b=dur_i,att_b=att[tvi],c(7,8,9)[tvi],c(30,32.5,35)[tvi]))}
            } else {
              att_b <- seas_his[which(seas_his$index == "att" & seas_his$gp_id == gpi),]
              att_b <- aggregate(att_b[,c("sdate","hdate","value")],by=list(thresh=att_b$thresh),FUN=function(x) {mean(x,na.rm=T)})
              att_b <- as.numeric(att_b$value)
              dloss <- c(); for (tvi in 1:3) {dloss <- c(dloss,calc_dloss(wbal_loc,sdate_i,dur_b=dur_i,att_b=att_b[tvi],c(7,8,9)[tvi],c(30,32.5,35)[tvi]))}
            }
            
            totrain <- calc_totrain(wbal_loc,sdate_i,hdate_i)
            raindays <- calc_raindays(wbal_loc,sdate_i,hdate_i,p_thresh=1)
            wes <- calc_wes(wbal_loc,sdate_i,anth_i)
            badsow <- calc_badsow(wbal_loc,sdate_i,soil_data$soilcp[iloc])
            badhar <- calc_badhar(wbal_loc,hdate_i,soil_data$soilcp[iloc])
            
            #create data.frame
            out_gpi <- data.frame(loc_id=iloc,lon=ilon,lat=ilat,year=yr,gp_id=gpi,sdate=sdate_i,
                                  hdate=hdate_i,anth=anth_i,dur=dur_i)
            out_gpi <- cbind(out_gpi, rbind(data.frame(index="hts1", thresh=c(35,37.5,40), value=hts1),
                                            data.frame(index="hts2", thresh=c(35,37.5,40), value=hts2),
                                            data.frame(index="lethal", thresh=c(43,47.5,50), value=lethal),
                                            data.frame(index="cdur", thresh=c(22,23,24), value=cdur),
                                            data.frame(index="ds1", thresh=c(0.4,0.5,0.6), value=ds1),
                                            data.frame(index="ds2", thresh=c(0.4,0.5,0.6), value=ds2),
                                            data.frame(index="ds3", thresh=c(0.4,0.5,0.6), value=ds3),
                                            data.frame(index="ds4", thresh=c(0.1,0.15,0.2), value=ds4),
                                            data.frame(index="att", thresh=c(1:3), value=att),
                                            data.frame(index="dloss", thresh=c(1:3), value=dloss),
                                            data.frame(index="totrain", thresh=NA, value=totrain),
                                            data.frame(index="raindays", thresh=NA, value=raindays),
                                            data.frame(index="wes", thresh=NA, value=wes),
                                            data.frame(index="badsow", thresh=NA, value=badsow),
                                            data.frame(index="badhar", thresh=NA, value=badhar)))
            #append output to data.frame
            out_seas <- rbind(out_seas,out_gpi)
          }
        }
      }
      out_indices <- out_seas; rm(out_seas)
      save(out_indices, file=paste(out_dir,"/indices_data_",sce,"_",gcm,".RData",sep=""))
    } else {
      load(file=paste(out_dir,"/indices_data_",sce,"_",gcm,".RData",sep=""))
    }
  }
}



