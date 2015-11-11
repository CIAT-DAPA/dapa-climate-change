#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

###
# extract weather data for a set of selected households in Africa, as follows:
# * precipitation: from CHIRPS
# * temperatures: 
#        (a) if <= 2010, from AgMERRA (Ruane et al. 2015)
#        (b) if > 2010, take temperature from last CRU version, and scale linearly using splines, 
#            function splineInterpolateMonthlytoDaily() in package RMAWGEN
# * for solar radiation:
#        (a) if <= 2010, from AgMERRA (Ruane et al. 2015)
#        (b) if > 2010, take climatological means 1981-2010 (30 years) of srad from AgMERRA
#            and estimate daily values using the WGEN method (Richardson and Wright, 1984)
###

#function to extract daily weather for a location or set of locations
extract_weather_loc <- function(year,chirps_dir,agmerra_dir,cru_dir,xy) {
  #if only one location follow normal procedure (i.e. no list)
  if (nrow(xy) == 1) {
    #get site details
    lon <- xy$lon[1]; lat <- xy$lat[1]; hhid <- xy$hhid[1]
    
    #create data.frame with dates
    dates <- format(seq(as.Date(paste0(year,"/1/1")), as.Date(paste0(year,"/12/31")), "days") ,"%Y-%m-%d")
    
    ###
    #all cases, extract precipitation data from CHIRPS
    dates <- extract_CHIRPS_single(in_dir=chirps_dir,year,dates,lon,lat)
    
    ###
    #extract the rest of data depending on whether it is before or after 2010
    if (year <= 2010) {
      agmerra_tx <- extract_AgMERRA_single(in_dir=agmerra_dir,year,varname="tmax",hhid=hhid,lon=lon,lat=lat)
      agmerra_tn <- extract_AgMERRA_single(in_dir=agmerra_dir,year,varname="tmin",hhid=hhid,lon=lon,lat=lat)
      agmerra_sr <- extract_AgMERRA_single(in_dir=agmerra_dir,year,varname="srad",hhid=hhid,lon=lon,lat=lat)
      dates <- merge(dates,agmerra_tx,by=c("date"))
      dates <- merge(dates,agmerra_tn,by=c("date","year"))
      dates <- merge(dates,agmerra_sr,by=c("date","year"))
      dates$year <- NULL
    } else if (year > 2010) {
      ###
      #for tmax, tmin extract from CRU and scale linearly
      dates <- extract_CRU_single(in_dir=cru_dir,year,dates,varname="tmx",lon,lat)
      dates <- extract_CRU_single(in_dir=cru_dir,year,dates,varname="tmn",lon,lat)
      
      ###
      #climatological mean solar radiation from AgMERRA and then daily generated using WGEN
      #algorithm (Richardson and Wright, 1984). AgMERRA data in MJ/m2/day
      agmerra_srad <- extract_AgMERRA_single(in_dir=agmerra_dir,year=NULL,varname="srad",year_range=c(1981,2010),hhid=hhid,lon=lon,lat=lat)
      agmerra_prate <- extract_AgMERRA_single(in_dir=agmerra_dir,year=NULL,varname="prate",year_range=c(1981,2010),hhid=hhid,lon=lon,lat=lat)
      data_agmerra <- merge(agmerra_srad,agmerra_prate,by=c("date","year"))
      data_agmerra$month <- unlist(lapply(paste(data_agmerra$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[2]}))
      
      ###
      #generate solar radiation for new year based on historical values and new year's precipitation
      dates <- wgen_srad(daily_hist=data_agmerra,dates,year,lon,lat)
      
      #some comparison plots
      #plot(data_agmerra$srad[which(data_agmerra$year==1985)],ty="l",ylim=c(10,50))
      #lines(dates$srad,col="red")
      #lines(dates$prec)
      #lines(data_agmerra$prate[which(data_agmerra$year==1985)],col="blue")
    }
    
    ###
    #finally, name of columns
    names(dates) <- c("DATE","RAIN","TMAX","TMIN","SRAD")
  } else {
    #create a list of data.frames with dates
    dates <- lapply(1:nrow(xy), FUN=function(x) {tdates <- format(seq(as.Date(paste0(year,"/1/1")), as.Date(paste0(year,"/12/31")), "days") ,"%Y-%m-%d"); return(tdates)})
    
    ###
    #all cases, extract precipitation data from CHIRPS
    dates <- extract_CHIRPS_mult(in_dir=chirps_dir,year,dates,xy)
    
    if (year <= 2010) {
      agmerra_tx <- extract_AgMERRA_mult(in_dir=agmerra_dir,year,varname="tmax",xy=xy)
      agmerra_tn <- extract_AgMERRA_mult(in_dir=agmerra_dir,year,varname="tmin",xy=xy)
      agmerra_sr <- extract_AgMERRA_mult(in_dir=agmerra_dir,year,varname="srad",xy=xy)
      for (x_i in 1:nrow(xy)) {
        dates[[x_i]] <- merge(dates[[x_i]],agmerra_tx[[x_i]],by=c("date"))
        dates[[x_i]] <- merge(dates[[x_i]],agmerra_tn[[x_i]],by=c("date","year"))
        dates[[x_i]] <- merge(dates[[x_i]],agmerra_sr[[x_i]],by=c("date","year"))
        dates[[x_i]]$year <- NULL
      }
    } else if (year > 2010) {
      ###
      #for tmax, tmin extract from CRU and scale linearly
      dates <- extract_CRU_mult(in_dir=cru_dir,year,dates,varname="tmx",xy)
      dates <- extract_CRU_mult(in_dir=cru_dir,year,dates,varname="tmn",xy)
      
      ###
      #climatological mean solar radiation from AgMERRA and then daily generated using WGEN
      #algorithm (Richardson and Wright, 1984). AgMERRA data in MJ/m2/day
      agmerra_srad <- extract_AgMERRA_mult(in_dir=agmerra_dir,year=NULL,varname="srad",year_range=c(1981,2010),xy=xy)
      agmerra_prate <- extract_AgMERRA_mult(in_dir=agmerra_dir,year=NULL,varname="prate",year_range=c(1981,2010),xy=xy)
      
      #loop locations for final calculations
      for (x_i in 1:nrow(xy)) {
        ###merge agmerra data
        data_agmerra <- merge(agmerra_srad[[x_i]],agmerra_prate[[x_i]],by=c("date","year"))
        data_agmerra$month <- unlist(lapply(paste(data_agmerra$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[2]}))
        
        ###
        #generate solar radiation for new year based on historical values and new year's precipitation
        dates[[x_i]] <- wgen_srad(daily_hist=data_agmerra,dates[[x_i]],year,lon=xy$lon[x_i],lat=xy$lat[x_i])
      }
    }
    
    ###
    #finally, name of columns
    for (x_i in 1:nrow(xy)) {names(dates[[x_i]]) <- c("DATE","RAIN","TMAX","TMIN","SRAD")}
  }
  
  #return object
  return(dates)
}



