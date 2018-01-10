##########################################################################################
## Purpose: Merge daily GHCND weather stations in a single csv file and convert to Monthly
## Author: Lizeth Llanos l.llanos@cgiar.org
## Modified by : Carlos Navarro c.e.navarro@cgiar.org
##########################################################################################

#################################
### 01- Daily Quality Control ###
#################################

daily_qc <- function(var="prec", bDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN", oDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN"){
    
  #  Merge all years in one single file
  stations <- list.dirs(paste0(bDir,"/ghcn.rg.", var), full.names = F)
  stations <- stations[stations != ""]
  
  dates <- seq(as.Date("1960/1/1"), as.Date("2010/12/31"), "days")
  dates = format(dates,"%Y%m%d")
  dates = cbind.data.frame("Date"=dates,"NA")
  
  mat <- as.data.frame(matrix(NA,nrow(dates),length(stations)))

  for (s in 1:length(stations)){
    
    years <- list.files(paste0(bDir,"/ghcn.rg.", var, "/", stations[s]), full.names = F)
    data <- lapply(paste0(bDir,"/ghcn.rg.", var, "/", stations[s], "/", years), function(x){read.csv(x, header=T)})
    
    allyears <- c()
    
    for (j in 1:length(data)){
      allyears <- rbind(allyears, cbind(as.numeric(paste0(strsplit(years[j],".csv")[1], sprintf("%02d", data[[j]]$MONTH), sprintf("%02d", data[[j]]$DOFM))), 
                                        data[[j]][,4]))
      cat(stations[s],years[j],"\n")
    }
    
    colnames(allyears) <- c("Date", stations[s])
    datSt <- merge(dates, allyears, by="Date",all.x=T)
    mat[,s] <- datSt[,3]
    
  }
  
  
  # Add dates
  year = as.numeric(substr(dates[,1],1,4))
  month = as.numeric(substr(dates[,1],5,6))
  day = as.numeric(substr(dates[,1],7,8))
  
  varfin = cbind(day,month,year,mat)
  names(varfin)=c("Day","Month","Year",stations)
  
  # Write daily file`(raw data)
  write.csv(varfin, paste0(oDir, "/", var, "_daily_all.csv"), row.names = F)
  
  
  data = sapply(varfin, as.numeric)
  
  if (var == "prec"){
    
    qc=function(x){
      pos=which(x>350 | x<0)
      if(length(pos)!=0){
        x[pos]=NA}
      return(x)
    }
    
  } else { 
    
    qc=function(x){
      pos=which(x>50 | x<(-20))
      if(length(pos)!=0){
        x[pos]=NA}
      return(x)
    }
    
  }
  
  # Set quality control 
  data_qc = apply(data[,-3:-1],2,qc)
  
  data_qc = cbind(day,month,year,data_qc)
  names(data_qc)=c("Day","Month","Year",stations)
  
  # Write daily quality controled
  write.csv(data_qc, paste0(oDir, "/", var, "_daily_all_qc.csv"), row.names = F)
  
  
}


##################################
### 02- Monthly aggregation QC ###
##################################

monthly_agg <- function(var="prec", bDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN", oDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN"){
  
  cat(var, " monthly aggregation")
  
  # Read daily QC data
  data_qc <- read.csv(paste0(bDir, "/", var, "_daily_all_qc.csv"), header = T)
  
  # Monthly aggregation based on min percent of NA
  
  if (var == "prec"){
    
    sum22=function(a,na.rm=T){
      na.x=sum(is.na(a))/length(a)
      if(na.x>=0.20){
        x=NA
      }else{x=sum(a,na.rm=any(!is.na(a)))}
      
      return(x)
    }
    
  } else {
    
    
    sum22=function(a,na.rm=T){
      na.x=mean(is.na(a))/length(a)
      if(na.x>=0.60){
        x=NA
      }else{x=mean(a,na.rm=any(!is.na(a)))}
      
      return(x)
    }
    
  }
  
  # Aggregate 
  monthly_var = aggregate(data_qc, list(Month=data_qc$Month, Year=data_qc$Year),sum22)
  monthly_var$day <- NULL
  
  # Write monthly quality controled
  write.csv(monthly_var, paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
  
  # which(apply(monthly_precip, 2, min,na.rm=T) == Inf)
  
}


##############################################################
### 03- Add oringinal monthly data to daily aggregate data ###
##############################################################

monthly_read <- function(var="tmax", bDir="S:/observed/weather_station/ghcn/organized-data", oDir="Z:/DATA/WP2/01_Weather_Stations/GHCN", rg=c(-79.5, -72, -11.9, 3), sY=1960, fY=2009){
  
  require(raster)
  rgExt <- extent(rg)
  
  # Read monthly data
  data <- lapply(paste0(bDir, "/", var, "-per-year/ghcn-", sY:fY, ".csv"),read.csv)
  
  # Select stations inside region and combine in one matrix
  pos = data[[1]][,4] < ymin(rgExt) | data[[1]][,4] > ymax(rgExt) | data[[1]][,5] < xmin(rgExt) | data[[1]][,5] > xmax(rgExt)
  stSel <- data[[1]][!pos, ]
  
  mth = rep(seq(1:12), length(data))
  year = sort(rep(sY:fY, 12))
  stLs <- stSel$ID
  
  dataMtx <- c(stSel[,7:18]/10)
  for(i in 2:length(data)){ 
    
    stSel_i <- data[[i]][!pos, ]
    dataMtx <- cbind(dataMtx, stSel_i[,7:18]/10)
    
  }
  
  stSel$YEAR <- NULL
  stSel$REPLICATED <- NULL
  stSel$LAT <- NULL
  stSel$LONG <- NULL
  stSel$ALT <- NULL
  stSel$ALT <- NULL
  stSel$ID <- NULL
  
  dataMt_t <- cbind(mth, year, t(dataMtx))
  colnames(dataMt_t) <- c("Month", "Year", paste0(gsub("GHCN", "", stLs)))
  
  dataMt_t <- rbind(dataMt_t, cbind(1:12, 2010, matrix(NA,12,ncol(dataMt_t)-2)))
  
  if (var == "rain"){
    # Read data from GHCND & merge
    ghcnd <- read.csv(paste0(oDir, "/prec_monthly_all.csv"), header = T)
    dataMt_t <- cbind(dataMt_t, ghcnd[,3:ncol(ghcnd)])
    write.csv(dataMt_t, paste0(oDir, "/prec_monthly_all.csv"), row.names=F)
  } else {
    ghcnd <- read.csv(paste0(oDir, "/", var, "_monthly_all.csv"), header = T)
    dataMt_t <- cbind(dataMt_t, ghcnd[,3:ncol(ghcnd)])
    write.csv(dataMt_t, paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
  }
  
}


#############################
### 03- Climatology Calcs ###
#############################

clim_calc <- function(var="prec",  bDir = "Z:/DATA/WP2/01_Weather_Stations/ECU", oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU", stDir="S:/observed/weather_station/ghcn", sY=1981, fY=2010){
  
  # Read monthly file
  monthly_var <- read.csv(paste0(bDir, "/", var, "_monthly_all.csv"), header=T)
  
  ## Climatology aggregation based on NA percent
  avg_var = function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    if(na.x>=0.50){
      x = NA
    }else{
      x = mean(a, na.rm = any(!is.na(a))) 
    }
    return(x)
  }
  
  # Years selection
  monthly_var <- monthly_var[ which(monthly_var$Year >= sY & monthly_var$Year <= fY),]
  
  # Period climatology calc
  monthly_avg = aggregate(monthly_var[,3:length(monthly_var)], list(Month=monthly_var$Month),avg_var)
  monthly_avg$month <- NULL
  monthly_avg$Month <- NULL
  monthly_avg$year <- NULL
  
  st_names <- as.data.frame(names(monthly_avg))
  #   names(st_names) <- "Station"
  
  # Remove rows with NA
  monthly_avg <- na.omit(cbind(st_names, round(t(monthly_avg), 1)))
  rownames(monthly_avg) <- NULL
  
  # Add month names
  mths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(monthly_avg) <- c("Station", mths)
  
  ## Add station info
  stInfo <- read.csv(paste0(stDir, "/raw/daily/ghcnd-stations.csv"), header=T)
  
  stInfo$Gsn <- NULL
  stInfo$Code <- NULL
  
  if(var =="prec"){
    stInfo2 <- read.csv(paste0(stDir, "/organized-data/ghcn_rain_stations.csv"), header=T)  
  } else {
    stInfo2 <- read.csv(paste0(stDir, "/organized-data/ghcn_", var, "_stations.csv"), header=T)
  }
  
  stInfo2 <- cbind(as.data.frame(gsub("GHCN", "", stInfo2$ID)), stInfo2[,2:ncol(stInfo2)], stInfo2$ID)
  names(stInfo2) <- names(stInfo)
  
  stInfo <- rbind(stInfo, stInfo2)
  monthly_avg$Station <- gsub("X", "", monthly_avg$Station)
  
  join <- merge(stInfo, monthly_avg, by = "Station", all = FALSE)
  
  # Combine info and data
  climData <- cbind(monthly_avg$Station, "GHCN", monthly_avg$Station, join$Name, substr(monthly_avg$Station,1,3), join$Lon, join$Lat, join$Alt, monthly_avg[,2:ncol(monthly_avg)], 30)
  names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
  
  # Write climatology 
  write.csv(climData, paste0(oDir, "/", var, "_climatology_", sY, "_", fY, ".csv"), row.names=F)
  
}


#####################################
### 04- Weather Station selection ###
#####################################

wth_sel <- function(var="prec", rg=c(-79.5, -72, -11.9, 3), rgName="amazon", bDir = "Z:/DATA/WP2/01_Weather_Stations/ECU", oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU", sY=1981, fY=2010){
  
  require(raster)
  # Read file
  clim <- read.csv(paste0(oDir, "/", var, "_climatology_", sY, "_", fY, ".csv"), header=T)
  
  rgExt <- extent(rg)
  
  # Station selection
  pos = clim$LONG < xmin(rgExt) | clim$LONG > xmax(rgExt) | clim$LAT < ymin(rgExt) | clim$LAT > ymax(rgExt)
  climSel <- clim[!pos, ]
  
  
  # varfin = varfin[ , which(names(varfin) %in% stList)]
  # varfin <- cbind(varfin, choco[,3:length(choco)])
  
  # Write climatology for wht stations inside region
  write.csv(climSel, paste0(oDir, "/", var, "_climatology_", sY, "_", fY, "_", rgName,".csv"), row.names=F)
  
}



# #Daily qc
# varList <- c("prec","tmax", "tmin")
# for (var in varList){
#   bDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN"
#   oDir ="Z:/DATA/WP2/01_Weather_Stations/GHCN"
#   daily_qc(var, bDir, oDir)
# }

# Monthly aggregation
varList <- c("prec","tmax", "tmin")
for (var in varList){
  bDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN"
  oDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN"
  monthly_agg(var, bDir, oDir)
}

# Monthly read
bDir <- "S:/observed/weather_station/ghcn/organized-data"
oDir <- "Z:/DATA/WP2/01_Weather_Stations/GHCN"
rg <- c(-80, -66, -16, 5)
sY=1960
fY=2009
varList <- c("rain","tmax", "tmin")
for (var in varList){
  monthly_read(var, bDir, oDir, rg, sY, fY)
}


# # Clim calcs
# sY=1986
# fY=2005
# rg <- c(-80, -66, -16, 5) #Study Region region
# # rg=c(-79.5, -72, -11.9, 3)
# rgName="amazon"
# bDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN"
# oDir = "Z:/DATA/WP2/01_Weather_Stations/GHCN"
# stDir="S:/observed/weather_station/ghcn"
# 
# varList <- c("prec", "tmax", "tmin")
# for (var in varList){
#   clim_calc(var, bDir, oDir, stDir, sY, fY)
#   wth_sel(var, rg, rgName, bDir, oDir, sY, fY)
# }
