##########################################################################################
## Purpose: Merge daily INHAMI weather stations in a single csv file and convert to Monthly
## Author: Lizeth Llanos l.llanos@cgiar.org
## Modified by : Carlos Navarro c.e.navarro@cgiar.org
##########################################################################################

## See wrap below

#################################
### 01- Daily Quality Control ###
#################################
daily_qc <- function(var="prec", bDir = "S:/observed/weather_station/ecu-inamhi/daily-raw", oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU"){

  cat(var, " merge daily")
  # List all txt files
  rutOrigen <- paste0(bDir, "/", var, "-per-station") 
  files <- list.files(rutOrigen, pattern="\\.txt$", full.names = F)
  
  # Set period
  # x = seq(as.Date("2030/01"), as.Date("2059/01"), "moths") 
  x=seq(as.Date("1960/1/1"), as.Date("2014/12/31"), "days")
  
  # Date format
  fechas = format(x,"%Y%m%d")
  fechas = cbind.data.frame("Date"=fechas,"NA")
  
  # Read all files
  Datos <- lapply(paste(rutOrigen,"/", files, sep=""), function(x){read.table(x, header=T, sep="\t")})
  
  # Replace ',' by '.'
  convert = function(x){
    y=as.numeric(sub(",", ".", x, fixed = TRUE))
    return(y)
  }
  Datos_n = lapply(Datos,convert)
  
  cat(var, " quality control")
  
  if(var == "prec"){
    
    # Quality control functions for precipitation
    
    qc = function(x){
      pos=which(x[,2]>350 | x[,2]<0)
      if(length(pos)!=0){
        x[pos,2]=NA}
      return(x)
    }
    
    Data_qc = lapply(Datos,qc)
    
    qc2=function(x){
      pos=which(x[,2]>350 | x[,2]<0)
      ps2=length(pos)
      return(pos2)
    }
    
    qc_all = unlist(lapply(Data_qc,qc))
    
  } else {
    
    # Quality control functions for temperature 
    
    qc = function(x){
      pos = which(x[,2]>50 | x[,2]<(-20))
      if(length(pos) != 0){
        x[pos,2] = NA}
      return(x) 
    }
    
    Data_qc=lapply(Datos,qc)
    
    qc2 = function(x){
      pos=which(x[,2]>50 | x[,2]<(-20))
      ps2=length(pos)
      return(pos2)
    }
    
    qc_all=unlist(lapply(Data_qc,qc))
    
    
  }
  
  # NA's percent function
  data_na= function(x){
    na=sum(is.na(x[,2]))/length(x[,2])
    return(na)
  }
  
  na_all = round(unlist(lapply(Data_qc,data_na)),3)
  na_all2 = cbind(files,na_all)
  datosprecip = as.data.frame(matrix(NA,nrow(fechas),length(Data_qc)))
  
  # Merge daily data
  for(j in 1:length(Data_qc)) {  
    
    final = merge(fechas,Data_qc[[j]],by="Date",all.x=T)
    datosprecip[,j]=final[,3]
  }
  
  # Add dates
  year = as.numeric(substr(fechas[,1],1,4))
  month = as.numeric(substr(fechas[,1],5,6))
  day = as.numeric(substr(fechas[,1],7,8))
  
  varfin = cbind(day,month,year,datosprecip)
  names(varfin)=c("Day","Month","Year",paste0(lapply(strsplit(files,"_"), function(x) x[1])))
  
  # Write daily file`(raw data)
  write.csv(varfin, paste0(oDir, "/", var, "_daily_all.csv"), row.names = F)
  
#   data_na2 = function(x){
#     na=sum(is.na(x)) / length(x)
#     return(na)
#   }
  
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
  names(data_qc)=c("Day","Month","Year",paste0(lapply(strsplit(files,"_"), function(x) x[1])))
  
  # Write daily quality controled
  write.csv(data_qc, paste0(oDir, "/", var, "_daily_all_qc.csv"), row.names = F)
  # write.csv(as.matrix(fechas), paste0(oDir, "/dates_daily.csv"), row.names = F)
  
#   nas = apply(data_qc,2,data_na2)
  
}


##################################
### 02- Monthly aggregation QC ###
##################################

monthly_agg <- function(var="prec", bDir = "Z:/DATA/WP2/01_Weather_Stations/ECU", oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU"){
  
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

monthly_read <- function(var="tmax", bDir="Z:/DATA/WP2/01_Weather_Stations/ECU", oDir="Z:/DATA/WP2/01_Weather_Stations/ECU", sY=1960, fY=2014){
  
  require(raster)
  require(reshape2)

  data <- read.table(paste0(bDir, "/", var, "_all_stations_proc.txt"), header = T)
  data <- data[ which(data$Year >= sY & data$Year <= fY),]
  
  fromqcDaily <- read.csv(paste0(bDir, "/", var, "_monthly_all.csv"), header = T)

  data <- data[!data$Station %in% toupper(names(fromqcDaily)), ]
  names(data) <- c("Station", "Year", 1:12)
  
  mth = rep(seq(1:12))
  year = sort(rep(sY:fY, 12))
  dateMt <- cbind("Date"=paste0(year, sprintf("%02d", mth)), "NA")
  
  mat <- as.data.frame(matrix(NA,nrow(dateMt),length(unique(data$Station))))
  
  for(i in 1:length(unique(data$Station))){
    
    sel <- melt(data[data$Station == unique(data$Station)[i], ], id.vars = c("Station", "Year") )
    sel <- as.data.frame(cbind(paste0(sel$Year, sprintf("%02d", sel$variable)), sel$value))
    names(sel) <- c("Date", paste0(id))

    mat[,i] <- merge(dateMt, sel, by="Date", all.x=T)[,3]
  }
  
  names(mat)= tolower(unique(data$Station))
  
  write.csv(cbind(fromqcDaily, mat), paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
  
}


#############################
### 03- Climatology Calcs ###
#############################

clim_calc <- function(var="prec",  bDir = "Z:/DATA/WP2/01_Weather_Stations/ECU", oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU", st_loc="S:/observed/weather_station/ecu-inamhi/daily-raw/station_catalog_daily.txt", sY=1981, fY=2010){
  
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
  
  # Fix station names
  st_names <- as.data.frame(names(monthly_avg))
#   names(st_names) <- "Station"
  
  # Remove rows with NA
  monthly_avg <- na.omit(cbind(st_names, round(t(monthly_avg), 1)))
  rownames(monthly_avg) <- NULL
  
  # Add month names
  mths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(monthly_avg) <- c("Station", mths)
  
  ## Add station info
  stInfo <- read.table(st_loc, header=T, sep = "\t")
  join <- merge(stInfo, monthly_avg, by = "Station", all = FALSE)
  
  # Combine info and data
  climData <- cbind(monthly_avg$Station, "INAMHI", monthly_avg$Station, join$Name, "ECUADOR", join$Lon, join$Lat, join$Alt, monthly_avg[,2:ncol(monthly_avg)], 30)
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



# Daily qc
varList <- c("prec","tmax", "tmin")
for (var in varList){
  bDir = "S:/observed/weather_station/ecu-inamhi/daily-raw"
  oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU"
  daily_qc(var, bDir, oDir)
}

# # Monthly aggregation
# varList <- c("prec","tmax", "tmin")
# for (var in varList){
#   bDir = "Z:/DATA/WP2/01_Weather_Stations/ECU"
#   oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU"
#   monthly_agg(var, bDir, oDir)
# }

# # Monthly read
# bDir <- "Z:/DATA/WP2/01_Weather_Stations/ECU"
# oDir <- "Z:/DATA/WP2/01_Weather_Stations/ECU"
# sY=1960
# fY=2014
# varList <- c("prec","tmax", "tmin")
# for (var in varList){
#   monthly_read(var, bDir, oDir, sY, fY)
# }

# # Clim calcs
# sY=1981
# fY=2010
# bDir = "Z:/DATA/WP2/01_Weather_Stations/ECU"
# oDir = "Z:/DATA/WP2/01_Weather_Stations/ECU"
# rg=c(-79.5, -72, -11.9, 3)
# rgName="amazon"
# st_loc="S:/observed/weather_station/ecu-inamhi/daily-raw/station_catalog_daily.txt"
# varList <- c("prec", "tmax", "tmin")
# for (var in varList){
#   clim_calc(var, bDir, oDir, st_loc, sY, fY)
#   wth_sel(var, rg, rgName, bDir, oDir, sY, fY)
# }
