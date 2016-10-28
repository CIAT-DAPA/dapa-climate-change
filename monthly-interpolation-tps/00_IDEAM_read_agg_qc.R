##########################################################################################
## Purpose: Merge daily IDEAM weather stations in a single csv file and convert to Monthly
## Author: Lizeth Llanos l.llanos@cgiar.org
## Modified by : Carlos Navarro c.e.navarro@cgiar.org
##########################################################################################

#################################
### 01- Daily Quality Control ###
#################################

daily_qc <- function(var="prec", bDir = "S:/observed/weather_station/col-ideam/daily-raw", oDir = "Z:/DATA/WP2/01_Weather_Stations/COL"){

  # List all txt files
  rutOrigen <- paste0(bDir, "/", var, "-per-station") 
  files <- list.files(rutOrigen, pattern="\\.txt$")
  
  # Set period
  # x = seq(as.Date("2030/01"), as.Date("2059/01"), "moths") 
  x=seq(as.Date("1965/1/1"), as.Date("2014/12/31"), "days")
  
  # Date format
  fechas = format(x,"%Y%m%d")
  fechas = cbind.data.frame("Date"=fechas,"NA")
  
  # Read all files
  Datos <- lapply(paste(rutOrigen,"/", files,sep=""), function(x){read.table(x, header=T, sep="\t")})
  
  # Replace ',' by '.'
  convert = function(x){
    y=as.numeric(sub(",", ".", x, fixed = TRUE))
    return(y)
  }
  Datos_n = lapply(Datos,convert)
  
  
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
    
  } else if (var == "rhum") {
    
    
    qc = function(x){
      pos = which(x[,2]>100 | x[,2]<0)
      if(length(pos) != 0){
        x[pos,2] = NA}
      return(x) 
    }
    
    Data_qc=lapply(Datos,qc)
    
    qc2 = function(x){
      pos=which(x[,2]>100 | x[,2]<0)
      ps2=length(pos)
      return(pos2)
    }
    
    qc_all=unlist(lapply(Data_qc,qc))
    
    
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
  names(varfin)=c("Day","Month","Year",substring(files,1,nchar(files)-(9+nchar(var))))
  
  # Write daily file`(raw data)
  write.csv(varfin, paste0(oDir, "/", var, "_daily_all.csv"), row.names = F)
  
  data_na2 = function(x){
    na=sum(is.na(x)) / length(x)
    return(na)
  }
  
  data = sapply(varfin, as.numeric)
  
  if (var == "prec"){
    
    qc=function(x){
      pos=which(x>350 | x<0)
      if(length(pos)!=0){
        x[pos]=NA}
      return(x)
    }
    
  } else if (var == "rhum") {
    
    qc=function(x){
      pos=which(x>100 | x<0)
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
  names(data_qc)=c("Day","Month","Year",substring(files,1,nchar(files)-(9+nchar(var))))
  
  # Write daily quality controled
  write.csv(data_qc, paste0(oDir, "/", var, "_daily_all_qc.csv"), row.names = F)
  # write.csv(as.matrix(fechas), paste0(oDir, "/dates_daily.csv"), row.names = F)
  
  nas = apply(varfin,2,data_na2)
  
}

##################################
### 02- Monthly aggregation QC ###
##################################

monthly_agg <- function(var="prec", bDir = "Z:/DATA/WP2/01_Weather_Stations/COL", oDir = "Z:/DATA/WP2/01_Weather_Stations/COL"){
  
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
      if(na.x>=0.50){
        x=NA
      }else{x=mean(a,na.rm=any(!is.na(a)))}
      
      return(x)
    }
    
  }
  
  # Aggregate 
  monthly_var = aggregate(data_qc, list(Month=data_qc$Month,Year=data_qc$Year),sum22)
  
  # Write monthly quality controled
  write.csv(monthly_var, paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
  
  # which(apply(monthly_precip, 2, min,na.rm=T) == Inf)
  
}

#############################
### 03- Climatology Calcs ###
#############################

clim_calc <- function(var="prec",  bDir = "Z:/DATA/WP2/01_Weather_Stations/COL", oDir = "Z:/DATA/WP2/01_Weather_Stations/COL", st_loc="S:/observed/weather_station/col-ideam/stations_names.csv", sY=1981, fY=2010){
  
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
  monthly_avg$Month <- NULL
  
  # Fix station names
  st_names <- as.data.frame(gsub("X", "", names(monthly_avg)))
  names(st_names) <- "Station"
  
  # Remove rows with NA
  monthly_avg <- na.omit(cbind(st_names, round(t(monthly_avg), 1)))
  rownames(monthly_avg) <- NULL
  
  # Add month names
  mths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(monthly_avg) <- c("Station", mths)
  
  ## Add station info
  stInfo <- read.csv(st_loc, header=T)
  join <- merge(stInfo, monthly_avg, by = "Station", all = FALSE)
  
  # Combine info and data
  climData <- cbind(join$Station, "IDEAM", join$Station, join$Name, "COLOMBIA", join$Lon, join$Lat, join$Alt, join[,8:ncol(join)], 30)
  names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
  
  # Write climatology 
  write.csv(climData, paste0(oDir, "/", var, "_climatology_", sY, "_", fY, ".csv"), row.names=F)
  
}


# sY=1976
# fY=2005
# 
# varList <- c("prec", "tmax", "tmin")
# for (var in varList){
#   clim_calc(var, bDir, oDir, st_loc, sY, fY)
# }


####################################
### 04- Add weather station info ###
####################################
add_info <- function(var="prec",  bDir = "Z:/DATA/WP2/01_Weather_Stations/COL", oDir = "Z:/DATA/WP2/01_Weather_Stations/COL", st_loc="S:/observed/weather_station/col-ideam/stations_names.csv", sY=1981, fY=2010){
  
  ## Add coordinates and location

  for (yr in c(1982,1983,1997,1998)){
    
    monthly_var <- read.csv(paste0(oDir, "/", var, "_climatology_", sY, "_", fY, ".csv"), header=T)
    st_loc <- read.csv(st_loc, header=T)
    # 
    # # monthly_var <- monthly_var[which(monthly_var$Year == yr),]
    # # year <- monthly_var$Year
    # # month <- monthly_var$Month
    # # date <- paste0(year, "_", month)
    st_code <- data.frame(Station=gsub("X", "", names(monthly_var[3:length(monthly_var)])))
    
    monthly_var_t <- cbind(st_code, data.frame(t(monthly_var[3:length(monthly_var)])))
    names(monthly_var_t) <- c("Station", date)
    
    c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
    
    cbind(1:length(), "IDEAM", monthly_var_t$Station, )
    
    join <- merge(st_loc, monthly_var_t, by = "Station", all = FALSE)
    write.csv(join, paste0(oDir, "/", var, "_", yr, ".csv"), row.names=F)
  }

  
  ## Averages by departament 
  join$Station <- NULL
  join$Lon <- NULL
  join$Lat <- NULL
  join$Alt <- NULL
  join$Name <- NULL
  
  if (var == "prec"){
    
    sum22=function(a,na.rm=T){
      na.x=sum(is.na(a))/length(a)
      if(na.x>=0.2){
        x=NA
      }else{x=sum(a,na.rm=any(!is.na(a)))}
      
      return(x)
    }
    
  } else {
    
    
    sum22=function(a,na.rm=T){
      na.x=mean(is.na(a))/length(a)
      if(na.x>=0.3){
        x=NA
      }else{x=mean(a,na.rm=any(!is.na(a)))}
      
      return(x)
    }
    
  }
  
  
  avg_2=function(a,na.rm=T){
    na.x=mean(is.na(a))/length(a)
    if(na.x>=1){
      x=NA
    }else{x=mean(a,na.rm=any(!is.na(a)))}
    
    return(x)
  }
  
  join_avg_mun <- aggregate(join[,3:length(join)], list(Municipality=join$Municipality, Departament=join$Departament), avg_2)
  
  ann_avg <- aggregate(t(join_avg_mun[,3:length(join_avg_mun)]), list(Year=year), sum22)
  year_agg <- ann_avg$Year
  ann_avg$Year <- NULL
  ann_avg_t <- data.frame(t(ann_avg))
  names(ann_avg_t) <- year_agg
  ann_avg_mun <- cbind("Municipality"=join_avg_mun$Municipality, "Departament"=join_avg_mun$Departament, ann_avg_t)
  
  write.csv(ann_avg_mun, paste0(oDir, "/", var, "_annual_by_mun.csv"), row.names=F)
  write.csv(join_avg_mun, paste0(oDir, "/", var, "_monthly_by_mun.csv"), row.names=F)
  write.csv(join, paste0(oDir, "/", var, "_monthly_by_stations.csv"), row.names=F)
  
  write.csv(t(ann_avg_mun), paste0(oDir, "/", var, "_annual_by_mun_t.csv"), row.names=F)
  
  ann_avg_mun_sort <- melt(ann_avg_mun, id.vars = c("Departament", "Municipality"), measure.vars = c("1965", "1966", "1967", "1968", "1969", "1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", "2014"))
  write.csv(ann_avg_mun_sort, paste0(oDir, "/", var, "_annual_by_mun_sort.csv"), row.names=F)
  
  
}

#####################################
### 05- Weather Station selection ###
#####################################

wth_sel <- function(var="prec", rg=c(-79.5, -72, -11.9, 3), rgName="amazon", bDir = "Z:/DATA/WP2/01_Weather_Stations/COL", oDir = "Z:/DATA/WP2/01_Weather_Stations/COL", sY=1981, fY=2010){
  
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


# var="rhum"
# bDir = "S:/observed/weather_station/col-ideam/daily-raw"
# oDir = "Z:/DATA/WP2/01_Weather_Stations/COL"
# daily_qc(var, bDir, oDir)

# var="rhum"
# bDir = "Z:/DATA/WP2/01_Weather_Stations/COL"
# oDir = "Z:/DATA/WP2/01_Weather_Stations/COL"
# monthly_agg(var, bDir, oDir)
# 
var="prec"
bDir = "D:/CIAT/climate_change/ideam_wht_st"
oDir = "D:/CIAT/Projects/col-usaid/02_monthly_interpolation/stations-averages"
st_loc= "D:/CIAT/climate_change/ideam_wht_st/stations_v2.csv"
sY=1986
fY=1995
for (var in c("tmin", "tmax")){
  clim_calc(var,  bDir, oDir, st_loc, sY, fY)
}

# var="rhum"
# rgName="amazon"
# bDir = "Z:/DATA/WP2/01_Weather_Stations/COL"
# oDir = "Z:/DATA/WP2/01_Weather_Stations/COL"
# sY=1981
# fY=2010
# rg <- c(-80, -66, -16, 5)
# wth_sel(var, rg, rgName, bDir, oDir, sY, fY)


