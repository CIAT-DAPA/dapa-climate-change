##########################################################################################
## Purpose: Merge daily INMET weather stations in a single csv file and convert to Monthly
## Author: Lizeth Llanos l.llanos@cgiar.org
## Modified by : Carlos Navarro c.e.navarro@cgiar.org
##########################################################################################

##################################
### 02- Monthly aggregation QC ###
##################################

monthly_agg <- function(var="prec", bDir = "Z:/DATA/WP2/01_Weather_Stations/COL", oDir = "Z:/DATA/WP2/01_Weather_Stations/COL"){
  
  cat(var, " merge monthly")
  # List all txt files
  rutOrigen <- paste0(bDir, "/", var, "-per-station") 
  files <- list.files(rutOrigen, pattern="\\.txt$", full.names = F)
  
  # Set period
  # x=seq(as.Date("1960/1/1"), as.Date("2014/12/31"), "days")
  
  fechas = expand.grid(Month=1:12, Year=1960:2015)
  
  # Read all files
  Datos <- lapply(paste(rutOrigen,"/", files, sep=""), function(x){read.table(x, header=T, sep="\t")})
  
  datosprecip = as.data.frame(matrix(NA,nrow(fechas),length(Datos)))
  
  # Merge daily data
  for(j in 1:length(Datos)) {  
    
    final = merge(fechas,Datos[j],by=c("Month", "Year"),all.x=T)
    datosprecip[,j]=final[,3]
  }
  
  varfin = cbind(fechas,datosprecip)
  names(varfin)=c("Month","Year",paste0(lapply(strsplit(files,"_"), function(x) x[1])))
  
  # Write monthly quality controled
  write.csv(varfin, paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
  
  
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
  monthly_avg <- cbind(st_names, round(t(monthly_avg), 1))
  rownames(monthly_avg) <- NULL
  
  # Add month names
  mths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(monthly_avg) <- c("Station", mths)
  
  ## Add station info
  stInfo <- read.csv(st_loc, header=T)
  stInfo$Station <-  sprintf("%06d", stInfo$Station) 
  join <- merge(stInfo, monthly_avg, by = "Station", all = FALSE)
  
  # Combine info and data
  climData <- cbind(join$Station, "SENAMHI", join$Station, join$Name, "PERU", join$Lon, join$Lat, join$Alt, join[which(colnames(join) %in% mths)], 30)
  names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
  
  # Write climatology 
  write.csv(climData, paste0(oDir, "/", var, "_climatology_", sY, "_", fY, ".csv"), row.names=F)
  write.csv(climData, paste0(oDir, "/", var, "_climatology_", sY, "_", fY, "_amazon.csv"), row.names=F)
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

var = "prec"
bDir = "Z:/DATA/WP2/01_Weather_Stations/PER/monthly-raw"
oDir = "Z:/DATA/WP2/01_Weather_Stations/PER"
monthly_agg(var, bDir, oDir)

# Clim calcs
sY=1981
fY=2010
bDir = "Z:/DATA/WP2/01_Weather_Stations/PER"
oDir = "Z:/DATA/WP2/01_Weather_Stations/PER"
# rg=c(-79.5, -72, -11.9, 3)
rgName="amazon"
st_loc="Z:/DATA/WP2/01_Weather_Stations/PER/stations.csv"
varList <- c("prec", "tmax", "tmin", "rhum")
for (var in varList){
  clim_calc(var, bDir, oDir, st_loc, sY, fY)
}

# 
# 
# var="rhum"
# rgName="amazon"
# bDir = "Z:/DATA/WP2/01_Weather_Stations/BRA"
# oDir = "Z:/DATA/WP2/01_Weather_Stations/BRA"
# sY=1981
# fY=2010
# rg <- c(-80, -66, -16, 5)
# wth_sel(var, rg, rgName, bDir, oDir, sY, fY)
# 

