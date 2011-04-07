#Julian Ramirez
#University of Leeds / CIAT
#17 March 2011

#Reading weather stations

#Details below:
#Precipitation stations inventory (v2.prcp.inv)
#01. ID station:      12 characters
#02. Station name:    31 characters
#03. Latitude:        7  characters
#04. Longitude:       7  characters
#05. Altitude:        5  characters

#Temperature stations inventory (v2.temperature.inv)
#01. ID station:                                                                        12 characters
#02. Station name:                                                                      31 characters
#03. Latitude:                                                                          7  characters
#04. Longitude:                                                                         8  characters
#05. Altitude (measured, m):                                                            5  characters
#06. Altitude (interpolated, m):                                                        4  characters
#07. Population type (R=Rural, S=Small town, U=Urban):                                  2  characters
#08. Population of small town or urban area (*1,000):                                   4  characters
#09. General topography (FL=Flat, HI=Hilly, MT=Mountain top, MV=Mountainous valley):    2  characters
#10. General vegetation (MA=Marsh, FO=Forested, IC=Ice, DE=Desert, CL=Clear, xx=NA):    2  characters
#11. Station location (IS=Island, CO=Coast, LA=Lake, no=None):                          2  characters
#12. Distance to the coast if station location = CO:                                    2  characters
#13. A if the station is an airport station, otherwise x:                               1  character
#14. Distance from airport to station (km):                                             2  characters
#15. Vegetation extracted from 0.5x0.5 degree dataset:                                  16 characters
#16. A, B or C, not sure:                                                               2  characters

#wd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/"
rm(list=ls()); gc(); gc(T)

readLocations <- function(wd) {
  setwd(wd)
  
  #Read precipitation stations
  #prcp.widths <- c(12,31,7,7,5)
  prcp.names <- c("ID","NAME","LAT","LONG","ALT")
  prcp.st <- read.fortran(file="./raw-data/v2.prcp.inv", format=c("A12","A31", "2F7", "F5"))
  prcp.st <- as.data.frame(prcp.st)
  names(prcp.st) <- prcp.names
  prcp.st$ID <- paste("GHCN", prcp.st$ID, sep="")
  prcp.st.nw <- data.frame(ID=substr(prcp.st$ID,1,15), LAT=prcp.st$LAT, LONG=prcp.st$LONG, ALT=prcp.st$ALT)
  write.csv(prcp.st.nw, "./organized-data/ghcn_rain_stations.csv", row.names=F, quote=F)
  write.csv(prcp.st.nw, "./organized-data/ghcn_rain_adj_stations.csv", row.names=F, quote=F)
  
  #Read temperature stations
  #temp.widths <- c(12,31,7,8,5,4,2,4,2,2,2,2,1,2,16,2)
  temp.names <- c("ID","NAME","LAT","LONG","ALT","ALT.INT","POP.TYPE","POP.QUANT","GEN.TOPO","GEN.VEG","ST.LOC","DIST.COAST","AIRPORT","DIST.AIRPORT","VEG","X")
  temp.st <- read.fortran(file="./raw-data/v2.temperature.inv", format=c("A12","A31","F7","F8","F5","F4","A2","F4","4A2","A1","F2","A16","A2"))
  temp.st <- as.data.frame(temp.st)
  names(temp.st) <- temp.names
  temp.st$ID <- paste("GHCN", temp.st$ID, sep="")
  temp.st.nw <- data.frame(ID=substr(temp.st$ID,1,15), LAT=temp.st$LAT, LONG=temp.st$LONG, ALT=temp.st$ALT)
  write.csv(temp.st.nw, "./organized-data/ghcn_tmean_stations.csv", row.names=F, quote=F)
  write.csv(temp.st.nw, "./organized-data/ghcn_tmean_adj_stations.csv", row.names=F, quote=F)
  write.csv(temp.st.nw, "./organized-data/ghcn_tmax_stations.csv", row.names=F, quote=F)
  write.csv(temp.st.nw, "./organized-data/ghcn_tmax_adj_stations.csv", row.names=F, quote=F)
  write.csv(temp.st.nw, "./organized-data/ghcn_tmin_stations.csv", row.names=F, quote=F)
  write.csv(temp.st.nw, "./organized-data/ghcn_tmin_adj_stations.csv", row.names=F, quote=F)
}

##############################################################
#Reading weather data
##############################################################

#Precipitation & temperature files format
#01. ID station:        12 characters
#02. Year:              4  characters
#03-14 Jan-Dec:         5  characters each

#vn <- "prcp_adj"
#outvn <- "rain_adj"
#wd <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/"

interpretGHCN <- function(vn="prcp_adj", outvn="rain_adj", wd="") {
  setwd(wd)
  
  cat("Reading and basic cleansing of input files \n")
  data.names <- c("ID","REPLICATED","YEAR","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  #data.widths <- c(12,4,rep(5,times=12))
  var.data <- read.fortran(paste("./raw-data/v2.", vn, sep=""), format=c("A11","I1","I4","12F5"))
  var.data <- as.data.frame(var.data)
  names(var.data) <- data.names
  
  #Filter out any rows before 1900
  #and clean the data from -9999 to NA, and from -8888 to 0
  var.data <- var.data[which(var.data$YEAR >= 1900),]
  for (i in 4:15) {
    var.data[which(var.data[,i] == -9999),i] <- NA
    var.data[which(var.data[,i] == -8888),i] <- 0
  }
  var.data$ID <- paste("GHCN", var.data$ID, sep="")
  
  cat("Writing interpreted file \n")
  #First write this as a csv for easy reading
  write.csv(var.data, paste("./organized-data/ghcn_", outvn, "_data.csv", sep=""), row.names=F, quote=F)
  #var.data <- read.csv(paste("./organized-data/ghcn_", outvn, "_data.csv", sep=""))
  
  #Number of stations
  stations <- unique(var.data$ID)
  n <- length(stations)
  
  cat("Merging and writing the dataset to avoid gaps when missing years \n")
  #Create a dummy data frame with this number of stations and null in all years for merging
  out.data <- expand.grid(YEAR=c(1900:2009),ID=stations)
  
  #Merge this data frame with the loaded data, and write the output
  m <- merge(out.data, var.data, all.x=T)
  write.csv(m, paste("./organized-data/ghcn_", outvn, "_data_all.csv", sep=""), row.names=F, quote=F)
  
  cat("Cleaning memory \n")
  rm(m); rm(var.data); rm(out.data); gc(T); gc()
  m <- read.csv(paste("./organized-data/ghcn_", outvn, "_data_all.csv", sep=""))
  
  cat("Merging data with stations locations \n")
  #Merge this data with the stations locations
  st.locations <- read.csv(paste("./organized-data/ghcn_", outvn, "_stations.csv", sep=""))
  m2 <- merge(m, st.locations, all.x=T); m2 <- m2[,c(1,2,3,16,17,18,4:15)]
  write.csv(m2, paste("./organized-data/ghcn_", outvn, "_data_all_xy.csv", sep=""), row.names=F, quote=F)
  
  cat("Splitting into per-station files \n")
  #Splitting the dataset into per-station files
  od <- paste("./organized-data/", outvn, "-per-station", sep="")
  if (!file.exists(od)) {dir.create(od)}
  for (st in stations) {
    st.data <- m2[which(m2$ID == st),]
    write.csv(st.data, paste(od, "/", st, ".csv", sep=""), row.names=F, quote=F)
  }
  
  cat("Splitting into per-year files \n")
  #Splitting the dataset into per-year files
  years <- unique(m$YEAR)
  ny <- length(years)
  ody <- paste("./organized-data/", outvn, "-per-year", sep="")
  if (!file.exists(ody)) {dir.create(ody)}
  for (yr in years) {
    st.data <- m2[which(m2$YEAR == yr),]
    write.csv(st.data, paste(ody, "/ghcn-", yr, ".csv", sep=""), row.names=F, quote=F)
  }
}

##############################################################
#Calculating 30-yr running means
##############################################################

#Calculate 30 year running average between 1961-1990, for comparisons with the same period of GCM data
#Load stations, and then per station calculate the average over the desired period for each month

baselineMean <- function(outvn, wd="") {
  setwd(wd)
  #outvn <- "rain_adj"
  
  #Loading station data
  od <- paste("./organized-data/", outvn, "-per-station", sep="")
  st.data <- read.csv(paste("./organized-data/ghcn_", outvn, "_data_all_xy.csv", sep=""))
  
  #Looping through stations to average and count number of years with data per month
  stations <- unique(st.data$ID)
  for (st in stations) {
    st.years <- read.csv(paste(od, "/", st, ".csv", sep=""))
    st.years <- st.years[which(st.years$YEAR >= 1961 & st.years$YEAR <= 1990),]
    
    #Calculating mean and counting
    st.mean <- t(colMeans(st.years[,2:ncol(st.years)], na.rm=T))
    st.count <- apply(st.years[,2:ncol(st.years)], 2, FUN=function(x) {length(which(!is.na(x)))}); st.count <- t(st.count)
    
    #Creating output data frame
    if (st == stations[1]) {
      summ.mean <- c(ID=st,st.mean)
      summ.count <- c(ID=st,st.count)
    } else {
      summ.mean <- rbind(summ.mean, c(ID=st,st.mean))
      summ.count <- rbind(summ.count, c(ID=st,st.count))
    }
  }
  
  #Fixing final data frames
  summ.mean <- as.data.frame(summ.mean); summ.mean <- summ.mean[,c(1,3:18)]
  names(summ.mean) <- c("ID","REPLICATED","LAT","LONG","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  
  summ.count <- as.data.frame(summ.count); summ.count <- summ.count[,c(1,3:18)]
  names(summ.count) <- c("ID","REPLICATED","LAT","LONG","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
  summ.count$REPLICATED <- summ.mean$REPLICATED; summ.count$LAT <- summ.mean$LAT; summ.count$LONG <- summ.mean$LONG; summ.count$ALT <- summ.mean$ALT
  
  #Writing final data frames
  write.csv(summ.mean, paste("./organized-data/ghcn_", outvn, "_1961_1990_mean.csv", sep=""), row.names=F, quote=F)
  write.csv(summ.count, paste("./organized-data/ghcn_", outvn, "_1961_1990_count.csv", sep=""), row.names=F, quote=F)
}
