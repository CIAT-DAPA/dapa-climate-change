#Julian Ramirez
#University of Leeds / CIAT
#17 March 2011

rm.clean <- function() {rm(list=ls()); gc(); gc(T)}
rm.clean()
setwd("F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/")

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

#Read precipitation stations

prcp.widths <- c(12,31,7,7,5)
prcp.names <- c("ID","NAME","LAT","LONG","ALT")
prcp.st <- read.fwf(file="./raw-data/v2.prcp.inv", widths=prcp.widths)
names(prcp.st) <- prcp.names
prcp.st.nw <- as.data.frame(cbind(ID=prcp.st$ID, LAT=prcp.st$LAT, LONG=prcp.st$LONG, ALT=prcp.st$ALT))
write.csv(prcp.st.nw, "./organized-data/ghcn_rain_stations.csv", row.names=F, quote=F)

#Read temperature stations
temp.widths <- c(12,31,7,8,5,4,2,4,2,2,2,2,1,2,16,2)
temp.names <- c("ID","NAME","LAT","LONG","ALT","ALT.INT","POP.TYPE","POP.QUANT","GEN.TOPO","GEN.VEG","ST.LOC","DIST.COAST","AIRPORT","DIST.AIRPORT","VEG","X")
temp.st <- read.fwf(file="./raw-data/v2.temperature.inv", widths=temp.widths)
names(temp.st) <- temp.names
temp.st.nw <- as.data.frame(cbind(ID=temp.st$ID, LAT=temp.st$LAT, LONG=temp.st$LONG, ALT=temp.st$ALT))
write.csv(temp.st.nw, "./organized-data/ghcn_temp_stations.csv", row.names=F, quote=F)

##############################################################
#Reading weather data
##############################################################
rm.clean() #Clean memory

#Precipitation & temperature files format
#01. ID station:        12 characters
#02. Year:              4  characters
#03-14 Jan-Dec:         5  characters each

vn <- "prcp_adj"
outvn <- "rain_adj"

data.names <- c("ID","YEAR","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
data.widths <- c(12,4,rep(5,times=12))
var.data <- read.fwf(paste("./raw-data/v2.", vn, sep=""), widths=data.widths)
names(var.data) <- data.names

#Filter out any rows before 1900
#and clean the data from -9999 to NA, and from -8888 to 0
var.data <- var.data[which(var.data$YEAR >= 1900),]
for (i in 3:14) {
  var.data[which(var.data[,i] == -9999),i] <- NA
  var.data[which(var.data[,i] == -8888),i] <- 0
}

#First write this as a csv for easy reading
write.csv(var.data, paste("./organized-data/ghcn_", outvn, "_data.csv", sep=""), row.names=F, quote=F)
# var.data <- read.csv(paste("./organized-data/ghcn_", outvn, "data.csv", sep=""))

#Number of stations
stations <- unique(var.data$ID)
n <- length(stations)

#Create a dummy data frame with this number of stations and null in all years for merging
out.data <- expand.grid(YEAR=c(1900:2009),ID=stations)

#Merge this data frame with the loaded data, and write the output
m <- merge(out.data, var.data, all.x=T)
write.csv(m, paste("./organized-data/ghcn_", outvn, "_data_all.csv", sep=""), row.names=F, quote=F)
# m <- read.csv(paste("./organized-data/ghcn_", outvn, "_data_all.csv", sep=""))

#Splitting the dataset into per-station files
od <- paste("./organized-data/", outvn, "-per-station", sep="")
if (!file.exists(od)) {dir.create(od)}
for (st in stations) {
  st.data <- m[which(m$ID == st),]
  write.csv(st.data, paste(od, "")
}

#Splitting the dataset into per-year files

#############################################



