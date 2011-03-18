#Julian Ramirez
#University of Leeds / CIAT
#17 March 2011

rm(list=ls()); gc(); gc(T)
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

#Reading weather data

#Precipitation file
#01. ID station:        12 characters
#02. Year:              4  characters
