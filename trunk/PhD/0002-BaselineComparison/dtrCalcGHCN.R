#Julian Ramirez
#CIAT / University of Leeds / CCAFS

#Purge memory
rm(list=ls()); g=gc()

in.dir <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/ghcn-weather-stations/organized-data"
setwd(in.dir)

tmin <- read.csv("ghcn_tmin_1961_1990_mean.csv")
tmax <- read.csv("ghcn_tmax_1961_1990_mean.csv")

dtr <- tmin

posJAN <- which(names(tmin) == "JAN")
posDEC <- which(names(tmin) == "DEC")

for (pos in posJAN:posDEC){
  dtr[,pos] <- tmax[,pos] - tmin[,pos]
}

write.csv(dtr,"ghcn_dtr_1961_1990_mean.csv",quote=F,row.names=F)
