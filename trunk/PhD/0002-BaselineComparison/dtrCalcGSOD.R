#Julian Ramirez
#CIAT / University of Leeds / CCAFS

#Purge memory
rm(list=ls()); g=gc()

#Moving to workspace
in.dir <- "F:/PhD-work/climate-data-assessment/comparisons/input-data/gsod-weather-stations/all-years"
setwd(in.dir)

#Loading tmin & tmax data
tmin <- read.csv("gsod-tmin-1961_1990-mean.csv")
tmax <- read.csv("gsod-tmax-1961_1990-mean.csv")

dtr <- tmin

posJAN <- which(names(tmin) == "JAN")
posDEC <- which(names(tmin) == "DEC")

for (pos in posJAN:posDEC){
  dtr[,pos] <- tmax[,pos] - tmin[,pos]
}

write.csv(dtr,"gsod-dtr-1961_1990-mean.csv",quote=F,row.names=F)

#Now generating the gsod-dtr-data-all.csv

tmin <- read.csv("gsod-tmin-data-all.csv")
tmax <- read.csv("gsod-tmax-data-all.csv")

dtr <- tmin

posJAN <- which(names(tmin) == "JAN")
posDEC <- which(names(tmin) == "DEC")

for (pos in posJAN:posDEC){
  dtr[,pos] <- tmax[,pos] - tmin[,pos]
}

write.csv(dtr,"gsod-dtr-data-all.csv",quote=F,row.names=F)


