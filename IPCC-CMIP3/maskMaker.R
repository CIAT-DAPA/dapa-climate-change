#Julian Ramirez-Villegas
#Nov, 2011

library(raster)
setwd("D:/CIAT_work/cassava-the-answer/africa_10min_future")

gcmList <- list.files(".")

for (gcm in gcmList) {
  setwd(paste("./",gcm,"/2020_2049/_asciis",sep=""))
  
  tmean <- stack(paste("tmean_",1:12,".asc",sep=""))
  prec <- stack(paste("prec_",1:12,".asc",sep=""))
  tmin <- stack(paste("tmin_",1:12,".asc",sep=""))
  tmax <- stack(paste("tmax_",1:12,".asc",sep=""))
  
  msk <- tmean[[1]]; msk[] <- 1
  msk[which(is.na(tmean[[1]][]) | is.na(tmean[[2]][]) | is.na(tmean[[3]][]) | is.na(tmean[[4]][])
     | is.na(tmean[[5]][]) | is.na(tmean[[6]][]) | is.na(tmean[[7]][]) | is.na(tmean[[7]][])
     | is.na(tmean[[8]][]) | is.na(tmean[[9]][]) | is.na(tmean[[10]][]) | is.na(tmean[[11]][])
     | is.na(tmean[[12]][])
     | is.na(prec[[1]][]) | is.na(prec[[2]][]) | is.na(prec[[3]][]) | is.na(prec[[4]][])
     | is.na(prec[[5]][]) | is.na(prec[[6]][]) | is.na(prec[[7]][]) | is.na(prec[[7]][])
     | is.na(prec[[8]][]) | is.na(prec[[9]][]) | is.na(prec[[10]][]) | is.na(prec[[11]][])
     | is.na(prec[[12]][])
     | is.na(tmin[[1]][]) | is.na(tmin[[2]][]) | is.na(tmin[[3]][]) | is.na(tmin[[4]][])
     | is.na(tmin[[5]][]) | is.na(tmin[[6]][]) | is.na(tmin[[7]][]) | is.na(tmin[[7]][])
     | is.na(tmin[[8]][]) | is.na(tmin[[9]][]) | is.na(tmin[[10]][]) | is.na(tmin[[11]][])
     | is.na(tmin[[12]][])
     | is.na(tmax[[1]][]) | is.na(tmax[[2]][]) | is.na(tmax[[3]][]) | is.na(tmax[[4]][])
     | is.na(tmax[[5]][]) | is.na(tmax[[6]][]) | is.na(tmax[[7]][]) | is.na(tmax[[7]][])
     | is.na(tmax[[8]][]) | is.na(tmax[[9]][]) | is.na(tmax[[10]][]) | is.na(tmax[[11]][])
     | is.na(tmax[[12]][]))] <- NA
  
  plot(msk)
  
  dir.create("./../_asciis_v2")
  for (i in 1:12) {
    for (v in c("tmean","tmax","tmin","prec")) {
      rs <- raster(paste(v,"_",i,".asc",sep=""))
      rs[which(is.na(msk[]))] <- NA
      writeRaster(rs,paste("./../_asciis_v2/",v,"_",i,".asc",sep=""),format='ascii')
      rm(rs); g=gc()
    }
  }

}
