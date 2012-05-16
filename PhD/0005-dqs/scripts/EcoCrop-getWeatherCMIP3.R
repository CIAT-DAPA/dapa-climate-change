#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP3 weather data

library(raster)

cmip3Dir <- "F:/climate_change/IPCC_CMIP3/20C3M/original-data"
comDir <- "Z:/PhD-work/data-quality-study/climate-comparison"

oDir <- paste(comDir,"/CMIP3_IN",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

fullSet <- list.files(cmip3Dir)
ctr <- 1
for (gcm in fullSet) {
  if (!file.exists(paste(cmip3Dir,"/",gcm,"/multiyr_avgs/1961_1990/tmax_01.asc",sep=""))) {
    hasr <- F
  } else {
    hasr <- T
  }
  
  if (hasr) {
    if (ctr == 1) {
      redList <- gcm
    } else {
      redList <- c(redList,gcm)
    }
    ctr <- ctr+1
  }
  
}

#get the indian extent
dumm_rs <- raster("F:/PhD-work/data-quality-study/EcoCrop-GNUT/climate/ind_coarse/prec_1.asc")
dumm_rs[] <- 1

for (gcm in redList) {
  cat("extracting",gcm,"\n")
  if (!file.exists(paste(oDir,"/",gcm,".csv",sep=""))) {
    for (vn in c("prec","tmean","tmin","tmax")) {
      jun <- raster(paste(cmip3Dir,"/",gcm,"/multiyr_avgs/1961_1990/",vn,"_06.asc",sep=""))
      jun <- crop(jun,dumm_rs)
      jun <- resample(jun,dumm_rs,method="bilinear")
      jun_val <- mean(jun[],na.rm=T)
      
      jul <- raster(paste(cmip3Dir,"/",gcm,"/multiyr_avgs/1961_1990/",vn,"_07.asc",sep=""))
      jul <- crop(jul,dumm_rs)
      jul <- resample(jul,dumm_rs,method="bilinear")
      jul_val <- mean(jul[],na.rm=T)
      
      aug <- raster(paste(cmip3Dir,"/",gcm,"/multiyr_avgs/1961_1990/",vn,"_08.asc",sep=""))
      aug <- crop(aug,dumm_rs)
      aug <- resample(aug,dumm_rs,method="bilinear")
      aug_val <- mean(aug[],na.rm=T)
      
      sep <- raster(paste(cmip3Dir,"/",gcm,"/multiyr_avgs/1961_1990/",vn,"_09.asc",sep=""))
      sep <- crop(sep,dumm_rs)
      sep <- resample(sep,dumm_rs,method="bilinear")
      sep_val <- mean(sep[],na.rm=T)
      
      out_row <- data.frame(GCM=gcm,YEAR="CL:1961-1990",VAR=vn,M6=jun_val,M7=jul_val,M8=aug_val,M9=sep_val)
      
      if (vn == "prec") {
        out_all <- out_row
      } else {
        out_all <- rbind(out_all,out_row)
      }
    }
    
    write.csv(out_all,paste(oDir,"/",gcm,".csv",sep=""),quote=T,row.names=F)
  }
}





