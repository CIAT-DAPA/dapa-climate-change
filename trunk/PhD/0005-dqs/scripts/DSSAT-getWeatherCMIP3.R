#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP3 weather data

library(raster)

cmip3Dir <- "F:/climate_change/IPCC_CMIP3/20C3M/original-data"
comDir <- "Z:/PhD-work/data-quality-study/climate-comparison"

oDir <- paste(comDir,"/CMIP3_GJ",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

fullSet <- list.files(cmip3Dir)
ctr <- 1
for (gcm in fullSet) {
  if (!file.exists(paste(cmip3Dir,"/",gcm,"/yearly_files/1985/tmax_01.asc",sep=""))) {
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

#use only the reduced list to make the stuff comparable
yi <- 1966
yf <- 1989

#get the indian extent
xt <- extent(raster(paste(comDir,"/0_input_data/mask.asc",sep="")))

#create 2.5x2.5 dummy raster
nc <- (xt@xmax-xt@xmin)/2.5
nr <- (xt@ymax-xt@ymin)/2.5
xt@ymin <- xt@ymax - round(nr+0.5,0)*2.5
nr <- (xt@ymax-xt@ymin)/2.5

dumm_rs <- raster(xt,ncol=nc,nrow=nr)
dumm_rs[] <- 1

for (gcm in redList) {
  cat("extracting",gcm,"\n")
  if (!file.exists(paste(oDir,"/",gcm,".csv",sep=""))) {
    for (yr in yi:yf) {
      cat("year",yr,"\n")
      for (vn in c("prec","tmax","tmin")) {
        jun <- raster(paste(cmip3Dir,"/",gcm,"/yearly_files/",yr,"/",vn,"_06.asc",sep=""))
        jun <- crop(jun,dumm_rs)
        jun <- resample(jun,dumm_rs,method="bilinear")
        jun_val <- extract(jun,cbind(X=68.75,Y=22.75))
        
        jul <- raster(paste(cmip3Dir,"/",gcm,"/yearly_files/",yr,"/",vn,"_07.asc",sep=""))
        jul <- crop(jul,dumm_rs)
        jul <- resample(jul,dumm_rs,method="bilinear")
        jul_val <- extract(jul,cbind(X=68.75,Y=22.75))
        
        aug <- raster(paste(cmip3Dir,"/",gcm,"/yearly_files/",yr,"/",vn,"_08.asc",sep=""))
        aug <- crop(aug,dumm_rs)
        aug <- resample(aug,dumm_rs,method="bilinear")
        aug_val <- extract(aug,cbind(X=68.75,Y=22.75))
        
        sep <- raster(paste(cmip3Dir,"/",gcm,"/yearly_files/",yr,"/",vn,"_09.asc",sep=""))
        sep <- crop(sep,dumm_rs)
        sep <- resample(sep,dumm_rs,method="bilinear")
        sep_val <- extract(sep,cbind(X=68.75,Y=22.75))
        
        if (vn == "prec") {
          out_row <- data.frame(GCM=gcm,YEAR=yr,VAR=vn,M6=jun_val,M7=jul_val,M8=aug_val,M9=sep_val)
        } else {
          out_row <- data.frame(GCM=gcm,YEAR=yr,VAR=vn,M6=jun_val,M7=jul_val,M8=aug_val,M9=sep_val)
        }
        
        if (vn == "prec") {
          out_all <- out_row
        } else {
          out_all <- rbind(out_all,out_row)
        }
      }
      
      if (yr == yi) {
        out_yrs <- out_all
      } else {
        out_yrs <- rbind(out_yrs,out_all)
      }
      
    }
    write.csv(out_yrs,paste(oDir,"/",gcm,".csv",sep=""),quote=T,row.names=F)
  }
}






