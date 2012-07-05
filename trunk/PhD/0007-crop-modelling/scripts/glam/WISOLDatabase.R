#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#get the coordinates of the WI.SOL soil profile data to map out the profiles

wd <- "F:/PhD-work/crop-modelling/GLAM/soil-data/WISOL_v1.1"
solFile <- "WI.SOL"

fso <- read.table(paste(wd,"/",solFile,sep=""),sep="\t")
#fso <- as.data.frame(fso[48800:53174,])
ctr <- 1
for (i in 1:nrow(fso)) {
  if (i %% 100 == 0) cat("row",i,"of",nrow(fso),"\n")
  #cat("row",i,"of",nrow(fso),"\n")
  rec <- paste(fso[i,])
  
  strID <- substr(rec,1,3)
  if (strID == "*WI") {
    isprof <- T
    pfID <- substr(rec,2,11)
    scID <- substr(rec,58,62)
    
    siteRow <- paste(fso[i+2,])
    country <- substr(siteRow,14,25); country <- gsub("  ","",country)
    lat <- as.numeric(substr(siteRow,26,33))
    lon <- as.numeric(substr(siteRow,34,42))
    scsFam <- substr(siteRow,44,nchar(siteRow))
    
    out_row <- data.frame(ID_WISE=pfID,ISOID=scID,COUNTRY=country,LON=lon,LAT=lat,SCS_FAM=scsFam)
    
  } else {
    isprof <- F
  }
  
  if (isprof) {
    if (ctr == 1) {
      out_all <- out_row
      ctr <- ctr+1
    } else {
      out_all <- rbind(out_all,out_row)
      ctr <- ctr+1
    }
  }
  
}

write.csv(out_all,paste(wd,"/SOL_profiles.csv",sep=""),quote=F,row.names=F)
