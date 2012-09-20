#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#1. Calculate monthly totals for pr, tas, tn, tx, and dtr

########################################################
#wrapper function for parallel processing of the monthly time series calculation
########################################################
wrapper_monthly_TS_rcp45 <- function(i) {
  #libraries
  library(raster); library(ncdf)
  
  #sourcing needed functions
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/12.02.calcMonthlyGCM_rcp45-functions.R",sep=""))
  
  #get the list of GCMs
  gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
  gcmList <- unique(gcmChars$GCM)
  gcm <- gcmList[i]
  
  thisGCM <- gcmChars[which(gcmChars$GCM == gcm),]
  ensList <- unique(thisGCM$Ensemble)
  for (ens in ensList) {
    #ens <- ensList[1]
    thisEns <- thisGCM[which(thisGCM$Ensemble == ens),]
    wlp <- thisEns$has_leap[1]
    
    #looping list of variables. These are all daily, so no worries about that
    #if the variable is not present, just skip it
    vnList <- c("pr","tas","dtr","tasmin","tasmax")
    for (vn in vnList) {
      #vn <- vnList[1]
      cat("\nvariable:",vn,"\n")
      
      yrc <- 1
      for (year in yi:yf) {
        #year <- yi
        cat("\nyear:",year,"\n")
        
        yroDir <- paste(mdDir,"/",gcm,"/",ens,"_monthly/",year,sep="")
        if (!file.exists(yroDir)) {dir.create(yroDir,recursive=T)}
        
        if (vn == "dtr") {
          yrDir1 <- paste(mdDir,"/",gcm,"/",ens,"/tasmin_",year,sep="")
          yrDir2 <- paste(mdDir,"/",gcm,"/",ens,"/tasmax_",year,sep="")
          flist1 <- list.files(yrDir1,pattern="\\.nc")
          flist2 <- list.files(yrDir2,pattern="\\.nc")
        } else {
          yrDir <- paste(mdDir,"/",gcm,"/",ens,"/",vn,"_",year,sep="")
          flist1 <- list.files(yrDir,pattern="\\.nc")
          flist2 <- flist1
          
          #this bit is to fix something weird going on with the GFDL datasets
          #those were adding an additional 2 to the month name and the extension
          #hence a file for month 01 would be named as *_mth_012_day_*.nc2, for
          #some unknown reason. Only necessary for "tas"
          dayList2 <- list.files(yrDir,pattern="\\.nc2")
          if (length(dayList2) != 0) {
            setwd(yrDir)
            system("rename .nc2 .nc *.nc2")
            setwd(mdDir)
            flist1 <- list.files(yrDir,pattern="\\.nc")
            flist2 <- flist1
          }
        }
        
        #process only if there are files in the folder
        if (length(flist1) > 0 & length(flist2) > 0) {
          for (mth in 1:12) {
            #mth <- 1
            cat(mth,". ",sep="")
            if (mth < 10) {mthstr <- paste("0",mth,sep="")} else {mthstr <- paste(mth)}
            
            #get the calendar and do not calculate the monthly total rainfall if there is incomplete
            #rainfall (no missing days). Ok for temperatures, but make sure you have at least 50% days
            dg <- createDateGridCMIP5(year,wlp)
            nd_mth <- nrow(dg[which(dg$MONTH == mth),])
            
            if (!file.exists(paste(yroDir,"/",vn,"_",mthstr,".tif",sep=""))) {
              #list files of that month
              if (vn == "dtr") {
                dfList1 <- list.files(yrDir1,pattern=paste("_mth_",mthstr,sep=""))
                dfList2 <- list.files(yrDir2,pattern=paste("_mth_",mthstr,sep=""))
                dif_nf1 <- nd_mth - length(dfList1)
                dif_nf2 <- nd_mth - length(dfList2)
                max_allow <- round(nd_mth*0.5,0)
                
                #if the number of missing days is above the maximum allowed (>50%)
                #for minimum temperature
                if (dif_nf1 > max_allow) {
                  rs <- raster(paste(yrDir1,"/",flist1[1],sep=""),varname="tasmin")
                  rs[] <- NA
                  rs1 <- rs
                } else {
                  #load stacks and convert variables
                  rstk1 <- stack(paste(yrDir1,"/",dfList1,sep=""),varname="tasmin")
                  rstk1 <- rstk1 - 273.15
                  rs1 <- mean(rstk1)
                }
                
                #if the number of missing days is above the maximum allowed (>50%)
                #for maximum temperature
                if (dif_nf2 > max_allow) {
                  rs <- raster(paste(yrDir2,"/",flist2[1],sep=""),varname="tasmax")
                  rs[] <- NA
                  rs2 <- rs
                } else {
                  rstk2 <- stack(paste(yrDir2,"/",dfList1,sep=""),varname="tasmax")
                  rstk2 <- rstk2 - 273.15
                  rs2 <- mean(rstk2)
                }
                
                #if both of the above were calculated then calculate the diurnal range
                if (dif_nf1 <= max_allow & dif_nf2 <= max_allow) {
                  #calculate dtr
                  rs <- rs2-rs1
                }
                
                #write the two variables variables
                rs1 <- writeRaster(rs1,paste(yroDir,"/tasmin_",mthstr,".tif",sep=""),format="GTiff")
                rs2 <- writeRaster(rs2,paste(yroDir,"/tasmax_",mthstr,".tif",sep=""),format="GTiff")
              } else {
                dfList <- list.files(yrDir,pattern=paste("_mth_",mthstr,sep=""))
                dif_nf <- nd_mth - length(dfList)
                max_allow <- round(nd_mth*0.5,0)
                
                #if the difference is greater than 0 for precip and greater than 50% of days
                #then this cannot be processed so the 
                if (vn == "pr" & dif_nf != 0) {
                  rs <- raster(paste(yrDir,"/",flist1[1],sep=""),varname=vn)
                  rs[] <- NA
                } else if (vn == "tas" & dif_nf > max_allow) {
                  #create a raster with all pixels set to NA
                  #if the length of the daily file list (dfList) is >0 then use the first file
                  rs <- raster(paste(yrDir,"/",flist1[1],sep=""),varname=vn)
                  rs[] <- NA
                } else {
                  #everything is fine, so then
                  #read these files into a raster stack
                  rstk <- stack(paste(yrDir,"/",dfList,sep=""),varname=vn)
                  if (vn == "pr") {
                    rstk <- rstk * 3600 * 24
                    rs <- sum(rstk)
                    rd <- calc(rstk,fun = function(x) {return(length(which(x>0.01)))})
                    rd <- writeRaster(rd,paste(yroDir,"/rd_",mthstr,".tif",sep=""),format="GTiff")
                  } else if (vn == "tas") {
                    rstk <- rstk - 273.15
                    rs <- mean(rstk)
                  }
                }
              }
              rs <- writeRaster(rs,paste(yroDir,"/",vn,"_",mthstr,".tif",sep=""),format="GTiff")
            }
          }
        }
        yrc <- yrc+1
      }
    }
  }
}


