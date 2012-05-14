#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Functions to get CMIP5 weather data
#gcm data extraction wrapper
gcm_wrapper <- function(i) {
  #sourcing functions
  source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir2,"/CMIP5-functions.R",sep=""))
  
  #get list of GCMs and selected GCMs
  gcmList <- unique(cChars$GCM)
  #gcm <- gcmList[13]
  gcm <- gcmList[i]
  
  #create GCM output dir
  outGCMDir <- paste(oDir,"/",gcm,sep="")
  if (!file.exists(outGCMDir)) {dir.create(outGCMDir)}
  
  #reduce characteristics list for this GCM
  thisGCM <- cChars[which(cChars$GCM == gcm),]
  ensList <- unique(thisGCM$Ensemble)
  
  #loop through ensemble members
  for (ens in ensList) {
    cat("Processing ensemble",paste(ens),"\n")
    #ens <- ensList[1]
    thisEns <- thisGCM[which(thisGCM$Ensemble == ens),]
    #year <- 1966
    for (vn in c("pr","tasmin","tasmax")) {
      cat("variable:",vn,"\n")
      #vn <- "pr" #tasmin, tasmax
      for (year in yi:yf) {
        cat("year:",year,"\n")
        fName <- paste(thisEns$naming[which(year > thisEns$iYear & year < thisEns$fYear)])
        fName <- gsub("%var%",vn,fName)
        iyr <- thisEns$iYear[which(year > thisEns$iYear & year < thisEns$fYear)]
        imt <- thisEns$iMonth[which(year > thisEns$iYear & year < thisEns$fYear)]
        idy <- thisEns$iDay[which(year > thisEns$iYear & year < thisEns$fYear)]
        wlp <- paste(thisEns$has_leap[which(year > thisEns$iYear & year < thisEns$fYear)])
        
        gFile <- paste(mdDir,"/",gcm,"/",ens,"/",fName,sep="")
        
        #get the indian extent
        xt <- extent(raster(paste(compDir,"/0_input_data/mask.asc",sep="")))
        
        #create 2.5x2.5 dummy raster
        nc <- (xt@xmax-xt@xmin)/2.5
        nr <- (xt@ymax-xt@ymin)/2.5
        xt@ymin <- xt@ymax - round(nr+0.5,0)*2.5
        nr <- (xt@ymax-xt@ymin)/2.5
        
        dumm_rs <- raster(xt,ncol=nc,nrow=nr)
        dumm_rs[] <- 1
        
        daily_data <- extractFromGCM(yr=year,gcmFile=gFile,iYear=iyr,iMth=imt,
                                     iDay=idy,wLeap=wlp,varName=vn,msk=dumm_rs,
                                     x=68.75,y=22.75,ccDir=compDir)
        
        dg <- createDateGridCMIP5(year=year,whatLeap=wlp)
        dg$VALUES <- daily_data
        names(dg)[7] <- vn
        
        oFile <- gsub("\\.nc",paste("_",year,".csv",sep=""),fName)
        oFile <- paste(outGCMDir,"/",oFile,sep="")
        write.csv(dg,oFile,row.names=F,quote=T)
      }
    }
  }
}




####Extract data from a given GCM
extractFromGCM <- function(yr,gcmFile,iYear,iMth,iDay,wLeap,varName,msk,x,y,ccDir) {
  nd <- leap(yr)
  if (wLeap=="all30") {
    nd <- 360
  } else if (wLeap == "no") {
    nd <- 365
  }
  
  for (d in 1:nd) {
    #read the day's data
    pos <- findNCPosCMIP5(thisYear=yr,thisDay=d,iniYear=iYear,iniMonth=iMth,iniDay=iDay,whatLeap=wLeap)
    rs <- raster(gcmFile,varname=varName,band=pos)
    
    #crop gcm data and scale to 2.5x2.5 model grid
    rs <- rotate(rs)
    rs <- crop(rs,msk)
    rs <- resample(rs,msk,method="bilinear")
    
    #kg/m2/s to mm
    rs <- rs*3600*24
    
    #extract weather for the gridcell we want
    #LAT=23.000, LON=72.000 
    xy <- cbind(LON=x,LAT=y)
    val <- extract(rs,xy)[1]
    
    # plot(rs,col=rev(rainbow(20)))
    # plot(wrld_simpl,add=T)
    # points(xy,pch=20)
    
    if (d == 1) {
      all_days <- val
    } else {
      all_days <- c(all_days,val)
    }
  }
  return(all_days)
}




############################ function to find the position of a day in
############################ a netCDF file
findNCPosCMIP5 <- function(thisYear,thisDay,iniYear,iniMonth,iniDay,whatLeap) {
  dg <- createDateGridCMIP5(year=thisYear,whatLeap=whatLeap)
  ijday <- dg$JD[which(dg$MONTH==iniMonth & dg$DAY == iniDay)]
  
  counter <- 0
  if (thisYear == iniYear & thisDay == ijday) {
    counter <- 0
  } else {
    #loop years
    for (yr in iniYear:thisYear) {
      if (yr == iniYear) {stDay <- ijday} else {stDay <- 1}
      
      if (yr < thisYear) {
        nd <- leap(yr)
        if (whatLeap=="all30") {
          nd <- 360
        } else if (whatLeap == "no") {
          nd <- 365
        }
        
        for (day in stDay:nd) {
          #look for time of day in that day
          counter <- counter+1
        }
      } else {
        #i'm in this year, so just count until that day regardless of total ndays
        for (day in stDay:thisDay) {
            counter <- counter+1
        }
      }
    }
  }
  return(counter)
}


###################################
createDateGridCMIP5 <- function(year,whatLeap) {
  #Date grid (accounting to leap years). This is for merging with the station data to avoid gaps
  if (whatLeap=="yes") {
    if (year%%4 == 0 & year%%100 != 0) { #This is a leap year
      date.grid <- data.frame(MONTH=c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),DAY=c(1:31,1:29,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
      date.grid$MTH.STR <- paste(date.grid$MONTH)
      date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
      date.grid$DAY.STR <- paste(date.grid$DAY)
      date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
      date.grid$MTH.DAY <- paste("M",date.grid$MTH.STR,"D",date.grid$DAY.STR,sep="")
    } else { #This is a non-leap year
      date.grid <- data.frame(MONTH=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),DAY=c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
      date.grid$MTH.STR <- paste(date.grid$MONTH)
      date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
      date.grid$DAY.STR <- paste(date.grid$DAY)
      date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
      date.grid$MTH.DAY <- paste("M",date.grid$MTH.STR,"D",date.grid$DAY.STR,sep="")
    }
  } else if (whatLeap=="no") {
    date.grid <- data.frame(MONTH=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),DAY=c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
    date.grid$MTH.STR <- paste(date.grid$MONTH)
    date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
    date.grid$DAY.STR <- paste(date.grid$DAY)
    date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
    date.grid$MTH.DAY <- paste("M",date.grid$MTH.STR,"D",date.grid$DAY.STR,sep="")
  } else if (whatLeap=="all30") {
    date.grid <- data.frame(MONTH=c(rep(1,30),rep(2,30),rep(3,30),rep(4,30),rep(5,30),rep(6,30),rep(7,30),rep(8,30),rep(9,30),rep(10,30),rep(11,30),rep(12,30)),DAY=c(1:30,1:30,1:30,1:30,1:30,1:30,1:30,1:30,1:30,1:30,1:30,1:30))
    date.grid$MTH.STR <- paste(date.grid$MONTH)
    date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
    date.grid$DAY.STR <- paste(date.grid$DAY)
    date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
    date.grid$MTH.DAY <- paste("M",date.grid$MTH.STR,"D",date.grid$DAY.STR,sep="")
  }
  
  date.grid$JD <- 1:nrow(date.grid) #Adding the Julian day
  return(date.grid)
}

