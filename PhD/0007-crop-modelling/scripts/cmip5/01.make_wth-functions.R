#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2012


#################################################################################
### function to write data for a given cell
#################################################################################
write_cmip5_cell <- function(cells,cell,gcm,ens,yi,yf,wh_leap,crop_dir,wth_dir,cmip_wth,sow_dir,crop_short) {
  #open the weather files for each variables and select corresponding years
  pr_data <- read.csv(paste(cmip_wth,"/pr/cell-",cell,".csv",sep=""))
  pr_data <- pr_data[which(pr_data$YEAR >= yi & pr_data$YEAR <= yf),]
  pr_mis <- as.numeric(apply(pr_data,1,check_missing,wh_leap,1000))
  pr_datac <- as.data.frame(t(apply(pr_data,1,correct_neg,wh_leap)))
  pr_datac <- cbind(YEAR=yi:yf,pr_datac)
  
  tx_data <- read.csv(paste(cmip_wth,"/tasmax/cell-",cell,".csv",sep=""))
  tx_data <- tx_data[which(tx_data$YEAR >= yi & tx_data$YEAR <= yf),]
  tx_mis <- as.numeric(apply(tx_data,1,check_missing,wh_leap,100))
  tx_mis <- max(c(tx_mis,as.numeric(apply(tx_data,1,check_missing,wh_leap,-100))))
  
  tn_data <- read.csv(paste(cmip_wth,"/tasmin/cell-",cell,".csv",sep=""))
  tn_data <- tn_data[which(tn_data$YEAR >= yi & tn_data$YEAR <= yf),]
  tn_mis <- as.numeric(apply(tn_data,1,check_missing,wh_leap,100))
  tn_mis <- max(c(tn_mis,as.numeric(apply(tn_data,1,check_missing,wh_leap,-100))))
  
  rs_data <- read.csv(paste(cmip_wth,"/rsds/cell-",cell,".csv",sep=""))
  rs_data <- rs_data[which(rs_data$YEAR >= yi & rs_data$YEAR <= yf),]
  rs_mis <- as.numeric(apply(rs_data,1,check_missing,wh_leap,100))
  
  ##########################################################################
  #only if there is no missing data this will be written per year to a file
  
  #folder and file where missing reports will be filed
  mis_dir <- paste(wth_dir,"/_missing",sep="")
  if (!file.exists(mis_dir)) {dir.create(mis_dir)}
  mis_fil <- paste(mis_dir,"/",gcm,"_ENS_",ens,".missing",sep="")
  
  #output folder
  owth_dir <- paste(wth_dir,"/",gcm,"_ENS_",ens,sep="")
  
  data_val <- max(c(pr_mis,tx_mis,tn_mis,rs_mis))
  if (data_val == 0) {
    #here verify if there was missing data for that particular scenario
    #if so, don't do anything, else write the wth data
    if (!file.exists(mis_fil)) {
      if (!file.exists(owth_dir)) {dir.create(owth_dir,recursive=T)}
      
      #load sowing date (khariff)
      sow_date <- read.fortran(paste(sow_dir,"/sowing_",cell,"_start.txt",sep=""),format=c("2I4","I6"))
      sow_date <- as.numeric(sow_date$V3)
      cells$SOW_DATE <- sow_date
      
      cat("constructing wth files for rainfed system\n")
      out_wth <- make_wth_cmip5(x=cells,cell,wthDir=paste(owth_dir,"/rfd_",cell,sep=""),
                                cmip_wthDataDir=cmip_wth,
                                fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE"),
                                what_leap=wh_leap,yi=yi,yf=yf,pr=pr_datac,tasmax=tx_data,
                                tasmin=tn_data,rsds=rs_data)
      
      #Study on groundnuts says that irrigated gnuts in Gujarat are sown between Jan-Feb and harvested
      #between April and May: sown in day 32 [zone 2]
      #in Uttar Pradesh it is 15th November (day 320) [zone 1]
      #in Andhra Pradesh it is 15th November (day 320) [zone 5]
      #in Karnataka and Tamil Nadu it is 15th January (day 15) [zone 5]
      #in Orissa it is 15h November (day 320) [zone 4]
      #in Madhya Pradesh it is 15th November (day 320) [zone 3]
      
      #This info was condensed into a raster file, which has the planting information per
      #Indian groundnut growing zone (that was done manually). Loading it...
      rabi_sow <- raster(paste(crop_dir,"/",tolower(crop_short),"-zones/plant_rabi.asc",sep=""))
      icells <- cells; icells$SOW_DATE <- extract(rabi_sow,cbind(x=icells$X,y=icells$Y))
      
      cat("constructing wth files for rabi (irrigated) system\n")
      owthDir <- make_wth_cmip5(x=icells,cell,wthDir=paste(owth_dir,"/irr_",cell,sep=""),
                                cmip_wthDataDir=cmip_wth,
                                fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE"),
                                what_leap=wh_leap,yi=yi,yf=yf,pr=pr_datac,tasmax=tx_data,
                                tasmin=tn_data,rsds=rs_data)
    }
  } else {
    #if there is missing data then i have to write some file indicative of
    #missing data, so i can verify and not write anything else for that model
    #(as needed)
    mf <- file(mis_fil,"a")
    cat("missing data found at cell:",cell,"\n",file=mf)
    close(mf)
    
    if (file.exists(owth_dir)) {
      system(paste("rm -rf ",owth_dir,sep=""))
    }
  }
  return(owth_dir)
}




#################################################################################
#################################################################################
# function to make weather for a number of cells using GCM data
#################################################################################
#################################################################################
make_wth_cmip5 <- function(x,cell,wthDir,cmip_wthDataDir,
                           fields=list(CELL="CELL",X="X",Y="Y",SOW_DATE="SOW_DATE"),
                           what_leap="yes",yi=1966,yf=1993,pr,tasmax,tasmin,rsds) {
  #checks
  if (length(which(toupper(names(fields)) %in% c("CELL","X","Y","SOW_DATE"))) != 4) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 4) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$X))] <- "X"
  names(x)[which(toupper(names(x)) == toupper(fields$Y))] <- "Y"
  names(x)[which(toupper(names(x)) == toupper(fields$SOW_DATE))] <- "SOW_DATE"
  
  #check if wthDir does exist
  if (!file.exists(wthDir)) {dir.create(wthDir)}
  
  #loop cells
  col <- 0; row <- 1
  for (cll in cell) {
    #site name and details
    lon <- x$X[which(x$CELL == cll)]; lat <- x$Y[which(x$CELL == cll)]
    
    if (col == 10) {
      col <- 1
      row <- row+1
    } else {
      col <- col+1
    }
    
    if (col < 10) {col_t <- paste("00",col,sep="")}
    if (col >= 10 & col < 100) {col_t <- paste("0",col,sep="")}
    if (col >= 100) {col_t <- paste(col)}
    
    if (row < 10) {row_t <- paste("00",row,sep="")}
    if (row >= 10 & row < 100) {row_t <- paste("0",row,sep="")}
    if (row >= 100) {row_t <- paste(row)}
    
    ###sowing date
    sdate <- x$SOW_DATE[which(x$CELL == cll)]
    hdate <- sdate+120
    
    s_details <- data.frame(NAME=paste("gridcell ",cll,sep=""),INSI="INGC",LAT=lat,LONG=lon,ELEV=-99,TAV=-99,AMP=-99,REFHT=-99,WNDHT=-99)
    
    #loop through years and write weather files
    for (yr in yi:yf) {
      #yr <- 1966
      #the below needs to be changed if you wanna write more than 1 cell
      wthfile <- paste(wthDir,"/ingc",row_t,col_t,yr,".wth",sep="")
      
      #get the weather data for that particular gridcell
      if (hdate > 365) {
        osdate <- 31 #output planting date
        
        #planted in prev. year, get that weather
        pyr <- yr-1
        #previous year
        tmin <- tasmin[which(tasmin$YEAR==pyr),]
        tmin$YEAR <- NULL
        tmin <- as.numeric(tmin)[1:365]
        tmin_1 <- tmin[(sdate-30):365]
        
        #harv year
        tmin <- tasmin[which(tasmin$YEAR==yr),]
        tmin$YEAR <- NULL
        tmin <- as.numeric(tmin)[1:365]
        tmin_2 <- tmin[1:(365-length(tmin_1))]
        tmin <- c(tmin_1,tmin_2)
        
        #previous year
        tmax <- tasmax[which(tasmax$YEAR==pyr),]
        tmax$YEAR <- NULL
        tmax <- as.numeric(tmax)[1:365]
        tmax_1 <- tmax[(sdate-30):365]
        
        #harv year
        tmax <- tasmax[which(tasmax$YEAR==yr),]
        tmax$YEAR <- NULL
        tmax <- as.numeric(tmax)[1:365]
        tmax_2 <- tmax[1:(365-length(tmax_1))]
        tmax <- c(tmax_1,tmax_2)
        
        #previous year
        prec <- pr[which(pr$YEAR==pyr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        prec_1 <- prec[(sdate-30):365]
        
        #harv year
        prec <- pr[which(pr$YEAR==yr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        prec_2 <- prec[1:(365-length(prec_1))]
        prec <- c(prec_1,prec_2)
        
        #previous year
        srad <- rsds[which(rsds$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
        srad_1 <- srad[(sdate-30):365]
        
        srad <- rsds[which(rsds$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
        srad_2 <- srad[1:(365-length(srad_1))]
        srad <- c(srad_1,srad_2)
        
      } else {
        osdate <- sdate #output planting date
        
        tmin <- tasmin[which(tasmin$YEAR==yr),]
        tmin$YEAR <- NULL
        tmin <- as.numeric(tmin)[1:365]
        
        tmax <- tasmax[which(tasmax$YEAR==yr),]
        tmax$YEAR <- NULL
        tmax <- as.numeric(tmax)[1:365]
        
        prec <- pr[which(pr$YEAR==yr),]
        prec$YEAR <- NULL
        prec <- as.numeric(prec)[1:365]
        
        srad <- rsds[which(rsds$YEAR==yr),]
        srad$YEAR <- NULL
        srad <- as.numeric(srad)[1:365]
      }
      
      wx <- data.frame(DATE=NA,JDAY=1:365,SRAD=srad,TMAX=tmax,TMIN=tmin,RAIN=prec)
      
      #fix matrix according to leap condition
      if (what_leap == "all30") {
        if (hdate > 365) {
          wx[72,c("SRAD","TMAX","TMIN","RAIN")] <- wx[71,c("SRAD","TMAX","TMIN","RAIN")]
          wx[73,c("SRAD","TMAX","TMIN","RAIN")] <- wx[71,c("SRAD","TMAX","TMIN","RAIN")]
          wx[74,c("SRAD","TMAX","TMIN","RAIN")] <- wx[71,c("SRAD","TMAX","TMIN","RAIN")]
          wx[75,c("SRAD","TMAX","TMIN","RAIN")] <- wx[71,c("SRAD","TMAX","TMIN","RAIN")]
          wx[76,c("SRAD","TMAX","TMIN","RAIN")] <- wx[71,c("SRAD","TMAX","TMIN","RAIN")]
        } else {
          wx <- wx[1:360,]
          dg30 <- createDateGridCMIP5(whatLeap=what_leap,year=yr)
          dg31 <- createDateGridCMIP5(whatLeap="no",year=yr)
          wx$MTH.DAY <- dg30$MTH.DAY
          wx2 <- merge(dg31,wx,by="MTH.DAY",sort=T,all.x=T,all.y=F)
          wx2[which(wx2$MTH.DAY == "M01D31"),c("SRAD","TMAX","TMIN","RAIN")] <- wx2[which(wx2$MTH.DAY == "M01D30"),c("SRAD","TMAX","TMIN","RAIN")]
          wx2[which(wx2$MTH.DAY == "M03D31"),c("SRAD","TMAX","TMIN","RAIN")] <- wx2[which(wx2$MTH.DAY == "M03D30"),c("SRAD","TMAX","TMIN","RAIN")]
          wx2[which(wx2$MTH.DAY == "M05D31"),c("SRAD","TMAX","TMIN","RAIN")] <- wx2[which(wx2$MTH.DAY == "M05D30"),c("SRAD","TMAX","TMIN","RAIN")]
          wx2[which(wx2$MTH.DAY == "M07D31"),c("SRAD","TMAX","TMIN","RAIN")] <- wx2[which(wx2$MTH.DAY == "M07D30"),c("SRAD","TMAX","TMIN","RAIN")]
          wx2[which(wx2$MTH.DAY == "M08D31"),c("SRAD","TMAX","TMIN","RAIN")] <- wx2[which(wx2$MTH.DAY == "M08D30"),c("SRAD","TMAX","TMIN","RAIN")]
          wx2[which(wx2$MTH.DAY == "M10D31"),c("SRAD","TMAX","TMIN","RAIN")] <- wx2[which(wx2$MTH.DAY == "M10D30"),c("SRAD","TMAX","TMIN","RAIN")]
          wx2[which(wx2$MTH.DAY == "M12D31"),c("SRAD","TMAX","TMIN","RAIN")] <- wx2[which(wx2$MTH.DAY == "M12D30"),c("SRAD","TMAX","TMIN","RAIN")]
          wx2$MTH.DAY <- NULL
          wx2$JDAY <- 1:365
          wx <- wx2
        }
      }
      
      #make field with date
      wx$DATE[which(wx$JDAY < 10)] <- paste(substr(yr,3,4),"00",wx$JDAY[which(wx$JDAY < 10)],sep="")
      wx$DATE[which(wx$JDAY >= 10 & wx$JDAY < 100)] <- paste(substr(yr,3,4),"0",wx$JDAY[which(wx$JDAY >= 10 & wx$JDAY < 100)],sep="")
      wx$DATE[which(wx$JDAY >= 100)] <- paste(substr(yr,3,4),wx$JDAY[which(wx$JDAY >= 100)],sep="")
      
      wthfile <- write_wth(inData=wx,outfile=wthfile,site.details=s_details)
    }
  }
  return(list(WTH_DIR=wthDir,SOW_DATE=osdate))
}


#########################################################
#function to check whether there is any missing data in the set of years selected
#########################################################
correct_neg <- function(x,wleap) {
  year <- x[1]
  data <- x[2:length(x)]
  
  nd <- leap(year)
  if (nd == 365) {data <- data[1:nd]}
  if (wleap == "no") {data <- data[1:365]}
  if (wleap == "all30") {data <- data[1:360]}
  
  data[which(data<0)] <- 0
  return(data)
}



#########################################################
#function to check whether there is any missing data in the set of years selected
#########################################################
check_missing <- function(x,wleap,thresh) {
  year <- x[1]
  data <- x[2:length(x)]
  
  nd <- leap(year)
  if (nd == 365) {data <- data[1:nd]}
  if (wleap == "no") {data <- data[1:365]}
  if (wleap == "all30") {data <- data[1:360]}
  
  nas <- length(which(is.na(data))) #number of no data
  if (thresh > 0) {
    nex <- length(which(data>thresh)) #number of data exceeding a given threshold
  } else {
    nex <- length(which(data<thresh)) #number of data exceeding a given threshold
  }
  
  nms <- max(c(nas,nex))
  return(nms)
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


#########################################################
#function to determine if leap year
#########################################################
leap <- function(year) {
  if (year%%4==0) {
    if (year%%100==0) {
      if (year%%400==0) {
        isLeap <- T
        nday <- 366
      } else {
        isLeap <- F
        nday <- 365
      }
    } else {
      isLeap <- T
      nday <- 366
    }
  } else {
    isLeap <- F
    nday <- 365
  }
  return(nday)
}
