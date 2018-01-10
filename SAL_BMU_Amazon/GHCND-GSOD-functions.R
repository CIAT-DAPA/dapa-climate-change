#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#December 2011

##############################################################################
#function to read in and input data for GHCN stuff
getRawGHCN <- function(ghcnDir,yr,rg,stations) {
  #list stations in folder
  reDir <- paste(hdir,"/ghcnd_",rg,sep="")
  stList <- list.files(reDir)
  
  #create output matrix
  dayMx <- as.data.frame(matrix(ncol=370,nrow=length(stList)))
  names(dayMx) <- c("ID","LON","LAT","ALT",1:366)
  dayMx$ID <- stList
  
  for (i in 1:nrow(dayMx)) {
    id <- dayMx$ID[i]
    stFile <- paste(reDir,"/",id,"/",yr,".csv",sep="")
    stData <- read.csv(stFile)
    
    #put everything back onto matrix
    dayMx$LON[i] <- stations$LON[which(stations$ID==dayMx$ID[i])]
    dayMx$LAT[i] <- stations$LAT[which(stations$ID==dayMx$ID[i])]
    dayMx$ALT[i] <- stations$ALT[which(stations$ID==dayMx$ID[i])]
    dd <- stData$PRCP; if (length(dd) == 365) {dd <- c(dd,NA)}
    dayMx[which(dayMx$ID==id),5:370] <- dd
  }
  return(dayMx)
}


#Function to get GSOD data and write it
getRawGSOD <- function(gsodDir,yr,rg) {
  #open list of stations
  stations <- read.csv(paste(gsodDir,"/ish-history.csv",sep=""))
  
  #list stations and set IDs
  yrDir <- paste(gsodDir,"/",yr,"_out-",rg,sep="")
  stList <- list.files(yrDir)
  #create output matrix
  dayMx <- as.data.frame(matrix(ncol=372,nrow=length(stList)))
  names(dayMx) <- c("ID","USAF","WBAN","LON","LAT","ALT",1:366)
  
  #if no stations exist for that year
  if (length(stList)!=0) {
    stList <- gsub(paste("-",yr,".csv",sep=""),"",stList)
    which.usaf <- seq(1,length(stList)*2-1,by=2)
    usaf <- unlist(strsplit(stList,"-",fixed=T))[which.usaf]
    wban <- unlist(strsplit(stList,"-",fixed=T))[-which.usaf]
    
    #get ids in output matrix
    dayMx$ID <- stList; dayMx$USAF <- usaf; dayMx$WBAN <- wban
    
    #assign IDs
    stations$ID <- paste(stations$USAF,"-",stations$WBAN,sep="")
    
    #assign latitude and longitude
    for (i in 1:length(stList)) {
      dayMx$LON[i] <- stations$LON[which(stations$ID==dayMx$ID[i])] / 1000
      dayMx$LAT[i] <- stations$LAT[which(stations$ID==dayMx$ID[i])] / 1000
      dayMx$ALT[i] <- stations$ELEV..1M.[which(stations$ID==dayMx$ID[i])] / 10
    }
    
    #read in and assign the variable data
    for (i in 1:nrow(dayMx)) {
      id <- dayMx$ID[i]
      stFile <- paste(yrDir,"/",id,"-",yr,".csv",sep="")
      stData <- read.csv(stFile)
      
      #make NA all those data that are untrustable, RAIN.FLAG == I | A | B | C | E
      stData$RAIN[which(stData$RAIN.FLAG=="I")] <- NA
#       stData$RAIN[which(stData$RAIN.FLAG=="A")] <- NA
#       stData$RAIN[which(stData$RAIN.FLAG=="B")] <- NA
#       stData$RAIN[which(stData$RAIN.FLAG=="C")] <- NA
#       stData$RAIN[which(stData$RAIN.FLAG=="E")] <- NA
#       
      #put everything onto matrix
      dd <- stData$RAIN; if (length(dd) == 365) {dd <- c(dd,NA)}
      dayMx[which(dayMx$ID==id),7:372] <- dd
    }
  }
  return(dayMx)
}

#count number of stations per day
countNADays <- function(dayData) {
  nas <- length(which(is.na(dayData)))
  return(nas)
}


####################################################################################
#Function to read in and convert GHCND data to a readable nice matrix with all dates
####################################################################################
convertGHCND <- function(id,yr,ddir,odir) {
  osdir <- paste(odir,"/",id,sep=""); if (!file.exists(osdir)) {dir.create(osdir)}
  oFile <- paste(osdir,"/",yr,".csv",sep="")
  if (!file.exists(oFile)) {
    nday <- leap(yr)
    bmat <- createBaseMat(nday)
    wData <- getDataGHCN(id,yr,ddir,bmat)
    wData$PRCP[which(wData$M=="P")] <- NA
    wData$PRCP[which(wData$Q=="I" | wData$Q=="M" | wData$Q=="N" | wData$Q=="X" | wData$Q=="O")] <- NA
    wData$PRCP <- wData$PRCP/10
    write.csv(wData,paste(osdir,"/",yr,".csv",sep=""),quote=F,row.names=F)
  }
}


####################################################################################
#Function to read in and convert GSOD data to a readable nice matrix with all dates
####################################################################################
convertGSOD <- function(id,yr,gdir,ogdir) {
  usaf <- gsub("USAF","",strsplit(id,"_")[[1]][1])
  wban <- gsub("WBAN","",strsplit(id,"_")[[1]][2])
  inFile <- paste(gdir,"/",usaf,"-",wban,"-",yr,".op.gz",sep="")
  oFile <- paste(ogdir,"/",usaf,"-",wban,"-",yr,".csv",sep="")
  
  #read in the data if input file exists and output file does not exist
  if (file.exists(inFile) & !file.exists(oFile)) {
    gdata <- read.fortran(gzfile(inFile),format=c("A6","A6","I6","2I2","F8","I3","F8","I3","F8","I3","F8","I3","F7","I3",
          "F7","I3","2F7","F8","A1","F7","A1","F6","A1","F6","I3","5I1"),skip=1,na.strings=c("9999.9","999.9","99.99"))
    cNames <- c("ID","WBAN","YEAR","MONTH","DAY","TMEAN","N.TMEAN","DEWP","N.DEWP","SLP","N.SLP","STP","N.STP","VISIB","N.VISIB","WDSP",
          "N.WDSP","MXSPD","GUST","TMAX","TMAX.FLAG","TMIN","TMIN.FLAG","RAIN","RAIN.FLAG","SNDP","FOG","PRCP","SNOW","HAIL","THUNDER","TORNADO")
    names(gdata) <- cNames
    #Improving ID fields to avoid mismatches
    gdata$ID <- paste("GSOD",gdata$ID,sep="")
    gdata$WBAN <- gsub(" ","",gdata$WBAN); gdata$WBAN <- paste("WBAN",gdata$WBAN,sep="")
    gdata$TMAX.FLAG[which(gdata$TMAX.FLAG == " ")] <- NA
    gdata$TMIN.FLAG[which(gdata$TMIN.FLAG == " ")] <- NA
    gdata$RAIN.FLAG[which(gdata$RAIN.FLAG == " ")] <- NA
    #get the data organised in a date matrix
    gdata$MTH.STR <- paste(gdata$MONTH)
    gdata$MTH.STR[which(gdata$MONTH<10)] <- paste(0,gdata$MONTH[which(gdata$MONTH<10)],sep="")
    gdata$DAY.STR <- paste(gdata$DAY)
    gdata$DAY.STR[which(gdata$DAY<10)] <- paste(0,gdata$DAY[which(gdata$DAY<10)],sep="")
    gdata$MTH.DAY <- paste("M",gdata$MTH.STR,"D",gdata$DAY.STR,sep="")
    gdata$MTH.STR <- NULL; gdata$DAY.STR <- NULL #remove extra fields
    dgrid <- createDateGrid(yr) #create date grid
    data.mgd <- merge(dgrid, gdata, all.x=T, all.y=F) #Merge both matrices
    data.mgd$ID <- gdata$ID[1]
    data.mgd$WBAN <- gdata$WBAN[1]
    data.mgd$YEAR <- gdata$YEAR[1]
    data.mgd$MONTH <- as.numeric(substr(data.mgd$MTH.DAY,2,3))
    data.mgd$DAY <- as.numeric(substr(data.mgd$MTH.DAY,5,6))
    #All unit conversion
    data.mgd$TMEAN <- (data.mgd$TMEAN-32)*(5/9) #Mean temperature from F to Celsius
    data.mgd$DEWP <- (data.mgd$DEWP-32)*(5/9) #Dewpoint temp from F to Celsius
    data.mgd$SLP <- data.mgd$SLP #Sea level pressure from millibars to millibars
    data.mgd$STP <- data.mgd$STP #Station pressure from millibars to millibars
    data.mgd$VISIB <- data.mgd$VISIB * 1.609344 #Visibility from miles to kilometers
    data.mgd$WDSP <- data.mgd$WDSP * 1.852 #Wind speed from knots to kph
    data.mgd$MXSPD <- data.mgd$MXSPD * 1.852 #Maximum wind speed from knots to kph
    data.mgd$GUST <- data.mgd$GUST * 1.852 #Wind gust reported from knots to kph
    data.mgd$TMAX <- (data.mgd$TMAX-32)*(5/9) #Max temperature from F to Celsius
    data.mgd$TMIN <- (data.mgd$TMIN-32)*(5/9) #Max temperature from F to Celsius
    data.mgd$RAIN <- data.mgd$RAIN * 25.4 #Rainfall from inch to millimeters
    data.mgd$SNDP <- data.mgd$SNDP * 25.4 #Snow depth from inch to millimeters
    #Writing the csv
    write.csv(data.mgd,oFile,row.names=F,quote=F)
  }
}


####################################################################################
#Function to create a base matrix for GSOD data
####################################################################################
createDateGrid <- function(year) {
  #Date grid (accounting to leap years). This is for merging with the station data to avoid gaps
  if (year%%4 == 0 & year%%100 != 0) { #This is a leap year
    date.grid <- data.frame(MONTH=c(rep(1,31),rep(2,29),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),DAY=c(1:31,1:29,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
    date.grid$MTH.STR <- paste(date.grid$MONTH)
    date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
    date.grid$DAY.STR <- paste(date.grid$DAY)
    date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
    date.grid$MTH.DAY <- paste("M",date.grid$MTH.STR,"D",date.grid$DAY.STR,sep="")
    date.grid$MONTH <- NULL; date.grid$DAY <- NULL; date.grid$MTH.STR <- NULL; date.grid$DAY.STR <- NULL
  } else { #This is a non-leap year
    date.grid <- data.frame(MONTH=c(rep(1,31),rep(2,28),rep(3,31),rep(4,30),rep(5,31),rep(6,30),rep(7,31),rep(8,31),rep(9,30),rep(10,31),rep(11,30),rep(12,31)),DAY=c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31))
    date.grid$MTH.STR <- paste(date.grid$MONTH)
    date.grid$MTH.STR[which(date.grid$MONTH<10)] <- paste(0,date.grid$MONTH[which(date.grid$MONTH<10)],sep="")
    date.grid$DAY.STR <- paste(date.grid$DAY)
    date.grid$DAY.STR[which(date.grid$DAY<10)] <- paste(0,date.grid$DAY[which(date.grid$DAY<10)],sep="")
    date.grid$MTH.DAY <- paste("M",date.grid$MTH.STR,"D",date.grid$DAY.STR,sep="")
    date.grid$MONTH <- NULL; date.grid$DAY <- NULL; date.grid$MTH.STR <- NULL; date.grid$DAY.STR <- NULL
  }
  date.grid$JD <- 1:nrow(date.grid) #Adding the Julian day
  return(date.grid)
}

####################################################################################
#Function to create a base matrix with day of year, month and day of month 
####################################################################################
createBaseMat <- function(nday) {
  baseMat <- data.frame(DAY=1:nday,MONTH=NA,DOFM=NA,PRCP=NA,M=NA,Q=NA,S=NA)
  if (nday==366) {
    months <- c(rep(1,times=31),rep(2,times=29),rep(3,times=31),rep(4,times=30),rep(5,times=31),
                rep(6,times=30),rep(7,times=31),rep(8,times=31),rep(9,times=30),rep(10,times=31),
                rep(11,times=30),rep(12,times=31))
    dofm <- c(1:31,1:29,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31)
  } else {
    months <- c(rep(1,times=31),rep(2,times=28),rep(3,times=31),rep(4,times=30),rep(5,times=31),
                rep(6,times=30),rep(7,times=31),rep(8,times=31),rep(9,times=30),rep(10,times=31),
                rep(11,times=30),rep(12,times=31))
    dofm <- c(1:31,1:28,1:31,1:30,1:31,1:30,1:31,1:31,1:30,1:31,1:30,1:31)
  }
  baseMat$MONTH <- months; baseMat$DOFM <- dofm
  return(baseMat)
}

####################################################################################
#Function to read in the daily data of the station and return a matrix 
#with quality results and data
####################################################################################
getDataGHCN <- function(id,year,data.dir,base.mat) {
  #year <- 1977 #define year (further this needs to be a loop)
  #data.dir <- paste(ghcnDir,"/ghcnd_all",sep="")
  #base.mat <- baseMat
  #id <- ghcn.dates$ID[1] #get id
  #check file exists so read
  fName <- paste(data.dir,"/",id,".dly",sep="") #set file name
  if (file.exists(fName)) {
    stData <- read.fortran(fName,format=c("A11","I4","I2","A4",rep(c("F5","3A1"),times=31))) #read data
    names(stData) <- c("ID","YEAR","MONTH","VARIABLE",rep(c("D","M","Q","S"),times=31)) #set names
    stData <- stData[which(stData$VARIABLE=="PRCP"),] #get only precip data
    stData <- stData[which(stData$YEAR == year),] #get only that year's data
    if (nrow(stData)!=0) { #do only if there is data for that year
      prcp <- apply(base.mat,1,searchData,"D",stData) #get actual data
      mflag <- apply(base.mat,1,searchData,"M",stData) #get measurement flag
      qflag <- apply(base.mat,1,searchData,"Q",stData) #get quality flag
      sflag <- apply(base.mat,1,searchData,"S",stData) #get source flag
      base.mat$PRCP <- prcp; base.mat$M <- mflag; base.mat$Q <- qflag; base.mat$S <- sflag
    }
  }
  return(base.mat)
}

#########################################################
#function to get the data in proper matrix
#########################################################
searchData <- function(row,field,st.data) {
  mth <- row[2] #month
  dom <- row[3] #day of month
  mthRow<- st.data[which(st.data$MONTH==mth),] #get row corresponding to month
  if (nrow(mthRow)==0) {
    reqval <- NA
  } else {
    scol <- as.numeric((dom-1)*4+5); fcol <- as.numeric(scol+3) #get locations
    reqcol <- mthRow[scol:fcol] #get required columns (day of month)
    reqval <- reqcol[,field]
    if (reqval == -9999) { #set NA is missing (as specified)
      reqval <- NA
    }
  }
  return(reqval) #return value
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

####################################################################################
#Function to convert to monthly data and calculate the 30yr averages for GSOD data
####################################################################################
mergeDailyGSOD <- function(oDir, ogdir, st_ids, usaf_ids){

  #QC functions
  qc_prec = function(x){
    pos=which(x>350 | x<0)
    if(length(pos)!=0){
      x[pos]=NA}
    return(x)
  }
  
  qc_temp = function(x){
    pos = which(x>50 | x<(-20))
    if(length(pos) != 0){
      x[pos] = NA}
    return(x) 
  }
  
  #Functions to sum and average columns
  sum_var = function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    if (na.x>=20) {
      x = NA
    } else {
      x = sum(a, na.rm = any(!is.na(a)) ) }
    return(x)}
  
  avg_var = function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    if(na.x>=0.20){
      x = NA
    }else{
      x = mean(a, na.rm = any(!is.na(a))) }
    return(x)}
  
  # Date format
  dates = format(seq(as.Date("1960/1/1"), as.Date("2010/12/31"), "days"),"%Y%m%d")
  dates <- cbind.data.frame(dates, "NA")
  names(dates) <- c("Date", "NA")
  
  dataMt_prec <- as.data.frame(matrix(NA,nrow(dates),length(st_ids)))
  dataMt_tmax <- as.data.frame(matrix(NA,nrow(dates),length(st_ids)))
  dataMt_tmin <- as.data.frame(matrix(NA,nrow(dates),length(st_ids)))
  
  ## Merge in one single daily file by variable
  for (j in 1:length(st_ids)){
    
    files <- list.files(ogdir, pattern=st_ids[j])
    
    if (length(files) > 0){
      
      st_data <- lapply(paste(ogdir,"/", files, sep=""), function(x){read.csv(x, header=T)})  
      st_data <- do.call(rbind, st_data)
      
      # Combine interest columns in a single data frame    
      date_gsod <- format(as.Date(paste0(st_data$YEAR, sprintf("%02d", st_data$MONTH), sprintf("%02d", st_data$DAY)), format="%Y%m%d"),format="%Y%m%d")
      st_var <- cbind("Date"=date_gsod, qc_prec(as.numeric(st_data$RAIN)), qc_prec(as.numeric(st_data$TMAX)), qc_prec(as.numeric(st_data$TMIN)) )
      
      merged =  merge(dates, st_var, by="Date", all.x=T)
      dataMt_prec[,j] = merged[,3]
      dataMt_tmax[,j] = merged[,4]
      dataMt_tmin[,j] = merged[,5]
      
    }
    
  }
  
  names(dataMt_prec) <- usaf_ids
  names(dataMt_tmax) <- usaf_ids
  names(dataMt_tmin) <- usaf_ids
  
  # Add dates
  year = as.numeric(substr(dates[,1],1,4))
  month = as.numeric(substr(dates[,1],5,6))
  day = as.numeric(substr(dates[,1],7,8))
  
  dataMt_prec <- cbind("Year"=year, "Month"=month, "Day"=day, dataMt_prec)
  dataMt_tmax <- cbind("Year"=year, "Month"=month, "Day"=day, dataMt_tmax)
  dataMt_tmin <- cbind("Year"=year, "Month"=month, "Day"=day, dataMt_tmin)
  
  write.csv(dataMt_prec, paste0(odir, "/prec_daily_all_qc.csv"), row.names=F)
  write.csv(dataMt_tmax, paste0(odir, "/tmax_daily_all_qc.csv"), row.names=F)
  write.csv(dataMt_tmin, paste0(odir, "/tmin_daily_all_qc.csv"), row.names=F)
  
}

####################################
### Monthly aggregation QC GSOD ####
####################################

monthly_agg <- function(var="prec", bDir = "Z:/DATA/WP2/01_Weather_Stations/GSOD", oDir = "Z:/DATA/WP2/01_Weather_Stations/GSOD"){
  
  # Read daily QC data
  data_qc <- read.csv(paste0(bDir, "/", var, "_daily_all_qc.csv"), header = T)
  
  # Monthly aggregation based on min percent of NA
  
  if (var == "prec"){
    
    sum22=function(a,na.rm=T){
      na.x=sum(is.na(a))/length(a)
      if(na.x>=0.20){
        x=NA
      }else{x=sum(a,na.rm=any(!is.na(a)))}
      
      return(x)
    }
    
  } else {
    
    
    sum22=function(a,na.rm=T){
      na.x=mean(is.na(a))/length(a)
      if(na.x>=0.60){
        x=NA
      }else{x=mean(a,na.rm=any(!is.na(a)))}
      
      return(x)
    }
    
  }
  
  # Aggregate 
  monthly_var = aggregate(data_qc, by=list(Month=data_qc$Month,Year=data_qc$Year),sum22)
  monthly_var[,3] <- NULL
  monthly_var[,3] <- NULL
  monthly_var[,3] <- NULL
  
  
  # Write monthly quality controled
  write.csv(monthly_var, paste0(oDir, "/", var, "_monthly_all.csv"), row.names=F)
  
  # which(apply(monthly_precip, 2, min,na.rm=T) == Inf)
  
}


##################################
### Climatology Calcs GSOD ####
##################################

clim_calc <- function(var="prec",  bDir = "Z:/DATA/WP2/01_Weather_Stations/COL", oDir = "Z:/DATA/WP2/01_Weather_Stations/COL", st_loc="Z:/DATA/WP2/01_Weather_Stations/GSOD", sY=1981, fY=2010){
  
  # Read monthly file
  monthly_var <- read.csv(paste0(bDir, "/", var, "_monthly_all.csv"), header=T)
  
  ## Climatology aggregation based on NA percent
  avg_var = function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    if(na.x>=0.50){
      x = NA
    }else{
      x = mean(a, na.rm = any(!is.na(a))) 
    }
    return(x)
  }
  
  # Years selection
  monthly_var <- monthly_var[ which(monthly_var$Year >= sY & monthly_var$Year <= fY),]
  
  # Period climatology calc
  monthly_avg = aggregate(monthly_var[,3:length(monthly_var)], list(Month=monthly_var$Month),avg_var)
  monthly_avg$Month <- NULL
  
  # Fix station names
  st_names <- as.data.frame(gsub("X", "", names(monthly_avg)))
  names(st_names) <- "Station"
  
  # Remove rows with NA
  monthly_avg <- na.omit(cbind(st_names, round(t(monthly_avg), 1)))
  rownames(monthly_avg) <- NULL
  
  # Add month names
  mths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(monthly_avg) <- c("Station", mths)
  
  ## Add station info
  stInfo <- read.csv(st_loc, header=T)
  join <- merge(stInfo, monthly_avg, by = "Station", all = FALSE)
  join_names <- merge(stInfo, st_names, by = "Station", all = FALSE)
  
  # Combine info and data
  climData <- cbind(monthly_avg$Station, "GSOD", monthly_avg$Station, join$Name, "GSOD", join$Lon, join$Lat, join$Alt, monthly_avg[,2:ncol(monthly_avg)], 30)
  names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
  
  # Write climatology 
  write.csv(climData, paste0(oDir, "/", var, "_climatology_", sY, "_", fY, ".csv"), row.names=F)
  write.csv(join_names, paste0(oDir, "/stations_names_", var, ".csv"), row.names=F)
  
}

