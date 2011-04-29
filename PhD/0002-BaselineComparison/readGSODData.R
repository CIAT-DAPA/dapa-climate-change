#Julian Ramirez
#CIAT / CCAFS / University of Leeds
#April 2011

rm(list=ls()); g=gc(); rm(g)

##############################################################
#Reading weather stations locations
##############################################################

#Read the original weather station locations file and modify it to a certain format

readWSLocation <- function(wd="") {
  setwd(wd)
  
  # First, read stations locations and create a decent CSV file
  st <- read.fortran("gsod-stations.txt", format=c("A7","A6","A30","3A3","A6","I7","I8","I7"))
  
  colNames <- c("ID","WBAN","STATIONNAME","ISO","FIPS","STATE","CALL","LAT","LONG","ALT")
  names(st) <- colNames
  
  #Formatting fields adequately
  st$ID <- substr(st$ID, 1, 6); st$ID <- paste("GSOD", st$ID, sep="")
  st$WBAN <- substr(st$WBAN,1,5); st$WBAN <- paste("WBAN",st$WBAN,sep="")
  st$ISO <- substr(st$ISO, 1, 2)
  st$FIPS <- substr(st$FIPS, 1, 2)
  st$STATE <- substr(st$STATE, 1, 2)
  st$CALL <- substr(st$CALL, 1, 4)
  st$LAT[which(st$LAT == -99999)] <- NA
  st$LONG[which(st$LONG == -999999)] <- NA
  st$ALT[which(st$ALT == -99999)] <- NA
  st$LAT <- st$LAT / 1000
  st$LONG <- st$LONG / 1000
  st$ALT <- st$ALT * 0.1
  
  #Writing output file
  write.csv(st, "gsod-stations.csv", quote=T, row.names=F)
  return(st)
}


##############################################################
#Reading weather stations data
##############################################################

#Read input op.gz files from the specified year's folder and:
#     1. Do the whole unit conversion
#     2. Store a formatted file in dir.in/formatted/stations, 
#        that has all days/months for the year regardless of availability (filled with NAs when unavailable)
#     3. Add the station locations
#     4. Calculate monthly totals
#     5. Join all stations at a daily step and monthly step into one single file into dir.in/formatted/summaries
#     6. Generate daily and monthly files files into dir.in/formatted/monthly and dir.in/formatted/daily

readStations <- function(wd="", year) {
  setwd(wd)
  
  #Defining input folders
  dir.in <- paste("./", year, sep="")
  dir.out <- paste(dir.in, "/formatted", sep=""); if (!file.exists(dir.out)) {dir.create(dir.out)}
  dir.out.stations <- paste(dir.out, "/stations", sep=""); if (!file.exists(dir.out.stations)) {dir.create(dir.out.stations)}
  st.list <- list.files(dir.in, pattern="op.gz")[1:51]
  
  #Loading stations locations station location
  st <- read.csv("gsod-stations.csv")
  
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
  
  cat("Reading files \n")
  
  #Looping through stations
  counter <- 0
  for (sts in st.list) {
    if (counter%%10 == 0) {cat("Processing", sts, "\n")}
    
    #Defining input and output files
    st.file.in <- paste(dir.in,"/", sts, sep="")
    st.file.out <- paste(dir.out.stations, "/", sts, ".csv", sep="")
    
    #Reading in data if file does not exist
    if (!file.exists(st.file.out)) {
      data <- read.fortran(gzfile(st.file.in),format=c("A6","A6","I6","2I2","F8","I3","F8","I3","F8","I3","F8","I3","F7","I3",
      "F7","I3","2F7","F8","A1","F7","A1","F6","A1","F6","I3","5I1"),skip=1,na.strings=c("9999.9","999.9","99.99"))
      
      #Defining column names
      cNames <- c("ID","WBAN","YEAR","MONTH","DAY","TMEAN","N.TMEAN","DEWP","N.DEWP","SLP","N.SLP","STP","N.STP","VISIB","N.VISIB","WDSP",
      "N.WDSP","MXSPD","GUST","TMAX","TMAX.FLAG","TMIN","TMIN.FLAG","RAIN","RAIN.FLAG","SNDP","FOG","RAIN","SNOW","HAIL","THUNDER","TORNADO")
      names(data) <- cNames
      
      #Improving ID fields to avoid mismatches
      data$ID <- paste("GSOD",data$ID,sep="")
      data$WBAN <- gsub(" ","",data$WBAN); data$WBAN <- paste("WBAN",data$WBAN,sep="")
      data$TMAX.FLAG[which(data$TMAX.FLAG == " ")] <- NA
      data$TMIN.FLAG[which(data$TMIN.FLAG == " ")] <- NA
      data$RAIN.FLAG[which(data$RAIN.FLAG == " ")] <- NA
      
      #Fill in the whole matrix in a 365/366 day matrix (all.x=T)
      data$MTH.STR <- paste(data$MONTH)
      data$MTH.STR[which(data$MONTH<10)] <- paste(0,data$MONTH[which(data$MONTH<10)],sep="")
      data$DAY.STR <- paste(data$DAY)
      data$DAY.STR[which(data$DAY<10)] <- paste(0,data$DAY[which(data$DAY<10)],sep="")
      data$MTH.DAY <- paste("M",data$MTH.STR,"D",data$DAY.STR,sep="")
      data$MTH.STR <- NULL; data$DAY.STR <- NULL
      
      #Merge both matrices
      data.mgd <- merge(date.grid, data, all.x=T, all.y=F)
      data.mgd$ID <- data$ID[1]
      data.mgd$WBAN <- data$WBAN[1]
      data.mgd$YEAR <- data$YEAR[1]
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
      
      #Adding latitude and longitude
      id <- data$ID[1]; wban <- data$WBAN[1] #Station id and wban (for matching)
      
      data.mgd$LONG <- st$LONG[which(st$ID == id & st$WBAN == wban)]
      data.mgd$LAT <- st$LAT[which(st$ID == id & st$WBAN == wban)]
      data.mgd$ALT <- st$ALT[which(st$ID == id & st$WBAN == wban)]
      
      #Reorganizing fields
      data.mgd <- data.mgd[,c(1:4,35,36,37,5:34)]
      
      #Writing formatted file
      write.csv(data.mgd, st.file.out, row.names=F, quote=F)
    } else {
      data.mgd <- read.csv(st.file.out)
    }
    
    #Calculate monthly totals
    for (mth in 1:12) {
      av.data <- data.mgd[which(data.mgd$MONTH == mth),c(11,13,15,17,19,21,23,24,25,27,29,31)]
      
      #Calculating averages over the whole month
      totals <- colMeans(av.data,na.rm=T)
      
      #Correcting for precip, that should be a sum and not average
      totals[which(names(totals) == "RAIN")] <- totals[which(names(totals) == "RAIN")] * length(which(!is.na(av.data$RAIN)))
      
      #Setting NA if too few data was used in the totalling
      if (length(which(is.na(av.data$RAIN))) > 3) {totals[which(names(totals) == "RAIN")] <- NA}
      if (length(which(is.na(av.data$TMEAN))) > 15) {totals[which(names(totals) == "TMEAN")] <- NA}
      if (length(which(is.na(av.data$TMAX))) > 15) {totals[which(names(totals) == "TMAX")] <- NA}
      if (length(which(is.na(av.data$TMIN))) > 15) {totals[which(names(totals) == "TMIN")] <- NA}
      if (length(which(is.na(av.data$DEWP))) > 15) {totals[which(names(totals) == "DEWP")] <- NA}
      if (length(which(is.na(av.data$SLP))) > 15) {totals[which(names(totals) == "SLP")] <- NA}
      if (length(which(is.na(av.data$STP))) > 15) {totals[which(names(totals) == "STP")] <- NA}
      if (length(which(is.na(av.data$VISIB))) > 15) {totals[which(names(totals) == "VISIB")] <- NA}
      if (length(which(is.na(av.data$WDSP))) > 15) {totals[which(names(totals) == "WDSP")] <- NA}
      if (length(which(is.na(av.data$MXSPD))) > 15) {totals[which(names(totals) == "MXSPD")] <- NA}
      if (length(which(is.na(av.data$GUST))) > 15) {totals[which(names(totals) == "GUST")] <- NA}
      if (length(which(is.na(av.data$SNDP))) > 15) {totals[which(names(totals) == "SNDP")] <- NA}
      
      #A totalising line
      totals <- cbind(data.frame(ID=paste(data.mgd$ID[1]), WBAN=paste(data.mgd$WBAN[1]), LONG=data.mgd$LONG[1], LAT=data.mgd$LAT[1], ALT=data.mgd$ALT[1], YEAR=data.mgd$YEAR[1], MONTH=mth), as.data.frame(t(totals)))
      
      #Comprising all into one single matrix
      if (mth == 1) {
        monthly <- totals
      } else {
        monthly <- rbind(monthly, totals)
      }
    }
    
    #Merge file into one single all-station all-days/months file per year
    if (counter == 0) {
      all.data <- data.mgd
      all.monthly <- monthly
    } else {
      all.data <- rbind(all.data,data.mgd)
      all.monthly <- rbind(all.monthly, monthly)
    }
    
    counter <- counter+1
  }
  
  all.data <- as.data.frame(all.data); all.monthly <- as.data.frame(all.monthly)
  row.names(all.data) <- 1:nrow(all.data); row.names(all.monthly) <- 1:nrow(all.monthly)
  
  dir.out.summaries <- paste(dir.out, "/summaries", sep=""); if (!file.exists(dir.out.summaries)) {dir.create(dir.out.summaries)}
  dir.out.daily <- paste(dir.out, "/daily", sep=""); if (!file.exists(dir.out.daily)) {dir.create(dir.out.daily)}
  dir.out.monthly <- paste(dir.out, "/monthly", sep=""); if (!file.exists(dir.out.monthly)) {dir.create(dir.out.monthly)}
  
  write.csv(all.data, paste(dir.out.summaries, "/", year, "-daily.csv", sep=""), row.names=F, quote=F)
  write.csv(all.monthly, paste(dir.out.summaries, "/", year, "-monthly.csv", sep=""), row.names=F, quote=F)

  #for a given year, produce daily and monthly files with all stations
  cat("\n Splitting months \n")
  mth.list <- unique(all.monthly$MONTH)
  for (mth in mth.list) {
    #Selecting data for the specified month
    all.st.month <- as.data.frame(all.monthly[which(all.monthly$MONTH == mth),])
    write.csv(all.st.month, paste(dir.out.monthly, "/stations-", mth, ".csv", sep=""), row.names=F, quote=F)
    
    day.list <- unique(all.data$DAY[which(all.data$MONTH == mth)])
    for (day in day.list) {
      #Select data for the specified day
      all.st.day <- all.data[which(all.data$MONTH == mth & all.data$DAY == day),]
      write.csv(all.st.day, paste(dir.out.daily, "/stations-", mth, "-", day, ".csv", sep=""), row.names=F, quote=F)
    }
  }
  
  #Split the all.monthly file into a file per variable with months as columns rather than in rows
  #From the monthly (all months) file create a YEAR,ID,LON,LAT,ALT,JAN,FEB,MAR for each variable (TEMP,TMAX,TMIN,RAIN)
  cat("\n Merging data \n")
  for (variable in c("TMEAN","TMAX","TMIN","RAIN")) {
    #Selecting column
    selCol <- which(names(all.monthly) == variable)
    
    #Selecting data
    data.in <- all.monthly[,c(1:7,selCol)]
    
    #Creating output data frame
    data.out <- data.frame(ID=NA,WBAN=NA,LONG=NA,LAT=NA,ALT=NA,YEAR=NA,JAN=NA,FEB=NA,MAR=NA,
        APR=NA,MAY=NA,JUN=NA,JUL=NA,AUG=NA,SEP=NA,OCT=NA,NOV=NA,DEC=NA)
    
    #Looping through stations
    st.codes <- paste(data.in$ID, data.in$WBAN,sep="")
    for (st in unique(st.codes)) {
      st.data <- data.in[which(data.in$ID == substr(st,1,10) & data.in$WBAN == substr(st,11,19)),]
      data.out <- rbind(data.out, c(paste(st.data$ID[1]),paste(st.data$WBAN[1]),st.data$LONG[1],st.data$LAT[1],st.data$ALT[1],st.data$YEAR[1],st.data[,ncol(st.data)]))
    }
    data.out <- data.out[2:nrow(data.out),]
    
    #Writing output
    write.csv(data.out, paste(dir.out.summaries, "/gsod-", year, "-", tolower(variable), "-data-all.csv", sep=""), row.names=F, quote=F)
  }
  return(dir.out)
}


##############################################################
#Merge year-variable files into a all-years file per variable
##############################################################

#Load individual year data and then do a rbind to merge all the years

joinYears <- function(wd, year.ini, year.fin, variable="tmean") {
  
  #Looping through selected years
  for (yr in year.ini:year.fin) {
    cat("Joining", yr, "\n")
    setwd(wd)
    dir.in <- paste("./", yr, "/formatted/summaries", sep="")
    file.in <- paste(dir.in, "/gsod-", yr, "-", variable, "-data-all.csv", sep="")
    
    if (yr == year.ini) {
      var.data <- read.csv(file.in)
    } else {
      var.data <- rbind(var.data, read.csv(file.in))
    }
  }
  
  dir.out <- "./all-years"; if (!file.exists(dir.out)) {dir.create(dir.out)}
  write.csv(var.data, paste(dir.out, "/gsod-", variable, "-data-all.csv", sep=""), row.names=F, quote=F)
  return(var.data)
}


##############################################################
#Calculating 30-yr running means
##############################################################

#Calculate 30 year running average between 1961-1990, for comparisons with the same period of GCM data
#Load stations, and then per station calculate the average over the desired period for each month

baselineMean <- function(outvn, wd="") {
  setwd(wd)
  #outvn <- "rain"
  
  #Loading station data
  od <- "./all-years"
  data.in <- read.csv(paste(od, "/gsod-", outvn, "-data-all.csv", sep=""))
  
  #Looping through stations to average and count number of years with data per month
  st.codes <- paste(data.in$ID, data.in$WBAN,sep="")
  stations <- unique(st.codes)
  for (st in stations) {
    st.years <- data.in[which(data.in$ID == substr(st, 1, 10) & data.in$WBAN == substr(st, 11, 19)),]
    
    #Calculating mean and counting
    st.mean <- t(colMeans(st.years[,3:ncol(st.years)], na.rm=T))
    st.count <- apply(st.years[,3:ncol(st.years)], 2, FUN=function(x) {length(which(!is.na(x)))}); st.count <- t(st.count)
    
    #Creating output data frame
    if (st == stations[1]) {
      summ.mean <- cbind(data.frame(ID=substr(st,1,10),WBAN=substr(st,11,19)),as.data.frame((st.mean)))
      summ.count <- cbind(data.frame(ID=substr(st,1,10),WBAN=substr(st,11,19)),as.data.frame((st.count)))
    } else {
      summ.mean <- rbind(summ.mean, cbind(data.frame(ID=substr(st,1,10),WBAN=substr(st,11,19)),as.data.frame((st.mean))))
      summ.count <- rbind(summ.count, cbind(data.frame(ID=substr(st,1,10),WBAN=substr(st,11,19)),as.data.frame((st.count))))
    }
  }
  
  #Fixing final data frames
  summ.mean <- summ.mean[,c(1:5,7:ncol(summ.mean))]
  summ.count <- summ.count[,c(1:5,7:ncol(summ.count))]
  
  #Writing final data frames
  write.csv(summ.mean, paste(od, "/gsod-", outvn, "-1961_1990-mean.csv", sep=""), row.names=F, quote=F)
  write.csv(summ.count, paste(od, "/gsod-", outvn, "-1961_1990-count.csv", sep=""), row.names=F, quote=F)
  return(stations)
}
