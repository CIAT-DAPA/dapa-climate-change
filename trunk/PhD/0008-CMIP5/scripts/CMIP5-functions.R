#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

########################################################
#wrapper function to extract daily data from CMIP5 GCMs
########################################################
CMIP5_extract_wrapper <- function(cells,cell,cChars,i=1,oDir) {
  #list of gcms and selected gcm
  gcmList <- unique(cChars$GCM)
  gcm <- gcmList[i]
  
  outGCMDir <- paste(oDir,"/",gcm,sep="")
  if (!file.exists(outGCMDir)) {dir.create(outGCMDir)}
  
  #reduce characteristics list for this GCM
  thisGCM <- cChars[which(cChars$GCM == gcm),]
  ensList <- unique(thisGCM$Ensemble)
  
  #coordinates of gridcell
  x <- cells$X[which(cells$CELL == cell)]; y <- cells$Y[which(cells$CELL == cell)];
  
  #loop through ensembles
  for (ens in ensList) {
    #ens <- ensList[1]
    cat("Processing ensemble",paste(ens),"\n")
    thisEns <- thisGCM[which(thisGCM$Ensemble == ens),]
    
    #create directory of ensemble
    outEnsDir <- paste(outGCMDir,"/",ens,sep="")
    if (!file.exists(outEnsDir)) {dir.create(outEnsDir)}
    
    #list variables
    #vnList <- list.files(paste(mdDir,"/",gcm,"/",ens,sep=""),pattern="_1965")
    #vnList <- gsub("_1965","",vnList)
    vnList <- c("pr","tasmin","tasmax","rsds")
    
    #loop through variables
    for (vn in vnList) {
      #vn <- vnList[1]
      cat("variable:",vn,"\n")
      
      #create a matrix where to put all data, with the form that i need
      out_df <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=367))
      names(out_df) <- c("YEAR",paste(1:366))
      
      #output variable directory
      outVarDir <- paste(outEnsDir,"/",vn,sep="")
      if (!file.exists(outVarDir)) {dir.create(outVarDir)}
      
      if (!file.exists(paste(outVarDir,"/cell-",cell,".csv",sep=""))) {
        #loop through years
        yrc <- 1
        for (year in yi:yf) {
          cat("\nprocessing year",year,"\n")
          #year <- 1961
          cat("year:",year,"\n")
          
          yrDir <- paste(mdDir,"/",gcm,"/",ens,"/",vn,"_",year,sep="")
          #list of daily nc files in the year folder
          dayList <- list.files(yrDir,pattern="\\.nc")
          
          #check which calendar is used
          wlp <- thisEns$has_leap[1]
          
          #calendar to fit data into
          dg <- createDateGridCMIP5(year,whatLeap=wlp)
          dg$YRDATA <- NA
          names(dg)[length(names(dg))] <- paste(vn)
          
          #organise the file list just to make sure that it will load in the
          #proper order
          for (dayFile in dayList) {
            mth <- gsub(gcm,"",dayFile)
            mth <- gsub(paste("_",ens,"_",sep=""),"",mth)
            mth <- gsub(paste(year,"_",sep=""),"",mth)
            mth <- gsub("\\.nc","",mth)
            day <- unlist(strsplit(mth,"_",fixed=T))[4]
            mth <- unlist(strsplit(mth,"_",fixed=T))[2]
            
            cat("processing day",day,"of month",mth,"\n")
            
            dg$YRDATA[which(dg$MTH.STR == mth & dg$DAY.STR == day)] <- dayFile
          }
          
          #load whole year as a raster stack
          rstk <- stack(paste(yrDir,"/",dayList,sep=""),varname=vn)
          
          #read raster file
          rs <- raster(paste(yrDir,"/",dayFile,sep=""),varname=vn)
          rs <- rotate(rs) #rotate raster file to a -180 to 180 grid
          
          #here i need to resample this raster file to 1x1d resolution 
          #so that the data can be nicely extracted
          rs <- crop(rs,dum_rs)
          #i use nearest neighbour in order to maintain the original GCM spatial
          #variation (i.e. coarse cells), but still make it comparable to my original
          #GLAM runs
          rs <- resample(rs,dum_rs,method="ngb")
          
          #flux to mm or K to C
          if (vn == "pr") {
            rs <- rs*3600*24
          } else {
            rs <- rs - 273.15
          }
          
          #extract value of cell
          cval <- extract(rs,cbind(X=x,Y=y))
          
          #put this into the year's matrix
          dg[which(dg$MTH.STR == mth & dg$DAY.STR == day),paste(vn)] <- cval
          
          out_row <- c(year,dg[,paste(vn)])
          if (length(out_row) < 367) {
            out_row <- c(out_row,rep(NA,times=(367-length(out_row))))
          }
          
          out_df[yrc,] <- out_row
          
          yrc <- yrc+1
        }
        
        write.csv(out_df,paste(outVarDir,"/cell-",cell,".csv",sep=""),row.names=F,quote=F)
      } else {
        cat("variable",vn,"was already processed\n")
      }
    }
    
  }
  return(outGCMDir)
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

