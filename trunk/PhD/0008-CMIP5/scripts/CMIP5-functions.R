#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT


########################################################
#wrapper function for parallel processing
########################################################
wrapper_CMIP_extract <- function(i) {
  #libraries
  library(raster); library(ncdf)
  
  #sourcing needed functions
  source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  
  #output gridcell data dir
  cDataDir <- paste(bDir,"/climate-data/gridcell-data",sep="")
  outDir <- paste(cDataDir,"/IND_CMIP5",sep="")
  if (!file.exists(outDir)) {dir.create(outDir)}
  
  #load GCM characteristics
  gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
  
  #load cell details
  cropName <- "gnut"
  all_cells <- read.csv(paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
  
  #get the indian extent
  drs <- raster(paste(src.dir2,"/data/mask.tif",sep=""))
  drs[which(!is.na(drs[]))] <- 1
  
  #extract the data for a given GCM
  od <- CMIP5_extract(cells=all_cells,cChars=gcmChars,dum_rs=drs,i=i,yi=ys,yf=ye,oDir=outDir)
}



########################################################
#function to extract daily data from CMIP5 GCMs
########################################################
CMIP5_extract <- function(cells,cChars,dum_rs,i=1,yi=1961,yf=2002,oDir) {
  #list of gcms and selected gcm
  gcmList <- unique(cChars$GCM)
  gcm <- gcmList[i]
  
  outGCMDir <- paste(oDir,"/",gcm,sep="")
  if (!file.exists(outGCMDir)) {dir.create(outGCMDir)}
  
  #reduce characteristics list for this GCM
  thisGCM <- cChars[which(cChars$GCM == gcm),]
  ensList <- unique(thisGCM$Ensemble)
  
  #loop through ensembles
  for (ens in ensList) {
    #ens <- ensList[1]
    cat("\nprocessing ensemble",paste(ens),"of model",paste(gcm),"\n")
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
      
      #output variable directory
      outVarDir <- paste(outEnsDir,"/",vn,sep="")
      if (!file.exists(outVarDir)) {dir.create(outVarDir)}
      
      flist <- list.files(outVarDir,pattern="\\.csv")
      
      if (length(flist) != nrow(cells)) {
        #loop through years
        yrc <- 1
        for (year in yi:yf) {
          #year <- 1961
          cat("\nyear:",year,"\n")
          
          yrDir <- paste(mdDir,"/",gcm,"/",ens,"/",vn,"_",year,sep="")
          #list of nc files in the year folder (could be monthly e.g. srad, or daily)
          ncList <- list.files(yrDir,pattern="\\.nc")
          
          #check if there is data for that year
          if (!file.exists(yrDir) | length(ncList) == 0) {
            #there is either no data or no folder for that year and variable, so need to generate
            #dummy monthly files with all values being NA
            
            #this would be done only if the object does not exist (i.e. if it was not)
            #created by a previous year)
            if (!exists("odf_all")) {
              odf_all <- list()
              for (cell in cells$CELL) {
                #create a matrix where to put all data, in the form that i need
                odf_all[[paste("c",cell,sep="")]] <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=13))
                names(odf_all[[paste("c",cell,sep="")]]) <- c("YEAR",paste("MONTH",1:12,sep=""))
              }
            }
            
            #put the year to a value to the cell values
            for (cell in cells$CELL) {
              odf_all[[paste("c",cell,sep="")]][yrc,"YEAR"] <- year
            }
            
          } else {
            #there is data, either monthly or daily, so it will be extracted
            if (length(ncList) == 12) {
              #loop cells in here and create bits of a list for each if this object did not exist
              if (!exists("odf_all")) {
                odf_all <- list()
                for (cell in cells$CELL) {
                  #create a matrix where to put all data, in the form that i need
                  odf_all[[paste("c",cell,sep="")]] <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=13))
                  names(odf_all[[paste("c",cell,sep="")]]) <- c("YEAR",paste("MONTH",1:12,sep=""))
                }
              }
              
              #we're dealing with monthly files, hence the daily date.grid seems not necessary
              dg <- data.frame(MONTH=1:12)
              dg$MTH.STR <- 1:12
              dg$MTH.STR[which(dg$MONTH < 10)] <- paste("0",dg$MONTH[which(dg$MONTH < 10)],sep="")
              dg$YRDATA <- NA
              dg$VARDATA <- NA
              names(dg)[length(names(dg))] <- paste(vn)
              
              #organise the file list just to make sure that it will load in the
              #proper order
              cat("sorting file list\n")
              for (mFile in ncList) {
                mth <- gsub(gcm,"",mFile)
                mth <- gsub(paste("_",ens,"_",sep=""),"",mth)
                mth <- gsub(paste(year,"_",sep=""),"",mth)
                mth <- gsub("\\.nc","",mth)
                mth <- unlist(strsplit(mth,"_",fixed=T))[2]
                dg$YRDATA[which(dg$MTH.STR == mth)] <- mFile
              }
            } else {
              #we're dealing with daily data
              #loop cells in here and create bits of a list for each
              if (!exists("odf_all")) {
                odf_all <- list()
                for (cell in cells$CELL) {
                  #create a matrix where to put all data, in the form that i need
                  odf_all[[paste("c",cell,sep="")]] <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=367))
                  names(odf_all[[paste("c",cell,sep="")]]) <- c("YEAR",paste(1:366))
                }
              }
              
              #list of daily nc files in the year folder
              dayList <- list.files(yrDir,pattern="\\.nc")
              
              #this bit is to fix something weird going on with the GFDL datasets
              #those were adding an additional 2 to the month name and the extension
              #hence a file for month 01 would be named as *_mth_012_day_*.nc2, for
              #some unknown reason
              dayList2 <- list.files(yrDir,pattern="\\.nc2")
              if (length(dayList) != 0) {
                setwd(yrDir)
                system("rename .nc2 .nc *.nc2")
                setwd(outGCMDir)
                dayList <- list.files(yrDir,pattern="\\.nc")
              }
              
              #check which calendar is used
              wlp <- thisEns$has_leap[1]
              
              #calendar to fit data into
              dg <- createDateGridCMIP5(year,whatLeap=wlp)
              dg$YRDATA <- NA
              dg$VARDATA <- NA
              names(dg)[length(names(dg))] <- paste(vn)
              
              #organise the file list just to make sure that it will load in the
              #proper order
              cat("sorting file list\n")
              for (dayFile in dayList) {
                mth <- gsub(gcm,"",dayFile)
                mth <- gsub(paste("_",ens,"_",sep=""),"",mth)
                mth <- gsub(paste(year,"_",sep=""),"",mth)
                mth <- gsub("\\.nc","",mth)
                day <- unlist(strsplit(mth,"_",fixed=T))[4]
                mth <- unlist(strsplit(mth,"_",fixed=T))[2]
                if (nchar(mth) == 3) {mth <- substr(mth,1,2)}
                dg$YRDATA[which(dg$MTH.STR == mth & dg$DAY.STR == day)] <- dayFile
              }
            }
            
            cat("loading and sorting out data\n")
            
            odayList <- dg$YRDATA
            
            #count if there are NAs in the list of files
            wna <- which(is.na(odayList))
            if (length(wna) > 0) {
              #there are some NAs, so the days need to be looped, so to ensure a proper extraction
              #this needs to produce a data frame that is similar to the output of the extract
              #command in raster
              
              all_vals <- matrix(nrow=nrow(cells),ncol=nrow(dg))
              for (day in 1:nrow(dg)) {
                dfil <- dg$YRDATA[day]
                #if the file for that day is not NA then load the raster
                if (!is.na(dfil)) {
                  rsd <- raster(paste(yrDir,"/",dfil,sep=""),varname=vn)
                  rsd <- rotate(rsd)
                  rsd <- crop(rsd,dum_rs)
                  rsd <- resample(rsd,dum_rs,method="ngb")
                  
                  #flux to mm | K to C | w/m2 to MJ/m2
                  if (vn == "pr") {
                    rsd <- rsd*3600*24
                  } else if (vn == "tasmin") {
                    rsd <- rsd - 273.15
                  } else if (vn == "tasmax") {
                    rsd <- rsd - 273.15
                  } else if (vn == "rsds") {
                    #w/m2/s = J/m2/s / 1000000 * 86400 = MJ/m2/day
                    rsd <- rsd * 24 * 3600 / 1000000
                  }
                  dvals <- extract(rsd,cbind(X=cells$X,Y=cells$Y))
                  all_vals[,day] <- as.numeric(dvals)
                }
              }
              
            } else {
              #load whole year as a raster stack
              rstk <- stack(paste(yrDir,"/",odayList,sep=""),varname=vn)
              
              #rotate and crop whole raster stack
              rstk <- rotate(rstk)
              rstk <- crop(rstk,dum_rs)
              
              #here i need to resample this raster file to 1x1d resolution 
              #so that the data can be nicely extracted
              
              #i use nearest neighbour in order to maintain the original GCM spatial
              #variation (i.e. coarse cells), but still make it comparable to my original
              #GLAM runs
              rstk <- resample(rstk,dum_rs,method="ngb")
              
              #flux to mm | K to C | w/m2 to MJ/m2
              if (vn == "pr") {
                rstk <- rstk*3600*24
              } else if (vn == "tasmin") {
                rstk <- rstk - 273.15
              } else if (vn == "tasmax") {
                rstk <- rstk - 273.15
              } else if (vn == "rsds") {
                #w/m2/s = J/m2/s / 1000000 * 86400 = MJ/m2/day
                rstk <- rstk * 24 * 3600 / 1000000
              }
              
              cat("extracting data for gridcells\n")
              #extract value of all cells
              all_vals <- extract(rstk,cbind(X=cells$X,Y=cells$Y))
            }
            
            cat("formatting output data.frame\n")
            ccnt <- 1
            for (cell in cells$CELL) {
              #put this into the year's matrix
              dg[,paste(vn)] <- as.numeric(all_vals[ccnt,])
              
              out_row <- c(year,dg[,paste(vn)])
              if (length(ncList) == 12) {
                out_row <- c(out_row)
              } else {
                if (length(out_row) < 367) {
                  out_row <- c(out_row,rep(NA,times=(367-length(out_row))))
                }
              }
              
              odf_all[[paste("c",cell,sep="")]][yrc,] <- out_row
              #out_df[yrc,] <- out_row
              ccnt <- ccnt+1
            }
            
          }
          yrc <- yrc+1
        }
        
        #here loop again to write the output
        cat("writing output files\n")
        for (cell in cells$CELL) {
          write.csv(odf_all[[paste("c",cell,sep="")]],paste(outVarDir,"/cell-",cell,".csv",sep=""),row.names=F,quote=F)
        }
        
      } else {
        cat("variable",vn,"was already processed\n")
      }
      if (exists("odf_all")) {rm(odf_all); g=gc(); rm(g)}
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

