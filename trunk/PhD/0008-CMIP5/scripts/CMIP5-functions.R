#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

##############################################################################
##############################################################################
#Format properly the CIAT daily rainfall data
loadWriteYear <- function(yr,wstSel,csvDir,outDir) {
  if (!file.exists(paste(outDir,"/",yr,".csv",sep=""))) {
    
    cat("\nprocessing year",yr,"\n")
    for (i in 1:nrow(wstSel)) {
      wsID <- wstSel$FILE[i]
      wsLon <- wstSel$FF_LON[i]
      wsLat <- wstSel$FF_LAT[i]
      wsAlt <- wstSel$ALT[i]
      
      if (i == 1) cat("started",i,"-",paste(wsID),"\n")
      if (i %% 100 == 0) cat("station",i,"-",paste(wsID),"\n")
      
      csvFile <- gsub("\\.rnf.gz","\\.csv",wsID)
      
      #reading csv file
      rData <- read.csv(paste(csvDir,"/",csvFile,sep=""))
      
      #get the data
      yrData <- rData[which(rData$YEAR == yr),]
      
      #divide by 10 to get data in mm
      yrData[,paste("X",1:366,sep="")] <- yrData[,paste("X",1:366,sep="")]*0.1
      
      if (nrow(yrData) == 0) {
        out_row <- data.frame(ID=paste(wsID),LON=wsLon,LAT=wsLat,ALT=wsAlt,t(rep(NA,times=366)))
      } else {
        out_row <- yrData
        out_row$FILE <- NULL; out_row$NAME <- NULL; out_row$ISO <- NULL; out_row$YEAR <- NULL
        out_row <- cbind(ID=paste(wsID),out_row)
      }
      
      #append the data
      if (i==1) {
        out_all <- out_row
      } else {
        out_all <- rbind(out_all,out_row)
      }
      
    }
    
    names(out_all) <- c("ID","LON","LAT","ALT",1:366)
    write.csv(out_all,paste(outDir,"/",yr,".csv",sep=""),quote=F,row.names=F)
  } else {
    cat("that year was already processed\n")
    out_all <- read.csv(paste(outDir,"/",yr,".csv",sep=""))
  }
  return(out_all)
}


##############################################################################
##############################################################################
#Calculate diurnal temperature range for a series of 24 values where the first 12
#correspond to tmax, and the second 12 correspond to tmin. To be used with *apply
calc_dtr <- function(x) {
  tmax <- x[1:12]
  tmin <- x[13:24]
  dtr <- tmax-tmin
  return(dtr)
}


##############################################################################
##############################################################################
#Function to process everything into a folder
AsctoGTiff <- function(this_dir) {
  ascList <- list.files(this_dir,pattern="\\.asc")
  if (length(grep("\\.gz",ascList)) > 0) {
    ascList <- ascList[-grep("\\.gz",ascList)]
  }
  
  if (length(ascList) == 0) {
    cat("This folder does not contain any raw ascii grid \n")
  } else {
    for (asc in ascList) {
      cat(asc,"\n")
      rs <- raster(paste(this_dir,"/",asc,sep=""))
      tifName <- gsub(".asc",".tif",asc)
      
      if (!file.exists(paste(this_dir,"/",tifName,sep=""))) {
        rs <- writeRaster(rs,paste(this_dir,"/",tifName,sep=""),format="GTiff")
      }
      
      if (!file.exists(paste(this_dir,"/",asc,".gz",sep=""))) {
        setwd(this_dir)
        system(paste("7z a -tgzip",paste(asc,".gz",sep=""),asc))
      }
      
      if (file.exists(paste(this_dir,"/",asc,sep=""))) {
        if (file.exists(paste(this_dir,"/",tifName,sep=""))) {
          if (file.exists(paste(this_dir,"/",asc,".gz",sep=""))) {
            x <- file.remove(paste(this_dir,"/",asc,sep=""))
          }
        }
      }
    }
  }
  return("Done!")
}



########################################################
#wrapper function for parallel processing of the climatology calculation
########################################################
wrapper_climatology <- function(i) {
  #libraries
  library(raster); library(rgdal)
  
  #sourcing needed functions
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  
  #get list of GCMs, and GCM name given i
  gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
  gcmList <- unique(gcmChars$GCM)
  gcm <- gcmList[i]
  
  #i/o directory with GCM data
  gcmDir <- paste(mdDir,"/",gcm,sep="")
  
  #list and loop through ensembles
  ensList <- list.files(gcmDir,pattern="_monthly")
  for (ens in ensList) {
    cat("\nensemble",paste(ens),"\n")
    #ens <- ensList[1]
    #output dir
    oClim <- paste(gcmDir,"/",gsub("_monthly","",ens),"_climatology",sep="")
    if (!file.exists(oClim)) {dir.create(oClim)}
    
    #list of variables
    vnList <- c("pr","rd","tas","dtr","tasmax","tasmin")
    #loop through variables
    for (vn in vnList) {
      #vn <- vnList[1]
      cat("\nvariable:",vn,"\n")
      
      #input folder (ensemble level)
      ensDir <- paste(gcmDir, "/", ens, sep="")
      m_seq <- 1:12; m_seq[which(m_seq < 10)] <- paste("0",m_seq[which(m_seq < 10)],sep="")
      for (mth in m_seq) {
        #mth <- m_seq[1]
        cat(mth,". ",sep="")
        
        #if the resulting file does not exist then process else do nothing
        if (!file.exists(paste(oClim,"/",vn,"_",mth,".tif",sep=""))) {
          #make, check list of files, and remove any NAs
          mfList <- paste(ensDir,"/",yi:yf,"/",vn,"_",mth,".tif",sep="")
          mfList <- as.character(sapply(mfList,checkExists)) #check existence of files
          mfList <- mfList[which(!is.na(mfList))]
          
          #if all files are NA, meaning there is nothing, don't calculate anything
          if (length(mfList) > 0) {
            #load stack of files
            #rstk <- stack(mfList)
            
            #here need to check the mean of these rasters and then load the raster stack
            rstk <- sapply(mfList,checkMaxMin,vn=vn)
            rstk <- stack(rstk)
            
            #calculate mean
            rs <- calc(rstk,fun = function(x) {mean(x,na.rm=T)})
            #write resulting raster
            rs <- writeRaster(rs,paste(oClim,"/",vn,"_",mth,".tif",sep=""),format="GTiff")
          }
        }
      }
    }
  }
}


########################################################
#check if a file exists, to be used in an "*apply" command
checkMaxMin <- function(x,vn) {
  x <- raster(x)
  if (vn == "pr") {
    x[which(x[] > 10000)] <- NA
    x[which(x[] < 0)] <- NA
  } else if (vn == "tas") {
    x[which(x[] > 100)] <- NA
    x[which(x[] < -100)] <- NA
  } else if (vn == "tasmax") {
    x[which(x[] > 100)] <- NA
    x[which(x[] < -100)] <- NA
  } else if (vn == "tasmin") {
    x[which(x[] > 100)] <- NA
    x[which(x[] < -100)] <- NA
  } else if (vn == "dtr") {
    x[which(x[] > 100)] <- NA
    x[which(x[] < 0)] <- NA
  } else if (vn == "rd") {
    x[which(x[] > 32)] <- NA
    x[which(x[] < 0)] <- NA
  }
  return(x)
}



########################################################
#check if a file exists, to be used in an "*apply" command
checkExists <- function(x) {
  if (file.exists(x)) {y <- x} else {y <- NA}
  return(y)
}


########################################################
#wrapper function for parallel processing of the monthly time series calculation
########################################################
wrapper_monthly_TS <- function(i) {
  #libraries
  library(raster); library(ncdf)
  
  #sourcing needed functions
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  
  #get the list of GCMs
  gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
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
    vnList <- c("pr","tas","dtr")
    for (vn in vnList) {
      #vn <- vnList[1]
      cat("\nvariable:",vn,"\n")
      
      yrc <- 1
      for (year in yi:yf) {
        #year <- 1960
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
              if (length(dayList2) != 0) {
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

