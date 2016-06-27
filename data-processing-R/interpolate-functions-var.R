#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

#load libraries
library(raster)

###############################################
#Extract the data from CRU and daily interpolated surfaces
extractIntCRU <- function(lon,lat,bDir,year,re,ndaysMth) {
  #folders and other stuff
  intDir <- paste(bDir,"/daily-interpolations-v3",sep="")
  cruDir <- paste(bDir,"/CRU_TS_v3-1_data/monthly_grids",sep="")
  
  #region details
  if (re=="eaf" | re=="waf") {rg <- "afr"} else {rg <- "sas"}
  rsDir <- paste(intDir,"/",year,"-",rg,sep="")
  
  #julian dates data.frame
  nd <- leap(year)
  theRow <- which(row.names(ndaysMth)==year)
  jDates <- data.frame(MONTH=rep(names(ndaysMth),times=as.numeric(ndaysMth[theRow,])),JDAY=1:nd,
                     DOM=rep(as.numeric(ndaysMth[theRow,]),times=as.numeric(ndaysMth[theRow,])))
  
  #load dummy raster
  dumm <- raster(paste(intDir,"/0_files/rain_",re,"_dummy.asc",sep="")); dumm[] <- NA
  
  #First verify if each raster exists, else create an all-NA raster there
  cat("Verifying existence of raster data \n")
  mcount <- 0
  for (i in 1:nd) {
    rsTest <- paste(rsDir,"/",i,"/rain_",re,".asc",sep="")
    if (!file.exists(rsTest)) { #if file does not exist then create it
      writeRaster(dumm,rsTest,format='ascii',overwrite=F)
      mcount <- mcount+1
    }
  }
  if (mcount > 0) { #text to state the creation
    cat(mcount, "files were missing and created as all-NA rasters \n")
  } else {
    cat("No files were missing \n")
  }
  
  #loop through months
  for (mth in month.abb) {
    #load the cru rasters
    mnth <- which(names(ndaysMth)==mth)
    cruRaster <- raster(paste(cruDir,"/pre/pre_",year,"_",mnth,sep=""))
    
    #load interpolated stack for this month
    theCol <- which(names(ndaysMth)==mth)
    ndays <- ndaysMth[theRow,theCol]
    iday <- min(jDates$JDAY[which(jDates$MONTH==mth)])
    eday <- max(jDates$JDAY[which(jDates$MONTH==mth)])
    
    #Now loading the daily data
    cat("Loading days",iday,"to",eday,"(",mth,")\n")
    intStk <- stack(paste(rsDir,"/",iday:eday,"/rain_",re,".asc",sep=""))
    
    #extract data for given lon,lats and neighbors (corners)
    xyMat <- expand.grid(X=c(lon-0.25,lon,lon+0.25),
                         Y=c(lat-0.25,lat,lat+0.25))
    cruCell <- unique(cellFromXY(cruRaster,xyMat))
    intVals <- extract(intStk,cbind(X=lon,Y=lat))
    intVals[which(intVals<0)] <- 0
    cruVals <- extract(cruRaster,cruCell)
    
    resrow <- data.frame(LON=lon,LAT=lat,CRU_RAIN=mean(cruVals,na.rm=T),INT_RAIN=sum(intVals,na.rm=T))
    
    if (mth == month.abb[1]) {
      resMx <- resrow
    } else {
      resMx <- rbind(resMx,resrow)
    }
  }
  return(resMx)
}


########################################################
#function to extract rainfall values from interpolated grids based on xy coordinates (lon,lat)
extractRainfall <- function(yDir,rname,lon,lat,dayList) {
  for (d in dayList) {
    cat("Extracting day",d,"\n")
    #read the raster
    dDir <- paste(yDir,"/",d,sep="")
    rs <- raster(paste(dDir,"/",rname,".asc",sep=""))
    rs[which(rs[]<0)] <- 0
    val <- extract(rs,cbind(X=lon,Y=lat))
    
    if (d==dayList[1]) {
      rainfall <- val
    } else {
      rainfall <- c(rainfall,val)
    }
  }
  return(data.frame(DOY=dList,RAIN=rainfall))
}


#function to create a gif file with all days of a year
#Function to reate a list
createGIF <- function(yDir,rname,wd,ht,dayList,dummy) {
  png(file="day_%003d.png", width=wd, height=ht)
    for (d in dayList) {
      cat("Plotting day",d,"\n")
      dDir <- paste(yDir,"/",d,sep="")
      #read the raster if exists, else plot a dummy 1 raster
      if (file.exists(paste(dDir,"/",rname,".asc",sep=""))) {
        rs <- raster(paste(dDir,"/",rname,".asc",sep=""))
        rs[which(rs[]<0)] <- 0
        #plot the raster
        plot(rs,zlim=c(0,150),col=colorRampPalette(c("light blue","blue","purple"))(100),
             main=paste("Day",d),useRaster=F)
      } else {
        rs <- raster(dummy)
        rs[which(!is.na(rs[]))] <- 1
        plot(rs,zlim=c(0,1),col=colorRampPalette(c("grey 60","grey 90"))(10),
             main=paste("Day",d,":: No data"),useRaster=F)
      }
      
      plot(wrld_simpl,add=T)
      #text(.5, .5, d, cex = 1.2)
    }
  dev.off()
  
  # convert the .png files to one .gif file using ImageMagick. 
  # The system() function executes the command as if it was done
  # in the terminal. the -delay flag sets the time between showing
  # the frames, i.e. the speed of the animation.
  ff <- file("conv.bat","w")
  cat("convert -delay 10 *.png ",paste(rname,".gif",sep=""),"\n",file=ff); close(ff)
  system("conv.bat"); ff <- file.remove("conv.bat")
  
  # remove the single png files
  ff <- file.remove(list.files(pattern=".png"))
  return(paste(rname,".gif",sep=""))
}


#function to interpolate a given day
interpolateDay <- function(i,gaData,iDir,ye,re,vr) {

  #defining output directories
  oyeDir <- paste(iDir,"/",ye,"-",re,sep=""); if (!file.exists(oyeDir)) {dir.create(oyeDir)}
  oDir <- paste(oyeDir,"/",i,sep=""); if (!file.exists(oDir)) {dir.create(oDir)}
  setwd(oDir)
  
  
  if (!file.exists(paste(vr, "_", re, ".asc", sep=""))) {
	
	intMx <- gaData[,c(1:4,4+i)] #get data for that i
	intMx$SOURCE <- gaData$SOURCE
	
  if (vr == "rain") {u <- "millimetres"} else {u <- "degrees"}
	names(intMx) <- c("ID","LON","LAT","ALT","VALS","SOURCE") #rename data frame fields
	intMx <- intMx[which(!is.na(intMx$VALS)),] #remove all rows with NAs in rainfall
	intMx <- intMx[which(!is.na(intMx$ID)),] #remove all rows with NAs in rainfall
	intMx$VALS <- intMx$VALS*10
	
	#reduce number of stations (one per 1 degree cell)
	elev <- raster(paste(iDir,"/0_files/alt-prj-", re, ".asc",sep=""))
	rs <- raster(ncol=ncol(elev),nrow=nrow(elev)); rs[] <- 1:ncell(rs)
	intMx$cells <- cellFromXY(rs,cbind(intMx$LON,intMx$LAT))
	uCells <- unique(intMx$cells)
	
# 	if (nrow(intMx)!= 0) {
# 	  for (cell in uCells) {
# 		selCells <- intMx[which(intMx$cells==cell),]
# 		#remove anything that is GSOD ONLY if there is either GHCN or CIAT data in that cell
# 		uSrc <- unique(selCells$SOURCE)
# 		if (length(uSrc)>1 & "GSOD" %in% uSrc) {
# 		  selCells <- selCells[which(selCells$SOURCE != "GSOD"),]
# 		}
# 		
# 		selCells$SOURCE <- NULL
# 		
# 		if (nrow(selCells)>1) { #if there are many stations in that cell then average the locations
# 		  outLine <- data.frame(ID=paste("MNCELL",cell,sep=""),
# 								LON=mean(selCells$LON,na.rm=T),LAT=mean(selCells$LAT,na.rm=T),
# 								ALT=mean(selCells$ALT,na.rm=T),VALS=mean(selCells$VALS,na.rm=T))
# 		} else {
# 		  outLine <- selCells; outLine$cells <- NULL
# 		}
# 		
# # 		if (cell==uCells[1]) { #rbinding
# # 		  outIntMx <- outLine
# # 		} else {
# # 		  outIntMx <- rbind(outIntMx,outLine)
# # 		}
# 	  }
# 	} else {
	  outIntMx <- intMx
# 	}
	
	#check if there is any record with missing altitude
	altNA <- which(is.na(outIntMx$ALT))
	if (length(altNA)>0) {
	  elev <- raster(paste(iDir,"/0_files/alt-prj-", re, ".asc",sep=""))
	  altVals <- extract(elev,cbind(X=outIntMx$LON[altNA],Y=outIntMx$LAT[altNA]))
	  outIntMx$ALT[altNA] <- altVals
	}
	
	#writing spline training file
	oDatFile <- paste(oDir,"/", vr, "_train.dat",sep="")
	writeDat(outIntMx,filename=oDatFile)
# 	if (nrow(outIntMx)>10) {
  #creating run file and running
  createRunFile(intMx, filename=paste(vr, "_train.dat", sep=""), variable=vr,
				varUnits=u, ivars=3, icovars=0, sivars=0, sicovars=0, sp.order=2, nsurf=1)
  fw <- file(paste(vr, "_fit.bat", sep=""), open="w") #Running
  cat(anuDir, "/splina < ", vr, "fit.cmd > ", vr, "fit.log\n", sep="", file=fw)
  close(fw)
  system(paste(vr, "_fit.bat", sep=""))
  
  if (!file.exists(paste(vr, ".sur", sep=""))) { #if failed then remove one of the zeros (for some reason this fixes it)
	zeros <- which(outIntMx$VALS==0)
	randSel <- sample(zeros,1)
	writeDat(outIntMx[-randSel,],filename=oDatFile)
	system(paste(vr, "_fit.bat", sep=""))
	writeDat(outIntMx,filename=oDatFile) #re-writing the correct stations file
	}
  
	#projecting
	alt <- raster(paste(iDir,"/0_files/alt-prj-", re, ".asc",sep=""))
	gFiles <- c(paste(iDir,"/0_files/lon-prj-", re, ".asc",sep=""),
				paste(iDir,"/0_files/lat-prj-", re, ".asc",sep=""),
				paste(iDir,"/0_files/alt-prj-", re, ".asc",sep=""))
	createPrjFile(alt, variable=vr, nsurf=1, gridfiles=gFiles)
	fw <- file(paste(vr, "_prj_", re, ".bat", sep=""), open="w")
	cat(paste(anuDir, "/lapgrd < ", vr, "prj.cmd > ", vr, "prj.log\n",sep=""), file=fw)
	close(fw)
	system(paste(vr, "_prj_", re, ".bat", sep=""))
	rs <- raster(paste(vr, "_1.asc", sep=""))
	rs <- rs/10; rs <- writeRaster(rs,paste(vr, "_", re, ".asc", sep=""),format='ascii',overwrite=T)
	fr <- file.remove(paste(vr, "_1.asc", sep=""))

# 	  
# 	} else {
# 	  cat("Too few data points \n")
# 	  fw <- file("process.fail", open="w") #Running
# 	  cat("Process was failed because there were too few data points \n", file=fw)
# 	  close(fw)
# 	}

  }

  return("Done!")
}

###################################################################
###################### Functional forms ###########################
writeDat <- function(stData, filename="dummy.dat") {
  #Creating line jump row
  jumpRow <- rep("\n", nrow(stData))
  
  stData$ID <- substr(stData$ID, 1, 10)
  
  #Extracting data into a matrix
  tk <- matrix(c(format(stData$ID,width=10),
    formatC(stData$LON,width=10,digits=3,format="f"),
    formatC(stData$LAT,width=10,digits=3,format="f"),
    formatC(stData$ALT,width=8,digits=2,format="f"),
    jumpRow,
    formatC(stData$VALS,width=9,digits=2,format="f"),
    jumpRow),
    nrow=nrow(stData),ncol=7,byrow=F)
  
  #Writing the file
  fw <- file(filename, open="w")
  cat(t(tk), file=fw, sep="")
  close(fw)
  return(filename)
}

########## creat fit (run file) ####################
createRunFile <- function(inData, filename="", variable="tmean", 
varUnits="metres", ivars=3, icovars=0, sivars=0, sicovars=0, sp.order=2, nsurf=12) {
  #Units matrix
  umx <- data.frame(ID=c(0:8), UNIT=c("undefined","metres","feet","kilometres","miles","degrees","radians","millimetres","mejajoules"))
  if (!tolower(varUnits) %in% umx$UNIT) {
    unit <- 0
    cat("Variable units not recognised, so using UNDEFINED \n")
  } else {
    unit <- umx$ID[which(umx$UNIT == tolower(varUnits))]
  }
  
  #removing any -999 from the elevation data
  altData <- inData$ALT[which(inData$ALT!=-999)]
  
  #Opening file in write mode
  fw <- file(paste(variable, "fit.cmd", sep=""), open="w")
  
  #Writing program details
  cat(variable, "\n",sep="",file=fw) #Variable name
  cat(unit, "\n", sep="",file=fw) #Units
  cat(ivars, "\n",sep="",file=fw) #Independent variables
  cat(icovars, "\n",sep="",file=fw) #Independent covariates
  cat(sivars, "\n",sep="",file=fw) #Surface independent variables
  cat(sicovars, "\n",sep="",file=fw) #Surface independent covariates
  cat(min(inData$LON), " ", max(inData$LON), " ", 0, " ", 5, "\n", sep="",file=fw) #Independent variable 1 limits transf. and unit code 
  cat(min(inData$LAT), " ", max(inData$LAT), " ", 0, " ", 5, "\n", sep="",file=fw) #Independent variable 2 limits transf. and unit code
  cat(min(altData,na.rm=T), " ", max(altData,na.rm=T), " ", 1, " ", 1, "\n", sep="",file=fw) #Independent variable 3 limits transf. and unit code
  cat("1000\n", sep="",file=fw)
  cat("0\n",sep="",file=fw) #Dependent variable transformation
  cat(sp.order, "\n",sep="",file=fw) #Order of spline
  cat(nsurf, "\n",sep="",file=fw) #Number of surfaces (one per month)
  cat("0\n",sep="",file=fw) #Error surface point weighting
  cat("1\n",sep="",file=fw) #Optimisation directive
  cat("1\n",sep="",file=fw) #Smoothing directive for each surface
  cat(filename, "\n", sep="",file=fw) #Filename
  cat(round((nrow(inData)+0.1*nrow(inData))), "\n", sep="", file=fw) #Maximum number of data points
  cat("10\n",sep="",file=fw) #Station label length
  cat("(a10,2f10.3,f8.2/1f9.2)\n",sep="",file=fw) #Datafile format
  cat(variable,".res\n", sep="", file=fw) #Residual data file
  cat(variable,".opt\n", sep="", file=fw) #Optimisation data file
  cat(variable,".sur\n", sep="", file=fw) #Surface coefficients file
  cat(variable,".lis\n", sep="", file=fw) #Data list file
  cat(variable,".cov\n", sep="", file=fw) #Error covariance file
  cat("\n",file=fw)
  
  #Closing file
  close(fw)
  return(paste(variable,".sur",sep=""))
}


####################################
createPrjFile <- function(msk, variable="prec", nsurf=0, gridfiles=c("longitude.asc","latitude.asc","altitude.asc")) {
  #Opening the file in writing mode
  fw <- file(paste(variable, "prj.cmd", sep=""), open="w")
  
  #Creating surface list
  if (length(nsurf) != 1) {
    for (a in 1:length(nsurf)) {
      if (a == 1) {os <- paste(nsurf[a], sep="")} else {os <- paste(os, " ", nsurf[a], sep="")}
    }
  }
  
  #Writing all parameters
  cat(variable, ".sur\n", sep="", file=fw) #Surface coefficients file
  
  if (length(nsurf) != 1) {
    cat(os, "\n", sep="", file=fw) #Surface members to calculate
  } else {
    cat(nsurf, "\n", sep="", file=fw) #Surface members to calculate
  }
  cat("1\n", sep="", file=fw) #Type of calculation (0 for summary, 1 for actual values)
  cat("\n", sep="", file=fw) #Error covariance file (blank, as i do not want it)
  cat("\n", sep="", file=fw) #Maximum standard error to be calculated (not provided so blank)
  cat("1\n", sep="", file=fw) #Grid point position (0 for corners, 1 for centres)
  cat("0\n", sep="", file=fw) #Variable that increments in a row (longitude) (0 as i will provide file)
  cat(msk@extent@xmin, " ", (msk@extent@xmin+ncol(msk)*res(msk)[2]), " ", res(msk)[2], "\n", sep="", file=fw) #Limits and resolution of latitude
  cat("0\n", sep="", file=fw) #Variable that increments in a column (latitude) (0 as i will provide file)
  cat(msk@extent@ymin, " ", (msk@extent@ymin+nrow(msk)*res(msk)[1]), " ", res(msk)[1], "\n", sep="", file=fw) #Limits and resolution of longitude
  cat("0\n", sep="", file=fw) #Mode of mask grid (not supplied so 0)
  cat("2\n", sep="", file=fw) #Mode of first independent grid (2 means ESRI AAII)
  cat(gridfiles[1], "\n", sep="", file=fw) #Name of independent variable (longitude)
  cat("2\n", sep="", file=fw) #Mode of second independent grid (2 means ESRI AAII)
  cat(gridfiles[2],"\n", sep="", file=fw) #Name of independent variable (latitude)
  cat("2\n", sep="", file=fw) #Mode of third independent grid (2 means ESRI AAII)
  cat(gridfiles[3], "\n", sep="", file=fw) #Name of third independent variable (elevation)
  cat("2\n", sep="", file=fw) #Mode of output surface grids
  cat("-999\n", sep="", file=fw) #No data value in output grid
  
  #Looping output filenames
  #fix for zero
  if (length(nsurf) == 1 & nsurf == 0) {
    for (srf in 1:12) {
      cat(variable, "_", srf, ".asc\n", sep="", file=fw) #Output file name
    }
  } else {
    for (srf in nsurf) {
      cat(variable, "_", srf, ".asc\n", sep="", file=fw) #Output file name
    }
  }
  cat("(",ncol(msk), "f", 10.3, ")\n", sep="", file=fw) #Output file format
  
  #Last line
  cat("\n",file=fw)
  
  #Closing the file
  close(fw)
}


#################################################################################
#Function to process all years and get the values
getAvailableWS <- function(re,gdir,hdir,cdir,iniyr,finyr,outEval) {
  for (ye in iniyr:finyr) {
    cat("\nProcessing year",ye,"\n")
    #loading the input data
    goData <- read.csv(paste(gdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
    goData$USAF <- NULL; goData$WBAN <- NULL #remove extra fields
    if (nrow(goData)>0) {
      goData$SOURCE <- "GSOD"
    } else {
      goData <- cbind(goData,SOURCE=logical(0))
    }
    
    ghData <- read.csv(paste(hdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
    if (nrow(ghData)>0) {
      ghData$SOURCE <- "GHCN"
    } else {
      ghData <- cbind(ghData,SOURCE=logical(0))
    }
    
    if (file.exists(paste(cdir,"/grouped_output-",re,"/",ye,".csv",sep=""))) {
      ciData <- read.csv(paste(cdir,"/grouped_output-",re,"/",ye,".csv",sep=""))
      ciData$SOURCE <- "CIAT"
      #merge the three datasets
      gaData <- rbind(goData,ghData,ciData)
    } else {
      #merge only gsod and ghcn
      gaData <- rbind(goData,ghData)
    }
    
    nd <- leap(ye) #check whether leap year so to remove day 366 if needed
    if (nd==365) {gaData$X366 <- NULL}
    
    for (day in 1:nd) {
      cat(day," ")
      intMx <- gaData[,c(1:4,4+day)] #get data for that day
      names(intMx) <- c("ID","LON","LAT","ALT","VALS") #rename matrix
      intMx <- intMx[which(!is.na(intMx$VALS)),] #remove all rows with NAs in rainfall
      
      #count of 0.5 degree gridcell station presence
      rs <- raster(ncol=720,nrow=360); rs[] <- 1:ncell(rs)
      intMx$cells <- cellFromXY(rs,cbind(intMx$LON,intMx$LAT))
      uCells <- unique(intMx$cells)
      
      uStat <- nrow(intMx)
      outRow <- data.frame(YEAR=ye,DAY=day,WST_TOTAL=uStat,WST_CELLS=length(uCells))
      
      if (day==1) {
        outAll <- outRow
      } else {
        outAll <- rbind(outAll,outRow)
      }
    }
    cat("\n")
    
    if (ye==1960) {
      outYearAll <- outAll
    } else {
      outYearAll <- rbind(outYearAll,outAll)
    }
  }
  write.csv(outYearAll,paste(outEval,"/all_years-availability-",re,".csv",sep=""),row.names=F,quote=F)
  return(paste(outEval,"/all_years-availability-",re,".csv",sep=""))
}


##############################################################################
##############################################################################
#function to get r2 and rmse of the interpolated data vs. cru, once analysed
getAllMetrics <- function(region,iniyr,finyr,ourEval) {
  #set up region and output folder
  if (region=="eaf" | region=="waf") {rgn <- "afr"} else {rgn <- "sas"}
  
  #loop through years
  for (yr in iniyr:finyr) {
    cat("year", yr,"\n")
    yrDir <- paste("./",yr,"-",rgn,"-eval",sep="")
    
    #load metrics
    mets <- read.csv(paste(yrDir,"/metrics-",region,".csv",sep=""))
    rawd <- read.csv(paste(yrDir,"/raw_eval_data-",region,".csv",sep=""))
    
    rawd <- rawd[which(!is.na(rawd$CRU_RAIN)),]
    rawd <- rawd[which(!is.na(rawd$INT_RAIN)),]
    
    #calculate origin-fixed correlation
    fit.f <- lm(rawd$CRU_RAIN ~ rawd$INT_RAIN - 1) #Fit forced to origin
    rsq.f <- summary(fit.f)$r.squared
    pval.f <- pf(summary(fit.f)$fstatistic[1],summary(fit.f)$fstatistic[2],summary(fit.f)$fstatistic[3],lower.tail=F)
    
    #calculate RMSE
    rawd$SQDIFF <- (rawd$CRU_RAIN-rawd$INT_RAIN)^2
    rmse <- sqrt(sum(rawd$SQDIFF)/nrow(rawd))
    
    outRow <- data.frame(YEAR=yr,RMSE=rmse,RSQ.FORCED=rsq.f,PVAL.FORCED=pval.f)
    
    if (yr==iniyr) {
      outAll <- outRow
    } else {
      outAll <- rbind(outAll,outRow)
    }
  }
  
  write.csv(outAll,paste(outEval,"/all_years-metrics-",region,".csv",sep=""),row.names=F,quote=F)
  return(paste(outEval,"/all_years-metrics-",region,".csv",sep=""))
}

