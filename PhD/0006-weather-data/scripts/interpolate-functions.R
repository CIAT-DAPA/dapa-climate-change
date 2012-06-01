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
interpolateDay <- function(day,gData,intDir,yr,rg) {
  
  #defining output directories
  oyrDir <- paste(intDir,"/",yr,"-",rg,sep=""); if (!file.exists(oyrDir)) {dir.create(oyrDir)}
  oDir <- paste(oyrDir,"/",day,sep=""); if (!file.exists(oDir)) {dir.create(oDir)}
  setwd(oDir)
  
  if (!file.exists("rain_waf.asc") & rg == "afr" | !file.exists("rain_igp.asc") & rg == "sas") {
    intMx <- gData[,c(1:4,4+day)] #get data for that day
    intMx$SOURCE <- gData$SOURCE
    names(intMx) <- c("ID","LON","LAT","ALT","RAIN","SOURCE") #rename data frame fields
    intMx <- intMx[which(!is.na(intMx$RAIN)),] #remove all rows with NAs in rainfall
    intMx$RAIN <- intMx$RAIN*10
    
    #reduce number of stations (one per 1 degree cell)
    rs <- raster(ncol=720,nrow=360); rs[] <- 1:ncell(rs)
    intMx$cells <- cellFromXY(rs,cbind(intMx$LON,intMx$LAT))
    uCells <- unique(intMx$cells)
    
    if (nrow(intMx)!= 0) {
      for (cell in uCells) {
        selCells <- intMx[which(intMx$cells==cell),]
        #remove anything that is GSOD ONLY if there is either GHCN or CIAT data in that cell
        uSrc <- unique(selCells$SOURCE)
        if (length(uSrc)>1 & "GSOD" %in% uSrc) {
          selCells <- selCells[which(selCells$SOURCE != "GSOD"),]
        }
        
        selCells$SOURCE <- NULL
        
        if (nrow(selCells)>1) { #if there are many stations in that cell then average the locations
          outLine <- data.frame(ID=paste("MNCELL",cell,sep=""),
                                LON=mean(selCells$LON,na.rm=T),LAT=mean(selCells$LAT,na.rm=T),
                                ALT=mean(selCells$ALT,na.rm=T),RAIN=mean(selCells$RAIN,na.rm=T))
        } else {
          outLine <- selCells; outLine$cells <- NULL
        }
        
        if (cell==uCells[1]) { #rbinding
          outIntMx <- outLine
        } else {
          outIntMx <- rbind(outIntMx,outLine)
        }
      }
    } else {
      outIntMx <- intMx
    }
    
    #check if there is any record with missing altitude
    altNA <- which(is.na(outIntMx$ALT))
    if (length(altNA)>0) {
      elev <- raster(paste(intDir,"/0_files/alt-glo.asc",sep=""))
      altVals <- extract(elev,cbind(X=outIntMx$LON[altNA],Y=outIntMx$LAT[altNA]))
      outIntMx$ALT[altNA] <- altVals
    }
    
    #writing spline training file
    oDatFile <- paste(oDir,"/training.dat",sep="")
    writeDat(outIntMx,filename=oDatFile)
    if (nrow(outIntMx)>10) {
      #creating run file and running
      createRunFile(intMx, filename="training.dat", variable='rain',
                    varUnits='millimetres', ivars=3, icovars=0, sivars=0, sicovars=0, sp.order=2, nsurf=1)
      fw <- file("rain_fit.bat", open="w") #Running
      cat(anuDir, "/splina < rainfit.cmd > rainfit.log\n", sep="", file=fw)
      close(fw)
      system("rain_fit.bat")
      
      if (!file.exists("rain.sur")) { #if failed then remove one of the zeros (for some reason this fixes it)
        zeros <- which(outIntMx$RAIN==0)
        randSel <- sample(zeros,1)
        writeDat(outIntMx[-randSel,],filename=oDatFile)
        system("rain_fit.bat")
        writeDat(outIntMx,filename=oDatFile) #re-writing the correct stations file
      }
      
      if (rg=='afr') { #projecting
        #East Africa
        alt <- raster(paste(intDir,"/0_files/alt-prj-eaf.asc",sep=""))
        gFiles <- c(paste(intDir,"/0_files/lon-prj-eaf.asc",sep=""),
                    paste(intDir,"/0_files/lat-prj-eaf.asc",sep=""),
                    paste(intDir,"/0_files/alt-prj-eaf.asc",sep=""))
        createPrjFile(alt, variable="rain", nsurf=1, gridfiles=gFiles)
        fw <- file("rain_prj_eaf.bat", open="w")
        cat(paste(anuDir, "/lapgrd < rainprj.cmd > rainprj.log\n",sep=""), file=fw)
        close(fw)
        system("rain_prj_eaf.bat")
        rs <- raster("rain_1.asc")
        rs <- rs/10; rs <- writeRaster(rs,"rain_eaf.asc",format='ascii',overwrite=T)
        fr <- file.remove("rain_1.asc")
        #West Africa
        alt <- raster(paste(intDir,"/0_files/alt-prj-waf.asc",sep=""))
        gFiles <- c(paste(intDir,"/0_files/lon-prj-waf.asc",sep=""),
                    paste(intDir,"/0_files/lat-prj-waf.asc",sep=""),
                    paste(intDir,"/0_files/alt-prj-waf.asc",sep=""))
        createPrjFile(alt, variable="rain", nsurf=1, gridfiles=gFiles)
        fw <- file("rain_prj_waf.bat", open="w")
        cat(paste(anuDir, "/lapgrd < rainprj.cmd > rainprj.log\n",sep=""), file=fw)
        close(fw)
        system("rain_prj_waf.bat")
        rs <- raster("rain_1.asc")
        rs <- rs/10; rs <- writeRaster(rs,"rain_waf.asc",format='ascii',overwrite=T)
        fr <- file.remove("rain_1.asc")
      } else if (rg=='sas') {
        #igp
        alt <- raster(paste(intDir,"/0_files/alt-prj-igp.asc",sep=""))
        gFiles <- c(paste(intDir,"/0_files/lon-prj-igp.asc",sep=""),
                    paste(intDir,"/0_files/lat-prj-igp.asc",sep=""),
                    paste(intDir,"/0_files/alt-prj-igp.asc",sep=""))
        createPrjFile(alt, variable="rain", nsurf=1, gridfiles=gFiles)
        fw <- file("rain_prj_igp.bat", open="w")
        cat(paste(anuDir, "/lapgrd < rainprj.cmd > rainprj.log\n",sep=""), file=fw)
        close(fw)
        system("rain_prj_igp.bat")
        rs <- raster("rain_1.asc")
        rs <- rs/10; rs <- writeRaster(rs,"rain_igp.asc",format='ascii')
        fr <- file.remove("rain_1.asc")
      }
    } else {
      cat("Too few data points \n")
      fw <- file("process.fail", open="w") #Running
      cat("Process was failed because there were too few data points \n", file=fw)
      close(fw)
    }
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
    formatC(stData$RAIN,width=9,digits=2,format="f"),
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
