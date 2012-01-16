#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL

#load libraries
library(raster)

#function to interpolate a given day
interpolateDay <- function(day,gData,intDir,yr,rg) {
  
  #defining output directories
  oyrDir <- paste(intDir,"/",yr,"-",rg,sep=""); if (!file.exists(oyrDir)) {dir.create(oyrDir)}
  oDir <- paste(oyrDir,"/",day,sep=""); if (!file.exists(oDir)) {dir.create(oDir)}
  setwd(oDir)
  
  if (!file.exists("rain_waf.asc")) {
    intMx <- gData[,c(1:4,4+day)] #get data for that day
    names(intMx) <- c("ID","LON","LAT","ALT","RAIN") #rename matrix
    intMx <- intMx[which(!is.na(intMx$RAIN)),] #remove all rows with NAs
    intMx$RAIN <- intMx$RAIN*10
    
    #reduce number of stations (one per 1 degree cell)
    rs <- raster(ncol=720,nrow=360); rs[] <- 1:ncell(rs)
    intMx$cells <- cellFromXY(rs,cbind(intMx$LON,intMx$LAT))
    uCells <- unique(intMx$cells)
    
    for (cell in uCells) {
      selCells <- intMx[which(intMx$cells==cell),]
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
    
    #writing spline training file
    oDatFile <- paste(oDir,"/training.dat",sep="")
    writeDat(outIntMx,filename=oDatFile)
    
    #creating run file and running
    createRunFile(intMx, filename="training.dat", variable='rain',
                  varUnits='millimetres', ivars=3, icovars=0, sivars=0, sicovars=0, sp.order=2, nsurf=1)
    fw <- file("rain_fit.bat", open="w") #Running
    cat(anuDir, "/splina < rainfit.cmd > rainfit.log\n", sep="", file=fw)
    close(fw)
    system("rain_fit.bat")
    
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
  cat(min(inData$ALT), " ", max(inData$ALT), " ", 1, " ", 1, "\n", sep="",file=fw) #Independent variable 3 limits transf. and unit code
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
  cat("(",ncol(msk), "f", 9.3, ")\n", sep="", file=fw) #Output file format
  
  #Last line
  cat("\n",file=fw)
  
  #Closing the file
  close(fw)
}
