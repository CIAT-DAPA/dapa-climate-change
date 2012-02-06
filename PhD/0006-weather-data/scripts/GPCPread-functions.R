#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL

#function to create the ascii files corresponding to the netcdf file
createASCII <- function(filename,aiDir,ncDir) {
  fname <- substr(filename,1,(nchar(filename)-3))
  yr <- as.numeric(substr(strsplit(filename,".",fixed=T)[[1]][3],1,4))
  mth <- as.numeric(substr(strsplit(filename,".",fixed=T)[[1]][3],5,6))
  
  #leap year and number of days
  nd <- leap(as.numeric(yr))
  if (nd==366) {
    mthDays <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    mthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  doyMx <- data.frame(DOY=1:nd,MTH=rep(1:12,times=mthDays),
                      DOM=c(1:mthDays[1],1:mthDays[2],1:mthDays[3],1:mthDays[4],1:mthDays[5],
                            1:mthDays[6],1:mthDays[7],1:mthDays[8],1:mthDays[9],1:mthDays[10],
                            1:mthDays[11],1:mthDays[12]))
  
  #create year dir
  yrDir <- paste(aiDir,"/",yr,sep="")
  if (!file.exists(yrDir)) {dir.create(yrDir)}
  
  for (i in 1:mthDays[mth]) {
    cat("Processing",i)
    doy <- doyMx$DOY[which(doyMx$MTH==mth & doyMx$DOM==i)]
    orname <- paste(yrDir,"/rain-",doy,".asc",sep="")
    if (!file.exists(orname)) {
      rs <- raster(paste(ncDir,"/",fname,".nc",sep=""),varname="precip",band=i)
      rs <- rotate(rs)
      rs <- writeRaster(rs,orname,format="ascii")
      cat(" done\n")
    } else {
      cat(" already exists\n")
    }
  }
  return(yrDir)
}

#function to convert the grads file to NetCDF
gradsToNC <- function(filename,grDir,ncDir,softPath) {
  #copy gunzip the file
  x <- file.copy(paste(grDir,"/",filename,sep=""),filename)
  system(paste("7za x",filename),show.output.on.console=F)
  fname <- substr(filename,1,(nchar(filename)-3))
  
  #create control file and tcl script
  x <- ctlCreate(fname,softPath)
  x <- tclCreate(fname,softPath)
  
  #run the script
  setwd(softPath)
  ff <- file("run.bat","w")
  cat("convsh1.91.exe < ",paste(fname,".tcl",sep=""),"\n",file=ff)
  close(ff)
  system("run.bat")
  
  #erase the temporary stuff and copy the netcdf file to the ncDir
  x <- file.copy(paste(fname,".nc",sep=""),paste(ncDir,"/",fname,".nc",sep=""))
  x <- file.remove(list.files(pattern=fname))
  return(paste(fname,".nc",sep=""))
}


#create the tcl script
tclCreate <- function(filename,softPath) {
  setwd(softPath)
  ff <- file(paste(filename,".tcl",sep=""),"w")
  cat("readfile 0 ",paste(filename,".ctl",sep=""),"\n",sep="",file=ff)
  cat("writefile netcdf ",paste(filename,".nc",sep=""),"0\n",file=ff)
  cat("clearall\n",file=ff)
  close(ff)
  return(paste(filename,".tcl",sep=""))
}


#function to create the .ctl file
ctlCreate <- function(filename,pDir) {
  setwd(pDir)
  yr <- substr(strsplit(filename,".",fixed=T)[[1]][3],1,4)
  mth <- substr(strsplit(filename,".",fixed=T)[[1]][3],5,6)
  mthName <- month.abb[as.numeric(mth)]
  
  #leap year and number of days
  nd <- leap(as.numeric(yr))
  if (nd==366) {
    mthDays <- c(31,29,31,30,31,30,31,31,30,31,30,31)
  } else {
    mthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  }
  
  #write the file
  ff <- file(paste(filename,".ctl",sep=""),"w")
  cat("DSET ^",filename,"\n",sep="",file=ff)
  cat("options yrev byteswapped\n",file=ff)
  cat("UNDEF -99999.0\n",file=ff)
  cat("FILEHEADER 1440\n",file=ff)
  cat("TITLE GPCP One-Degree Daily Precipitation\n",file=ff)
  cat("XDEF  360 LINEAR 0.5 1\n",file=ff)
  cat("YDEF  180 LINEAR -89.5 1\n",file=ff)
  cat("TDEF ",mthDays[as.numeric(mth)]," LINEAR 01",mthName,as.numeric(yr)," 1dy\n",sep="",file=ff)
  cat("VARS 1\n",file=ff)
  cat("precip  0 99 GPCP 1DD Precipitation (mm/day)\n",file=ff)
  cat("ENDVARS\n",file=ff)
  close(ff)
  return(paste(filename,".ctl",sep=""))
}

