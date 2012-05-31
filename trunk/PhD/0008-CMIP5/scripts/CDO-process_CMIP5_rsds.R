#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP5 weather data
library(raster)

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#bDir <- "/nfs/a102/eejarv"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"
#yi <- 1960
#yf <- 2005
#i <- 1 #file to process

#source(paste(src.dir2,"/scripts/CDO-process_CMIP5_rsds.R",sep=""))

#sourcing needed functions
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#GCM data dir
oDir <- paste(bDir,"/CMIP5/baseline",sep="")
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}

#load GCM characteristics
cChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
srFileList <- unique(cChars$srad_naming)
srFileList <- srFileList[grep("\\.nc",srFileList)]
srFile <- paste(srFileList[i])

#GCM file belongs to
gcm <- paste(cChars$GCM[which(cChars$srad_naming == srFile)][1])
outGCMDir <- paste(oDir,"/",gcm,sep="")

#ensemble file belongs to
ens <- paste(cChars$Ensemble[which(cChars$srad_naming == srFile)][1])
outEnsDir <- paste(outGCMDir,"/",ens,sep="") #directory of ensemble

#here disaggregate the name of the file
all_fs <- unlist(strsplit(srFile,"_",fixed=T))
step <- all_fs[2]
period <- all_fs[6]; period <- gsub("\\.nc","",period)
vn <- all_fs[1]

iperiod <- unlist(strsplit(period,"-",fixed=T))[1]
fperiod <- unlist(strsplit(period,"-",fixed=T))[2]

yini <- as.numeric(substr(iperiod,1,4))
yend <- as.numeric(substr(fperiod,1,4))

#here you need to dump the memory working space /dev/shm
setwd("/dev/shm")
system(paste("rm -f *"))
setwd(bDir)

#which is the control file
conFile <- paste(outEnsDir,"/",gsub("\\.nc","\\.control",srFile),sep="")
#conFile <- paste(outEnsDir,"/",vn,"_",gcm,"_",ens,".control",sep="")

#if the control file does not exist
if (!file.exists(conFile)) {
  #year series should be the intersection between the 
  yr_srs <- (yini:yend)[(yini:yend) %in% (yi:yf)]
  
  #loop through years
  for (year in yr_srs) {
    #year <- yr_srs[1]
    cat("year:",year,"\n")
    fName <- srFile
    
    #leap condition
    whatLeap <- paste(cChars$has_leap[which(cChars$srad_naming == srFile)][1])
    
    #create output year dir
    outYearDir <- paste(outEnsDir,"/",vn,"_",year,sep="")
    if (!file.exists(outYearDir)) {dir.create(outYearDir)}
    
    #there was only one file with the year data that i need, so just do the process for that one
    ### this is when the year is not split into two different files
    
    #check if year was already done
    nfil <- length(list.files(outYearDir,pattern="\\.nc"))
    nd <- leap(year)
    if (whatLeap=="all30") {
      nd <- 360
    } else if (whatLeap == "no") {
      nd <- 365
    }
    
    if (tolower(step) == "amon") {nd <- 12}
    
    if (nfil != nd) {
      #name of netCDF file
      ncFile <- paste(mdDir,"/",gcm,"/",ens,"/",fName,sep="")
      
      #copy to /dev/shm if it does not exist already
      if (!file.exists(paste("/dev/shm/",fName,sep=""))) {
        setwd("/dev/shm")
        system(paste("rm -f *"))
        setwd(bDir)
        x <- file.copy(ncFile,"/dev/shm")
      }
      
      #select the year I'm looking for using the CDO command "selyear"
      setwd("/dev/shm")
      tFile <- paste(gcm,"_",ens,"_",year,".nc",sep="")
      system(paste("cdo selyear,",year," ",fName," ",tFile,sep=""))
      
      #now use splitmon to split the months, and remove tFile
      mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
      system(paste("cdo splitmon ",tFile," ",mon_px,sep=""))
      x <- file.remove(tFile)
      
      if (tolower(step) == "amon") {
        #move the monthly files to the output folder
        system(paste("mv -f *_mth_* ",outYearDir,sep=""))
      } else {
        #now loop the monthly files
        mFiles <- list.files(".",pattern="_mth_")
        for (mf in mFiles) {
          #split the monthly file into daily files and remove monthly file
          day_px <- paste(gsub("\\.nc","",mf),"_day_",sep="")
          system(paste("cdo splitday ",mf," ",day_px,sep=""))
          x <- file.remove(mf)
        }
        #move the daily files to the output folder
        system(paste("mv -f *_mth_* ",outYearDir,sep=""))
      }
    } else {
      cat("this year was already done!\n")
    }
  }
  #write control file
  cfo <- file(conFile,"w")
  cat("processed on",date(),"by",paste(as.data.frame(t(Sys.info()))$login),"@",
      paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cfo)
  close(cfo)
  
} else {
  ("this job was already done!\n")
}


if (file.exists(conFile) & file.exists(paste(outEnsDir,"/",srFile,sep=""))) {
  setwd(outEnsDir)
  x <- file.remove(srFile)
}














