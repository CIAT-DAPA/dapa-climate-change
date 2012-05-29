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
#yi <- 1961
#yf <- 2005
#i <- 1 #gcm to process

#source(paste(src.dir2,"/scripts/CDO-process_CMIP5",sep=""))

#sourcing needed functions
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#GCM data dir
oDir <- paste(bDir,"/CMIP5/baseline",sep="")
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}

#initial and final years
#yi <- 1961
#yf <- 2005

#load GCM characteristics
cChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
gcmList <- unique(cChars$GCM)
gcm <- gcmList[i]

outGCMDir <- paste(oDir,"/",gcm,sep="")
if (!file.exists(outGCMDir)) {dir.create(outGCMDir)}

#reduce characteristics list for this GCM
thisGCM <- cChars[which(cChars$GCM == gcm),]
ensList <- unique(thisGCM$Ensemble)

for (ens in ensList) {
  #ens <- ensList[1]
  cat("Processing ensemble",paste(ens),"\n")
  thisEns <- thisGCM[which(thisGCM$Ensemble == ens),]
  
  #create directory of ensemble
  outEnsDir <- paste(outGCMDir,"/",ens,sep="")
  if (!file.exists(outEnsDir)) {dir.create(outEnsDir)}
  
  #list of variables depends on number of nc files (i.e. tas is not always available)
  patn <- gsub("%var%","",thisEns$naming[1])
  ncf <- list.files(outEnsDir,pattern=patn)
  if (length(ncf) == 4) {
    vnList <- c("pr","tasmin","tasmax","tas")
  } else if (length(ncf) == 3) {
    vnList <- c("pr","tasmin","tasmax")
  } else {
    stop("number of files not 3 or 4, check!")
  }
  
  #loop through variables
  for (vn in vnList) {
    #vn <- "pr" #tasmin, tasmax
    cat("variable:",vn,"\n")
    
    #here you need to dump the memory working space /dev/shm
    setwd("/dev/shm")
    system(paste("rm -f *"))
    setwd(bDir)
    
    #which is the control file
    conFile <- paste(outEnsDir,"/",vn,"_",gcm,"_",ens,".control",sep="") #gsub("\\.nc",paste("_",year,".control",sep=""),fName)
    
    #if the control file does not exist
    if (!file.exists(conFile)) {
    #loop through years
      for (year in yi:yf) {
        #year <- 1960
        cat("year:",year,"\n")
        fName <- paste(thisEns$naming[which(year >= thisEns$iYear & year <= thisEns$fYear)])
        fName <- gsub("%var%",vn,fName)
        
        #leap condition
        whatLeap <- paste(thisEns$has_leap[which(year >= thisEns$iYear & year <= thisEns$fYear)][1])
        
        #create output year dir
        outYearDir <- paste(outEnsDir,"/",vn,"_",year,sep="")
        if (!file.exists(outYearDir)) {dir.create(outYearDir)}
        
        #if there are two files i need to read both, extract the first 11 months from the first 
        #one and the last month from the second file. As this only happens with UKMO models
        #i did not care about making it generic (i.e. it works only for a 360 day calendar)
        #although the above might not apply to the process done in this script
        if (length(fName) > 1) {
          
          #check if year was already done
          nfil <- length(list.files(outYearDir,pattern="\\.nc"))
          nd <- leap(year)
          if (whatLeap=="all30") {
            nd <- 360
          } else if (whatLeap == "no") {
            nd <- 365
          }
          
          if (nfil != nd) {
            
            #first part of the year
            ncFile <- paste(mdDir,"/",gcm,"/",ens,"/",fName,sep="")[1]
            
            #copy to /dev/shm if it does not exist already
            if (!file.exists(paste("/dev/shm/",fName[1],sep=""))) {
              setwd("/dev/shm")
              system(paste("rm -f *"))
              setwd(bDir)
              x <- file.copy(ncFile,"/dev/shm")
            }
            
            #select the year I'm looking for using the CDO command "selyear"
            setwd("/dev/shm")
            tFile <- paste(gcm,"_",ens,"_",year,".nc",sep="")
            system(paste("cdo selyear,",year," ",fName[1]," ",tFile,sep=""))
            
            #now use splitmon to split the months, and remove tFile
            mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
            system(paste("cdo splitmon ",tFile," ",mon_px,sep=""))
            x <- file.remove(tFile)
            
            #second part of the year
            ncFile <- paste(mdDir,"/",gcm,"/",ens,"/",fName,sep="")[2]
            
            #copy to /dev/shm if it does not exist already
            if (!file.exists(paste("/dev/shm/",fName[2],sep=""))) {
              x <- file.copy(ncFile,"/dev/shm")
            }
            
            #select the year I'm looking for using the CDO command "selyear"
            setwd("/dev/shm")
            tFile <- paste(gcm,"_",ens,"_",year,".nc",sep="")
            system(paste("cdo selyear,",year," ",fName[2]," ",tFile,sep=""))
            
            #now use splitmon to split the months, and remove tFile
            mon_px <- paste(gcm,"_",ens,"_",year,"_mth_",sep="")
            system(paste("cdo splitmon ",tFile," ",mon_px,sep=""))
            x <- file.remove(tFile)
            
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
            
          } else {
            cat("this year was already done!\n")
          }
          
        } else {
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
          } else {
            cat("this year was already done!\n")
          }
          
        }
        
      }
    #write control file
    cfo <- file(conFile,"w")
    cat("processed on",date(),"by",paste(as.data.frame(t(Sys.info()))$login),"@",
        paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=cfo)
    close(cfo)
    } else {
      cat("this job was already done!\n")
    }
  }
  
  #removing ensemble original big files only if the number of control files equals
  #the number of original nc files, else stop
  setwd(outEnsDir)
  cnc <- length(list.files(".",pattern="\\.nc"))
  cct <- length(list.files(".",pattern="\\.control"))
  if (cnc == cct) {
    system("rm -f *.nc")
  } else {
    stop("something weird happened, need to check before removing original files")
  }
  
}














