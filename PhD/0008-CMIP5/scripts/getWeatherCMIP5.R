#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP5 weather data
library(raster)

src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#GCM data dir
mdDir <- "H:/CMIP5/baseline"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"

#outdir
bDir <- "F:/PhD-work/crop-modelling/climate-data"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data"
oDir <- paste(bDir,"/CMIP5",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#initial and final years
yi <- 1960
yf <- 2005

#load GCM characteristics
cChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
gcmList <- unique(cChars$GCM)
gcm <- gcmList[1]

outGCMDir <- paste(oDir,"/",gcm,sep="")
if (!file.exists(outGCMDir)) {dir.create(outGCMDir)}

#reduce characteristics list for this GCM
thisGCM <- cChars[which(cChars$GCM == gcm),]
ensList <- unique(thisGCM$Ensemble)

for (ens in ensList) {
  cat("Processing ensemble",paste(ens),"\n")
  #ens <- ensList[1]
  thisEns <- thisGCM[which(thisGCM$Ensemble == ens),]
  #year <- 1960
  
  #create directory of ensemble
  outEnsDir <- paste(oDir,"/",ens,sep="")
  if (!file.exists(outEnsDir)) {dir.create(outEnsDir)}
  
  #loop through variables
  for (vn in c("pr","tasmin","tasmax")) {
    cat("variable:",vn,"\n")
    #vn <- "pr" #tasmin, tasmax
    
    for (year in yi:yf) {
      cat("year:",year,"\n")
      fName <- paste(thisEns$naming[which(year >= thisEns$iYear & year <= thisEns$fYear)])
      fName <- gsub("%var%",vn,fName)
      
      #if there are two files i need to read both, extract the first 11 months from the first 
      #one and the last month from the second file. As this only happens with UKMO models
      #i did not care about making it generic (i.e. it works only for a 360 day calendar)
      if (length(fName) > 1) {
        oFile <- gsub("\\.nc",paste("_",year,".csv",sep=""),fName)[1]
        oFile <- paste(outGCMDir,"/",oFile,sep="")
        
        
      }
      
    }
    
  }
  
}














