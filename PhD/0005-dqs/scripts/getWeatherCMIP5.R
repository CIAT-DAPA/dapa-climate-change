#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP5 weather data
library(raster)

src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

yi <- 1966
yf <- 1989

bDir <- "F:/PhD-work/data-quality-study"
#bDir <- "~/PhD-work/data-quality-study"
compDir <- paste(bDir,"/climate-comparison",sep="")
source(paste(compDir,"/CMIP5-functions.R",sep=""))
mdDir <- "H:/CMIP5/baseline"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"

cChars <- read.table(paste(compDir,"/0_input_data/CMIP5gcms.tab",sep=""),sep="\t",header=T)

oDir <- paste(compDir,"/CMIP5_GJ",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#gcm data extraction wrapper
gcm_wrapper <- function(i) {
  #get list of GCMs and selected GCMs
  gcmList <- unique(cChars$GCM)
  #gcm <- gcmList[13]
  gcm <- gcmList[i]
  
  #create GCM output dir
  outGCMDir <- paste(oDir,"/",gcm,sep="")
  if (!file.exists(outGCMDir)) {dir.create(outGCMDir)}
  
  #reduce characteristics list for this GCM
  thisGCM <- cChars[which(cChars$GCM == gcm),]
  ensList <- unique(thisGCM$Ensemble)
  
  #loop through ensemble members
  for (ens in ensList) {
    cat("Processing ensemble",paste(ens),"\n")
    #ens <- ensList[1]
    thisEns <- thisGCM[which(thisGCM$Ensemble == ens),]
    #year <- 1966
    for (vn in c("pr","tasmin","tasmax")) {
      #vn <- "pr" #tasmin, tasmax
      for (year in yi:yf) {
        fName <- paste(thisEns$naming[which(year > thisEns$iYear & year < thisEns$fYear)])
        fName <- gsub("%var%",vn,fName)
        iyr <- thisEns$iYear[which(year > thisEns$iYear & year < thisEns$fYear)]
        imt <- thisEns$iMonth[which(year > thisEns$iYear & year < thisEns$fYear)]
        idy <- thisEns$iDay[which(year > thisEns$iYear & year < thisEns$fYear)]
        wlp <- paste(thisEns$has_leap[which(year > thisEns$iYear & year < thisEns$fYear)])
        
        gFile <- paste(mdDir,"/",gcm,"/",ens,"/",fName,sep="")
        
        #get the indian extent
        xt <- extent(raster(paste(compDir,"/0_input_data/mask.asc",sep="")))
        
        #create 2.5x2.5 dummy raster
        nc <- (xt@xmax-xt@xmin)/2.5
        nr <- (xt@ymax-xt@ymin)/2.5
        xt@ymin <- xt@ymax - round(nr+0.5,0)*2.5
        nr <- (xt@ymax-xt@ymin)/2.5
        
        dumm_rs <- raster(xt,ncol=nc,nrow=nr)
        dumm_rs[] <- 1
        
        daily_data <- extractFromGCM(yr=year,gcmFile=gFile,iYear=iyr,iMth=imt,
                                    iDay=idy,wLeap=wlp,varName=vn,msk=dumm_rs,
                                    x=68.75,y=22.75,ccDir=compDir)
        
        dg <- createDateGridCMIP5(year=year,whatLeap=wlp)
        dg$VALUES <- daily_data
        names(dg)[7] <- vn
        
        oFile <- gsub("\\.nc",paste("_",year,".csv",sep=""),fName)
        oFile <- paste(outGCMDir,"/",oFile,sep="")
        write.csv(dg,oFile,row.names=F,quote=T)
      }
    }
  }
}

