#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("Error: do not run whole thing \n")

library(raster)
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts/"
#src.dir <- "/home/jramirez/dapa-climate-change/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/interpolate-functions.R",sep=""))
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

#folders and locations
bd <- "F:/PhD-work/crop-modelling/climate-data"
#bd <- "/andromeda_data1/jramirez/crop-modelling/climate-data"

#Determine if leap year or not (calculate number of days)
ndm <- ts(diff(seq(as.Date("1960-01-01"), as.Date("2010-01-01"), by = "month")), 
               start = c(1960, 01), freq = 12) #get days in each month of time series
ndm <- data.frame(matrix(ndm,ncol=12,byrow=T))
row.names(ndm) <- c(1960:2009)
names(ndm) <- month.abb

#set up region and output folder
region <- "eaf"
if (region=="eaf" | region=="waf") {rgn <- "afr"} else {rgn <- "sas"}

#load altitude raster to get the xy coordinates of the points to validate
alt_rs <- raster(paste(bd,"/daily-interpolations/0_files/alt-prj-",region,".asc",sep=""))
xy <- xyFromCell(alt_rs,which(!is.na(alt_rs[])))

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=15) #initiate cluster

#export functions
sfExport("leap")
sfExport("extractIntCRU")

#export variables
sfExport("bd")
sfExport("region")
sfExport("rgn")
sfExport("alt_rs")
sfExport("xy")
sfExport("ndm")

#Control function for the comparison
controlCompare <- function(ye) {
  library(raster)
  
  #output folder
  oDir <- paste(bd,"/daily-interpolations/",ye,"-",rgn,"-eval",sep="")
  if (!file.exists(oDir)) {dir.create(oDir)}
  oRawDir <- paste(oDir,"/raw_eval_data",sep="")
  if (!file.exists(oRawDir)) {dir.create(oRawDir)}
  
  #naming output files
  ormx <- paste(oDir,"/raw_eval_data-",region,".csv",sep="")
  omet <- paste(oDir,"/metrics-",region,".csv",sep="")
  ofig <- paste(oDir,"/cru_vs_int-",region,".tif",sep="")
  
  #if metrics file does not exist then repeat year
  if (!file.exists(ofig)) {
    for (i in 1:nrow(xy)) {
      cat("\nProcessing cell",i,"of",nrow(xy),"\n")
      
      #Process or load the cell data
      ormxPre <- paste(oRawDir,"/",region,"-raw_eval_data-cell-",i,".csv",sep="")
      if (!file.exists(ormxPre)) {
        rmx <- extractIntCRU(lon=xy[i,1],lat=xy[i,2],bd,ye,region,ndm)
        rmx$CRU_RAIN <- rmx$CRU_RAIN*0.1
        rmx <- cbind(CELL=i,rmx)
        write.csv(rmx, ormxPre,quote=F,row.names=F)
      } else {
        rmx <- read.csv(ormxPre)
      }
      
      #calculate some type of metric here (correlation, total bias, rmse) and cat everything
      rmse <- (rmx$CRU_RAIN-rmx$INT_RAIN)^2
      rmse <- sqrt(sum(rmse)/12)
      corr <- cor(rmx$INT_RAIN,rmx$CRU_RAIN)
      bias <- sum(rmx$CRU_RAIN-rmx$INT_RAIN)
      resrow <- data.frame(CELL=i,LON=xy[i,1],LAT=xy[i,2],RMSE=rmse,CORR=corr,BIAS=bias)
      
      #concatenating all processed cells
      if (i == 1) {
        resall <- resrow
        rmxall <- rmx
      } else {
        resall <- rbind(resall,resrow)
        rmxall <- rbind(rmxall,rmx)
      }
    }
    #writing the data
    write.csv(rmxall,ormx,quote=F,row.names=F)
    write.csv(resall,omet,quote=F,row.names=F)
    
    #plotting the x-y graph
    lims <- c(0,max(rmxall$CRU_RAIN,rmxall$INT_RAIN,na.rm=T))
    tiff(ofig,compression="lzw",res=150,height=1000,1000)
    plot(rmxall$CRU_RAIN,rmxall$INT_RAIN,xlim=lims,ylim=lims,pch=20,cex=0.8,
         xlab="CRU Rainfall (mm)",ylab="Interpolated Rainfall (mm)")
    abline(0,1)
    grid()
    dev.off()
  } else {
    cat("Year",ye,"already evaluated \n")
  }
}

#run the control function
system.time(sfSapply(as.vector(1960:2009), controlCompare))

#stop the cluster
sfStop()
