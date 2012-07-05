#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL
stop("Do not run whole thing")

#process india TropMet 1-deg data

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

wd <- "F:/PhD-work/crop-modelling/climate-data/IND-TropMet_day"
setwd(wd)

library(raster)

iniyr <- 1960
finyr <- 2008

band_counter <- 1
for (ye in iniyr:finyr) {
  cat("\nProcessing year",ye,"starting",band_counter,"\n")
  nd <- leap(ye) #check whether leap year so to remove day 366 if needed
  
  yrDir <- paste(wd,"/",ye,sep="")
  if (!file.exists(yrDir)) {dir.create(yrDir)}
  
  in_rs <- paste(wd,"/0_input_data/india_data.nc",sep="")
  
  #loop through days
  for (i in 1:nd) {
    cat(i," ")
    out_rs <- paste(yrDir,"/rain-",i,".asc",sep="")
    if (!file.exists(out_rs)) {
      rs <- raster(in_rs,varname="IMD_DAILY",band=band_counter)
      rs <- writeRaster(rs,out_rs,format='ascii')
    }
    band_counter <- band_counter+1
  }
  cat("\n")
}


# rs2 <- raster("./../daily-interpolations/1960-sas/1/rain_igp.asc")
# rs2 <- crop(rs2,rs)
# xy <- xyFromCell(rs,which(is.na(rs[])))
# cells <- cellFromXY(rs2,xy)
# rs2[cells] <- NA

