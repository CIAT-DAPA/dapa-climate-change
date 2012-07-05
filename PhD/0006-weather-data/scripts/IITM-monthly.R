#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#sourcing needed functions
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

bd <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
#bd <- "W:/eejarv/PhD-work/crop-modelling"
setwd(bd)

wd <- paste(bd,"/climate-data/IND-TropMet_day",sep="")
od <- paste(bd,"/climate-data/IND-TropMet_mon",sep="")
if (!file.exists(od)) {dir.create(od)}

library(raster); library(rgdal)

iniyr <- 1960
finyr <- 2008


for (yr in iniyr:finyr) {
  #yr <- c(iniyr:finyr)[1]
  cat("\nprocessing year",yr,"...\n")
  
  #output year dir
  oyr_dir <- paste(od,"/",yr,sep="")
  if (!file.exists(oyr_dir)) {dir.create(oyr_dir)}
  
  #create calendar
  dg <- createDateGrid(yr)
  dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
  dg$DAY <- as.numeric(substr(dg$MTH.DAY,5,6))
  
  for (m in 1:12) {
    #m <- 1
    cat(m,". ",sep="")
    
    #days in this month
    days_m <- dg$JD[which(dg$MTH == m)]
    ors_name <- paste(oyr_dir,"/rain-",m,".tif",sep="")
    
    if (!file.exists(ors_name)) {
      stk <- stack(paste(wd,"/",yr,"/rain-",days_m,".asc",sep="")) #load rasters
      rtot <- calc(stk,fun= function(x) {sum(x)}) #calculate total of month
      rtot <- writeRaster(rtot,ors_name,format="GTiff") #write output raster
      rm(rtot); rm(stk); g=gc(); rm(g)
    }
  }
  cat("\n")
}









