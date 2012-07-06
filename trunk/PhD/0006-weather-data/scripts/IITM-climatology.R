#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012


bd <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
#bd <- "W:/eejarv/PhD-work/crop-modelling"

wd <- paste(bd,"/climate-data/IND-TropMet_mon",sep="")
od <- paste(bd,"/climate-data/IND-TropMet_clm",sep="")
if (!file.exists(od)) {dir.create(od)}

library(raster); library(rgdal)

iniyr <- 1966
finyr <- 1993

ocl_dir <- paste(od,"/climatology_",iniyr,"_",finyr,sep="")
if (!file.exists(ocl_dir)) {dir.create(ocl_dir)}

#calculate mean climatology for all months
for (m in 1:12) {
  #m <- 1
  cat(m,". ",sep="")
  
  ors_name <- paste(ocl_dir,"/rain-",m,".tif",sep="")
  if (!file.exists(ors_name)) {
    stk <- stack(paste(wd,"/",iniyr:finyr,"/rain-",m,".tif",sep="")) #load rasters
    rtot <- calc(stk,fun= function(x) {mean(x)}) #calculate total of month
    rtot <- writeRaster(rtot,ors_name,format="GTiff") #write output raster
    rm(rtot); rm(stk); g=gc(); rm(g)
  }
}
cat("\n")



