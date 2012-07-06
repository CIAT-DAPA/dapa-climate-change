#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#June 2012

#load libraries
library(raster); library(rgdal)

bd <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
#bd <- "W:/eejarv/PhD-work/crop-modelling"
wd <- paste(bd,"/climate-data/CRU_TS_v3-1_data",sep="")

iniyr <- 1966
finyr <- 1993

variable <- "tmp"
data_dir <- paste(wd,"/monthly_grids/",variable,"_1dd",sep="")

oclim_dir <- paste(wd,"/climatology_",iniyr,"_",finyr,sep="")
if (!file.exists(oclim_dir)) {dir.create(oclim_dir)}

ovar_dir <- paste(oclim_dir,"/",variable,"_1dd",sep="")
if (!file.exists(ovar_dir)) {dir.create(ovar_dir)}

for (m in 1:12) {
  #m <- 1
  cat(m,". ",sep="")
  
  ors_name <- paste(ovar_dir,"/",variable,"_",m,".asc",sep="")
  if (!file.exists(ors_name)) {
    stk <- stack(paste(data_dir,"/",variable,"_",iniyr:finyr,"_",m,".asc",sep="")) #load rasters
    gmean <- calc(stk,fun= function(x) {mean(x)}) #calculate total of month
    gmean <- writeRaster(gmean,ors_name,format="ascii") #write output raster
    rm(gmean); rm(stk); g=gc(); rm(g)
  }
}
cat("\n")


