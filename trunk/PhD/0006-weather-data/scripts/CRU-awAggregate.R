#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL

#cut to AFASIA domain
#aggregate to 1 degree using area-weighted average

#load libraries
library(raster); library(rgdal)

#sourcing needed functions
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/awAggregate-functions.R",sep=""))


#bd <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
bd <- "F:/PhD-work/crop-modelling"
wd <- paste(bd,"/climate-data/CRU_TS_v3-1_data",sep="")

xt <- extent(raster(paste(bd,"/climate-data/worldclim/afasia_10min/prec_1.asc",sep="")))
xt@xmax <- xt@xmax+2
xt@ymax <- xt@ymax+2
xt@xmin <- xt@xmin-2
xt@ymin <- xt@ymin-2

variable <- "tmp"

outFolder <- paste(wd,"/monthly_grids/",variable,"_1dd",sep="")
if (!file.exists(outFolder)) {dir.create(outFolder)}

library(snowfall)
sfInit(cpus=3,parallel=T)

sfExport("outFolder")
sfExport("awAggWrapper")
sfExport("awBasicFunction")
sfExport("xt")
sfExport("wd")
sfExport("variable")

system.time(sfSapply(as.vector(1960:2009), agg_wrapper))

sfStop()



