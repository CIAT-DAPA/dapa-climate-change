#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

#Get CMIP5 weather data
library(raster)

src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir2,"/CMIP5-functions.R",sep=""))

yi <- 1966
yf <- 1989

bDir <- "F:/PhD-work/data-quality-study"
#bDir <- "~/PhD-work/data-quality-study"
compDir <- paste(bDir,"/climate-comparison",sep="")

mdDir <- "H:/CMIP5/baseline"
#mdDir <- "/nfs/a102/eejarv/CMIP5/baseline"

cChars <- read.table(paste(compDir,"/0_input_data/CMIP5gcms.tab",sep=""),sep="\t",header=T)
#gcmList <- unique(cChars$GCM)

oDir <- paste(compDir,"/CMIP5_GJ",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#Parallelise the GCMs
library(snowfall)
sfInit(parallel=T,cpus=12)

sfExport("src.dir")
sfExport("src.dir2")
sfExport("bDir")
sfExport("oDir")
sfExport("mdDir")
sfExport("compDir")
sfExport("ccChars")
sfExport("yi")
sfExport("yf")

gcm_wrapper(13)
system.time(sfSapply(as.vector(1:21), gcm_wrapper))

sfStop()


