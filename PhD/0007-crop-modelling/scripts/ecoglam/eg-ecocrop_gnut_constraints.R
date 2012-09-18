#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL

#### LIBRARIES: raster, maptools, rgdal, sp
library(raster); library(rgdal); library(maptools); library(MASS)
data(wrld_simpl)
stop("not to run yet")

#sources dir
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop"
src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#sourcing functions
source(paste(src.dir,"/src/EcoCrop.R",sep=""))
source(paste(src.dir,"/src/constraints.R",sep=""))

######################################################################
#basic information
crop_name <- "gnut"
r_dir <- "W:/eejarv/PhD-work/crop-modelling"
#r_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
b_dir <- paste(r_dir,"/GLAM",sep="")
crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")
ec_dir <- paste(crop_dir,"/ecg_analyses/ecocrop-",tolower(crop_name),sep="")

#constraints dir
cons_dir <- paste(ec_dir,"/constraints",sep="")
if (!file.exists(cons_dir)) {dir.create(cons_dir)}

#run details
gs <- 1
p <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
p <- p[which(p$GS==gs),]
vl <- "tmean"
rw <- which(p$VARIABLE == vl)

#sensitivity table
sTable <- list()
sTable$Tmin <- list(VALUE=5,TYPE="p")
sTable$Topmin <- list(VALUE=5,TYPE="p")
sTable$Topmax <- list(VALUE=-5,TYPE="p")
sTable$Tmax <- list(VALUE=-5,TYPE="p")
sTable$Rmin <- list(VALUE=5,TYPE="p")
sTable$Ropmin <- list(VALUE=5,TYPE="p")
sTable$Ropmax <- list(VALUE=-5,TYPE="p")
sTable$Rmax <- list(VALUE=-5,TYPE="p")


#model runs
#GLAM baseline (1966_1993)
clm_dir <- paste(ec_dir,"/climate/climatology/1966_1993",sep="")
eco <- consCalc(climPath=clm_dir, 
                Gmin=120,Gmax=120,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],
                Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                outfolder=paste(cons_dir,"/clm_1966_1993",sep=""), 
                cropname=paste(gs,"-",crop_name,"-",vl[rw-1],sep=""),ext=".tif",
                sensTable=sTable)

#complete baseline (1960_2000)
clm_dir <- paste(ec_dir,"/climate/climatology/1960_2000",sep="")
eco <- consCalc(climPath=clm_dir, 
                Gmin=120,Gmax=120,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],
                Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                outfolder=paste(cons_dir,"/clm_1960_2000",sep=""), 
                cropname=paste(gs,"-",crop_name,"-",vl[rw-1],sep=""),ext=".tif",
                sensTable=sTable)




