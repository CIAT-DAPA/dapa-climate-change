#Julian Ramirez-Villegas
#June 2013
#CIAT / CCAFS / UoL
stop("not to run yet")

#### LIBRARIES: raster, rgdal, sp
library(sp); library(raster); library(rgdal)

#sources dir
#src.dir <- "~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#source the model and other functions
source(paste(src.dir,"/scripts/niche_based/EcoCrop-model.R",sep=""))
source(paste(src.dir,"/scripts/niche_based/nb-07-ecocrop_gnut-fun.R",sep=""))

#basic information
crop_name <- "gnut"

#i/o directories
#bDir <- "/mnt/a17/eejarv/PhD-work/crop-modelling/niche-based"
bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/niche-based"
occDir <- paste(bDir,"/occurrences",sep="")
calDir <- paste(bDir,"/calendar",sep="")
envDir <- paste(bDir,"/env-data",sep="")
clmDir <- paste(envDir,"/climate",sep="")
cmip5Dir <- paste(clmDir,"/cmip5",sep="")
modDir <- paste(bDir,"/models",sep="")
ecoDir <- paste(modDir,"/EcoCrop",sep="")
dataDir <- paste(ecoDir,"/data",sep="")
prjDir <- paste(ecoDir,"/proj",sep="")
mskDir <- paste(envDir,"/mask",sep="")


#here load Sacks et al. (2010) planting dates to get an approximation of when each
#of these points is sown. Use the average sowing and harvest date
#JRV changed to Jones and Thornton methodology
pdate <- raster(paste(calDir,"/plant_doy_ind_jt_1dd.tif",sep=""))
hdate <- raster(paste(calDir,"/harvest_doy_ind_jt_1dd.tif",sep=""))

#open evaluation file to select only those runs that had RMSE<0.5 and OR < 0.1 (?)
skill <- read.csv(paste(dataDir,"/skill.csv",sep=""))
skill <- skill[which(skill$TYPE == "_suitability"),]

######################################################################
#future scenario runs

#run details (parameter sets)
parList <- read.csv(paste(dataDir,"/parameter_sets.csv",sep=""))
rwList <- unique(parList$RUN)

#list of scenarios to run
sceList <- list.files(cmip5Dir)

#list of gcms
gcmList <- list.files(paste(cmip5Dir,"/",sceList[1],sep=""))

#full matrix of runs
runs_all <- expand.grid(ECORUN=rwList,SCENARIO=sceList,GCM=gcmList)

ini <- 4501
fin <- 4800; if (fin > nrow(runs_all)) {fin <- nrow(runs_all)}

#start of run loop
for (ri in ini:fin) {
  #ri <- 1
  #starting
  gcm <- paste(runs_all$GCM[ri])
  sce <- paste(runs_all$SCENARIO[ri])
  rw <- runs_all$ECORUN[ri]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n",sep="")
  cat("running ecocrop (run: ",rw,") in ",sce," for GCM ",gcm,"\n",sep="")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n",sep="")
  tpset <- parList[which(parList$RUN == rw),]
  
  #parameter values
  tkill <- tpset$KILL[2] - 40
  tmin <- tpset$MIN[2]; tmax <- tpset$MAX[2]
  topmin <- tpset$OPMIN[2]; topmax <- tpset$OPMAX[2]
  rmin <- tpset$MIN[1]; rmax <- tpset$MAX[1]
  ropmin <- tpset$OPMIN[1]; ropmax <- tpset$OPMAX[1]
  
  #output directory
  outf <- paste(prjDir,"/",sce,"/",gcm,"/run_",rw,sep="")
  
  #a. model run
  if (!file.exists(paste(outf,"/",crop_name,"_suitability.tif",sep=""))) {
    cat("running the model...\n")
    eco <- suitCalc(climPath=paste(cmip5Dir,"/",sce,"/",gcm,sep=""), 
                    sowDat=paste(calDir,"/plant_doy_ind_jt_1dd.tif",sep=""),
                    harDat=paste(calDir,"/harvest_doy_ind_jt_1dd.tif",sep=""),
                    Gmin=NA,Gmax=NA,Tkmp=tkill,Tmin=tmin,Topmin=topmin,
                    Topmax=topmax,Tmax=tmax,Rmin=rmin,Ropmin=ropmin,
                    Ropmax=ropmax,Rmax=rmax, 
                    outfolder=outf,
                    cropname=crop_name,ext=".tif",cropClimate=F)
    #remove prediction
    rm(eco); g=gc(); rm(g)
  }
}





