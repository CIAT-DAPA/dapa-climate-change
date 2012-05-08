#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#March 2012
stop("Do not runt the whole thing")

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#eljefe
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#sourcing needed functions
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/watbal.R",sep=""))
source(paste(src.dir2,"/climateSignals-functions.R",sep=""))

library(raster)

#Climate signals on yield for Indian Groundnuts
#local
#bDir <- "F:/PhD-work/crop-modelling"

#eljefe
#bDir <- "~/PhD-work/crop-modelling"

sradDir <- paste(bDir,"/climate-data/CRU_CL_v1-1_data",sep="")
tempDir <- paste(bDir,"/climate-data/CRU_TS_v3-1_data",sep="")
era40Dir <- paste(bDir,"/climate-data/ERA-40",sep="")

y_iyr <- 1966
y_eyr <- 2001

#Planting dates as in literature
#khariff: summer rainfed
#         1. July 1 to 15 is optimal (Kabat and Satapathy 2011) (http://www.indianjournals.com/ijor.aspx?target=ijor:oryza&volume=48&issue=2&article=018)
#         2. Rice mainly grown rainfed in areas with heavy annual rainfall (wikipedia -http://en.wikipedia.org/wiki/Rice_production_in_India)
#         3. Rice can be grown (in certain states) in both summer and winter seasons (West Bengal, 
#            Andhra Pradesh, Assam and Orissa)
#         4. Sarkari and Reddy (2006): between 20th May and 20th July is optimal, but
#            in general from May to first week of August (http://epubs.icar.org.in/ejournal/index.php/IJAgS/article/view/2869)
#         5. In Cuttack (Orissa state) 20-30 May is optimal (Sharma and Reddy 1992). Time
#            of sowing is decided depending on pre-monsoon rainfall and subsequent accumulation
#            of water (Jha and Gangadharan 1989). Hence the only time available is mid-May to
#            early June (see http://journals.cambridge.org/download.php?file=%2FAGS%2FAGS118_02%2FS0021859600068763a.pdf&code=1c0153360d72d8bf74f024857fd4885c)

#rabi: winter irrigated (there's very little, but in some areas people plant rice after rice) and
#      in some other areas there is even triple cropping (summer, autumm, winter). 
#      EOS-webster (http://eos-webster.sr.unh.edu/data_guides/india_crops_dg.jsp;jsessionid=C29270422E37887A030E531726398406#imageSpot)
#      say that there's quite a lot of irrigation in India, particularly in the south-east
#      and nort-west of India. So, it would be difficult to track the rainfall signal on
#      the yield, as it seems that the system is actually quite complex, with different
#      amounts of irrigation, different type of systems, and of course, different planting dates
#      

#As per the planting dates
# Sacks et al. (2010) report the planting date for the Khariff (rainfed) season
# and this is what they call "rice" (what I call "main"). Nevertheless, they provide a
# "rice2" ("second" for me) season. The second season seems to correspond to winter wheat
# (at least in West Bengal -north east), but in Tamil Nadu (south) it could be autumm or
# winter.
# 

#      According to these maps (http://eos-webster.sr.unh.edu/data_guides/india_crops_dg.jsp;jsessionid=C29270422E37887A030E531726398406#imageSpot)
#      these areas seem to correspond with those of double and triple rice cropping (simultaneously). 
#      So, here are my conclusions
#      
#      1. The Tamil Nadu and West Bengal areas are likely to have a mixture of double, 
#         triple and single rice growing season.
#      2. Signal attribution and model runs will be tricky to do in areas with double reported
#         planting date (in addition to we don't have a third planting date). So my suggestion
#         here would be to go with those two, and for the second season only analyse the pixels
#         reported in Sacks et al. (2010). We've got no evidence for the third growing season, or
#         rule to set it up
#

ncFile <- paste(bDir,"/climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
mthRainAsc <- paste(bDir,"/climate-data/IND-TropMet",sep="")

cropDir <- paste(bDir,"/GLAM/climate-signals-yield/RICE",sep="")
ydDir <- paste(bDir,"/GLAM/climate-signals-yield/RICE/raster/gridded",sep="")
oDir <- paste(bDir,"/GLAM/climate-signals-yield/RICE/signals",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#create mask
metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-1966.asc",sep=""))
msk <- maskCreate(metFile,yldFile)

############
#determine planting date using pjones algorithm?

if (!file.exists(paste(oDir,"/cells-process.csv",sep=""))) {
  pCells <- data.frame(CELL=1:ncell(msk))
  pCells$X <- xFromCell(msk,pCells$CELL); pCells$Y <- yFromCell(msk,pCells$CELL)
  pCells$Z <- extract(msk,cbind(X=pCells$X,Y=pCells$Y))
  pCells <- pCells[which(!is.na(pCells$Z)),]
  write.csv(pCells,paste(oDir,"/cells-process.csv",sep=""),quote=F,row.names=F)
} else {
  pCells <- read.csv(paste(oDir,"/cells-process.csv",sep=""))
}

# plot(msk)
# text(x=pCells$X,y=pCells$Y,labels=pCells$CELL,cex=0.35)

pday <- raster(paste(cropDir,"/calendar/main/plant_lr.tif",sep="")) #planting date
hday <- raster(paste(cropDir,"/calendar/main/harvest_lr.tif",sep="")) #harvest date

#calculate growing season duration
hdaym <- hday
hdaym[which(hday[] < pday[])] <- hdaym[which(hday[] < pday[])] + 365
gdur <- hdaym-pday

############################################################################
############################################################################
################### Using ea/ep ratio to estimate when the crop was planted
############################################################################
############################################################################
###Parameters
sd_default=166; ed_default=296; thresh=0.5
tbase=8; topt=30; tmax=42 #taken from the ORYZA2000 model
tcrit=33; tlim=42

#tcrit suggested 40-42 Tao et al. (2008)
#review of Shah et al. (2011) suggests that 32 (Tillering), 33.7 (Anthesis), 35 (Flowering)
#34 (grain formation) and 29 (grain ripening) are critical thresholds
#Jagadish et al. (2007) suggest 33 as the critical threshold

oDir <- paste(bDir,"/GLAM/climate-signals-yield/RICE/signals/ea_ep_ratio",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=3) #initiate cluster

#export functions and data
sfExport("sd_default"); sfExport("ed_default"); sfExport("thresh")
sfExport("tbase"); sfExport("topt"); sfExport("tmax"); sfExport("tcrit"); sfExport("tlim")
sfExport("pday"); sfExport("hday")
sfExport("pCells")
sfExport("oDir")
sfExport("ncFile")
sfExport("mthRainAsc")
sfExport("ydDir")
sfExport("bDir")
sfExport("sradDir")
sfExport("tempDir")
sfExport("era40Dir")
sfExport("y_iyr")
sfExport("y_eyr")
sfExport("src.dir")
sfExport("src.dir2")

#remove all those cells in the list that do not exist
cellList <- NULL
for (cell in pCells$CELL) {
  if (!file.exists(paste(oDir,"/climate_cell-",cell,".csv",sep=""))) {
    cellList <- c(cellList,cell)
  }
}

#run the control function
system.time(sfSapply(as.vector(cellList), cell_wrapper))

#stop the cluster
sfStop()


################### get the signals into a grid
#important fields
iyr <- 1966; fyr <- 2001
if (fyr < iyr) {
  tser <- (1900+iyr):(2000+fyr)
} else {
  tser <- 1900+(iyr:fyr)
}
tser <- substr(tser,3,4)


#for a given cell extract the yield data and make the correlation for each detrending technique
x <- calcSignals(techn="lin",ydDir=ydDir,oDir=oDir,tser)
x <- calcSignals(techn="loe",ydDir=ydDir,oDir=oDir,tser)
x <- calcSignals(techn="fou",ydDir=ydDir,oDir=oDir,tser)
x <- calcSignals(techn="qua",ydDir=ydDir,oDir=oDir,tser)

#plot all the rasters (correlations and p values)
x <- plotSignals(techn="lin",oDir,pval=0.1)
x <- plotSignals(techn="loe",oDir,pval=0.1)
x <- plotSignals(techn="fou",oDir,pval=0.1)
x <- plotSignals(techn="qua",oDir,pval=0.1)



############################################################################
############################################################################
############################# With planting dates from Sacks et al. (2010)
############################# SEASON: MAIN
############################################################################
############################################################################
###Parameters
tbase=8; topt=30; tmax=42 #taken from the ORYZA2000 model
tcrit=33; tlim=42

#tcrit suggested 40-42 Tao et al. (2008)
#review of Shah et al. (2011) suggests that 32 (Tillering), 33.7 (Anthesis), 35 (Flowering)
#34 (grain formation) and 29 (grain ripening) are critical thresholds
#Jagadish et al. (2007) suggest 33 as the critical threshold

pday <- raster(paste(cropDir,"/calendar/main/plant_lr.tif",sep="")) #planting date
hday <- raster(paste(cropDir,"/calendar/main/harvest_lr.tif",sep="")) #harvest date

oDir <- paste(bDir,"/GLAM/climate-signals-yield/RICE/signals/sacks_pdate_main",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

#export functions and data
sfExport("tbase"); sfExport("topt"); sfExport("tmax"); sfExport("tcrit"); sfExport("tlim")
sfExport("pday"); sfExport("hday")
sfExport("pCells")
sfExport("oDir")
sfExport("ncFile")
sfExport("mthRainAsc")
sfExport("ydDir")
sfExport("bDir")
sfExport("sradDir")
sfExport("tempDir")
sfExport("era40Dir")
sfExport("y_iyr")
sfExport("y_eyr")
sfExport("src.dir")
sfExport("src.dir2")

#remove all those cells in the list that do not exist
cellList <- NULL
for (cell in pCells$CELL) {
  if (!file.exists(paste(oDir,"/climate_cell-",cell,".csv",sep=""))) {
    cellList <- c(cellList,cell)
  }
}

#run the control function
system.time(sfSapply(as.vector(cellList), cell_wrapper_irr))

#stop the cluster
sfStop()


################### get the signals into a grid
#important fields
iyr <- 1966; fyr <- 2001
if (fyr < iyr) {
  tser <- (1900+iyr):(2000+fyr)
} else {
  tser <- 1900+(iyr:fyr)
}
tser <- substr(tser,3,4)

#for a given cell extract the yield data and make the correlation for each detrending technique
x <- calcSignals(techn="lin",ydDir=ydDir,oDir=oDir)
x <- calcSignals(techn="loe",ydDir=ydDir,oDir=oDir)
x <- calcSignals(techn="fou",ydDir=ydDir,oDir=oDir)
x <- calcSignals(techn="qua",ydDir=ydDir,oDir=oDir)


#plot all the rasters (correlations and p values)
x <- plotSignals(techn="lin",oDir,pval=0.1)
x <- plotSignals(techn="loe",oDir,pval=0.1)
x <- plotSignals(techn="fou",oDir,pval=0.1)
x <- plotSignals(techn="qua",oDir,pval=0.1)


############################################################################
############################################################################
############################# With planting dates from Sacks et al. (2010)
############################# SEASON: SECOND
############################################################################
############################################################################
###Parameters
tbase=8; topt=30; tmax=42 #taken from the ORYZA2000 model
tcrit=33; tlim=42

#tcrit suggested 40-42 Tao et al. (2008)
#review of Shah et al. (2011) suggests that 32 (Tillering), 33.7 (Anthesis), 35 (Flowering)
#34 (grain formation) and 29 (grain ripening) are critical thresholds
#Jagadish et al. (2007) suggest 33 as the critical threshold

pday <- raster(paste(cropDir,"/calendar/second/plant_lr.tif",sep="")) #planting date
hday <- raster(paste(cropDir,"/calendar/second/harvest_lr.tif",sep="")) #harvest date

pCells$PDAY <- extract(pday,cbind(X=pCells$X,Y=pCells$Y))
pCells <- pCells[which(!is.na(pCells$PDAY)),]

oDir <- paste(bDir,"/GLAM/climate-signals-yield/RICE/signals/sacks_pdate_second",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

#export functions and data
sfExport("tbase"); sfExport("topt"); sfExport("tmax"); sfExport("tcrit"); sfExport("tlim")
sfExport("pday"); sfExport("hday")
sfExport("pCells")
sfExport("oDir")
sfExport("ncFile")
sfExport("mthRainAsc")
sfExport("ydDir")
sfExport("bDir")
sfExport("sradDir")
sfExport("tempDir")
sfExport("era40Dir")
sfExport("y_iyr")
sfExport("y_eyr")
sfExport("src.dir")
sfExport("src.dir2")

#remove all those cells in the list that do not exist
cellList <- NULL
for (cell in pCells$CELL) {
  if (!file.exists(paste(oDir,"/climate_cell-",cell,".csv",sep=""))) {
    cellList <- c(cellList,cell)
  }
}

#run the control function
system.time(sfSapply(as.vector(cellList), cell_wrapper_irr))

#stop the cluster
sfStop()


################### get the signals into a grid
#important fields
iyr <- 1966; fyr <- 2001
if (fyr < iyr) {
  tser <- (1900+iyr):(2000+fyr)
} else {
  tser <- 1900+(iyr:fyr)
}
tser <- substr(tser,3,4)

#for a given cell extract the yield data and make the correlation for each detrending technique
x <- calcSignals(techn="lin",ydDir=ydDir,oDir=oDir)
x <- calcSignals(techn="loe",ydDir=ydDir,oDir=oDir)
x <- calcSignals(techn="fou",ydDir=ydDir,oDir=oDir)
x <- calcSignals(techn="qua",ydDir=ydDir,oDir=oDir)


#plot all the rasters (correlations and p values)
x <- plotSignals(techn="lin",oDir,pval=0.1)
x <- plotSignals(techn="loe",oDir,pval=0.1)
x <- plotSignals(techn="fou",oDir,pval=0.1)
x <- plotSignals(techn="qua",oDir,pval=0.1)


