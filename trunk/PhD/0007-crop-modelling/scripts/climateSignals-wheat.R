#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#March 2012
stop("Do not runt the whole thing")

#sourcing needed functions
#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

#eljefe
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"

source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/watbal.R",sep=""))
#source(paste(src.dir2,"/climateSignals-functions.R",sep=""))
source(paste(src.dir2,"/climateSignals-functions.v2.R",sep=""))

library(raster)

#Climate signals on yield for Indian wheat

#local
#bDir <- "F:/PhD-work/crop-modelling"

#eljefe
#bDir <- "~/PhD-work/crop-modelling"

#sradDir <- paste(bDir,"/climate-data/CRU_CL_v1-1_data",sep="")
#tempDir <- paste(bDir,"/climate-data/CRU_TS_v3-1_data",sep="")
#era40Dir <- paste(bDir,"/climate-data/ERA-40",sep="")

sradDir <- paste(bDir,"/climate-data/gridcell-data/IND/cru_srad",sep="")
tempDir <- paste(bDir,"/climate-data/gridcell-data/IND",sep="")
era40Dir <- paste(bDir,"/climate-data/gridcell-data/IND/srad_e40",sep="")

y_iyr <- 1966
y_eyr <- 1998

#Planting dates as in literature
#     1. Early: 10 November (Lobell et al. 2012)
#     2. Normal: 25 November (Lobell et al. 2012)
#     3. Late: 10 December (Lobell et al. 2012)
#     4. Last Dates upto which sowing would be economical are 
#        (http://krishisewa.com/articles/w_sowing.html)
#        zones at (http://krishisewa.com/krishi/Azone.html)
              #upto 25th December - in North-West plain Zone
              #upto 10th December - in North-East plain & Central Zone
              #upto 30th November - in Peninsular Zone
#     5. Very early: mid-October (De et al. 1983) (http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=4596924)
#     6. Planting should be done when tmean < 22C
#     7. Harvest is likely to be ~April

ncFile <- paste(bDir,"/climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
#mthRainAsc <- paste(bDir,"/climate-data/IND-TropMet",sep="")
mthRainAsc <- paste(bDir,"/climate-data/gridcell-data/IND/rain",sep="")

cropDir <- paste(bDir,"/GLAM/climate-signals-yield/WHEAT",sep="")
ydDir <- paste(bDir,"/GLAM/climate-signals-yield/WHEAT/raster/gridded",sep="")
oDir <- paste(bDir,"/GLAM/climate-signals-yield/WHEAT/signals",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#create mask
metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
msk <- maskCreate(metFile,yldFile)

############

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

#For irrigated use the Sacks et al. (2010) data
#load sacks data (scaled to 1dd)
pday <- raster(paste(cropDir,"/calendar/wwin/plant_lr.tif",sep="")) #planting date
hday <- raster(paste(cropDir,"/calendar/wwin/harvest_lr.tif",sep="")) #harvest date

#calculate growing season duration
hdaym <- hday
hdaym[which(hday[] < pday[])] <- hdaym[which(hday[] < pday[])] + 365
gdur <- hdaym-pday

#plot(gdur)

#pick a cell and year for a test case
#yr <- 1966
#cell <- 427 #856
#x <- cell_wrapper_irr(427)

############################################################################
############################################################################
############################# With planting dates from Sacks et al. (2010)
############################################################################
############################################################################
###Parameters
tbase=1; topt=22.1; tmax=35.4; tcrit=34; tlim=40

oDir <- paste(oDir,"/sacks_pdate",sep="")
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
sfExport("cropDir")
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


########################################################################
#for a given cell extract the yield data and make the correlation
########################################################################

#important fields
iyr <- 66; fyr <- 98
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


#it is unlikely that there is anything that is not winter wheat in India.
#as winter crops are irrigated, it is impossible to derive the sowing dates using
#the watbal routine. Hence, I decided to only use planting dates as reported in 
#Sacks et al. (2010)

