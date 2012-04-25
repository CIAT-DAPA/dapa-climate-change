#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#March 2012
stop("Do not runt the whole thing")

#sourcing needed functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/watbal.R",sep=""))

src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir2,"/climateSignals-functions.R",sep=""))

library(raster)

#Climate signals on yield for Indian Groundnuts

bDir <- "F:/PhD-work/crop-modelling"
sradDir <- paste(bDir,"/climate-data/CRU_CL_v1-1_data",sep="")
tempDir <- paste(bDir,"/climate-data/CRU_TS_v3-1_data",sep="")

y_iyr <- 1966
y_eyr <- 1995

#Planting dates as in literature
#summer (only separated in Sardana and Kandhola 2007):
#         1. End of April to early May
#khariff: summer rainfed. 
#         1. From first week of June to last week of July (Talawar 2004) (regular and late monsoon)
#         2. From end of May to early June (Sardana and Kandhola 2007) (pre-monsoon)
#         3. Third week of June (Singh and Oswalt 1995)
#         4. First half of July (Singh et al. 1986)
#         5. 20th April (normal summer) (Harinath and Vasanthi 1998 http://www.indianjournals.com/ijor.aspx?target=ijor:ijpp1&volume=26&issue=2&article=001)
#rabi: winter irrigated
#         1. From mid December to mid January (Ramadoss and Myers 2004 http://www.regional.org.au/au/asa/2004/poster/4/1/2/1243_ramadoss.htm)
#         2. Mid September to first of November
#         3. Early rabi: 5-20 October (Harinath and Vasanthi 1998 http://www.indianjournals.com/ijor.aspx?target=ijor:ijpp1&volume=26&issue=2&article=001)
#         4. Normal rabi: 5 November (Harinath and Vasanthi 1998 http://www.indianjournals.com/ijor.aspx?target=ijor:ijpp1&volume=26&issue=2&article=001)

ncFile <- paste(bDir,"/climate-data/IND-TropMet/0_input_data/india_data.nc",sep="")
mthRainAsc <- paste(bDir,"/climate-data/IND-TropMet",sep="")

ydDir <- paste(bDir,"/GLAM/climate-signals-yield/GNUT/raster/gridded",sep="")
oDir <- paste(bDir,"/GLAM/climate-signals-yield/GNUT/signals",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#create mask
metFile <- raster(ncFile,band=0)
yldFile <- raster(paste(ydDir,"/raw/raw-66.asc",sep=""))
msk <- maskCreate(metFile,yldFile)

############
#determine planting date using pjones algorithm?

pCells <- data.frame(CELL=1:ncell(msk))
pCells$X <- xFromCell(msk,pCells$CELL); pCells$Y <- yFromCell(msk,pCells$CELL)
pCells$Z <- extract(msk,cbind(X=pCells$X,Y=pCells$Y))
pCells <- pCells[which(!is.na(pCells$Z)),]
write.csv(pCells,paste(oDir,"/cells-process.csv",sep=""),quote=F,row.names=F)

# plot(msk)
# text(x=pCells$X,y=pCells$Y,labels=pCells$CELL,cex=0.35)

############################# Anything that could be rainfed
###Parameters
sd_default=165; ed_default=225
thresh=0.5
tbase=10; topt=28; tmax=50
tcrit=34; tlim=40

#parallelisation
library(snowfall)
sfInit(parallel=T,cpus=4) #initiate cluster

#export functions and data
sfExport("sd_default"); sfExport("ed_default"); sfExport("thresh")
sfExport("tbase"); sfExport("topt"); sfExport("tmax"); sfExport("tcrit"); sfExport("tlim")
sfExport("pCells")
sfExport("oDir")
sfExport("ncFile")
sfExport("mthRainAsc")
sfExport("ydDir")
sfExport("bDir")
sfExport("sradDir")
sfExport("tempDir")
sfExport("y_iyr")
sfExport("y_eyr")
sfExport("src.dir")
sfExport("src.dir2")

#run the control function
system.time(sfSapply(as.vector(pCells$CELL), cell_wrapper))

#stop the cluster
sfStop()


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




