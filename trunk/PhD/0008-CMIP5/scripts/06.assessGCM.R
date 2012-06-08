#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT

library(raster); library(rgdal); library(maptools)

#6. Calculation of metrics as stated in methods

#variables to be set
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "/nfs/a102/eejarv/CMIP5"
#e40Dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"


#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data/scripts"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#mdDir <- "V:/eejarv/CMIP5"
#e40Dir <- "W:/eejarv/PhD-work/crop-modelling/climate-data/ERA-40"

#source functions
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))

#input directories and configurations
inputDD <- paste(mdDir,"/assessment/input-data",sep="")
outputDD <- paste(mdDir,"/assessment/output-data",sep="")

#climatology data
clWCL <- paste(inputDD,"/wcl-data",sep="")
clCRU <- paste(inputDD,"/cru-data",sep="")
clE40 <- e40Dir
clWST <- paste(inputDD,"/wcl-weather-stations",sep="")

#time series data
tsWST <- paste(inputDD,"/all-weather-stations",sep="")
tsE40 <- e40Dir
tsCRU <- paste(inputDD,"/cru-ts-data",sep="")

#administrative areas data
admDir <- paste(inputDD,"/adm-data",sep="")

#list of gcms and countries/regions
gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")
regions <- read.table(paste(src.dir2,"/data/regions.tab",sep=""),header=T,sep="\t")

#variables to analyse
vnList <- data.frame(VID=1:3,GCM=c("pr","tas","dtr"),WCL=c("prec","tmean","dtr"),
                     CL_CRU=c("prec","tmean","dtr"),TS_CRU=c("pre","tmp","dtr"),
                     E40=c("prec","tasm",NA),CL_WST=c("rain","tean","dtr"),
                     TS_WST=c("pr","tas","dtr"))

#scaling factors to datasets per variable
scList <- data.frame(VID=1:3,GCM=c(1,1,1),WCL=c(1,1,1),
                     CL_CRU=c(1,1,1),TS_CRU=c(0.1,0.1,0.1),
                     E40=c(1,1,NA),CL_WST=c(1,1,1),
                     TS_WST=c(1,0.1,0.1))


#processes to complete
gcmList <- unique(paste(gcmChars$GCM,"_ENS_",gcmChars$Ensemble,sep=""))
isoList <- regions$ISO
procList <- expand.grid(GCM=gcmList,ISO=isoList)


#a. mean climates: for each area using the values of GCM gridcells and the mean
#                  values of the datasets calculate the following
#   - pearson & p-value (origin-forced)
#   - slope (origin-forced)
#   - rmse

#check those that are done already
procList <- check_done(procList)

#determine number of CPUs
ncpus <- nrow(procList)
if (ncpus>12) {ncpus <- 12}

#here do the parallelisation
#load library and create cluster
library(snowfall)
sfInit(parallel=T,cpus=ncpus)

#export variables
sfExport("src.dir")
sfExport("src.dir2")
sfExport("mdDir")
sfExport("e40Dir")
sfExport("inputDD")
sfExport("outputDD")
sfExport("clWCL")
sfExport("clCRU")
sfExport("clE40")
sfExport("clWST")
sfExport("admDir")
sfExport("vnList")
sfExport("scList")
sfExport("procList")
sfExport("regions")


#run the function in parallel
system.time(sfSapply(as.vector(1:nrow(procList)),mean_climate_skill))

#stop the cluster
sfStop()




#b. interannual variability: for each gridcell using the monthly series of GCMs,
#                            matched with the data (scaled) from each source, calculate
#                            the following:
#   - pearson & p-value (origin-forced)
#   - slope (origin-forced)
#   - rmse

#specify initial and final years
yi <- 1961
yf <- 2000

this_proc <- 65

#here the process starts for a given country-gcm_ens combination
iso <- paste(procList$ISO[this_proc])
reg <- paste(regions$REGION[which(regions$ISO == iso)])
gcm <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[1]
ens <- unlist(strsplit(paste(procList$GCM[this_proc]),"_ENS_",fixed=T))[2]

cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("process started for",iso,"-",gcm,"-",ens,"\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")

oDir <- paste(outputDD,"/",reg,"/",iso,sep="") #create output directory
if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}
procDir <- paste(oDir,"/y.proc",sep="") #output directory for .proc files
if (!file.exists(procDir)) {dir.create(procDir)}

#load shapefile
shp <- readShapePoly(paste(admDir,"/",reg,"/",iso,"_adm/",iso,"0.shp",sep=""))


for (vid in 1:3) {
  if (vid == 1) {calc_mean <- F} else {calc_mean <- T}
  vn_gcm <- paste(vnList$GCM[vid]) #variable name
  cat("processing variable",vid,":",vn_gcm,"\n")
  
  procFil <- paste(procDir,"/",vn_gcm,"_",gcm,"_",ens,".proc",sep="") #check file
  if (!file.exists(procFil)) {
    sc_gcm <- scList$GCM[vid]
    tsGCM <- paste(mdDir,"/baseline/",gcm,"/",ens,"_monthly",sep="")
    
    #here loop through months
    gcm_vals <- list()
    for (m in 1:12) {
      #m <- 1
      cat(m,". ",sep="")
      if (m < 10) {mth <- paste("0",m,sep="")} else {mth <- paste(m)}
      fList <- paste(tsGCM,"/",yi:yf,"/",vn_gcm,"_",mth,".tif",sep="")
      fPres <- as.character(sapply(fList,checkExists))
      fList <- data.frame(YEAR=yi:yf,PRESENT=F,FILE=fPres)
      fList$PRESENT[which(!is.na(fList$FILE))] <- T
      fList$FILE <- paste(fList$FILE)
      fList$FILE[which(!fList$PRESENT)] <- paste(mdDir,"/baseline/",gcm,"/",ens,"_climatology/pr_01.tif",sep="")
      
      if (which(!fList$PRESENT) != length(yi:yf)) {
        
        ######extract for GCM
        fPres <- fList$FILE
        gcm_data <- stack(fPres) #load all GCM data
        gcm_data <- rotate(gcm_data) #rotate the GCM data so that it matches the -180 to 180 system
        msk <- createMask(shp,gcm_data[[1]]) #create a mask with the shapefile with resolution of gcm
        gcm_data <- crop(gcm_data,msk) #cut gcm data to extent of country mask
        #those that are NA for files need to be turned into NAs in whole file general
        for (gmiss in which(!fList$PRESENT)) {
          gcm_data <- setValues(gcm_data,values=rep(NA,times=ncell(gcm_data)),layer=gmiss)
        }
        xyNA <- xyFromCell(msk,which(is.na(msk[]))) #get the locations that are NA in the mask
        gcm_data[cellFromXY(gcm_data,xyNA)] <- NA #set anything outside the actual country mask to NA
        xy <- as.data.frame(xyFromCell(msk,which(!is.na(msk[])))) #which gridcells are to be calculated
        xyt <- as.data.frame(t(xy)) #transpose xy data.frame for handling many cells
        names(xyt) <- paste("C",1:ncol(xyt),sep="") #names of columns data.frame
        gcm_mvals <- lapply(as.list(xyt),FUN = function(xy,x) {extract(gcm_data,cbind(xy[1],xy[2]))},gcm_data) #extract values for that month
        
        if (m == 1) { #create list of matrices if we're in first month
          gcm_vals <- lapply(gcm_mvals,FUN= function(x,ny) {matrix(data=NA,nrow=ny,ncol=12)},length(yi:yf))
        }
        for (j in 1:length(gcm_vals)) { #assign values to the list
          gcm_vals[[j]][,m] <- gcm_mvals[[j]]
        }
        
        ######extract the cru data
        vn_cru <- paste(vnList$TS_CRU[vid]) #variable name
        sc_cru <- scList$TS_CRU[vid]
        otsCRU <- paste(oDir,"/ts-CRU",sep="") #create output folder
        if (!file.exists(otsCRU)) {dir.create(otsCRU)} #create output folder
        tsCRU
        
        
      }
      cat("\n")
      gcm_vals <- lapply(gcm_vals,FUN= function(x) {x[,c(12,1:11)]}) #re-ordering
      gcm_vals <- lapply(gcm_vals,FUN= function(x,sc) {x * sc},sc_gcm) #scaling
    } 
    
  }
}



















