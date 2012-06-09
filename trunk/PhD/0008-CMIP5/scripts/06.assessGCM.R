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

this_proc <- 1

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
    fList_all <- data.frame()
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
      fList_all <- rbind(fList_all,fList)
      
      #if (length(which(!fList$PRESENT)) != length(yi:yf)) {}
      ######extract for GCM
      fPres <- fList$FILE
      gcm_data <- stack(fPres) #load all GCM data
      gcm_data <- rotate(gcm_data) #rotate the GCM data so that it matches the -180 to 180 system
      msk <- createMask(shp,gcm_data[[1]]) #create a mask with the shapefile with resolution of gcm
      gcm_data <- crop(gcm_data,msk) #cut gcm data to extent of country mask
      #those that are NA for files need to be turned into NAs in whole file general
      for (gmiss in which(!fList$PRESENT)) { #set to NA any missing layer
        gcm_data <- setValues(gcm_data,values=rep(NA,times=ncell(gcm_data)),layer=gmiss)
      }
      xyNA <- xyFromCell(msk,which(is.na(msk[]))) #get the locations that are NA in the mask
      gcm_data[cellFromXY(gcm_data,xyNA)] <- NA #set anything outside the actual country mask to NA
      xy <- as.data.frame(xyFromCell(msk,which(!is.na(msk[])))) #which gridcells are to be calculated
      xyt <- as.data.frame(t(xy)) #transpose xy data.frame for handling many cells
      names(xyt) <- paste("C",1:ncol(xyt),sep="") #names of columns data.frame
      gcm_mvals <- lapply(as.list(xyt),FUN = function(xy,x) {extract(x,cbind(xy[1],xy[2]))},gcm_data) #extract values for that month
      if (m == 1) { #create list of matrices if we're in first month
        gcm_vals <- lapply(gcm_mvals,FUN= function(x,ny) {matrix(data=NA,nrow=ny,ncol=12)},length(yi:yf))
      }
      for (j in 1:length(gcm_vals)) { #assign values to the list
        gcm_vals[[j]][,m] <- gcm_mvals[[j]]
      }
      
      ######extract the cru data
      if (length(which(!fList$PRESENT)) != length(yi:yf)) {
        vn_cru <- paste(vnList$TS_CRU[vid]) #variable name
        sc_cru <- scList$TS_CRU[vid]
        otsCRU <- paste(oDir,"/ts-CRU",sep="") #create output folder
        if (!file.exists(otsCRU)) {dir.create(otsCRU)} #create output folder
        if (!file.exists(paste(otsCRU,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))) {
          cru_data <- stack(paste(tsCRU,"/cru_ts_3_10.1901.2009.",vn_cru,"_",yi:yf,"_",m,".tif",sep="")) #load all cru data
          cru_data <- crop(cru_data,msk) #cut cru data to extent of country mask
          fct <- res(msk)/res(cru_data)
          cru_data <- aggregate(cru_data,fact=fct,fun=mean) #average onto model grid
          cru_data <- resample(cru_data,msk,method="ngb") #resample aggregated result
          cru_data[cellFromXY(cru_data,xyNA)] <- NA #set anything outside the actual country mask to NA
          cru_mvals <- lapply(as.list(xyt),FUN = function(xy,x) {extract(x,cbind(xy[1],xy[2]))},cru_data) #extract values for that month
          if (m == 1) {cru_vals <- gcm_vals}
          for (j in 1:length(cru_vals)) { #assign values to the list
            cru_vals[[j]][,m] <- cru_mvals[[j]]
          }
        }
      } else {
        if (m == 1) {
          cru_vals <- gcm_vals
        } else {
          cru_mvals <- gcm_mvals
          for (j in 1:length(cru_vals)) { #assign values to the list
            cru_vals[[j]][,m] <- cru_mvals[[j]]
          }
        }
      }
      
      ######extract the era40 data
      if (length(which(!fList$PRESENT)) != length(yi:yf)) {
        vn_e40 <- paste(vnList$E40[vid]) #variable name
        sc_e40 <- scList$E40[vid]
        if (vn_e40 != "NA") {
          otsE40 <- paste(oDir,"/ts-E40",sep="") #create output folder
          if (!file.exists(otsE40)) {dir.create(otsE40)} #create output folder
          if (!file.exists(paste(otsE40,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))) {
            e40_data <- stack(paste(tsE40,"/monthly_data_",vn_e40,"/",yi:yf,"/",vn_e40,"_",m,".tif",sep="")) #load all era40 data
            e40_data <- crop(e40_data,msk) #cut cru data to extent of country mask
            e40_data <- resample(e40_data,msk,method="ngb") #resample to model grid
            e40_data[cellFromXY(e40_data,xyNA)] <- NA #set anything outside the actual country mask to NA
            e40_mvals <- lapply(as.list(xyt),FUN = function(xy,x) {extract(x,cbind(xy[1],xy[2]))},e40_data) #extract values for that month
            if (m == 1) {e40_vals <- gcm_vals}
            for (j in 1:length(cru_vals)) { #assign values to the list
              e40_vals[[j]][,m] <- e40_mvals[[j]]
            }
          }
        }
      } else {
        if (m == 1) {
          e40_vals <- gcm_vals
        } else {
          e40_mvals <- gcm_mvals
          for (j in 1:length(e40_vals)) { #assign values to the list
            e40_vals[[j]][,m] <- e40_mvals[[j]]
          }
        }
      }
    }
    cat("\n")
    
    #fixing the gcm data
    gcm_vals <- lapply(gcm_vals,FUN= function(x) {x[,c(12,1:11)]}) #re-ordering
    gcm_vals <- lapply(gcm_vals,FUN= function(x,sc) {x * sc},sc_gcm) #scaling
    
    ######extract the wst historical data (does not need to be monthly looped)
    if (!file.exists(paste(otsWST,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))) {
      vn_wst <- paste(vnList$TS_WST[vid])
      sc_wst <- scList$TS_WST[vid]
      otsWST <- paste(oDir,"/ts-WST",sep="") #create output folder
      if (!file.exists(otsWST)) {dir.create(otsWST)} #create output folder
      if (length(which(!fList_all$PRESENT)) != (length(yi:yf)*12)) {
        wst_raw <- read.csv(paste(tsWST,"/all_",vn_wst,"_data_ts_",yi,"-",yf,"_xy.csv",sep="")) #load raw data file
        wst_raw$ISIN <- extract(msk,cbind(x=wst_raw$LONG,y=wst_raw$LAT)) #extract cell values
        wst_raw <- wst_raw[which(!is.na(wst_raw$ISIN)),] #remove anything outside domain
        wst_raw$ISIN <- NULL
        wst_raw$CELL <- cellFromXY(msk,cbind(x=wst_raw$LONG,y=wst_raw$LAT))
        wst_data <- cbind(wst_raw[,c("YEAR","LONG","LAT",toupper(month.abb),"CELL")])
        wst_vals <- gcm_vals #get the list to put in the values
        xyMatch <- xy; xyMatch$CELL <- cellFromXY(msk,cbind(x=xy$x,y=xy$y))
        for (yr in yi:yf) {
          wst_data2 <- wst_data[which(wst_data$YEAR == yr),]
          wst_data2 <- as.data.frame(t(sapply(unique(wst_data$CELL),getMean_points,wst_data2)))
          names(wst_data2) <- c("CELL",toupper(month.abb))
          wst_data2 <- merge(xyMatch,wst_data2,by="CELL",all=T)
          wst_vals2 <- as.matrix(wst_data2[,toupper(month.abb)])
          wst_vals2[which(is.na(wst_vals2[,]))] <- NA
          for (j in 1:length(wst_vals)) {
            wst_vals[[j]][which((yi:yf %in% yr)),] <- wst_vals2[j,]
          }
        }
      } else {
        wst_vals <- gcm_vals
      }
      wst_vals <- lapply(wst_vals,FUN= function(x) {x[,c(12,1:11)]}) #re-ordering
      wst_vals <- lapply(wst_vals,FUN= function(x,sc) {x * sc},sc_wst) #scaling
      s_skill <- mapply(FUN= function(obs,gcm,mn) {seasonal_skill(obs,gcm,mn)},wst_vals,gcm_vals,MoreArgs=list(calc_mean),USE.NAMES=T) #assess seasonal skill using the two matrices
      s_skill <- as.data.frame(s_skill) #do some transformation to the output of the above
      s_skill <- lapply(s_skill,FUN= function(x) {as.data.frame(x)}) #do some transformation to the output of the above
      os_skill <- data.frame() #get everything into a nice data.frame that can be writen
      for (j in 1:length(s_skill)) {
        j_skill <- cbind(CELL=names(s_skill)[j],s_skill[[j]])
        os_skill <- rbind(os_skill,j_skill)
      }
      xyMatch <- xy; xyMatch$CELL <- paste("C",1:nrow(xyMatch),sep="")
      os_skill <- merge(os_skill,xyMatch,by="CELL",sort=F)
      os_skill <- os_skill[,c(1,9,10,2:8)]
      names(os_skill)[2:3] <- c("LON","LAT")
      write.csv(os_skill,paste(otsWST,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""),row.names=F,quote=T) #write output
    }
    
    #calculating with cru time series
    if (!file.exists(paste(otsCRU,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))) {
      cru_vals <- lapply(cru_vals,FUN= function(x) {x[,c(12,1:11)]}) #re-ordering
      cru_vals <- lapply(cru_vals,FUN= function(x,sc) {x * sc},sc_cru) #scaling
      s_skill <- mapply(FUN= function(obs,gcm,mn) {seasonal_skill(obs,gcm,mn)},cru_vals,gcm_vals,MoreArgs=list(calc_mean),USE.NAMES=T) #assess seasonal skill using the two matrices
      s_skill <- as.data.frame(s_skill) #do some transformation to the output of the above
      s_skill <- lapply(s_skill,FUN= function(x) {as.data.frame(x)}) #do some transformation to the output of the above
      os_skill <- data.frame() #get everything into a nice data.frame that can be writen
      for (j in 1:length(s_skill)) {
        j_skill <- cbind(CELL=names(s_skill)[j],s_skill[[j]])
        os_skill <- rbind(os_skill,j_skill)
      }
      xyMatch <- xy; xyMatch$CELL <- paste("C",1:nrow(xyMatch),sep="")
      os_skill <- merge(os_skill,xyMatch,by="CELL",sort=F)
      os_skill <- os_skill[,c(1,9,10,2:8)]
      names(os_skill)[2:3] <- c("LON","LAT")
      write.csv(os_skill,paste(otsCRU,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""),row.names=F,quote=T) #write output
    }
    
    #calculating with cru time series
    if (!file.exists(paste(otsE40,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""))) {
      if (vn_e40 != "NA") {
        e40_vals <- lapply(e40_vals,FUN= function(x) {x[,c(12,1:11)]}) #re-ordering
        e40_vals <- lapply(e40_vals,FUN= function(x,sc) {x * sc},sc_e40) #scaling
        s_skill <- mapply(FUN= function(obs,gcm,mn) {seasonal_skill(obs,gcm,mn)},e40_vals,gcm_vals,MoreArgs=list(calc_mean),USE.NAMES=T) #assess seasonal skill using the two matrices
        s_skill <- as.data.frame(s_skill) #do some transformation to the output of the above
        s_skill <- lapply(s_skill,FUN= function(x) {as.data.frame(x)}) #do some transformation to the output of the above
        os_skill <- data.frame() #get everything into a nice data.frame that can be writen
        for (j in 1:length(s_skill)) {
          j_skill <- cbind(CELL=names(s_skill)[j],s_skill[[j]])
          os_skill <- rbind(os_skill,j_skill)
        }
        xyMatch <- xy; xyMatch$CELL <- paste("C",1:nrow(xyMatch),sep="")
        os_skill <- merge(os_skill,xyMatch,by="CELL",sort=F)
        os_skill <- os_skill[,c(1,9,10,2:8)]
        names(os_skill)[2:3] <- c("LON","LAT")
        write.csv(os_skill,paste(otsE40,"/",vn_gcm,"_",gcm,"_",ens,".csv",sep=""),row.names=F,quote=T) #write output
      }
    }
    
    pfx <- file(procFil,"w")
    cat("processed on",date(),"by",paste(as.data.frame(t(Sys.info()))$login),"@",
        paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=pfx)
    close(pfx)
  }
}



















