#Julian Ramirez-Villegas
#May 2012
#UoL / CCAFS / CIAT


#plot points of seasons on top of boxplot
plotpoints <- function(pdata,param="CCOEF") {
  cnames <- levels(pdata$ISO)
  ctr <- 1
  for (iso in cnames) {
    djf <- pdata[which(pdata$ISO == paste(iso) & pdata$SEASON == "DJF"),]
    mam <- pdata[which(pdata$ISO == paste(iso) & pdata$SEASON == "MAM"),]
    jja <- pdata[which(pdata$ISO == paste(iso) & pdata$SEASON == "JJA"),]
    son <- pdata[which(pdata$ISO == paste(iso) & pdata$SEASON == "SON"),]
    ann <- pdata[which(pdata$ISO == paste(iso) & pdata$SEASON == "ANN"),]
    points(djf[,param],rep(ctr+.25,times=nrow(djf)),pch=20,cex=0.65,lwd=0.1,col="red")
    points(mam[,param],rep(ctr+.1,times=nrow(mam)),pch=20,cex=0.65,lwd=0.1,col="blue")
    points(ann[,param],rep(ctr,times=nrow(ann)),pch=20,cex=0.65,lwd=0.1,col="black")
    points(jja[,param],rep(ctr-.1,times=nrow(jja)),pch=20,cex=0.65,lwd=0.1,col="purple")
    points(son[,param],rep(ctr-.25,times=nrow(son)),pch=20,cex=0.65,lwd=0.1,col="dark green")
    ctr <- ctr+1
  }
}


##############################################################################
# get metrics for mean climate skill assessment
# revised version for ERL paper
##############################################################################
get_mean_climate_metrics <- function(proc,cmip5Dir,regions) {
  gcm <- paste(proc[1]) #paste(procList$GCM[1])
  dst <- paste(proc[3]) #paste(procList$OBS[1])
  vn <- paste(proc[4]) #paste(procList$VAR[1])
  iso <- paste(proc[2]) #paste(procList$ISO[1])
  reg <- paste(regions$REGION[which(regions$ISO == iso)])
  
  metsDir <- paste(cmip5Dir,"/assessment/output-data-cmip3/",reg,"/",iso,"",sep="")
  fname <- paste(metsDir,"/",dst,"/",vn,"_",gcm,".csv",sep="")
  cat(fname,"\n")
  if (file.exists(fname)) {
    mets <- read.csv(paste(fname))
    mets <- cbind(GCM=gcm,OBS=dst,VAR=vn,ISO=iso,REG=reg,mets)
  } else {
    mets <- data.frame(GCM=gcm,OBS=dst,VAR=vn,ISO=iso,REG=reg,SEASON=NA,CCOEF=NA,
                       PVAL=NA,RSQ=NA,CCOEF2=NA,PVAL2=NA,RSQ2=NA,MBR=NA,RMSE=NA,
                       PRMSE1=NA,PRMSE2=NA,PRMSE3=NA,PRMSE4=NA,mean_OBS=NA,mean_GCM=NA,
                       std_OBS=NA,std_GCM=NA,N=NA)
  }
  return(mets)
}


##############################################################################
#checking comparisons that are already processed
##############################################################################
check_done <- function(procList,check_dir="x.proc") {
  ndList <- c()
  for (i in 1:nrow(procList)) {
    iso <- paste(procList$ISO[i])
    reg <- paste(regions$REGION[which(regions$ISO == iso)])
    gcm <- paste(procList$GCM[i])
    
    oDir <- paste(outputDD,"/",reg,"/",iso,sep="")
    procDir <- paste(oDir,"/",check_dir,sep="")
    for (vid in 1:3) {
      vn_gcm <- paste(vnList$GCM[vid]) #variable name
      procFil <- paste(procDir,"/",vn_gcm,"_",gcm,".proc",sep="") #check file
      if (!file.exists(procFil)) {
        ndList <- c(ndList,i)
      }
    }
  }
  ndList <- unique(ndList)
  procList <- procList[ndList,]
  if (nrow(procList) != 0) {row.names(procList) <- 1:nrow(procList)}
  return(procList)
}


#################################################################################
#function to assess interannual variability of a gcm against three obs. datasets#
#################################################################################
interannual_vi <- function(this_proc) {
  library(raster); library(rgdal); library(maptools) #package loading
  source(paste(src.dir2,"/scripts/CMIP3-functions.R",sep="")) #source functions
  source(paste(src.dir,"/GHCND-GSOD-functions.R",sep="")) #source functions
  
  #here the process starts for a given country-gcm_ens combination
  iso <- paste(procList$ISO[this_proc])
  reg <- paste(regions$REGION[which(regions$ISO == iso)])
  gcm <- paste(procList$GCM[this_proc])
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",iso,"-",gcm,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  oDir <- paste(outputDD,"/",reg,"/",iso,sep="") #create output directory
  if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}
  procDir <- paste(oDir,"/z.proc",sep="") #output directory for .proc files
  if (!file.exists(procDir)) {dir.create(procDir)}
  
  #load shapefile
  shp <- readShapePoly(paste(admDir,"/",reg,"/",iso,"_adm/",iso,"0.shp",sep=""))
  
  for (vid in 1:3) {
    #vid <- 1
    if (vid == 1) {calc_mean <- F} else {calc_mean <- T}
    vn_gcm <- paste(vnList$GCM[vid]) #variable name
    cat("processing variable",vid,":",vn_gcm,"\n")
    
    procFil <- paste(procDir,"/",vn_gcm,"_",gcm,".proc",sep="") #check file
    if (!file.exists(procFil)) {
      sc_gcm <- scList$GCM[vid]
      tsGCM <- paste(mdDir,"/",gcm,"/yearly_files",sep="")
      
      #here loop through months
      gcm_vals <- list()
      fList_all <- data.frame()
      for (m in 1:12) {
        #m <- 1
        cat(m,". ",sep="")
        if (m < 10) {mth <- paste("0",m,sep="")} else {mth <- paste(m)}
        
        #number of days in month
        if (m == 2) {nd <- 28}
        if (m %in% c(1,3,5,7,8,10,12)) {nd <- 31}
        if (m %in% c(4,6,9,11)) {nd <- 30}
        
        #check GCM rasters exist
        fList <- paste(tsGCM,"/",yi:yf,"/",vn_gcm,"_",mth,".nc",sep="")
        fPres <- as.character(sapply(fList,checkExists))
        fList <- data.frame(YEAR=yi:yf,PRESENT=F,FILE=fPres)
        fList$PRESENT[which(!is.na(fList$FILE))] <- T
        fList$FILE <- paste(fList$FILE)
        fList$FILE[which(!fList$PRESENT)] <- paste(mdDir,"/",gcm,"/multiyr_avgs/1961_2000/prec_01.nc",sep="")
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
          if (vn_gcm == "prec") {
            gcm_vals[[j]][,m] <- gcm_mvals[[j]] * 3600 * 24 * nd
          } else if (vn_gcm == "tmean") {
            gcm_vals[[j]][,m] <- gcm_mvals[[j]] - 273.15
          }
        }
        
        ######extract the cru data
        if (length(which(!fList$PRESENT)) != length(yi:yf)) {
          vn_cru <- paste(vnList$TS_CRU[vid]) #variable name
          sc_cru <- scList$TS_CRU[vid]
          otsCRU <- paste(oDir,"/vi-CRU",sep="") #create output folder
          if (!file.exists(otsCRU)) {dir.create(otsCRU)} #create output folder
          if (!file.exists(paste(otsCRU,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
            cru_data <- stack(paste(tsCRU,"/cru_ts_3_10.1901.2009.",vn_cru,"_",yi:yf,"_",m,".tif",sep="")) #load all cru data
            cru_data <- crop(cru_data,msk) #cut cru data to extent of country mask
            fct <- res(msk)/res(cru_data)
            if (class(try(aggregate(cru_data,fact=fct,fun=mean),silent=T)) != "try-error") {
              cru_data <- aggregate(cru_data,fact=fct,fun=mean) #average onto model grid
            }
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
          otsE40 <- paste(oDir,"/vi-E40",sep="") #create output folder
          if (!file.exists(otsE40)) {dir.create(otsE40)} #create output folder
          if (!file.exists(paste(otsE40,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
            e40_data <- stack(paste(tsE40,"/monthly_data_",vn_e40,"/",yi:yf,"/",vn_e40,"_",m,".tif",sep="")) #load all era40 data
            e40_data <- crop(e40_data,msk) #cut cru data to extent of country mask
            e40_data <- resample(e40_data,msk,method="ngb") #resample to model grid
            e40_data[cellFromXY(e40_data,xyNA)] <- NA #set anything outside the actual country mask to NA
            e40_mvals <- lapply(as.list(xyt),FUN = function(xy,x) {extract(x,cbind(xy[1],xy[2]))},e40_data) #extract values for that month
            if (m == 1) {e40_vals <- gcm_vals}
            for (j in 1:length(e40_vals)) { #assign values to the list
              e40_vals[[j]][,m] <- e40_mvals[[j]]
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
      
      #function for organizing the seasons properly: DJF,MAM,JJA,SON
      seas_organize <- function(x) {
        y <- as.vector(t(x))
        y <- c(NA,y[1:(length(y)-1)])
        y <- matrix(y,ncol=12,nrow=nrow(gcm_vals[[1]]),byrow=T)
        return(y)
      }
      
      if (vid == 1) {
        thresh <- 10000
      } else {
        thresh <- 100
      }
      gcm_check_minmax <- function(x,thresh) {
        y <- as.vector(x)
        y[which(y > thresh)] <- NA
        y <- matrix(y,ncol=12,nrow=nrow(gcm_vals[[1]]),byrow=F)
        return(y)
      }
      gcm_vals <- lapply(gcm_vals,FUN= gcm_check_minmax,thresh) #fixing the no data problem
      gcm_vals <- lapply(gcm_vals,FUN= seas_organize) #re-ordering
      gcm_vals <- lapply(gcm_vals,FUN= function(x,sc) {x * sc},sc_gcm) #scaling
      
      ######extract the wst historical data (does not need to be monthly looped)
      vn_wst <- paste(vnList$TS_WST[vid]) #variable name
      sc_wst <- scList$TS_WST[vid]
      otsWST <- paste(oDir,"/vi-WST",sep="") #create output folder
      if (!file.exists(otsWST)) {dir.create(otsWST)} #create output folder
      if (!file.exists(paste(otsWST,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
        vn_wst <- paste(vnList$TS_WST[vid])
        sc_wst <- scList$TS_WST[vid]
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
            if (length(wst_data2) == 0) {
              wst_data2 <- as.data.frame(cbind(xyMatch$CELL,matrix(NA,nrow=nrow(xyMatch),ncol=12)))
            }
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
        wst_vals <- lapply(wst_vals,FUN= seas_organize) #re-ordering
        wst_vals <- lapply(wst_vals,FUN= function(x,sc) {x * sc},sc_wst) #scaling
        
        s_skill <- mapply(FUN= function(obs,gcm,mn) {seasonal_vi(obs,gcm,mn)},wst_vals,gcm_vals,MoreArgs=list(calc_mean),USE.NAMES=T) #assess seasonal skill using the two matrices
        s_skill <- as.data.frame(s_skill) #do some transformation to the output of the above
        s_skill <- lapply(s_skill,FUN= function(x) {as.data.frame(x)}) #do some transformation to the output of the above
        os_skill <- data.frame() #get everything into a nice data.frame that can be writen
        for (j in 1:length(s_skill)) {
          j_skill <- cbind(CELL=names(s_skill)[j],s_skill[[j]])
          os_skill <- rbind(os_skill,j_skill)
        }
        xyMatch <- xy; xyMatch$CELL <- paste("C",1:nrow(xyMatch),sep="")
        os_skill <- merge(os_skill,xyMatch,by="CELL",sort=F)
        os_skill <- os_skill[,c(1,4,5,2:3)]
        names(os_skill)[2:3] <- c("LON","LAT")
        write.csv(os_skill,paste(otsWST,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
      }
      
      #calculating with cru time series
      if (!file.exists(paste(otsCRU,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
        cru_vals <- lapply(cru_vals,FUN= seas_organize) #re-ordering
        cru_vals <- lapply(cru_vals,FUN= function(x,sc) {x * sc},sc_cru) #scaling
        s_skill <- mapply(FUN= function(obs,gcm,mn) {seasonal_vi(obs,gcm,mn)},cru_vals,gcm_vals,MoreArgs=list(calc_mean),USE.NAMES=T) #assess seasonal skill using the two matrices
        s_skill <- as.data.frame(s_skill) #do some transformation to the output of the above
        s_skill <- lapply(s_skill,FUN= function(x) {as.data.frame(x)}) #do some transformation to the output of the above
        os_skill <- data.frame() #get everything into a nice data.frame that can be writen
        for (j in 1:length(s_skill)) {
          j_skill <- cbind(CELL=names(s_skill)[j],s_skill[[j]])
          os_skill <- rbind(os_skill,j_skill)
        }
        xyMatch <- xy; xyMatch$CELL <- paste("C",1:nrow(xyMatch),sep="")
        os_skill <- merge(os_skill,xyMatch,by="CELL",sort=F)
        os_skill <- os_skill[,c(1,4,5,2:3)]
        names(os_skill)[2:3] <- c("LON","LAT")
        write.csv(os_skill,paste(otsCRU,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
      }
      
      #calculating with ERA-40 time series
      if (!file.exists(paste(otsE40,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
        e40_vals <- lapply(e40_vals,FUN= seas_organize) #re-ordering
        e40_vals <- lapply(e40_vals,FUN= function(x,sc) {x * sc},sc_e40) #scaling
        s_skill <- mapply(FUN= function(obs,gcm,mn) {seasonal_vi(obs,gcm,mn)},e40_vals,gcm_vals,MoreArgs=list(calc_mean),USE.NAMES=T) #assess seasonal skill using the two matrices
        s_skill <- as.data.frame(s_skill) #do some transformation to the output of the above
        s_skill <- lapply(s_skill,FUN= function(x) {as.data.frame(x)}) #do some transformation to the output of the above
        os_skill <- data.frame() #get everything into a nice data.frame that can be writen
        for (j in 1:length(s_skill)) {
          j_skill <- cbind(CELL=names(s_skill)[j],s_skill[[j]])
          os_skill <- rbind(os_skill,j_skill)
        }
        xyMatch <- xy; xyMatch$CELL <- paste("C",1:nrow(xyMatch),sep="")
        os_skill <- merge(os_skill,xyMatch,by="CELL",sort=F)
        os_skill <- os_skill[,c(1,4,5,2:3)]
        names(os_skill)[2:3] <- c("LON","LAT")
        write.csv(os_skill,paste(otsE40,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
      }
      
      pfx <- file(procFil,"w")
      cat("processed on",date(),"by",paste(as.data.frame(t(Sys.info()))$login),"@",
          paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=pfx)
      close(pfx)
    }
  }
  return(oDir)
}



##############################################################################
#### function to assess mean climate of a gcm against four obs. datasets######
#### revised version for ERL paper resubmission
#### added calculation of rmse/sigma
##############################################################################
mean_climate_skill <- function(this_proc) {
  library(raster); library(rgdal); library(maptools) #package loading
  source(paste(src.dir2,"/scripts/CMIP3-functions.R",sep="")) #source functions
  source(paste(src.dir,"/GHCND-GSOD-functions.R",sep="")) #source functions
  
  #here the process starts for a given country-gcm_ens combination
  iso <- paste(procList$ISO[this_proc])
  reg <- paste(regions$REGION[which(regions$ISO == iso)])
  gcm <- paste(procList$GCM[this_proc])
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("process started for",iso,"-",gcm,"\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  oDir <- paste(outputDD,"/",reg,"/",iso,sep="") #create output directory
  if (!file.exists(oDir)) {dir.create(oDir,recursive=T)}
  procDir <- paste(oDir,"/x.proc",sep="") #output directory for .proc files
  if (!file.exists(procDir)) {dir.create(procDir)}
  
  #load shapefile
  shp <- readShapePoly(paste(admDir,"/",reg,"/",iso,"_adm/",iso,"0.shp",sep=""))
  
  #load GCM climatology data
  for (vid in 1:3) {
    #vid <- 1
    if (vid == 1) {calc_mean <- F} else {calc_mean <- T}
    vn_gcm <- paste(vnList$GCM[vid]) #variable name
    cat("processing variable",vid,":",vn_gcm,"\n")
    
    procFil <- paste(procDir,"/",vn_gcm,"_",gcm,".proc",sep="") #check file
    if (!file.exists(procFil)) {
      sc_gcm <- scList$GCM[vid]
      clGCM <- paste(mdDir,"/",gcm,"/multiyr_avgs/1961_2000",sep="")
      fList <- list.files(clGCM,pattern=paste(vn_gcm,"_",sep=""))
      if (length(fList)==0) {
        fPres <- fList
      } else {
        fPres <- as.character(sapply(paste(clGCM,"/",fList,sep=""),checkExists))
      }
      
      if (length(fPres) != 0) {
        #gcm data loading
        gcm_data <- stack(fPres) #load all GCM data
        gcm_data <- rotate(gcm_data) #rotate the GCM data so that it matches the -180 to 180 system
        msk <- createMask(shp,gcm_data[[1]]) #create a mask with the shapefile with resolution of gcm
        gcm_data <- crop(gcm_data,msk) #cut gcm data to extent of country mask
        xyNA <- xyFromCell(msk,which(is.na(msk[]))) #get the locations that are NA in the mask
        xy <- as.data.frame(xyFromCell(msk,which(!is.na(msk[])))) #which gridcells are to be calculated
        gcm_data[cellFromXY(gcm_data,xyNA)] <- NA #set anything outside the actual country mask to NA
        gcm_vals <- extract(gcm_data,xy) #extract GCM data
        
        #transform kg*m-2*s-1 to mm/month
        #1 kg*m-2*s-1 = 3600 * 24 * NDM mm/mont
        #NDM: number of days in month
        if (vn_gcm == "prec") {
          for (mm in 1:12) {
            if (mm == 2) {nd <- 28}
            if (mm %in% c(1,3,5,7,8,10,12)) {nd <- 31}
            if (mm %in% c(4,6,9,11)) {nd <- 30}
            gcm_vals[,mm] <- gcm_vals[,mm] * 3600 * 24 * nd
          }
        } else if (vn_gcm == "tmean") {
          #transform K to oC
          gcm_vals <- gcm_vals - 273.15
        }
        
        gcm_vals <- gcm_vals[,c(12,1:11)] * sc_gcm #re-order and scale
        
        #### worldclim data
        vn_wcl <- paste(vnList$WCL[vid]) #variable name
        sc_wcl <- scList$WCL[vid]
        oclWCL <- paste(oDir,"/cl-WCL",sep="") #create output folder
        if (!file.exists(oclWCL)) {dir.create(oclWCL)} #create output folder
        if (!file.exists(paste(oclWCL,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          wcl_data <- stack(paste(clWCL,"/",vn_wcl,"_",1:12,".tif",sep="")) #load all worldclim data
          wcl_data <- crop(wcl_data,msk) #cut worldclim data to extent of country mask
          wcl_vals <- sapply(1:12,mean_high_res_stk,xy,wcl_data) #average of worldclim onto model grid
          wcl_vals <- wcl_vals[,c(12,1:11)] * sc_wcl #re-order and scale
          s_skill <- seasonal_skill(wcl_vals,gcm_vals,calc_mean) #assess seasonal skill using the two matrices
          write.csv(s_skill,paste(oclWCL,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
          rm(wcl_data); g=gc()
        }
        
        #### cru data 
        vn_cru <- paste(vnList$CL_CRU[vid]) #variable name
        sc_cru <- scList$CL_CRU[vid]
        oclCRU <- paste(oDir,"/cl-CRU",sep="") #create output folder
        if (!file.exists(oclCRU)) {dir.create(oclCRU)} #create output folder
        if (!file.exists(paste(oclCRU,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          cru_data <- stack(paste(clCRU,"/",vn_cru,"_",1:12,".tif",sep="")) #load all cru data
          cru_data <- crop(cru_data,msk) #cut cru data to extent of country mask
          cru_vals <- sapply(1:12,mean_high_res_stk,xy,cru_data) #average of cru onto model grid
          cru_vals <- cru_vals[,c(12,1:11)] * sc_cru #re-order and scale
          s_skill <- seasonal_skill(cru_vals,gcm_vals,calc_mean) #assess seasonal skill using the two matrices
          write.csv(s_skill,paste(oclCRU,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
          rm(cru_data); g=gc()
        }
        
        #### era-40 data
        vn_e40 <- paste(vnList$E40[vid])
        sc_e40 <- scList$E40[vid]
        oclE40 <- paste(oDir,"/cl-E40",sep="") #create output folder
        if (!file.exists(oclE40)) {dir.create(oclE40)} #create output folder
        if (!file.exists(paste(oclE40,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          e40_data <- stack(paste(clE40,"/climatology_data_",vn_e40,"/",vn_e40,"_",1:12,".tif",sep="")) #load all era40 data
          e40_data <- crop(e40_data,msk) #cut era40 data to extent of country mask
          e40_data <- resample(e40_data,msk,method="ngb")
          e40_data[cellFromXY(e40_data,xyNA)] <- NA #set anything outside the actual country mask to NA
          e40_vals <- extract(e40_data,xy) #extract era40 data
          e40_vals <- e40_vals[,c(12,1:11)] * sc_e40 #re-order and scale
          s_skill <- seasonal_skill(e40_vals,gcm_vals,calc_mean) #assess seasonal skill using the two matrices
          write.csv(s_skill,paste(oclE40,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
          rm(e40_data); g=gc()
        }
        
        #### clWST
        vn_wst <- paste(vnList$CL_WST[vid])
        sc_wst <- scList$CL_WST[vid]
        oclWST <- paste(oDir,"/cl-WST",sep="") #create output folder
        if (!file.exists(oclWST)) {dir.create(oclWST)} #create output folder
        if (!file.exists(paste(oclWST,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          wst_raw <- read.dbf(paste(clWST,"/wc_",vn_wst,"_stations.dbf",sep=""))
          wst_raw <- wst_raw[which(!is.na(wst_raw$NYEARS)),]
          wst_raw <- wst_raw[which(wst_raw$NYEARS >= 10),]
          wst_raw$ISIN <- extract(msk,cbind(x=wst_raw$LONG,y=wst_raw$LAT))
          wst_raw <- wst_raw[which(!is.na(wst_raw$ISIN)),]
          wst_raw$ISIN <- NULL
          wst_raw$CELL <- cellFromXY(msk,cbind(x=wst_raw$LONG,y=wst_raw$LAT))
          wst_data <- cbind(wst_raw[,c("LONG","LAT",toupper(month.abb),"CELL")])
          wst_data <- as.data.frame(t(sapply(unique(wst_data$CELL),getMean_points,wst_data)))
          names(wst_data) <- c("CELL",toupper(month.abb))
          xyMatch <- xy; xyMatch$CELL <- cellFromXY(msk,cbind(x=xy$x,y=xy$y))
          wst_data <- merge(xyMatch,wst_data,by="CELL",all=T)
          wst_vals <- as.matrix(wst_data[,toupper(month.abb)])
          wst_vals <- wst_vals[,c(12,1:11)] * sc_wst #reorder and scale
          s_skill <- seasonal_skill(wst_vals,gcm_vals,calc_mean) #assess seasonal skill using the two matrices
          write.csv(s_skill,paste(oclWST,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
          rm(wst_raw); g=gc()
        }
        
      } else {
        dum_vals <- matrix(data=NA,nrow=20,ncol=12)
        s_skill <- seasonal_skill(dum_vals,dum_vals)
        
        oclWCL <- paste(oDir,"/cl-WCL",sep="") #create output folder
        if (!file.exists(oclWCL)) {dir.create(oclWCL)} #create output folder
        if (!file.exists(paste(oclWCL,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          write.csv(s_skill,paste(oclWCL,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
        }
        
        oclCRU <- paste(oDir,"/cl-CRU",sep="") #create output folder
        if (!file.exists(oclCRU)) {dir.create(oclCRU)} #create output folder
        if (!file.exists(paste(oclCRU,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          write.csv(s_skill,paste(oclCRU,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
        }
        
        oclWST <- paste(oDir,"/cl-WST",sep="") #create output folder
        if (!file.exists(oclWST)) {dir.create(oclWST)} #create output folder
        if (!file.exists(paste(oclWST,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          write.csv(s_skill,paste(oclWST,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
        }
        
        oclE40 <- paste(oDir,"/cl-E40",sep="") #create output folder
        if (!file.exists(oclE40)) {dir.create(oclE40)} #create output folder
        if (!file.exists(paste(oclE40,"/",vn_gcm,"_",gcm,".csv",sep=""))) {
          write.csv(s_skill,paste(oclE40,"/",vn_gcm,"_",gcm,".csv",sep=""),row.names=F,quote=T) #write output
        }
      }
      
      pfx <- file(procFil,"w")
      cat("processed on",date(),"by",paste(as.data.frame(t(Sys.info()))$login),"@",
          paste(as.data.frame(t(Sys.info()))$nodename),"\n",file=pfx)
      close(pfx)
      
    }
  }
  return(oDir)
}




#### calculate seasonal skill for two matrices of data, either being pixels
#### of a given domain, or being months of a given time series
#### note that col 1 in matrix is december, and last is november
###########################################################################
###########################################################################
seasonal_skill <- function(obs_vals,gcm_vals,calc_mean=T) {
  #now calculate the metrics for the seasons and whole year
  djf <- data.frame(OBS=(obs_vals[,1]+obs_vals[,2]+obs_vals[,3]),
                    GCM=(gcm_vals[,1]+gcm_vals[,2]+gcm_vals[,3]))
  mam <- data.frame(OBS=(obs_vals[,4]+obs_vals[,5]+obs_vals[,6]),
                    GCM=(gcm_vals[,4]+gcm_vals[,5]+gcm_vals[,6]))
  jja <- data.frame(OBS=(obs_vals[,7]+obs_vals[,8]+obs_vals[,9]),
                    GCM=(gcm_vals[,7]+gcm_vals[,8]+gcm_vals[,9]))
  son <- data.frame(OBS=(obs_vals[,10]+obs_vals[,11]+obs_vals[,12]),
                    GCM=(gcm_vals[,10]+gcm_vals[,11]+gcm_vals[,12]))
  ann <- data.frame(OBS=rowSums(obs_vals),GCM=rowSums(gcm_vals))
  
  if (calc_mean) {
    djf <- djf / 3; mam <- mam / 3; jja <- jja / 3; son <- son / 3
    ann <- ann / 12
  }
  
  #calculate individual season skill metrics
  djf_mx <- calc_metrics(djf)
  mam_mx <- calc_metrics(mam)
  jja_mx <- calc_metrics(jja)
  son_mx <- calc_metrics(son)
  ann_mx <- calc_metrics(ann)
  
  #final output matrix
  all_mx <- rbind(djf_mx,mam_mx,jja_mx,son_mx,ann_mx)
  row.names(all_mx) <- 1:5
  all_mx <- cbind(SEASON=c("DJF","MAM","JJA","SON","ANN"),all_mx)
  return(all_mx)
}


#### calculate seasonal variability index for two matrices of data, 
#### for a given pixel each row being a year of a given time series
#### note that col 1 in matrix is december, and last is november
###########################################################################
###########################################################################
seasonal_vi <- function(obs_vals,gcm_vals,calc_mean=T) {
  #now calculate the metrics for the seasons and whole year
  djf <- data.frame(OBS=(obs_vals[,1]+obs_vals[,2]+obs_vals[,3]),
                    GCM=(gcm_vals[,1]+gcm_vals[,2]+gcm_vals[,3]))
  mam <- data.frame(OBS=(obs_vals[,4]+obs_vals[,5]+obs_vals[,6]),
                    GCM=(gcm_vals[,4]+gcm_vals[,5]+gcm_vals[,6]))
  jja <- data.frame(OBS=(obs_vals[,7]+obs_vals[,8]+obs_vals[,9]),
                    GCM=(gcm_vals[,7]+gcm_vals[,8]+gcm_vals[,9]))
  son <- data.frame(OBS=(obs_vals[,10]+obs_vals[,11]+obs_vals[,12]),
                    GCM=(gcm_vals[,10]+gcm_vals[,11]+gcm_vals[,12]))
  ann <- data.frame(OBS=rowSums(obs_vals),GCM=rowSums(gcm_vals))
  
  if (calc_mean) {
    djf <- djf / 3; mam <- mam / 3; jja <- jja / 3; son <- son / 3
    ann <- ann / 12
  }
  
  #calculate individual season skill metrics
  djf_vi <- ((sd(djf$GCM,na.rm=T)/sd(djf$OBS,na.rm=T)) - (sd(djf$OBS,na.rm=T)/sd(djf$GCM,na.rm=T)))^2
  djf_mx <- data.frame(VI=djf_vi)
  
  mam_vi <- ((sd(mam$GCM,na.rm=T)/sd(mam$OBS,na.rm=T)) - (sd(mam$OBS,na.rm=T)/sd(mam$GCM,na.rm=T)))^2
  mam_mx <- data.frame(VI=mam_vi)
  
  jja_vi <- ((sd(jja$GCM,na.rm=T)/sd(jja$OBS,na.rm=T)) - (sd(jja$OBS,na.rm=T)/sd(jja$GCM,na.rm=T)))^2
  jja_mx <- data.frame(VI=jja_vi)
  
  son_vi <- ((sd(son$GCM,na.rm=T)/sd(son$OBS,na.rm=T)) - (sd(son$OBS,na.rm=T)/sd(son$GCM,na.rm=T)))^2
  son_mx <- data.frame(VI=son_vi)
  
  ann_vi <- ((sd(ann$GCM,na.rm=T)/sd(ann$OBS,na.rm=T)) - (sd(ann$OBS,na.rm=T)/sd(ann$GCM,na.rm=T)))^2
  ann_mx <- data.frame(VI=ann_vi)
  
  #final output matrix
  all_mx <- rbind(djf_mx,mam_mx,jja_mx,son_mx,ann_mx)
  row.names(all_mx) <- 1:5
  all_mx <- cbind(SEASON=c("DJF","MAM","JJA","SON","ANN"),all_mx)
  return(all_mx)
}


###########################################################################
###########################################################################
###calculate metrics for a given matrix of two columns
###revised version for NCC paper
calc_metrics <- function(vals) {
  #remove any column with NA
  vals <- vals[which(!is.na(vals$GCM)),]
  vals <- vals[which(is.finite(vals$GCM)),]
  vals <- vals[which(!is.na(vals$OBS)),]
  vals <- vals[which(is.finite(vals$OBS)),]
  
  #Check if vals has any of its columns with full zeros
  nz.GCM <- length(which(vals$GCM == 0))
  nz.OBS <- length(which(vals$OBS == 0))
  
  #Check unique values in vals columns
  uv.GCM <- length(unique(vals$GCM))
  uv.OBS <- length(unique(vals$OBS))
  
  if (nrow(vals) > 0) {
    if (nz.GCM == nrow(vals) | nz.OBS == nrow(vals) | uv.GCM == 1 | uv.OBS == 1) {
      lfit <- lm(vals$OBS ~ vals$GCM - 1) #Fit forced to origin
      pval <- NA
      ccoef2 <- NA
      pval2 <- NA
    } else {
      lfit <- lm(vals$OBS~vals$GCM-1)
      pval <- pf(summary(lfit)$fstatistic[1],summary(lfit)$fstatistic[2],summary(lfit)$fstatistic[3],lower.tail=F)
      
      #calculate ccoef2 only if 3 or more observations
      if (nrow(vals) < 3) {
        ccoef2 <- NA
        pval2 <- NA
      } else {
        ccoef2 <- cor.test(vals$OBS,vals$GCM,method="pearson")$estimate
        pval2 <- cor.test(vals$OBS,vals$GCM,method="pearson")$p.value
      }
      
    }
    ccoef <- lfit$coefficients * sqrt(vals$GCM %*% vals$GCM) / sqrt(vals$OBS %*% vals$OBS)
    rsq <- summary(lfit)$r.squared
    rsq2 <- ccoef2^2
    mbr <- as.numeric(lfit$coefficients)
    rmse <- sqrt(sum((vals$OBS-vals$GCM)^2)/nrow(vals))
    prmse1 <- rmse/mean(vals$OBS,na.rm=T)*100
    prmse2 <- rmse/mean(vals$GCM,na.rm=T)*100
    prmse3 <- rmse/sd(vals$OBS,na.rm=T)*100
    prmse4 <- rmse/sd(vals$GCM,na.rm=T)*100
    mean_obs <- mean(vals$OBS,na.rm=T)
    mean_gcm <- mean(vals$GCM,na.rm=T)
    std_obs <- sd(vals$OBS,na.rm=T)
    std_gcm <- sd(vals$GCM,na.rm=T)
  } else {
    pval <- NA; ccoef <- NA; rsq <- NA; pval2 <- NA; ccoef2 <- NA; rsq2 <- NA
    mbr <- NA; rmse <- NA; prmse1 <- NA; prmse2 <- NA; prmse3 <- NA; prmse4 <- NA
    mean_obs <- NA; mean_gcm <- NA; std_obs <- NA; std_gcm <- NA
  }
  npts <- nrow(vals)
  
  out_df <- data.frame(CCOEF=ccoef,PVAL=pval,RSQ=rsq,CCOEF2=ccoef2,PVAL2=pval2,RSQ2=rsq2,
                       MBR=mbr,RMSE=rmse,PRMSE1=prmse1,PRMSE2=prmse2,PRMSE3=prmse3,
                       PRMSE4=prmse4,mean_OBS=mean_obs,mean_GCM=mean_gcm,std_OBS=std_obs,
                       std_GCM=std_gcm,N=npts)
  return(out_df)
}


####function to calculate the means of points for a given gridcell
####to be used with *apply
getMean_points <- function(i,vals) {
  cellVals <- vals[which(vals$CELL==i),c(toupper(month.abb))]
  mv <- cbind(CELL=i,t(colMeans(cellVals,na.rm=T)))
  return(mv)
}


###function to calculate the means of high resolution cells for
###a number of rasters in a rasterstack
mean_high_res_stk <- function(m,xy,wcl_data) {
  rs <- wcl_data[[m]]
  new_vals <- sapply(1:nrow(xy),getMean_cells,xy,rs)
  return(new_vals)
}

###function to calculate the mean of a number of gridcells that are
###inside a coarse gridcell
getMean_cells <- function(i,xy,rs) {
  xrs <- xres(rs); yrs <- yres(rs)
  xt <- extent(xy$x[i]-xrs/2,xy$x[i]+xrs/2,xy$y[i]-yrs/2,xy$y[i]+yrs/2)
  rs_cell <- crop(rs,xt)
  mv <- mean(rs_cell[],na.rm=T)
  return(mv)
}

#Function to create a raster with a desired resolution from a shapefile
createMask <- function(shp, rs) { #Function to create a mask
  #Original bounding box
  xres <- xres(rs)
  yres <- yres(rs)
  xn <- shp@bbox[1,1] - 2*xres
  xx <- shp@bbox[1,2] + 2*xres
  yn <- shp@bbox[2,1] - 2*yres
  yx <- shp@bbox[2,2] + 2*yres
  
  #Modified bounding box to match resolution
  disx <- round(xx - xn)
  nc <- round(disx/xres)
  disx <- nc*xres
  xx <- xn + disx
  
  disy <- round(yx-yn)
  nr <- round(disy/yres)
  disy <- nr*yres
  yx <- yn + disy
  
  #Create and rasterize the shapefile
  xt <- extent(c(xn,xx,yn,yx))
  rs <- crop(rs,xt); rs[] <- 1
  rs <- rasterize(shp, rs, silent=T, getCover=T)
  rs[which(rs[] == 0)] <- NA; rs[which(!is.na(rs[]))] <- 1
  return(rs)
}





##############################################################################
##############################################################################
#Function to process everything into a folder
AsctoGTiff <- function(this_dir,keepASC=F) {
  ascList <- list.files(this_dir,pattern="\\.asc")
  if (length(grep("\\.gz",ascList)) > 0) {
    ascList <- ascList[-grep("\\.gz",ascList)]
  }
  
  if (length(ascList) == 0) {
    cat("This folder does not contain any raw ascii grid \n")
  } else {
    for (asc in ascList) {
      cat(asc,"\n")
      rs <- raster(paste(this_dir,"/",asc,sep=""))
      tifName <- gsub(".asc",".tif",asc)
      
      if (!file.exists(paste(this_dir,"/",tifName,sep=""))) {
        rs <- writeRaster(rs,paste(this_dir,"/",tifName,sep=""),format="GTiff")
      }
      
      if (keepASC) {
        if (!file.exists(paste(this_dir,"/",asc,".gz",sep=""))) {
          setwd(this_dir)
          system(paste("7z a -tgzip",paste(asc,".gz",sep=""),asc))
        }
      }
      
      if (file.exists(paste(this_dir,"/",asc,sep=""))) {
        if (file.exists(paste(this_dir,"/",tifName,sep=""))) {
          if (keepASC) {
            if (file.exists(paste(this_dir,"/",asc,".gz",sep=""))) {
              x <- file.remove(paste(this_dir,"/",asc,sep=""))
            }
          } else {
            x <- file.remove(paste(this_dir,"/",asc,sep=""))
          }
        }
      }
    }
  }
  return("Done!")
}



########################################################
#check if a file exists, to be used in an "*apply" command
checkExists <- function(x) {
  if (file.exists(x)) {y <- x} else {y <- NA}
  return(y)
}


