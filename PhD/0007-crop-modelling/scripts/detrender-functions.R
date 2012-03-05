#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL

#Functions for yield data detrending
###############################################################################
#Control function for parallelisation
#Note: some objects need to be exported to nodes before running this
controlYear <- function(year) {
  library(raster)
  cat("Processing year",(1900+year),"\n")
  outName <- paste(outDataDir,"/",dataType,"-",year,".asc",sep="")
  if (!file.exists(outName)) {
    yr_rs <- createYearRaster(inyData,rk,year,"Y","DISID")
    yr_rs <- writeRaster(yr_rs,outName,format="ascii")
    rm(yr_rs); g=gc(); rm(g)
    cat("Done\n")
  } else {
    cat("File already exists \n")
  }
}


##############################################################################
#Function to create a raster layer out from the district-level yield data for a given year
createYearRaster <- function(xData,dis_rs,ye,prefix,IDField) {
  yField <- paste(prefix,ye,sep="")
  out_rs <- raster(dis_rs)
  in_dat <- data.frame(xData[,IDField],xData[,yField])
  
  disCount <- 1
  for (dis in unique(xData[,IDField])) {
    cat("Processing district",dis,"\n")
    out_rs[which(dis_rs[]==dis)] <- xData[disCount,yField]
    disCount <- disCount+1
  }
  out_rs[which(out_rs[]==-9999)] <- NA
  return(out_rs)
}


##########################################################################################
##########################################################################################
#Function to create rasters out from the summary district-level statistics
createSummaryRasters <- function(msk_rs,dis_rs,stats,IDField,cDir) {
  #make rasters to fill in
  harea.mean <- raster(msk_rs); harea.sd <- raster(msk_rs)
  tprod.mean <- raster(msk_rs); tprod.sd <- raster(msk_rs)
  yield.raw.mean <- raster(msk_rs); yield.raw.sd <- raster(msk_rs)
  yield.loe.mean <- raster(msk_rs); yield.loe.sd <- raster(msk_rs)
  yield.lin.mean <- raster(msk_rs); yield.lin.sd <- raster(msk_rs)
  yield.qua.mean <- raster(msk_rs); yield.qua.sd <- raster(msk_rs)
  yield.fou.mean <- raster(msk_rs); yield.fou.sd <- raster(msk_rs)
  
  #get district list
  districts <- unique(stats[,IDField])
  
  #loop through districts to produce maps of all summary metrics
  for (j in 1:length(districts)) {
    dis <- districts[j]
    cat("Processing district",dis,"(",j"of",length(districts),")","\n")
    harea.mean[which(rk[]==dis)] <- stats$HAREA_MEAN[j]
    harea.sd[which(rk[]==dis)] <- stats$HAREA_SD[j]
    tprod.mean[which(rk[]==dis)] <- stats$TPROD_MEAN[j]
    tprod.sd[which(rk[]==dis)] <- stats$TPROD_SD[j]
    yield.raw.mean[which(rk[]==dis)] <- stats$YIELD_RAW_MEAN[j]
    yield.raw.sd[which(rk[]==dis)] <- stats$YIELD_RAW_SD[j]
    yield.loe.mean[which(rk[]==dis)] <- stats$YIELD_LOE_MEAN[j]
    yield.loe.sd[which(rk[]==dis)] <- stats$YIELD_LOE_SD[j]
    yield.lin.mean[which(rk[]==dis)] <- stats$YIELD_LIN_MEAN[j]
    yield.lin.sd[which(rk[]==dis)] <- stats$YIELD_LIN_SD[j]
    yield.qua.mean[which(rk[]==dis)] <- stats$YIELD_QUA_MEAN[j]
    yield.qua.sd[which(rk[]==dis)] <- stats$YIELD_QUA_SD[j]
    yield.fou.mean[which(rk[]==dis)] <- stats$YIELD_FOU_MEAN[j]
    yield.fou.sd[which(rk[]==dis)] <- stats$YIELD_FOU_SD[j]
  }
  outSumDir <- paste(cDir,"/raster/summaries",sep="")
  if (!file.exists(outSumDir)) {dir.create(outSumDir)}
  
  #set the data correctly
  harea.mean[which(harea.mean[]==-9999)] <- NA
  harea.sd[which(harea.sd[]==-9999)] <- NA
  tprod.mean[which(tprod.mean[]==-9999)] <- NA
  tprod.sd[which(tprod.sd[]==-9999)] <- NA
  yield.raw.mean[which(yield.raw.mean[]==-9999)] <- NA
  yield.raw.sd[which(yield.raw.sd[]==-9999)] <- NA
  yield.loe.mean[which(yield.loe.mean[]==-9999)] <- NA
  yield.loe.sd[which(yield.loe.sd[]==-9999)] <- NA
  yield.lin.mean[which(yield.lin.mean[]==-9999)] <- NA
  yield.lin.sd[which(yield.lin.sd[]==-9999)] <- NA
  yield.qua.mean[which(yield.qua.mean[]==-9999)] <- NA
  yield.qua.sd[which(yield.qua.sd[]==-9999)] <- NA
  yield.fou.mean[which(yield.fou.mean[]==-9999)] <- NA
  yield.fou.sd[which(yield.fou.mean[]==-9999)] <- NA
  
  #Write individual rasters
  harea.mean <- writeRaster(harea.mean,paste(outSumDir,"/harea-mean.asc",sep=""),format="ascii")
  harea.sd <- writeRaster(harea.sd,paste(outSumDir,"/harea-sd.asc",sep=""),format="ascii")
  tprod.mean <- writeRaster(tprod.mean,paste(outSumDir,"/tprod-mean.asc",sep=""),format="ascii")
  tprod.sd <- writeRaster(tprod.sd,paste(outSumDir,"/tprod-sd.asc",sep=""),format="ascii")
  yield.raw.mean <- writeRaster(yield.raw.mean,paste(outSumDir,"/yield-raw-mean.asc",sep=""),format="ascii")
  yield.raw.sd <- writeRaster(yield.raw.sd,paste(outSumDir,"/yield-raw-sd.asc",sep=""),format="ascii")
  yield.loe.mean <- writeRaster(yield.loe.mean,paste(outSumDir,"/yield-loe-mean.asc",sep=""),format="ascii")
  yield.loe.sd <- writeRaster(yield.loe.sd,paste(outSumDir,"/yield-loe-sd.asc",sep=""),format="ascii")
  yield.lin.mean <- writeRaster(yield.lin.mean,paste(outSumDir,"/yield-lin-mean.asc",sep=""),format="ascii")
  yield.lin.sd <- writeRaster(yield.lin.sd,paste(outSumDir,"/yield-lin-sd.asc",sep=""),format="ascii")
  yield.qua.mean <- writeRaster(yield.qua.mean,paste(outSumDir,"/yield-qua-mean.asc",sep=""),format="ascii")
  yield.qua.sd <- writeRaster(yield.qua.sd,paste(outSumDir,"/yield-qua-sd.asc",sep=""),format="ascii")
  yield.fou.mean <- writeRaster(yield.fou.mean,paste(outSumDir,"/yield-fou-mean.asc",sep=""),format="ascii")
  yield.fou.sd <- writeRaster(yield.fou.sd,paste(outSumDir,"/yield-fou-sd.asc",sep=""),format="ascii")
  return("Done!")
}


################################################################################
################################################################################
#calculate average historical area and other statistics
calcSummary <- function(yield,IDField,yFields,hFields,pFields,yrSeries,cDir,cropName,raw,loe,lin,qua,fou) {
  districts <- unique(yield[,IDField])
  for (j in 1:length(districts)) {
    dis <- districts[j]
    cat("Processing district",dis,"\n")
    rowData <- yield[which(yield[,IDField]==dis),]
    hdat <- data.frame(rep(dis,times=length(hFields)),yrSeries,t(rowData[,hFields]))
    names(hdat) <- c(IDField,"YEAR","HAREA"); row.names(hdat) <- 1:nrow(hdat)
    
    pdat <- data.frame(DISID=rep(dis,times=length(pFields)),YEAR=yrSeries,t(rowData[,pFields]))
    names(pdat) <- c(IDField,"YEAR","TPROD"); row.names(pdat) <- 1:nrow(pdat)
    
    rawdat <- data.frame(DISID=rep(dis,times=length(yFields)),YEAR=yrSeries,t(raw[j,yFields]))
    names(rawdat) <- c(IDField,"YEAR","YIELD"); row.names(rawdat) <- 1:nrow(rawdat)
    
    loedat <- data.frame(DISID=rep(dis,times=length(yFields)),YEAR=yrSeries,t(loe[j,yFields]))
    names(loedat) <- c(IDField,"YEAR","YIELD"); row.names(loedat) <- 1:nrow(loedat)
    
    lindat <- data.frame(DISID=rep(dis,times=length(yFields)),YEAR=yrSeries,t(lin[j,yFields]))
    names(lindat) <- c(IDField,"YEAR","YIELD"); row.names(lindat) <- 1:nrow(lindat)
    
    quadat <- data.frame(DISID=rep(dis,times=length(yFields)),YEAR=yrSeries,t(qua[j,yFields]))
    names(quadat) <- c(IDField,"YEAR","YIELD"); row.names(quadat) <- 1:nrow(quadat)
    
    foudat <- data.frame(DISID=rep(dis,times=length(yFields)),YEAR=yrSeries,t(fou[j,yFields]))
    names(foudat) <- c(IDField,"YEAR","YIELD"); row.names(foudat) <- 1:nrow(foudat)
    
    #remove any -9999 row in harv area and production data
    hdat <- hdat[which(hdat$HAREA!=-9999),]
    pdat <- pdat[which(pdat$TPROD!=-9999),]
    rawdat <- rawdat[which(rawdat$YIELD!=-9999),]
    loedat <- loedat[which(loedat$YIELD!=-9999),]
    lindat <- lindat[which(lindat$YIELD!=-9999),]
    quadat <- quadat[which(quadat$YIELD!=-9999),]
    foudat <- foudat[which(foudat$YIELD!=-9999),]
    
    outRow <- data.frame(DISID=dis,HAREA_MEAN=mean(hdat$HAREA,na.rm=T),HAREA_SD=sd(hdat$HAREA,na.rm=T),
                         TPROD_MEAN=mean(pdat$TPROD,na.rm=T),TPROD_SD=sd(pdat$TPROD,na.rm=T),
                         YIELD_RAW_MEAN=mean(rawdat$YIELD,na.rm=T),YIELD_RAW_SD=sd(rawdat$YIELD,na.rm=T),
                         YIELD_LOE_MEAN=mean(loedat$YIELD,na.rm=T),YIELD_LOE_SD=sd(loedat$YIELD,na.rm=T),
                         YIELD_LIN_MEAN=mean(lindat$YIELD,na.rm=T),YIELD_LIN_SD=sd(lindat$YIELD,na.rm=T),
                         YIELD_QUA_MEAN=mean(quadat$YIELD,na.rm=T),YIELD_QUA_SD=sd(quadat$YIELD,na.rm=T),
                         YIELD_FOU_MEAN=mean(foudat$YIELD,na.rm=T),YIELD_FOU_SD=sd(foudat$YIELD,na.rm=T))
    
    if (j==1) {
      outAll <- outRow
    } else {
      outAll <- rbind(outAll,outRow)
    }
  }
  write.csv(outAll,paste(cDir,"/data/detrended-IND2-",cropName,"-summary.csv",sep=""),row.names=F,quote=F)
  return(outAll)
}



############################################################################
############################################################################
#Wrapper: this would do the yield detrending for all districts
detrendAll <- function(yield,IDField,yFields,iyr,fyr,cDir,cropName) {
  yrSeries <- 1900+(iyr:fyr)
  districts <- unique(yield[,IDField])
  for (j in 1:length(districts)) {
    dis <- districts[j]
    cat("Processing district",dis,"\n")
    rowData <- yield[which(yield[,IDField]==dis),]
    ydat <- data.frame(rep(dis,times=length(yFields)),yrSeries,t(rowData[,yFields]))
    names(ydat) <- c(IDField,"YEAR","YIELD"); row.names(ydat) <- 1:nrow(ydat)
    
    #run the wrapper
    limit <- round(nrow(ydat)*.75)
    x <- detrendWrapper(ydat,dis,lim=limit,iyr,fyr)
    
    if (j==1) {
      raw <- x$RAW
      loe <- x$LOESS
      lin <- x$LINEAR
      qua <- x$QUADRATIC
      fou <- x$FOURIER
    } else {
      raw <- rbind(raw,x$RAW)
      loe <- rbind(loe,x$LOESS)
      lin <- rbind(lin,x$LINEAR)
      qua <- rbind(qua,x$QUADRATIC)
      fou <- rbind(fou,x$FOURIER)
    }
  }
  write.csv(raw,paste(cDir,"/data/detrended-IND2-",cropName,"-raw.csv",sep=""),quote=F,row.names=F)
  write.csv(loe,paste(cDir,"/data/detrended-IND2-",cropName,"-loess.csv",sep=""),quote=F,row.names=F)
  write.csv(lin,paste(cDir,"/data/detrended-IND2-",cropName,"-linear.csv",sep=""),quote=F,row.names=F)
  write.csv(qua,paste(cDir,"/data/detrended-IND2-",cropName,"-quadratic.csv",sep=""),quote=F,row.names=F)
  write.csv(fou,paste(cDir,"/data/detrended-IND2-",cropName,"-fourier.csv",sep=""),quote=F,row.names=F)
  return(paste(cDir,"/data",sep=""))
}



######################################################
#Detrend wrapper to consider different availability cases
detrendWrapper <- function(yd,dis,lim=23,iyr,fyr) {
  #count NAs and zeros
  yd$YIELD[which(yd$YIELD==0)] <- -9999
  zeros <- length(which(yd$YIELD==0))
  nas <- length(which(yd$YIELD==-9999))
  #case 1 the yields are all zero this district has no area of that crop
  if (nas>lim) {
    #do not detrend if there are less than 23 yield records (district is useless for modelling)
    #create data frames for each method and for raw data
    df.rw <- data.frame(DISID=dis,t(yd$YIELD))
    names(df.rw) <- c("DISID",paste("Y",iyr:fyr,sep=""))
    df.lo <- df.rw #loess
    df.li <- df.rw #linear
    df.qa <- df.rw #quadratic
    df.sm <- df.rw #fourier smoothing
  } else if (zeros!=0) {
    #case 2 there are some years with zero yields somewhere in the series
    #these years are removed from the series
    yd2 <- yd[-which(yd$YIELD==0),]
    yd2 <- detrendAllMethods(yd2,eDir,dDir)
    
    #merge back the zero-removed stuff with the original
    yd$LOESS_PRED <- 0; yd$LOESS_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LOESS_PRED
    yd$LM_PRED <- 0; yd$LM_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LM_PRED
    yd$PM_PRED <- 0; yd$PM_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$PM_PRED
    yd$SMTH_PRED <- 0; yd$SMTH_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$SMTH_PRED
    yd$LOESS_ADJ <- 0; yd$LOESS_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LOESS_ADJ
    yd$LM_ADJ <- 0; yd$LM_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LM_ADJ
    yd$PM_ADJ <- 0; yd$PM_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$PM_ADJ
    yd$SMTH_ADJ <- 0; yd$SMTH_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$SMTH_ADJ
    
    #create data frames for each method and for raw data
    df.rw <- data.frame(DISID=dis,t(yd$YIELD)) #raw
    names(df.rw) <- c("DISID",paste("Y",iyr:fyr,sep="")) #raw
    df.lo <- data.frame(DISID=dis,t(yd$LOESS_ADJ)) #loess
    names(df.lo) <- c("DISID",paste("Y",iyr:fyr,sep="")) #loess
    df.li <- data.frame(DISID=dis,t(yd$LM_ADJ)) #linear
    names(df.li) <- c("DISID",paste("Y",iyr:fyr,sep="")) #linear
    df.qa <- data.frame(DISID=dis,t(yd$PM_ADJ)) #quadratic
    names(df.qa) <- c("DISID",paste("Y",iyr:fyr,sep="")) #quadratic
    df.sm <- data.frame(DISID=dis,t(yd$SMTH_ADJ)) #fourier smoothing
    names(df.sm) <- c("DISID",paste("Y",iyr:fyr,sep="")) #fourier smoothing
    
  } else if (nas==nrow(yd)) {
    #case 3 the yields are all NA (-9999) this district I will put NA to everything
    #create data frames for each method and for raw data
    df.rw <- data.frame(DISID=dis,t(rep(NA,times=nrow(yd))))
    names(df.rw) <- c("DISID",paste("Y",iyr:fyr,sep=""))
    df.lo <- df.rw #loess
    df.li <- df.lo #linear
    df.qa <- df.lo #quadratic
    df.sm <- df.lo #fourier smoothing
  } else if (nas != 0) {
    #case 4 there are some years with -9999 yields somewhere in the series
    #these years are removed from the series
    yd2 <- yd[-which(yd$YIELD==-9999),]
    yd2 <- detrendAllMethods(yd2,eDir,dDir)
    
    #merge back the NA-removed stuff with the original
    yd$LOESS_PRED <- -9999; yd$LOESS_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LOESS_PRED
    yd$LM_PRED <- -9999; yd$LM_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LM_PRED
    yd$PM_PRED <- -9999; yd$PM_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$PM_PRED
    yd$SMTH_PRED <- -9999; yd$SMTH_PRED[which(yd$YEAR%in%yd2$YEAR)] <- yd2$SMTH_PRED
    yd$LOESS_ADJ <- -9999; yd$LOESS_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LOESS_ADJ
    yd$LM_ADJ <- -9999; yd$LM_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$LM_ADJ
    yd$PM_ADJ <- -9999; yd$PM_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$PM_ADJ
    yd$SMTH_ADJ <- -9999; yd$SMTH_ADJ[which(yd$YEAR%in%yd2$YEAR)] <- yd2$SMTH_ADJ
    
    #create data frames for each method and for raw data
    df.rw <- data.frame(DISID=dis,t(yd$YIELD)) #raw
    names(df.rw) <- c("DISID",paste("Y",iyr:fyr,sep="")) #raw
    df.lo <- data.frame(DISID=dis,t(yd$LOESS_ADJ)) #loess
    names(df.lo) <- c("DISID",paste("Y",iyr:fyr,sep="")) #loess
    df.li <- data.frame(DISID=dis,t(yd$LM_ADJ)) #linear
    names(df.li) <- c("DISID",paste("Y",iyr:fyr,sep="")) #linear
    df.qa <- data.frame(DISID=dis,t(yd$PM_ADJ)) #quadratic
    names(df.qa) <- c("DISID",paste("Y",iyr:fyr,sep="")) #quadratic
    df.sm <- data.frame(DISID=dis,t(yd$SMTH_ADJ)) #fourier smoothing
    names(df.sm) <- c("DISID",paste("Y",iyr:fyr,sep="")) #fourier smoothing
    
  } else {
    #case 5 the data are all ok
    yd <- detrendAllMethods(yd,eDir,dDir)
    #create data frames for each method and for raw data
    df.rw <- data.frame(DISID=dis,t(yd$YIELD)) #raw
    names(df.rw) <- c("DISID",paste("Y",iyr:fyr,sep="")) #raw
    df.lo <- data.frame(DISID=dis,t(yd$LOESS_ADJ)) #loess
    names(df.lo) <- c("DISID",paste("Y",iyr:fyr,sep="")) #loess
    df.li <- data.frame(DISID=dis,t(yd$LM_ADJ)) #linear
    names(df.li) <- c("DISID",paste("Y",iyr:fyr,sep="")) #linear
    df.qa <- data.frame(DISID=dis,t(yd$PM_ADJ)) #quadratic
    names(df.qa) <- c("DISID",paste("Y",iyr:fyr,sep="")) #quadratic
    df.sm <- data.frame(DISID=dis,t(yd$SMTH_ADJ)) #fourier smoothing
    names(df.sm) <- c("DISID",paste("Y",iyr:fyr,sep="")) #fourier smoothing
  }
  return(list(RAW=df.rw,LOESS=df.lo,LINEAR=df.li,QUADRATIC=df.qa,FOURIER=df.sm))
}


#############################################
#Function to detrend using four different methods (linear, loess, quadratic and fourier smoothing)
detrendAllMethods <- function(yData,emuDir,detDir) {
  #compute lowess
  y.lo <- loess(yData$YIELD~yData$YEAR)
  y.pr <- predict(y.lo,yData$YEAR,se=T) #theoretical prediction
  yData$LOESS_PRED <- y.pr$fit
  
  #compute linear
  y.lf <- lm(yData$YIELD~yData$YEAR)
  y.pr <- predict.lm(y.lf,data=yData$YEAR)
  yData$LM_PRED <- y.pr
  
  #compute polynomial regression
  y.pf <- lm(formula = yData$YIELD ~ poly(yData$YEAR, degree = 2,raw=T))
  y.pr <- predict(y.pf,data=yData$YEAR)
  yData$PM_PRED <- y.pr
  
  #do the fourier smoothing
  #write the data onto a data file
  if (file.exists(paste(detDir,"/OUT.TXT",sep=""))) {x <- file.remove(paste(detDir,"/OUT.TXT",sep=""))}
  write.table(yData$YIELD,paste(detDir,"/data.txt",sep=""),sep="",row.names=F,col.names=F)
  setwd(emuDir)
  system("DOSBox.exe")
  y.pr <- read.table(paste(detDir,"/OUT.TXT",sep="")); names(y.pr) <- "SMTH_PRED"
  yData$SMTH_PRED <- as.vector(y.pr$SMTH_PRED)
  
  # #plot data and regression
  # plot(yData$YEAR,yData$YIELD,pch=20)
  # lines(yData$YEAR,yData$YIELD)
  # lines(yData$YEAR,yData$LOESS_PRED,col="red")
  # lines(yData$YEAR,yData$LM_PRED,col="orange")
  # lines(yData$YEAR,yData$PM_PRED,col="dark green")
  # lines(yData$YEAR,yData$SMTH_PRED,col="blue")
  
  #calculate relative errors and remove trend for each method
  rd <- (yData$YIELD-yData$LOESS_PRED)/yData$LOESS_PRED #loess
  yData$LOESS_ADJ <- (rd+1)*yData$YIELD[nrow(yData)] #loess
  
  rd <- (yData$YIELD-yData$LM_PRED)/yData$LM_PRED #linear
  yData$LM_ADJ <- (rd+1)*yData$YIELD[nrow(yData)] #linear
  
  rd <- (yData$YIELD-yData$PM_PRED)/yData$PM_PRED #quadratic
  yData$PM_ADJ <- (rd+1)*yData$YIELD[nrow(yData)] #quadratic
  
  rd <- (yData$YIELD-yData$SMTH_PRED)/yData$SMTH_PRED #quadratic
  yData$SMTH_ADJ <- (rd+1)*yData$YIELD[nrow(yData)] #quadratic
  
  # #plot detrended data
  # mn <- min(yData[,c("LOESS_ADJ","LM_ADJ","PM_ADJ")])
  # mx <- max(yData[,c("LOESS_ADJ","LM_ADJ","PM_ADJ")])
  # plot(yData$YEAR,yData$LOESS_ADJ,pch=20,ylim=c(mn,mx),
  #      cex=0.8,ylab="Detrended yield (kg/ha)",xlab=NA); 
  # lines(yData$YEAR,yData$LOESS_ADJ)
  # points(yData$YEAR,yData$LM_ADJ,pch=20,col="red",cex=0.8); lines(yData$YEAR,yData$LM_ADJ,col="red")
  # points(yData$YEAR,yData$PM_ADJ,pch=20,col="blue",cex=0.8); lines(yData$YEAR,yData$PM_ADJ,col="blue")
  # points(yData$YEAR,yData$SMTH_ADJ,pch=20,col="orange",cex=0.8); lines(yData$YEAR,yData$SMTH_ADJ,col="orange")
  return(yData)
}
