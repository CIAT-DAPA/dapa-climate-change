#Julian Ramirez-Villegas
#Jan 2012
#functions for PGR paper

#calculate overlap between novel part of a distribution and another distribution
calc_novel_overlap <- function(this_loc,novel_pdf,gcm_outDir,crop_name,period) {
  #1. load output of location
  #location of file in chunks of 10k files. File number limitation
  if (this_loc <= 10000) {
    tl_datFile <- paste(gcm_outDir,"/part_1/loc_",this_loc,".RData",sep="")
  } else if (this_loc > 10000 & loc <= 20000) {
    tl_datFile <- paste(gcm_outDir,"/part_2/loc_",this_loc,".RData",sep="")
  } else if (this_loc > 20000) {
    tl_datFile <- paste(gcm_outDir,"/part_3/loc_",this_loc,".RData",sep="")
  }
  load(tl_datFile)
  
  #2. calculate 1-99% CRU PDF
  t_gsData <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$GS_DATA
  t_qlims <- as.numeric(quantile(t_gsData$CRU,probs=c(0.01,0.99)))
  t_cru_in <- output[[crop_name]][[paste("OVERLAP_",period,sep="")]]$PDF_CRU
  t_cru_in <- t_cru_in[which(t_cru_in$x >= t_qlims[1] & t_cru_in$x <= t_qlims[2]),]
  rm(output)
  
  #3. overlap with novel_pdf
  t_ovr <- novel_pdf[which(novel_pdf$x <= max(t_cru_in$x) & novel_pdf$x >= min(t_cru_in$x)),]
  
  #4. return overlap value
  if (nrow(t_ovr) > 0) {
    t_oa <- integrate.xy(t_ovr$x,t_ovr$y)
  } else {
    t_oa <- 0
  }
  novel_a <- integrate.xy(novel_pdf$x,novel_pdf$y)
  t_frac <- t_oa/novel_a
  return(t_frac)
}


###plotting
plot_overlap <- function(cropname, period, xt=extent(-130,160,-45,75), fig_dir) {
  #load world simpl
  require(maptools); data(wrld_simpl)
  
  #general plot characteristics
  rs <- out_rs[[toupper(cropname)]][[toupper(period)]]
  rs <- crop(rs,xt)
  
  ht <- 1000
  fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
  wt <- ht*(fct+.1)
  wld <- list("sp.polygons",wrld_simpl,lwd=0.8,first=F)
  grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=30), norths=seq(-90,90,by=30))
  grli <- list("sp.lines",grat,lwd=0.5,lty=2,first=F)
  
  
  ############################################
  brks <- seq(0,1,by=0.025)
  brks.lab <- round(brks,2)
  cols <- c(colorRampPalette(c("dark red","red","orange","yellow","light blue","blue","dark blue"))(length(brks)))
  layt <- list(wld,grli)
  
  #do the plot
  figName <- paste(tolower(cropname),"_",tolower(period),sep="")
  tiffName <- paste(fig_dir,"/",figName,".tif",sep="")
  tiff(tiffName,res=300,compression="lzw",height=ht,width=wt)
  x <- spplot(rs,sp.layout=layt,col.regions=cols,
             par.settings=list(fontsize=list(text=8)),
             at=brks,pretty=brks)
  print(x)
  dev.off()
}



#function to get data from RData of a given grid cell
get_loc_fraction <- function(loc) {
  cell <- gCells$LOC[loc]
  lon <- gCells$LON[loc]; lat <- gCells$LAT[loc]
  rice <- gCells$RICE[loc]; wspr <- gCells$WSPR[loc]
  wwin <- gCells$WWIN[loc]; mill <- gCells$MILL[loc]
  sorg <- gCells$SORG[loc]
  
  #location of file in chunks of 10k files. File number limitation
  if (loc <= 10000) {
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
  }
  
  if (file.exists(oDatFile)) {
    load(oDatFile)
    if (rice == 1) {
      rice1 <- output$RICE$OVERLAP_2035$FRACTION
      rice2 <- output$RICE$OVERLAP_2075$FRACTION
    } else {
      rice1 <- NA
      rice2 <- NA
    }
    
    if (wspr == 1) {
      wspr1 <- output$WSPR$OVERLAP_2035$FRACTION
      wspr2 <- output$WSPR$OVERLAP_2075$FRACTION
    } else {
      wspr1 <- NA
      wspr2 <- NA
    }
    
    if (wwin == 1) {
      wwin1 <- output$WWIN$OVERLAP_2035$FRACTION
      wwin2 <- output$WWIN$OVERLAP_2075$FRACTION
    } else {
      wwin1 <- NA
      wwin2 <- NA
    }
    
    if (mill == 1) {
      mill1 <- output$MILL$OVERLAP_2035$FRACTION
      mill2 <- output$MILL$OVERLAP_2075$FRACTION
    } else {
      mill1 <- NA
      mill2 <- NA
    }
    
    if (sorg == 1) {
      sorg1 <- output$SORG$OVERLAP_2035$FRACTION
      sorg2 <- output$SORG$OVERLAP_2075$FRACTION
    } else {
      sorg1 <- NA
      sorg2 <- NA
    }
    rm(output)
    
    #outputs
    out_res <- data.frame(LOC=cell,RICE1=rice1,RICE2=rice2,WSPR1=wspr1,WSPR2=wspr2,
                          WWIN1=wwin1,WWIN2=wwin2,MILL1=mill1,MILL2=mill2,SORG1=sorg1,
                          SORG2=sorg2)
  } else {
    out_res <- data.frame(LOC=cell,RICE1=NA,RICE2=NA,WSPR1=NA,WSPR2=NA,
                          WWIN1=NA,WWIN2=NA,MILL1=NA,MILL2=NA,SORG1=NA,SORG2=NA)
  }
  return(out_res)
}


#check if processes are done
check_done <- function(loc) {
  #location of file in chunks of 10k files. File number limitation
  cell <- gCells$LOC[loc]
  if (loc <= 10000) {
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
  }
  #add to list if not exists
  fStatus <- file.exists(oDatFile)
  return(fStatus)
}


#function to analyse a given grid cell
analyse_gridcell <- function(loc) {
  library(raster); library(sfsmisc); library(rgdal); library(sp)
  source(paste(src.dir,"/pgr-cc-adaptation-functions.R",sep=""))
  #loc <- 1
  #details
  cell <- gCells$LOC[loc]
  lon <- gCells$LON[loc]; lat <- gCells$LAT[loc]
  rice <- gCells$RICE[loc]; wspr <- gCells$WSPR[loc]
  wwin <- gCells$WWIN[loc]; mill <- gCells$MILL[loc]
  sorg <- gCells$SORG[loc]
  
  cat("\nXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  cat("processing grid cell",cell,"(",loc,"of",nrow(gCells),")\n")
  cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
  
  #location of file in chunks of 10k files. File number limitation
  if (loc <= 10000) {
    if (!file.exists(paste(gcm_outDir,"/part_1",sep=""))) {dir.create(paste(gcm_outDir,"/part_1",sep=""))}
    oDatFile <- paste(gcm_outDir,"/part_1/loc_",loc,".RData",sep="")
    bDatFile <- paste(out_bDir,"/bcc-csm1-1-m_ENS_r1i1p1/part_1/loc_",loc,".RData",sep="")
  } else if (loc > 10000 & loc <= 20000) {
    if (!file.exists(paste(gcm_outDir,"/part_2",sep=""))) {dir.create(paste(gcm_outDir,"/part_2",sep=""))}
    oDatFile <- paste(gcm_outDir,"/part_2/loc_",loc,".RData",sep="")
    bDatFile <- paste(out_bDir,"/bcc-csm1-1-m_ENS_r1i1p1/part_2/loc_",loc,".RData",sep="")
  } else if (loc > 20000) {
    if (!file.exists(paste(gcm_outDir,"/part_3",sep=""))) {dir.create(paste(gcm_outDir,"/part_3",sep=""))}
    oDatFile <- paste(gcm_outDir,"/part_3/loc_",loc,".RData",sep="")
    bDatFile <- paste(out_bDir,"/bcc-csm1-1-m_ENS_r1i1p1/part_3/loc_",loc,".RData",sep="")
  }
  
  
  if (!file.exists(oDatFile)) {
    #extract GCM data for this loc
    hisData <- get_loc_data(lon,lat,hisDir,gcm,ens,vn="tasmax",scratch,yi_h,yf_h,sce="historical")
    rcpData1 <- get_loc_data(lon,lat,rcpDir,gcm,ens,vn="tasmax",scratch,yi_f1,yf_f1,sce="rcp45")
    rcpData2 <- get_loc_data(lon,lat,rcpDir,gcm,ens,vn="tasmax",scratch,yi_f2,yf_f2,sce="rcp45")
    chgData1 <- merge(hisData,rcpData1,by="MONTH",sort=F)
    chgData2 <- merge(hisData,rcpData2,by="MONTH",sort=F)
    names(chgData1) <- c("MONTH","HIS","RCP")
    names(chgData2) <- c("MONTH","HIS","RCP")
    chgData1$CHG <- chgData1$RCP - chgData1$HIS
    chgData2$CHG <- chgData2$RCP - chgData2$HIS
    
    #extract CRU data for this loc
    if (file.exists(bDatFile)) {
      load(bDatFile)
      cruData <- output$DATA_CRU
      rm(output); g=gc(); rm(g)
    } else {
      cruData <- get_loc_data_cru(lon,lat,cruDir,vn="tmx",scratch,yi_h,yf_h)
      names(cruData)[5] <- "CRU"
    }
    
    
    #calculate overlap for rice
    out_rice <- list()
    if (rice == 1) {
      pl <- extract(ca_rs$RICE$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$RICE$HR,cbind(x=lon,y=lat))
      out_rice$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_rice$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_rice$OVERLAP_2035 <- NA
      out_rice$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for wspr
    out_wspr <- list()
    if (wspr == 1) {
      pl <- extract(ca_rs$WSPR$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$WSPR$HR,cbind(x=lon,y=lat))
      out_wspr$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_wspr$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_wspr$OVERLAP_2035 <- NA
      out_wspr$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for wwin
    out_wwin <- list()
    if (wwin == 1) {
      pl <- extract(ca_rs$WWIN$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$WWIN$HR,cbind(x=lon,y=lat))
      out_wwin$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_wwin$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_wwin$OVERLAP_2035 <- NA
      out_wwin$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for mill
    out_mill <- list()
    if (mill == 1) {
      pl <- extract(ca_rs$MILL$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$MILL$HR,cbind(x=lon,y=lat))
      out_mill$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_mill$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_mill$OVERLAP_2035 <- NA
      out_mill$OVERLAP_2075 <- NA
    }
    
    #calculate overlap for mill
    out_sorg <- list()
    if (sorg == 1) {
      pl <- extract(ca_rs$SORG$PL,cbind(x=lon,y=lat))
      hr <- extract(ca_rs$SORG$HR,cbind(x=lon,y=lat))
      out_sorg$OVERLAP_2035 <- calc_overlap(cell,lon,lat,cruData,chgData1,pl,hr)
      out_sorg$OVERLAP_2075 <- calc_overlap(cell,lon,lat,cruData,chgData2,pl,hr)
    } else {
      out_sorg$OVERLAP_2035 <- NA
      out_sorg$OVERLAP_2075 <- NA
    }
    
    #write RData as a test
    output <- list(DATA_CRU=cruData,DATA_2035=chgData1,DATA_2075=chgData2,
                   RICE=out_rice,WSPR=out_wspr,WWIN=out_wwin,MILL=out_mill,
                   SORG=out_sorg)
    save(list=c("output"),file=oDatFile)
  }
}


#function to calculate overlap between historical and future climates
calc_overlap <- function(cell,lon,lat,cruData,chgData,pl,hr) {
  #merge cruData with chgData
  allData <- merge(cruData,chgData,by="MONTH",sort=F)
  allData$DEL <- allData$CRU+allData$CHG
  
  #filter to growing season
  if (hr > pl) {gs <- pl:hr}
  if (hr < pl) {gs <- c(pl:12,1:hr)}
  if (hr == pl) {gs <- pl}
  allData <- allData[which(allData$MONTH %in% gs),]
  
  #calculate 5-95% pdf of current climate
  qlims <- as.numeric(quantile(allData$CRU,probs=c(0.01,0.99)))
  dp1 <- density(allData$CRU)
  xy1 <- data.frame(x=dp1$x,y=dp1$y)
  xy1_in <- xy1[which(xy1$x >= qlims[1] & xy1$x <= qlims[2]),]
  
  #calculate 5-95% pdf of future climate
  #qlims <- as.numeric(quantile(allData$DEL,probs=c(0.05,0.95)))
  dp2 <- density(allData$DEL)
  xy2 <- data.frame(x=dp2$x,y=dp2$y)
  ovr <- xy2[which(xy2$x <= max(xy1_in$x)),]
  #xy2 <- xy2[which(xy2$x >= qlims[1] & xy2$x <= qlims[2]),]
  
  #plots
  #plot(xy1$x,xy1$y,ty="l",xlim=c(-30,40),ylim=c(0,.05))
  #lines(xy2$x,xy2$y,col="red")
  #abline(v=min(xy1_in$x),lty=2); abline(v=max(xy1_in$x),lty=2)
  #lines(ovr$x,ovr$y,col="blue")
  
  #areas
  a1 <- integrate.xy(xy1$x,xy1$y)
  a2 <- integrate.xy(xy2$x,xy2$y)
  if (nrow(ovr) < 2) {
    ao <- 0
  } else {
    ao <- integrate.xy(ovr$x,ovr$y)
  }
  frac <- ao/a2
  
  #output data for this grid cell
  out_data <- list()
  out_data$LON <- lon
  out_data$LAT <- lat
  out_data$LOC <- cell
  out_data$PL <- pl
  out_data$HR <- hr
  out_data$GS_DATA <- allData
  out_data$PDF_CRU <- xy1
  out_data$PDF_DEL <- xy2
  out_data$INT_CRU <- a1
  out_data$INT_DEL <- a2
  out_data$INT_OVR <- ao
  out_data$FRACTION <- frac
  return(out_data)
}



#extract data from netcdf files using cdo
get_loc_data_cru <- function(lon,lat,in_dataDir,vn="tmx",scratch,yi,yf) {
  #load historical data of tasmax
  fil <- paste("cru_ts_3_10.1966.2005.",vn,".dat.nc",sep="")
  
  #inner scratch dir
  inScratch <- paste(scratch,"/",gsub(".dat.nc","",fil),sep="")
  if (!file.exists(inScratch)) {dir.create(inScratch)}
  setwd(inScratch)
  
  #copy files to temporary scratch folder
  inFil <- paste(in_dataDir,"/",fil,sep="")
  if (!file.exists(paste(inScratch,"/data.nc",sep=""))) {
    system(paste("cp -f ",inFil," data.nc",sep=""))
  }
  
  odat <- paste("data_lon-",lon,"_lat-",lat,".tab",sep="")
  if (!file.exists(odat)) {
    #extract values using CDO
    system(paste("cdo -outputtab,year,month,lon,lat,value -remapnn,lon=",lon,"_lat=",lat," data.nc > ",odat,sep=""))
  }
  
  #organise table
  loc_data <- read.table(odat,header=F,sep="")
  names(loc_data) <- c("YEAR","MONTH","LON","LAT","VALUE")
  loc_data$VALUE <- loc_data$VALUE
  loc_data <- loc_data[which(loc_data$YEAR %in% yi:yf),]
  
  #remove file
  if (file.exists(odat)) {system(paste("rm -f ",odat,sep=""))}
  
  #return object
  return(loc_data)
}



#extract data from netcdf files using cdo
get_loc_data <- function(lon,lat,in_dataDir,gcm,ens,vn="tasmax",scratch,yi,yf,sce="historical") {
  #load historical data of tasmax
  gcm_dataDir <- paste(in_dataDir,"/",vn,"/",gcm,"/",ens,sep="")
  gcmFiles <- list.files(gcm_dataDir,pattern=paste(gcm,"_",sce,"_",ens,sep=""))
  
  #loop through files
  loc_hdata <- data.frame()
  for (fil in gcmFiles) {
    #inner scratch dir
    inScratch <- paste(scratch,"/",gsub(".nc","",fil),sep="")
    if (!file.exists(inScratch)) {dir.create(inScratch)}
    setwd(inScratch)
    
    #copy files to temporary scratch folder
    inFil <- paste(gcm_dataDir,"/",fil,sep="")
    if (!file.exists(paste(inScratch,"/data.nc",sep=""))) {
      system(paste("cp -f ",inFil," data.nc",sep=""))
    }
    
    #output tabular data file
    odat <- paste("data_lon-",lon,"_lat-",lat,".tab",sep="")
    if (file.exists(odat)) {system(paste("rm -f ",odat,sep=""))}
    
    #extract values using CDO
    system(paste("cdo -outputtab,year,month,lon,lat,value -remapnn,lon=",lon,"_lat=",lat," data.nc > ",odat,sep=""))
    
    #organise table
    loc_data <- read.table(odat,header=F,sep="")
    names(loc_data) <- c("YEAR","MONTH","LON","LAT","VALUE")
    loc_data$VALUE <- loc_data$VALUE - 273.15
    loc_hdata <- rbind(loc_hdata,loc_data)
    
    #remove file
    if (file.exists(odat)) {system(paste("rm -f ",odat,sep=""))}
  }
  
  #remove extra years and comput climatology
  loc_hdata <- loc_hdata[which(loc_hdata$YEAR %in% yi:yf),]
  loc_data_cl <- data.frame(VALUE=tapply(loc_hdata$VALUE,loc_hdata$MONTH,FUN=mean))
  loc_data_cl <- cbind(MONTH=row.names(loc_data_cl),loc_data_cl)
  
  #return object
  return(loc_data_cl)
}



#find month given a Julian day 
find_month <- function(x) {
  #x <- vals[1]
  x <- round(x,0)
  if (x==0) {x <- 1}
  if (x>365) {x <- 365}
  dg <- createDateGrid(2000)
  dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
  dg$DOM <- as.numeric(substr(dg$MTH.DAY,5,6))
  m <- dg$MTH[which(dg$JD == x)]
  return(m)
}


#get only first listed ensemble member for each gcm
get_single_ens <- function(x,list1,list2) {
  gcm <- paste(x)
  ens <- paste(list2$ENS[which(list2$GCM == gcm)][1])
  yri_h <- list1$YI[which(list1$GCM == gcm & list1$ENS == ens)]
  yrf_h <- list1$YF[which(list1$GCM == gcm & list1$ENS == ens)]
  yri_f <- list2$YI[which(list2$GCM == gcm & list2$ENS == ens)]
  yrf_f <- list2$YF[which(list2$GCM == gcm & list2$ENS == ens)]
  yret <- c(gcm,ens,yri_h,yrf_h,yri_f,yrf_f)
  return(yret)
}


#get list of GCM and ensemble members
get_gcmList <- function(vn,data_dir,yi,yf) {
  rawList <- list.files(paste(data_dir,"/",vn,sep=""),recursive=T,include.dirs=F)
  mList <- lapply(rawList,FUN=get_details)
  mList <- as.data.frame(do.call("rbind",mList))
  names(mList) <- c("GCM","ENS","YI","YF")
  mList <- cbind(GCM_ENS=paste(mList$GCM,"_ENS_",mList$ENS,sep=""),mList)
  mList$YI <- as.numeric(paste(mList$YI))
  mList$YF <- as.numeric(paste(mList$YF))
  
  #eliminate repetitions (due to files being splitted in groups of years)
  uList <- paste(unique(mList$GCM_ENS))
  uList <- lapply(uList,FUN=get_final_details,mList)
  uList <- as.data.frame(do.call("rbind",uList))
  names(uList) <- c("GCM","ENS","YI","YF")
  uList <- cbind(GCM_ENS=paste(uList$GCM,"_ENS_",uList$ENS,sep=""),uList)
  uList$YI <- as.numeric(paste(uList$YI))
  uList$YF <- as.numeric(paste(uList$YF))
  
  #check if period includes yi (1965) and yf (2005)
  uList$PERIOD <- F
  uList$PERIOD[which(uList$YI <= yi & uList$YF >= yf)] <- T
  uList <- uList[which(uList$PERIOD),]
  uList$PERIOD <- NULL
  return(uList)
}


#get final details (without repetition)
get_final_details <- function(x,list2) {
  gcm <- unlist(strsplit(x,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(x,"_ENS_",fixed=T))[2]
  yri <- min(list2$YI[which(list2$GCM_ENS==x)])
  yrf <- max(list2$YF[which(list2$GCM_ENS==x)])
  yret <- c(gcm,ens,yri,yrf)
  return(yret)
}


#get details of listed files
get_details <- function(x) {
  y <- unlist(strsplit(x,"/",fixed=T))
  gcm <- y[1]
  ens <- y[2]
  fil <- y[3]
  yrs <- unlist(strsplit(fil,"_",fixed=T))
  yrs <- yrs[length(yrs)]
  yrs <- gsub(".nc","",yrs)
  yri <- as.numeric(substr(unlist(strsplit(yrs,"-",fixed=T))[1],1,4))
  yrf <- as.numeric(substr(unlist(strsplit(yrs,"-",fixed=T))[2],1,4))
  yret <- c(gcm,ens,yri,yrf)
  return(yret)
}

