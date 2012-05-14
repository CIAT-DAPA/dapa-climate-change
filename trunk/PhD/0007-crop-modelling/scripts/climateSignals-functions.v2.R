#Julian Ramirez-Villegas
#March 2012
#Modified May 2012 to be more efficient (see prepareData.R and prepareData-functions.R)
#CIAT / CCAFS / UoL

#plot the calculated signals
plotSignals <- function(techn="lin",oDir,pval) {
  library(maptools); data(wrld_simpl)
  
  #define signal dir and create output dir
  signalDir <- paste(oDir,"/1dd_signals/",techn,"-signals",sep="")
  figDir <- paste(oDir,"/1dd_signals/",techn,"-signals/figures",sep="")
  if (!file.exists(figDir)) {dir.create(figDir)}
  
  #list rasters
  rList <- list.files(signalDir,pattern="\\.R.asc")
  rList <- gsub(paste(techn,"-",sep=""),"",rList)
  rList <- gsub(paste(".R.asc"),"",rList)
  
  #loop through rasters
  for (rName in rList) {
    cat("plotting",rName,"\n")
    #reading raster
    rs <- raster(paste(signalDir,"/",techn,"-",rName,".R.asc",sep=""))
    pv <- raster(paste(signalDir,"/",techn,"-",rName,".PVAL.asc",sep=""))
    
    xt <- extent(c(rs@extent@xmin,95,rs@extent@ymin,35))
    rs <- crop(rs,xt)
    pv <- crop(pv,xt)
    
    if (length(which(is.na(rs[]))) == ncell(rs)) {
      rs[] <- 0
    }
    
    #select cells that are below a given pval (normally 0.05 or 0.1)
    pv_cells <- xyFromCell(pv,which(pv[] <= pval))
    
    #get plot area
    ht <- 1500
    ratio <- (rs@extent@xmax - rs@extent@xmin)/(rs@extent@ymax - rs@extent@ymin)+0.15
    wt <- ht/ratio
    
    #generate breaks
    brks <- seq(-1,1,by=0.1)
    nb <- length(brks)-1
    cols <- colorRampPalette(c("red","orange","yellow","gray 80","light blue","blue","dark blue"))(nb)
    
    #create tiff image
    tiff(paste(figDir,"/",rName,".tif",sep=""),res=300,compression="lzw",height=ht,width=wt,pointsize=8)
    par(mar=c(3,3,1,1))
    plot(rs,col=cols,breaks=brks,lab.breaks=brks,
         horizontal=T,axes=T,zlim=c(-1,1),legend.width=1,
         legend.shrink=0.95,useRaster=T)
    if (nrow(pv_cells)>0) {
      points(pv_cells,pch=20,cex=0.75)
    }
    plot(wrld_simpl,add=T)
    grid()
    dev.off()
  }
}


#
#calculate climate signals (correlations) over all gridcells
calcSignals <- function(techn,ydDir,oDir,tser) {
  #loading yield data stack
  yd_stk <- stack(paste(ydDir,"/",techn,"/",techn,"-",tser,".asc",sep=""))
  
  #read example climate cell to get names of fields
  clCellList <- list.files(oDir,pattern="climate_cell-")
  cl_dumm <- read.csv(paste(oDir,"/",clCellList[1],sep=""))
  env_vars <- names(cl_dumm)[2:ncol(cl_dumm)]
  
  #loop through gridcells
  for (cell in pCells$CELL) {
    cat("Processing cell",cell,"\n")
    x <- pCells$X[which(pCells$CELL==cell)]; y <- pCells$Y[which(pCells$CELL==cell)]
    
    yd_vals <- extract(yd_stk,cbind(X=x,Y=y))
    
    if (file.exists(paste(oDir,"/climate_cell-",cell,".csv",sep=""))) {
      cl_data <- read.csv(paste(oDir,"/climate_cell-",cell,".csv",sep=""))
      
      all_data <- cl_data
      all_data$YIELD <- t(yd_vals)
      all_data <- all_data[which(all_data$YIELD!=0),]
      all_data <- all_data[which(!is.na(all_data$YIELD)),]
      
      #loop through each possible variable
      for (evar in env_vars) {
        #perform the correlation test
        if (nrow(all_data)>=2) {
          if (length(unique(all_data$YIELD)) > 2 & length(unique(all_data[,evar])) > 2) {
            ct <- cor.test(all_data$YIELD,all_data[,evar])
          } else {
            ct <- list()
            ct$estimate <- NA
            ct$p.value <- NA
          }
        } else {
          ct <- list()
          ct$estimate <- NA
          ct$p.value <- NA
        }
        if (evar == env_vars[1]) {
          out_row <- data.frame(CELL=cell,LON=x,LAT=y,NOBS=nrow(all_data),R=ct$estimate,PVAL=ct$p.value)
          names(out_row)[5:6] <- c(paste(evar,c(".R",".PVAL"),sep=""))
          out_all <- out_row
        } else {
          out_row <- data.frame(R=ct$estimate,PVAL=ct$p.value)
          names(out_row) <- c(paste(evar,c(".R",".PVAL"),sep=""))
          out_all <- cbind(out_all,out_row)
        }
      }
    } else {
      
      for (evar in env_vars) {
        #blank list of -dummy correlation test
        ct <- list()
        ct$estimate <- NA
        ct$p.value <- NA
        
        if (evar == env_vars[1]) {
          out_row <- data.frame(CELL=cell,LON=x,LAT=y,NOBS=nrow(all_data),R=ct$estimate,PVAL=ct$p.value)
          names(out_row)[5:6] <- c(paste(evar,c(".R",".PVAL"),sep=""))
          out_all <- out_row
        } else {
          out_row <- data.frame(R=ct$estimate,PVAL=ct$p.value)
          names(out_row) <- c(paste(evar,c(".R",".PVAL"),sep=""))
          out_all <- cbind(out_all,out_row)
        }
      }
      
    }
    
    if (cell == pCells$CELL[1]) {
      out_sign <- out_all
    } else {
      out_sign <- rbind(out_sign,out_all)
    }
  }
  oSignDir <- paste(oDir,"/1dd_signals",sep="")
  if (!file.exists(oSignDir)) {dir.create(oSignDir)}
  
  oTechDir <- paste(oSignDir,"/",techn,"-signals",sep="")
  if (!file.exists(oTechDir)) {dir.create(oTechDir)}
  
  write.csv(out_sign,paste(oTechDir,"/signals-",techn,".csv",sep=""),quote=F,row.names=F)
  
  #write rasters
  rs_names <- names(out_sign)[5:ncol(out_sign)]
  cat("Writing rasters\n")
  for (rname in rs_names) {
    out_rs <- raster(msk)
    out_rs[pCells$CELL] <- out_sign[,rname]
    #plot(out_rs)
    out_rs <- writeRaster(out_rs,paste(oTechDir,"/",techn,"-",rname,".asc",sep=""),format="ascii")
  }
  return(oTechDir)
}



#wrap for a given cell to be able to further parallelise (for rainfed crops)
cell_wrapper <- function(cell) {
  library(raster)
  source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir,"/watbal.R",sep=""))
  source(paste(src.dir2,"/climateSignals-functions.v2.R",sep=""))
  if (!file.exists(paste(oDir,"/climate_cell-",cell,".csv",sep=""))) {
    x <- pCells$X[which(pCells$CELL==cell)]; y <- pCells$Y[which(pCells$CELL==cell)]
    
    #extract planting dates default
    thisPDay <- round(extract(pday,cbind(X=x,Y=y)),0)
    thisHDay <- round(extract(hday,cbind(X=x,Y=y)),0)
    
    #loop through years
    for (yr in y_iyr:y_eyr) {
      gs_data <- processYear(cell,ncFile=ncFile,mthRainAsc=mthRainAsc,year=yr,x,y,
                             tempDir=tempDir,sradDir=sradDir,era40Dir=era40Dir,
                             sd_default=sd_default,ed_default=ed_default,thresh=thresh,
                             tbase=tbase,topt=topt,tmax=tmax,tcrit=tcrit,tlim=tlim)
      if (yr==y_iyr) {
        gs_out <- gs_data
      } else {
        gs_out <- rbind(gs_out,gs_data)
      }
    }
    write.csv(gs_out,paste(oDir,"/climate_cell-",cell,".csv",sep=""),quote=F,row.names=F)
  }
}


################################################################################
#wrap for a given cell to be able to further parallelise (for irrigated crops)
cell_wrapper_irr <- function(cell) {
  library(raster); library(rgdal)
  source(paste(src.dir,"/GHCND-GSOD-functions.R",sep=""))
  source(paste(src.dir,"/watbal.R",sep=""))
  source(paste(src.dir2,"/climateSignals-functions.v2.R",sep=""))
  
  if (!file.exists(paste(oDir,"/climate_cell-",cell,".csv",sep=""))) {
    #location of cell
    x <- pCells$X[which(pCells$CELL==cell)]; y <- pCells$Y[which(pCells$CELL==cell)]
    
    #extract planting dates
    thisPDay <- round(extract(pday,cbind(X=x,Y=y)),0)
    thisHDay <- round(extract(hday,cbind(X=x,Y=y)),0)
    
    #loop through years
    for (yr in y_iyr:y_eyr) {
      cat("\n",yr,"\n")
      gsmet <- processYear_irr(cell=cell,lon=x,lat=y,year=yr,pDay=thisPDay,hDay=thisHDay,ncFile=ncFile,
                               mthRainAsc=mthRainAsc,tempDir=tempDir,sradDir=sradDir,
                               era40Dir=era40Dir,tbase=tbase,topt=topt,tmax=tmax,
                               tcrit=tcrit,tlim=tlim)
      
      if (yr==y_iyr) {
        gs_out <- gsmet
      } else {
        gs_out <- rbind(gs_out,gsmet)
      }
    }
    write.csv(gs_out,paste(oDir,"/climate_cell-",cell,".csv",sep=""),quote=F,row.names=F)
  }
}

# pyr <- processYear(ncFile,mthRainAsc,year,x,y,tempDir,sradDir,era40Dir,sd_default=165,ed_default=225,
#                    thresh=0.5,tbase=10,topt=28,tmax=50,tcrit=34,tlim=40)

#get relevant growing season metrics for a given year for a rainfed crop
#that is, using PGJ basic watbal algorithm
processYear <- function(cell,ncFile,mthRainAsc,year,x,y,tempDir,sradDir,era40Dir,sd_default=165,ed_default=225,
                        thresh=0.5,tbase=10,topt=28,tmax=50,tcrit=34,tlim=40) {
  cat("\nProcessing year",year,"\n")
  nd <- leap(year)
  
  #extract daily weather from Indian TropMet grids
  cat("daily rainfall: loading...\n")
  #out_all <- extractDaily(ncFile,x,y,year,nd,mthRainAsc)
  out_all <- read.csv(paste(mthRainAsc,"/cell-",cell,".csv",sep=""))
  out_all <- out_all[which(out_all$YEAR==year),]
  out_all$YEAR <- NULL
  
  if (nd == 365) {
    out_all$DAY366 <- NULL
  }
  
  out_all <- as.data.frame(t(out_all)); row.names(out_all) <- 1:nd; names(out_all) <- "RAIN"
  out_all <- data.frame(DAY=1:nd,RAIN=out_all$RAIN)
  
  #i need to first calculate the potential evapotranspiration. 
  #I will use the Priestley-Taylor equation. Main references:
  #                 *Weis and Menzel (2008)
  #                 *Challinor et al. (2004)
  #
  
  #extract solar radiation from ERA40 reanalysis
  cat("daily solar radiation from ERA40: loading...\n")
  #sradERA40 <- extractERA(era40.dir=era40Dir,x,y,year,nd,varName="srad")
  sradERA40 <- read.csv(paste(era40Dir,"/cell-",cell,".csv",sep=""))
  sradERA40 <- sradERA40[which(sradERA40$YEAR==year),]
  sradERA40$YEAR <- NULL
  
  if (nd == 365) {
    sradERA40$DAY366 <- NULL
  }
  
  sradERA40 <- as.data.frame(t(sradERA40)); row.names(sradERA40) <- 1:nd; names(sradERA40) <- "SRAD"
  out_all$SRAD <- sradERA40$SRAD
  
  #need to load monthly temperature data
  #read 14 months
  cat("extracting temperature data : loading...\n")
  #tmin_stk <- stack(c(paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(year-1),"_12.asc",sep=""),
  #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",year,"_",1:12,".asc",sep=""),
  #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(year+1),"_1.asc",sep="")))
  #tmin_vals <- extract(tmin_stk,cbind(X=x,Y=y))*0.1
  
  tmin_vals <- read.csv(paste(tempDir,"/cru_tmn/cell-",cell,".csv",sep=""))
  tmin_vals <- tmin_vals[which(tmin_vals$YEAR==year | tmin_vals$YEAR==(year-1) | tmin_vals$YEAR==(year+1)),]
  tmin_vals$YEAR <- NULL
  tmin_vals <- c(tmin_vals$MONTH12[1],as.numeric(tmin_vals[2,]),tmin_vals$MONTH1[3])
  
  daily_tmin <- linearise(tmin_vals)[16:(nd+15)] #interpolate to daily
  out_all$TMIN <- daily_tmin #put data into matrix
  
  #tmax_stk <- stack(c(paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(year-1),"_12.asc",sep=""),
  #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",year,"_",1:12,".asc",sep=""),
  #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(year+1),"_1.asc",sep="")))
  #tmax_vals <- extract(tmax_stk,cbind(X=x,Y=y))*0.1
  
  tmax_vals <- read.csv(paste(tempDir,"/cru_tmx/cell-",cell,".csv",sep=""))
  tmax_vals <- tmax_vals[which(tmax_vals$YEAR==year | tmax_vals$YEAR==(year-1) | tmax_vals$YEAR==(year+1)),]
  tmax_vals$YEAR <- NULL
  tmax_vals <- c(tmax_vals$MONTH12[1],as.numeric(tmax_vals[2,]),tmax_vals$MONTH1[3])
  
  daily_tmax <- linearise(tmax_vals)[16:(nd+15)] #interpolate to daily
  out_all$TMAX <- daily_tmax #put data into matrix
  
  #load monthly solar radiation data
  cat("extracting solar radiation data : loading...\n")
  #srad_stk <- stack(c(paste(sradDir,"/srad_1dd/srad_","12.asc",sep=""),
  #                    paste(sradDir,"/srad_1dd/srad_",1:12,".asc",sep=""),
  #                    paste(sradDir,"/srad_1dd/srad_","1.asc",sep="")))
  #srad_vals <- extract(srad_stk,cbind(X=x,Y=y))
  
  srad_vals <- read.csv(paste(sradDir,"/cell-",cell,".csv",sep=""))
  srad_vals$YEAR <- NULL
  srad_vals <- c(srad_vals$MONTH12,as.numeric(srad_vals),srad_vals$MONTH1)
  
  daily_srad <- linearise(srad_vals)[16:(nd+15)] #interpolate to daily
  out_all$SRAD_CL <- daily_srad*60*60*24/1000000 #put into matrix and convert from W/m^2 to MJ/m^2/day
  
  out_all_tmp <- out_all; out_all$SRAD_CL <- NULL
  
  #Calculate the water balance
  cat("Calculating water balance \n")
  out_all$ETMAX <- NA; out_all$AVAIL <- NA; out_all$ERATIO <- NA
  out_all$CUM_RAIN <- NA; out_all$RUNOFF <- NA; out_all$DEMAND <- NA
  out_all <- watbal_wrapper(out_all)
  
  #calculate growing seasons
  cat("Find out growing seasons \n")
  gs <- gsl_find(out_all$ERATIO,ea_thresh=thresh,n_start=5,n_end=12,sd_default=sd_default,ed_default=ed_default)
  gs$GSL <- gs$END-gs$START
  
  # plot(out_all$DAY,out_all$RAIN,ty="l")
  # plot(out_all$DAY,out_all$TMIN,ty="l")
  # plot(out_all$DAY,out_all$TMAX,ty="l")
  # plot(out_all$DAY,out_all$SRAD,ty="l")
  # plot(out_all$DAY,out_all$ERATIO,ty="l"); abline(h=0.35,col="red",lty=2); abline(h=0.5,col="red")
  # plot(out_all$DAY,out_all$RUNOFF,ty="l")
  # plot(out_all$DAY,out_all$AVAIL,ty="l")
  # plot(out_all$DAY,out_all$ETMAX,ty="l")
  # plot(out_all$DAY,out_all$CUM_RAIN,ty="l")
  # plot(out_all$DAY,out_all$DEMAND,ty="l")
  # plot(out_all$DAY,out_all$ERATIO,ty="l",ylim=c(-0.05,1)); abline(h=0.35,col="red",lty=2); abline(h=0.5,col="red")
  
  # for (i in 1:nrow(gs)) {
  #   lines(x=gs[i,1:2],y=c(-0.025,-0.025),lwd=2,col="blue")
  # }
  
  out_all$SRAD_E40 <- out_all$SRAD
  out_all$SRAD <- out_all_tmp$SRAD_CL
  
  #select the longest growing season
  cat("Getting final climate metrics \n")
  gs_sel <- gs[which(gs$GSL==max(gs$GSL)),]
  gs_data <- gs_metrics(out_all,gs_sel,thresh=thresh,tbase=tbase,topt=topt,tmax=tmax,tcrit=tcrit,tlim=tlim,year)
  gs_data <- cbind(YEAR=year,gs_data)
  
  return(gs_data)
}


####################################################################################
####################################################################################
#Wrapper to extract the growing season data for irrigated crops
processYear_irr <- function(cell,lon,lat,year,pDay,hDay,ncFile,mthRainAsc,tempDir,sradDir,
                            era40Dir,tbase,topt,tmax,tcrit,tlim) {
  #extract weather series
  cat("extract weather series\n")
  wthSeries <- getGSWeather(cell=cell,x=lon,y=lat,yr=year,thisPDay=pDay,thisHDay=hDay,
                            ncFile=ncFile,mthRainAsc=mthRainAsc,tempDir=tempDir,
                            sradDir=sradDir,era40Dir=era40Dir)
  
  #here calculate the growing season and whole year metrics
  gsWth <- wthSeries$WTH_GS; row.names(gsWth) <- gsWth$DAY
  yrWth <- wthSeries$WTH_ALL
  
  #calculate growing season and yearly seasonal metrics
  cat("calculating gs and seasonal metrics\n")
  gsm <- gs_metrics_irr(yr=year,wth_gs=gsWth,wth_all=yrWth,tbase=tbase,topt=topt,tmax=tmax,tcrit=tcrit,tlim=tlim)
  gsm <- cbind(YEAR=year,gsm)
  return(gsm)
}



#determine indicators for a rainfed crop
# 1. number of stress days (days with ea_thresh<0.15)
# 2. number of rain days
# 3. maximum number of consecutive dry days
# 4. number of days with temperature >10 (Challinor et al. 2004)
# 5. number of days with temperature >28
# 5. number of days with temperature >34
# 6. number of days with temperature >40
# 7. number of days with temperature >50
# 8. total rainfall during growing period
# 9. growing degree days with temperature above 10 Celsius

#calculate gs metrics for a rainfed crop
gs_metrics <- function(out_all,gs_sel,thresh=0.5,tbase=10,topt=28,tmax=50,tcrit=34,tlim=40,year) {
  stress_days <- 0; rain_days <- 0; cons_dd <- 0; ddays <- 0
  tb <- 0; to <- 0; tx <- 0; txcrit <- 0; txlim <- 0
  rain <- 0; gdd <- 0
  
  gs_tmean <- (out_all$TMIN + out_all$TMAX)/2
  gs_tmean <- mean(gs_tmean)
  
  gs_srad <- sum(out_all$SRAD)
  gs_e40_srad <- sum(out_all$SRAD_E40)
  
  #matrix of dates
  dgrid <- createDateGrid(year)
  dgrid$MONTH <- substr(dgrid$MTH.DAY,1,3)
  dgrid$MONTH <- as.numeric(gsub("M","",dgrid$MONTH))
  
  #calc Q1, Q2, Q3, Q4  raindays, rain, tmean
  days_q1 <- which(dgrid$MONTH %in% c(1:3))
  rain_q1 <- sum(out_all$RAIN[days_q1])
  tean_q1 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q1 <- mean(tean_q1[days_q1])
  rdays_q1 <- length(which(out_all$RAIN[days_q1] > 0))
  srad_q1 <- sum(out_all$SRAD[days_q1])
  srad_e40_q1 <- sum(out_all$SRAD_E40[days_q1])
  
  days_q2 <- which(dgrid$MONTH %in% c(4:6))
  rain_q2 <- sum(out_all$RAIN[days_q2])
  tean_q2 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q2 <- mean(tean_q2[days_q2])
  rdays_q2 <- length(which(out_all$RAIN[days_q2] > 0))
  srad_q2 <- sum(out_all$SRAD[days_q2])
  srad_e40_q2 <- sum(out_all$SRAD_E40[days_q2])
  
  days_q3 <- which(dgrid$MONTH %in% c(7:9))
  rain_q3 <- sum(out_all$RAIN[days_q3])
  tean_q3 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q3 <- mean(tean_q3[days_q3])
  rdays_q3 <- length(which(out_all$RAIN[days_q3] > 0))
  srad_q3 <- sum(out_all$SRAD[days_q3])
  srad_e40_q3 <- sum(out_all$SRAD_E40[days_q3])
  
  days_q4 <- which(dgrid$MONTH %in% c(10:12))
  rain_q4 <- sum(out_all$RAIN[days_q4])
  tean_q4 <- (out_all$TMIN + out_all$TMAX)/2
  tean_q4 <- mean(tean_q4[days_q4])
  rdays_q4 <- length(which(out_all$RAIN[days_q4] > 0))
  srad_q4 <- sum(out_all$SRAD[days_q4])
  srad_e40_q4 <- sum(out_all$SRAD_E40[days_q4])
  
  #calc SEM1, SEM2
  days_s1 <- which(dgrid$MONTH %in% c(1:6))
  rain_s1 <- sum(out_all$RAIN[days_s1])
  tean_s1 <- (out_all$TMIN + out_all$TMAX)/2
  tean_s1 <- mean(tean_s1[days_s1])
  rdays_s1 <- length(which(out_all$RAIN[days_s1] > 0))
  srad_s1 <- sum(out_all$SRAD[days_s1])
  srad_e40_s1 <- sum(out_all$SRAD_E40[days_s1])
  
  days_s2 <- which(dgrid$MONTH %in% c(7:12))
  rain_s2 <- sum(out_all$RAIN[days_s2])
  tean_s2 <- (out_all$TMIN + out_all$TMAX)/2
  tean_s2 <- mean(tean_s2[days_s2])
  rdays_s2 <- length(which(out_all$RAIN[days_s2] > 0))
  srad_s2 <- sum(out_all$SRAD[days_s2])
  srad_e40_s2 <- sum(out_all$SRAD_E40[days_s2])
  
  #loop through growing season days
  for (gday in gs_sel$START:gs_sel$END) {
    if (out_all$ERATIO[gday] < thresh) {stress_days <- stress_days+1}
    if (out_all$RAIN[gday] > 0) {rain_days <- rain_days+1}
    if (out_all$RAIN[gday] == 0) {
      #cat(ddays," ")
      ddays <- ddays+1
      if (ddays >= cons_dd) {
        cons_dd <- ddays
      }
    } else {
      ddays <- 0
    }
    tmean <- (out_all$TMIN[gday]+out_all$TMAX[gday])/2
    if (tmean > tbase) {tb <- tb+1}
    if (tmean > topt) {to <- to+1}
    if (tmean > tmax) {tx <- tx+1}
    if (out_all$TMAX[gday] > 34) {txcrit <- txcrit+1}
    if (out_all$TMAX[gday] > 40) {txlim <- txlim+1}
    rain <- rain+out_all$RAIN[gday]
    
    if (tmean < tbase) {
      gdd <- gdd+0
    } else  if (tmean >= tbase & tmean <= topt) {
      gdd <- gdd + (tmean-tbase)
    } else if (tmean > topt) {
      gdd <- gdd + (topt-tbase)
    }
    
  }
  out <- data.frame(RAIN=rain,SRAD=gs_srad,SRAD_E40=gs_e40_srad,GDD=gdd,
                    STRESS_DAYS=stress_days,RDAYS=rain_days,X_CONS_DD=cons_dd,
                    D_TB=tb,D_TO=to,D_TX=tx,D_TXCRIT=txcrit,D_TXLIM=txlim,GS_TMEAN=gs_tmean,
                    Q1_TMEAN=tean_q1,Q1_RAIN=rain_q1,Q1_RDAYS=rdays_q1,Q1_SRAD=srad_q1,Q1_E40_SRAD=srad_e40_q1,
                    Q2_TMEAN=tean_q2,Q2_RAIN=rain_q2,Q2_RDAYS=rdays_q2,Q2_SRAD=srad_q2,Q2_E40_SRAD=srad_e40_q2,
                    Q3_TMEAN=tean_q3,Q3_RAIN=rain_q3,Q3_RDAYS=rdays_q3,Q3_SRAD=srad_q3,Q3_E40_SRAD=srad_e40_q3,
                    Q4_TMEAN=tean_q4,Q4_RAIN=rain_q4,Q4_RDAYS=rdays_q4,Q4_SRAD=srad_q4,Q4_E40_SRAD=srad_e40_q4,
                    S1_TMEAN=tean_s1,S1_RAIN=rain_s1,S1_RDAYS=rdays_s1,S1_SRAD=srad_s1,S1_E40_SRAD=srad_e40_s1,
                    S2_TMEAN=tean_s2,S2_RAIN=rain_s2,S2_RDAYS=rdays_s2,S2_SRAD=srad_s2,S2_E40_SRAD=srad_e40_s2)
  return(out)
}


#######################################################################
#Calculate growing season and overall year metrics for irrigated crops
#######################################################################
gs_metrics_irr <- function(yr,wth_gs,wth_all,tbase=1,topt=22.1,tmax=35.4,tcrit=34,tlim=40) {
  #init routine values
  tb <- 0; to <- 0; tx <- 0; gdd <- 0
  ddays <- 0; cons_dd <- 0
  
  #total radiation during the growing season
  gs_srad <- sum(wth_gs$SRAD)
  gs_srad_e40 <- sum(wth_gs$SRAD_E40)
  
  #mean temperature during the growing season
  gs_tmean <- (wth_gs$TMAX + wth_gs$TMIN) / 2
  gs_tmean <- mean(gs_tmean)
  
  #number of days Tmax above an HTS threshold (Tcrit)
  d_tcrit <- length(which(wth_gs$TMAX >= tcrit))
  
  #number of days Tmax above an HTS threshold (Tlim)
  d_tlim <- length(which(wth_gs$TMAX >= tlim))
  
  #growing degree days using Tb, Topt and Tmax
  for (d in 1:nrow(wth_gs)) {
    tmean <- (wth_gs$TMIN[d]+wth_gs$TMAX[d])/2
    
    #number of days above Tb, Topt and Tmax, respectively
    if (tmean > tbase) {tb <- tb+1}
    if (tmean > topt) {to <- to+1}
    if (tmean > tmax) {tx <- tx+1}
    
    if (tmean < tbase) {
      gdd <- gdd+0
    } else  if (tmean >= tbase & tmean <= topt) {
      gdd <- gdd + (tmean-tbase)
    } else if (tmean > topt) {
      gdd <- gdd + (topt-tbase)
    }
    
    #max number of dry days
    if (wth_gs$RAIN[d] == 0) {
      ddays <- ddays+1
      if (ddays >= cons_dd) {
        cons_dd <- ddays
      }
    } else {
      ddays <- 0
    }
  }
  
  
  #total rainfall during the growing season
  gs_rain <- sum(wth_gs$RAIN)
  
  #number of rain days during the growing season
  gs_rdays <- length(which(wth_gs$RAIN > 0))
  
  #matrix of dates
  dgrid <- createDateGrid(yr)
  dgrid$MONTH <- substr(dgrid$MTH.DAY,1,3)
  dgrid$MONTH <- as.numeric(gsub("M","",dgrid$MONTH))
  
  #q1 stuff
  days_q1 <- which(dgrid$MONTH %in% c(1:3))
  rain_q1 <- sum(wth_all$RAIN[days_q1])
  tean_q1 <- (wth_all$TMIN + wth_all$TMAX)/2
  tean_q1 <- mean(tean_q1[days_q1])
  rdays_q1 <- length(which(wth_all$RAIN[days_q1] > 0))
  srad_q1 <- sum(wth_all$SRAD[days_q1])
  srad_e40_q1 <- sum(wth_all$SRAD_E40[days_q1])
  
  #q2 stuff
  days_q2 <- which(dgrid$MONTH %in% c(4:6))
  rain_q2 <- sum(wth_all$RAIN[days_q2])
  tean_q2 <- (wth_all$TMIN + wth_all$TMAX)/2
  tean_q2 <- mean(tean_q2[days_q2])
  rdays_q2 <- length(which(wth_all$RAIN[days_q2] > 0))
  srad_q2 <- sum(wth_all$SRAD[days_q2])
  srad_e40_q2 <- sum(wth_all$SRAD_E40[days_q2])
  
  #q3 stuff
  days_q3 <- which(dgrid$MONTH %in% c(7:9))
  rain_q3 <- sum(wth_all$RAIN[days_q3])
  tean_q3 <- (wth_all$TMIN + wth_all$TMAX)/2
  tean_q3 <- mean(tean_q3[days_q3])
  rdays_q3 <- length(which(wth_all$RAIN[days_q3] > 0))
  srad_q3 <- sum(wth_all$SRAD[days_q3])
  srad_e40_q3 <- sum(wth_all$SRAD_E40[days_q3])
  
  #q4 stuff
  days_q4 <- which(dgrid$MONTH %in% c(10:12))
  rain_q4 <- sum(wth_all$RAIN[days_q4])
  tean_q4 <- (wth_all$TMIN + wth_all$TMAX)/2
  tean_q4 <- mean(tean_q4[days_q4])
  rdays_q4 <- length(which(wth_all$RAIN[days_q4] > 0))
  srad_q4 <- sum(wth_all$SRAD[days_q4])
  srad_e40_q4 <- sum(wth_all$SRAD_E40[days_q4])
  
  #s1 stuff
  days_s1 <- which(dgrid$MONTH %in% c(1:6))
  rain_s1 <- sum(wth_all$RAIN[days_s1])
  tean_s1 <- (wth_all$TMIN + wth_all$TMAX)/2
  tean_s1 <- mean(tean_s1[days_s1])
  rdays_s1 <- length(which(wth_all$RAIN[days_s1] > 0))
  srad_s1 <- sum(wth_all$SRAD[days_s1])
  srad_e40_s1 <- sum(wth_all$SRAD_E40[days_s1])
  
  #s2 stuff
  days_s2 <- which(dgrid$MONTH %in% c(7:12))
  rain_s2 <- sum(wth_all$RAIN[days_s2])
  tean_s2 <- (wth_all$TMIN + wth_all$TMAX)/2
  tean_s2 <- mean(tean_s2[days_s2])
  rdays_s2 <- length(which(wth_all$RAIN[days_s2] > 0))
  srad_s2 <- sum(wth_all$SRAD[days_s2])
  srad_e40_s2 <- sum(wth_all$SRAD_E40[days_s2])
  
  #output row
  out <- data.frame(RAIN=gs_rain,GDD=gdd,RDAYS=gs_rdays,GS_SRAD=gs_srad,GS_SRAD_E40=gs_srad_e40,
                    X_CONS_DD=cons_dd,D_TB=tb,D_TO=to,D_TX=tx,
                    D_TXCRIT=d_tcrit,D_TXLIM=d_tlim,GS_TMEAN=gs_tmean,
                    Q1_TMEAN=tean_q1,Q1_RAIN=rain_q1,Q1_RDAYS=rdays_q1,Q1_SRAD=srad_q1,Q1_SRAD_E40=srad_e40_q1,
                    Q2_TMEAN=tean_q2,Q2_RAIN=rain_q2,Q2_RDAYS=rdays_q2,Q2_SRAD=srad_q2,Q2_SRAD_E40=srad_e40_q2,
                    Q3_TMEAN=tean_q3,Q3_RAIN=rain_q3,Q3_RDAYS=rdays_q3,Q3_SRAD=srad_q3,Q3_SRAD_E40=srad_e40_q3,
                    Q4_TMEAN=tean_q4,Q4_RAIN=rain_q4,Q4_RDAYS=rdays_q4,Q4_SRAD=srad_q4,Q4_SRAD_E40=srad_e40_q4,
                    S1_TMEAN=tean_s1,S1_RAIN=rain_s1,S1_RDAYS=rdays_s1,S1_SRAD=srad_s1,S1_SRAD_E40=srad_e40_s1,
                    S2_TMEAN=tean_s2,S2_RAIN=rain_s2,S2_RDAYS=rdays_s2,S2_SRAD=srad_s2,S2_SRAD_E40=srad_e40_s2)
  
  return(out)
}



#to linearise monthly data
linearise <- function(input_vals) {
  day_mid <- c(-15,16,45,75,106,136,167,197,228,259,289,320,350,381)
  daily_vals <- rep(NA,times=(day_mid[14]-day_mid[1]+1))
  for (mth in 1:13) {
    deltawth <- (input_vals[mth+1]-input_vals[mth]) / (day_mid[mth+1]-day_mid[mth])
    
    for (d in day_mid[mth]:day_mid[mth+1]) {
      daily_vals[d+16] <- input_vals[mth] + deltawth*(d-day_mid[mth])
    }
  }
  return(daily_vals)
}


#function to create mask from yield adn met grids
maskCreate <- function(met,yld) {
  #create mask with pixels that are not NA in both
  met[which(!is.na(met[]))] <- 1
  yld[which(!is.na(yld[]))] <- 1
  
  #get coordinates and values of rasters
  cellMx <- data.frame(CELL=1:ncell(yld))
  cellMx$X <- xFromCell(yld,cellMx$CELL); cellMx$Y <- yFromCell(yld,cellMx$CELL)
  cellMx$VALS.YLD <- extract(yld,cbind(X=cellMx$X,Y=cellMx$Y))
  cellMx$VALS.MET <- extract(met,cbind(X=cellMx$X,Y=cellMx$Y))
  cellMx$VALS.OUT <- 1
  cellMx$VALS.OUT[which(is.na(cellMx$VALS.YLD) | is.na(cellMx$VALS.MET))] <- NA
  
  #assign mask data
  msk <- raster(yld)
  msk[cellMx$CELL] <- cellMx$VALS.OUT
  return(msk)
}



#########################################################################################
#########################################################################################
#extract a year's series of weather conditions for a growing season specified by the planting
#and harvest date (normally from Sacks et al. 2010)
getGSWeather <- function(cell,x,y,yr,thisPDay,thisHDay,ncFile,mthRainAsc,tempDir,sradDir,era40Dir) {
  
  #
  #Verify if planting date is before or after harvest date
  if (thisPDay >= thisHDay) {
    cat("crop was planted in previous year, extracting that weather data also \n")
    pyr <- yr-1
    #sowPos <- findNCPos(pyr,thisPDay)
    #harPos <- findNCPos(yr,thisHDay)
    
    cat("rainfall of planting year: loading...\n")
    nd <- leap(pyr)
    #rainPlant <- extractDaily(ncFile,x,y,pyr,nd,mthRainAsc)
    rainPlant <- read.csv(paste(mthRainAsc,"/cell-",cell,".csv",sep=""))
    rainPlant <- rainPlant[which(rainPlant$YEAR==pyr),]
    rainPlant$YEAR <- NULL
    
    if (nd == 365) {
      rainPlant$DAY366 <- NULL
    }
    
    rainPlant <- as.data.frame(t(rainPlant)); row.names(rainPlant) <- 1:nd; names(rainPlant) <- "RAIN"
    rainPlant <- data.frame(DAY=1:nd,RAIN=rainPlant$RAIN)
    
    nd <- leap(yr)
    cat("rainfall of harvest year: loading...\n")
    #rainHarv <- extractDaily(ncFile,x,y,yr,nd,mthRainAsc)
    
    rainHarv <- read.csv(paste(mthRainAsc,"/cell-",cell,".csv",sep=""))
    rainHarv <- rainHarv[which(rainHarv$YEAR==yr),]
    rainHarv$YEAR <- NULL
    
    if (nd == 365) {
      rainHarv$DAY366 <- NULL
    }
    
    rainHarv <- as.data.frame(t(rainHarv)); row.names(rainHarv) <- 1:nd; names(rainHarv) <- "RAIN"
    rainHarv <- data.frame(DAY=1:nd,RAIN=rainHarv$RAIN)
    
    #do a dataset with the actual planting dates
    wthGS <- rbind(rainPlant[thisPDay:(nrow(rainPlant)),],rainHarv[1:thisHDay,])
    wthGS$DAY <- 1:nrow(wthGS)
    
    #do a dataset with half one year to half the next
    if (nd == 365) {
      wthAll <- rbind(rainPlant[183:(nrow(rainPlant)),],rainHarv[1:182,])
    } else {
      wthAll <- rbind(rainPlant[183:(nrow(rainPlant)),],rainHarv[1:183,])
    }
    wthAll$DAY <- 1:nrow(wthAll)
    
    #get the srad data from ERA40
    #extract era40 data
    cat("E40 radiation of planting year: loading...\n")
    nd <- leap(pyr)
    #sradDay_plant <- extractERA(era40.dir=era40Dir,x=x,y=y,year=pyr,nd=nd,varName="srad")
    sradDay_plant <- read.csv(paste(era40Dir,"/cell-",cell,".csv",sep=""))
    sradDay_plant <- sradDay_plant[which(sradDay_plant$YEAR==pyr),]
    sradDay_plant$YEAR <- NULL
    
    if (nd == 365) {
      sradDay_plant$DAY366 <- NULL
    }
    
    sradDay_plant <- as.data.frame(t(sradDay_plant)); row.names(sradDay_plant) <- 1:nd; names(sradDay_plant) <- "SRAD"
    
    nd <- leap(yr)
    cat("E40 radiation of harvest year: loading...\n")
    #sradDay_harv <- extractERA(era40.dir=era40Dir,x=x,y=y,year=yr,nd=nd,varName="srad")
    sradDay_harv <- read.csv(paste(era40Dir,"/cell-",cell,".csv",sep=""))
    sradDay_harv <- sradDay_harv[which(sradDay_harv$YEAR==yr),]
    sradDay_harv$YEAR <- NULL
    
    if (nd == 365) {
      sradDay_harv$DAY366 <- NULL
    }
    
    sradDay_harv <- as.data.frame(t(sradDay_harv)); row.names(sradDay_harv) <- 1:nd; names(sradDay_harv) <- "SRAD"
    
    wthGS$SRAD_E40 <- c(sradDay_plant$SRAD[thisPDay:(nrow(sradDay_plant))],
                        sradDay_harv$SRAD[1:thisHDay])
    
    if (nd == 365) {
      wthAll$SRAD_E40 <- c(sradDay_plant$SRAD[183:(nrow(sradDay_plant))],sradDay_plant$SRAD[1:182])
    } else {
      wthAll$SRAD_E40 <- c(sradDay_plant$SRAD[183:(nrow(sradDay_plant))],sradDay_plant$SRAD[1:183])
    }
    
    #get the temperature data, planting year
    cat("extracting min temperature data \n")
    #tmin_stk <- stack(c(paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(pyr-1),"_12.asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",pyr,"_",1:12,".asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(pyr+1),"_1.asc",sep="")))
    #tmin_vals <- extract(tmin_stk,cbind(X=x,Y=y))*0.1
    
    tmin_vals <- read.csv(paste(tempDir,"/cru_tmn/cell-",cell,".csv",sep=""))
    tmin_vals <- tmin_vals[which(tmin_vals$YEAR==pyr | tmin_vals$YEAR==(pyr-1) | tmin_vals$YEAR==(pyr+1)),]
    tmin_vals$YEAR <- NULL
    tmin_vals <- c(tmin_vals$MONTH12[1],as.numeric(tmin_vals[2,]),tmin_vals$MONTH1[3])
    
    nd <- leap(pyr)
    daily_tminPlant <- linearise(tmin_vals)[16:(nd+15)] #interpolate to daily
    
    #harvest year
    #tmin_stk <- stack(c(paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(yr-1),"_12.asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",yr,"_",1:12,".asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(yr+1),"_1.asc",sep="")))
    #tmin_vals <- extract(tmin_stk,cbind(X=x,Y=y))*0.1
    
    tmin_vals <- read.csv(paste(tempDir,"/cru_tmn/cell-",cell,".csv",sep=""))
    tmin_vals <- tmin_vals[which(tmin_vals$YEAR==yr | tmin_vals$YEAR==(yr-1) | tmin_vals$YEAR==(yr+1)),]
    tmin_vals$YEAR <- NULL
    tmin_vals <- c(tmin_vals$MONTH12[1],as.numeric(tmin_vals[2,]),tmin_vals$MONTH1[3])
    
    nd <- leap(yr)
    daily_tminHarv <- linearise(tmin_vals)[16:(nd+15)] #interpolate to daily
    
    #do a dataset with the actual planting dates
    tminGS <- c(daily_tminPlant[thisPDay:(length(daily_tminPlant))],daily_tminHarv[1:thisHDay])
    wthGS$TMIN <- tminGS
    
    #do a dataset with half one year to half the next
    if (nd == 365) {
      tminAll <- c(daily_tminPlant[183:(length(daily_tminPlant))],daily_tminHarv[1:182])
    } else {
      tminAll <- c(daily_tminPlant[183:(length(daily_tminPlant))],daily_tminHarv[1:183])
    }
    wthAll$TMIN <- tminAll
    
    ###### Maximum temperature, planting year
    cat("extracting max temperature data \n")
    #tmax_stk <- stack(c(paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(pyr-1),"_12.asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",pyr,"_",1:12,".asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(pyr+1),"_1.asc",sep="")))
    #tmax_vals <- extract(tmax_stk,cbind(X=x,Y=y))*0.1
    
    tmax_vals <- read.csv(paste(tempDir,"/cru_tmx/cell-",cell,".csv",sep=""))
    tmax_vals <- tmax_vals[which(tmax_vals$YEAR==pyr | tmax_vals$YEAR==(pyr-1) | tmax_vals$YEAR==(pyr+1)),]
    tmax_vals$YEAR <- NULL
    tmax_vals <- c(tmax_vals$MONTH12[1],as.numeric(tmax_vals[2,]),tmax_vals$MONTH1[3])
    
    nd <- leap(pyr)
    daily_tmaxPlant <- linearise(tmax_vals)[16:(nd+15)] #interpolate to daily
    
    #harvest year
    #tmax_stk <- stack(c(paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(yr-1),"_12.asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",yr,"_",1:12,".asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(yr+1),"_1.asc",sep="")))
    #tmax_vals <- extract(tmax_stk,cbind(X=x,Y=y))*0.1
    
    tmax_vals <- read.csv(paste(tempDir,"/cru_tmx/cell-",cell,".csv",sep=""))
    tmax_vals <- tmax_vals[which(tmax_vals$YEAR==yr | tmax_vals$YEAR==(yr-1) | tmax_vals$YEAR==(yr+1)),]
    tmax_vals$YEAR <- NULL
    tmax_vals <- c(tmax_vals$MONTH12[1],as.numeric(tmax_vals[2,]),tmax_vals$MONTH1[3])
    
    nd <- leap(yr)
    daily_tmaxHarv <- linearise(tmax_vals)[16:(nd+15)] #interpolate to daily
    
    #do a dataset with the actual planting dates
    tmaxGS <- c(daily_tmaxPlant[thisPDay:(length(daily_tmaxPlant))],daily_tmaxHarv[1:thisHDay])
    wthGS$TMAX <- tmaxGS
    
    #do a dataset with half one year to half the next
    if (nd == 365) {
      tmaxAll <- c(daily_tmaxPlant[183:(length(daily_tmaxPlant))],daily_tmaxHarv[1:182])
    } else {
      tmaxAll <- c(daily_tmaxPlant[183:(length(daily_tmaxPlant))],daily_tmaxHarv[1:183])
    }
    wthAll$TMAX <- tmaxAll
    
    #get the radiation data
    #load monthly solar radiation data
    cat("extracting solar radiation data \n")
    #srad_stk <- stack(c(paste(sradDir,"/srad_1dd/srad_","12.asc",sep=""),
    #                    paste(sradDir,"/srad_1dd/srad_",1:12,".asc",sep=""),
    #                    paste(sradDir,"/srad_1dd/srad_","1.asc",sep="")))
    #srad_vals <- extract(srad_stk,cbind(X=x,Y=y))
    
    srad_vals <- read.csv(paste(sradDir,"/cell-",cell,".csv",sep=""))
    srad_vals$YEAR <- NULL
    srad_vals <- c(srad_vals$MONTH12,as.numeric(srad_vals),srad_vals$MONTH1)
    
    nd <- leap(pyr)
    daily_sradPlant <- linearise(srad_vals)[16:(nd+15)] #interpolate to daily
    
    nd <- leap(yr)
    daily_sradHarv <- linearise(srad_vals)[16:(nd+15)] #interpolate to daily
    
    #planting to harvest
    sradGS <- c(daily_sradPlant[thisPDay:(length(daily_sradPlant))],daily_sradHarv[1:thisHDay])
    wthGS$SRAD <- sradGS*60*60*24/1000000 #put into matrix and convert from W/m^2 to MJ/m^2/day
    
    #whole year
    if (nd == 365) {
      sradAll <- c(daily_sradPlant[183:(length(daily_sradPlant))],daily_sradHarv[1:182])
    } else {
      sradAll <- c(daily_sradPlant[183:(length(daily_sradPlant))],daily_sradHarv[1:183])
    }
    wthAll$SRAD <- sradAll*60*60*24/1000000 #put into matrix and convert from W/m^2 to MJ/m^2/day
    
  } else {
    
    ########################################
    #####Crop was planted in the same year
    ########################################
    cat("crop was planted and harvested in the same year \n")
    
    #extract rainfall
    nd <- leap(yr)
    #rain_daily <- extractDaily(ncFile,x,y,yr,nd,mthRainAsc)
    rain_daily <- read.csv(paste(mthRainAsc,"/cell-",cell,".csv",sep=""))
    rain_daily <- rain_daily[which(rain_daily$YEAR==yr),]
    rain_daily$YEAR <- NULL
    
    if (nd == 365) {
      rain_daily$DAY366 <- NULL
    }
    
    rain_daily <- as.data.frame(t(rain_daily)); row.names(rain_daily) <- 1:nd; names(rain_daily) <- "RAIN"
    rain_daily <- data.frame(DAY=1:nd,RAIN=rain_daily$RAIN)
    
    #do a dataset with the actual planting dates
    wthGS <- rbind(rain_daily[thisPDay:thisHDay,])
    wthGS$DAY <- 1:nrow(wthGS)
    
    #do a dataset with half one year to half the next
    wthAll <- rain_daily
    
    #extract solar radiation
    #sradDay <- extractERA(era40.dir=era40Dir,x=x,y=y,year=yr,nd=nd,varName="srad")
    sradDay <- read.csv(paste(era40Dir,"/cell-",cell,".csv",sep=""))
    sradDay <- sradDay[which(sradDay$YEAR==yr),]
    sradDay$YEAR <- NULL
    
    if (nd == 365) {
      sradDay$DAY366 <- NULL
    }
    
    sradDay <- as.data.frame(t(sradDay)); row.names(sradDay) <- 1:nd; names(sradDay) <- "SRAD"
    
    wthGS$SRAD_E40 <- sradDay$SRAD[thisPDay:thisHDay] #put into growing season weather
    wthAll$SRAD_E40 <- sradDay$SRAD
    
    #get the temperature data
    cat("extracting min temperature data : loading...\n")
    #tmin_stk <- stack(c(paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(yr-1),"_12.asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",yr,"_",1:12,".asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmn_1dd/tmn_",(yr+1),"_1.asc",sep="")))
    #tmin_vals <- extract(tmin_stk,cbind(X=x,Y=y))*0.1
    
    tmin_vals <- read.csv(paste(tempDir,"/cru_tmn/cell-",cell,".csv",sep=""))
    tmin_vals <- tmin_vals[which(tmin_vals$YEAR==yr | tmin_vals$YEAR==(yr-1) | tmin_vals$YEAR==(yr+1)),]
    tmin_vals$YEAR <- NULL
    tmin_vals <- c(tmin_vals$MONTH12[1],as.numeric(tmin_vals[2,]),tmin_vals$MONTH1[3])
    daily_tmin <- linearise(tmin_vals)[16:(nd+15)] #interpolate to daily
    
    #do a dataset with the actual planting dates
    tminGS <- c(daily_tmin[thisPDay:thisHDay])
    wthGS$TMIN <- tminGS
    
    #put into whole year dataset
    wthAll$TMIN <- daily_tmin
    
    ###### Maximum temperature, planting year
    cat("extracting max temperature data \n")
    #tmax_stk <- stack(c(paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(yr-1),"_12.asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",yr,"_",1:12,".asc",sep=""),
    #                    paste(tempDir,"/monthly_grids/tmx_1dd/tmx_",(yr+1),"_1.asc",sep="")))
    #tmax_vals <- extract(tmax_stk,cbind(X=x,Y=y))*0.1
    
    tmax_vals <- read.csv(paste(tempDir,"/cru_tmx/cell-",cell,".csv",sep=""))
    tmax_vals <- tmax_vals[which(tmax_vals$YEAR==yr | tmax_vals$YEAR==(yr-1) | tmax_vals$YEAR==(yr+1)),]
    tmax_vals$YEAR <- NULL
    tmax_vals <- c(tmax_vals$MONTH12[1],as.numeric(tmax_vals[2,]),tmax_vals$MONTH1[3])
    daily_tmax <- linearise(tmax_vals)[16:(nd+15)] #interpolate to daily
    
    #do a dataset with the actual planting dates
    tmaxGS <- c(daily_tmax[thisPDay:thisHDay])
    wthGS$TMAX <- tmaxGS
    
    #do a dataset with half one year to half the next
    wthAll$TMAX <- daily_tmax
    
    #get the radiation data
    #load monthly solar radiation data
    cat("extracting solar radiation data \n")
    #srad_stk <- stack(c(paste(sradDir,"/srad_1dd/srad_","12.asc",sep=""),
    #                    paste(sradDir,"/srad_1dd/srad_",1:12,".asc",sep=""),
    #                    paste(sradDir,"/srad_1dd/srad_","1.asc",sep="")))
    #srad_vals <- extract(srad_stk,cbind(X=x,Y=y))
    
    srad_vals <- read.csv(paste(sradDir,"/cell-",cell,".csv",sep=""))
    srad_vals$YEAR <- NULL
    srad_vals <- c(srad_vals$MONTH12,as.numeric(srad_vals),srad_vals$MONTH1)
    
    daily_srad <- linearise(srad_vals)[16:(nd+15)] #interpolate to daily
    
    #planting to harvest
    sradGS <- c(daily_srad[thisPDay:thisHDay])
    wthGS$SRAD <- sradGS*60*60*24/1000000 #put into matrix and convert from W/m^2 to MJ/m^2/day
    
    #whole year
    wthAll$SRAD <- daily_srad*60*60*24/1000000 #put into matrix and convert from W/m^2 to MJ/m^2/day
  }
  return(list(WTH_GS=wthGS,WTH_ALL=wthAll))
}


######################################################################
#find out what is the continous date that i refer to in the nc file
findNCPos <- function(thisYr,thisDay) {
  tropMet_iyr <- 1960
  counter <- 0
  
  for (yr in tropMet_iyr:thisYr) {
    if (yr<thisYr) {
      nd <- leap(yr)
      for (j in 1:nd) {
        counter <- counter+1
      }
    } else {
      for (j in 1:thisDay) {
        counter <- counter+1
      }
    }
  }
  return(counter)
}


extractDaily <- function(ncFile,x,y,year,nd,mthRainAsc) {
  cat("extracting daily data \n")
  
  #randomly pick if from NC or from Asciis (only done once at the beginning of extraction)
  rint <- round(rnorm(1,0.5,0.1),0)
  #if (rint == 0) {cat("using nc \n")} else {cat("using asc\n")}
  #loop and extract days
  for (d in 1:nd) {
    #cat(d," ",sep="")
    #if the random value is 0 use netCDF
    if (rint==0) {
      ncPos <- findNCPos(year,d)
      rs <- raster(ncFile,band=ncPos)
    } else { #if random value is 1 use ASCII
      rs <- raster(paste(mthRainAsc,"/",year,"/rain-",d,".asc",sep=""))
    }
    out_row <- data.frame(DAY=d,RAIN=extract(rs,cbind(X=x,Y=y)))
    if (d==1) {
      out_all <- out_row
    } else {
      out_all <- rbind(out_all,out_row)
    }
  }
  #cat("\n")
  return(out_all)
}


################################################################
#extract ERA40 data for a given variable specified by varName
#in a given location specified by x,y, and year
extractERA <- function(era40.dir,x,y,year,nd,varName="srad") {
  cat("extracting daily ERA40 data \n")
  
  era40DataDir <- paste(era40.dir,"/daily_data_",tolower(varName),"/",year,sep="")
  
  for (d in 1:nd) {
    if (!file.exists(paste(era40DataDir,"/",tolower(varName),"_",d,".asc",sep=""))) {
      out_row <- data.frame(DAY=d,VALUE=NA)
    } else {
      rs <- raster(paste(era40DataDir,"/",tolower(varName),"_",d,".asc",sep=""))
      out_row <- data.frame(DAY=d,VALUE=extract(rs,cbind(X=x,Y=y)))
    }
    if (d==1) {
      out_all <- out_row
    } else {
      out_all <- rbind(out_all,out_row)
    }
  }
  names(out_all) <- c("DAY",toupper(varName))
  return(out_all)
}
