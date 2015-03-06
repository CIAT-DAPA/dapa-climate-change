#Perform quantile mapping bias correction of daily climate data

#Load libraries
library(qmap)
library(ncdf)
library(raster)
library(lubridate)

BC_Qmap <- function(wfdDir="//dapadfs/data_cluster_4/observed/gridded_products/wfd", gcmHistDir="Z:/bid/gcm_0_5deg_lat", gcmFutDir="//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat", outDir="Z:/bid/bc_0_5deg_lat", var="prec", gcm="bcc_csm1_1") {
  
  if(var == "prec"){varmod <- "Rainf"}
  if(var == "rsds"){varmod <- "SWdown"}
  
  setwd(outDir)
  
  if (!file.exists(paste(outDir, "/", gcm, "/1971_2000", sep=""))) {dir.create(paste(outDir, "/", gcm, "/1971_2000", sep=""), recursive=T)}
  if (!file.exists(paste(outDir, "/", gcm, "/2020_2049", sep=""))) {dir.create(paste(outDir, "/", gcm, "/2020_2049", sep=""), recursive=T)}
  
  #Load mask for Latin America
  maskWFDLat <- raster(paste(wfdDir,"/raw/mask_wfd_lat.nc",sep=''))
  
  #Load historical observations (looping through months...)
  #Load each month as raster stack with daily data
  years.hist = 1950:2000  #goes from 1950 to 2001, should use this period eventually (needs to be consistent with GCM.hist)
  dates.hist = seq(as.Date('1950-01-01'),as.Date('2000-12-31'),by=1)
  months = c(paste(0,1:9,sep=''),10:12)
  ncell <- dim(rasterToPoints(maskWFDLat))[1]
  hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x dates
  
  for (j in 1:length(years.hist)){
    for (k in 1:12){
      
      if (var == "prec"){
        
        WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD_GPCC/lat_', varmod, '_daily_WFD_GPCC_',years.hist[j],months[k],'.nc',sep=''))        
        
        xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
        xmax(WFD) = xmax(WFD) - 360
        WFD = mask(WFD, maskWFDLat)  #cut to Latin America
        hist.dat = rasterToPoints(WFD)  #extract values (gridcells by days)
        if (j==1 & k==1)  {
          cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
        }
        n = dim(hist.dat)[2] - 2  #calculate number of days in month
        dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
        ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
        hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]*86400  #convert to mm before putting in big matrix
        print(paste(j,k))  #print month & year of loop
        
      } else {
        
        WFD = stack(paste(wfdDir,'/nc-files/wfd_0_5_deg_lat/', varmod, '_daily_WFD/lat_', varmod, '_daily_WFD_',years.hist[j],months[k],'.nc',sep=''))        
        
        xmin(WFD) = xmin(WFD) - 360  #shift to proper longitude
        xmax(WFD) = xmax(WFD) - 360
        WFD = mask(WFD, maskWFDLat)  #cut to Latin America
        hist.dat = rasterToPoints(WFD)  #extract values (gridcells by days)
        if (j==1 & k==1)  {
          cells.loc = hist.dat[,1:2]  #extract lat/ long's of gridcells
        }
        n = dim(hist.dat)[2] - 2  #calculate number of days in month
        dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
        ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
        hist.dat.all[,ind.mo] = hist.dat[,3:(n+2)]  #convert to mm before putting in big matrix
        print(paste(j,k))  #print month & year of loop
        
      }
    }
  }
  
  #Load historical GCM data
  years.gcm.hist = 1950:2000
  dates.gcm.hist = seq(as.Date('1950-01-01'),as.Date('2000-12-31'),by=1)
  gcmHist.dat.all = array(NA,dim=c(ncell,length(dates.gcm.hist)))  #gridcells x days x months x years
  
  for (j in 1:length(years.gcm.hist))  {
    for (k in 1:12)  {
      gcmHist = stack(paste(gcmHistDir, '/',gcm,'/1950_2000/by-month/',var,'_',years.gcm.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist = mask(gcmHist, maskWFDLat)  #cut to Latin America
      gcmHist.dat = rasterToPoints(gcmHist)
      n = dim(gcmHist.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.gcm.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.gcm.hist[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.gcm.hist)  #find indices of this month in big dates vector
      gcmHist.dat.all[,ind.mo] = gcmHist.dat[,3:(n+2)]  #put in super-matrix
      print(paste(j,k))  #print month & year of loop
    }
  }
  
  #Load future GCM data
  years.gcm.fut = 2020:2049
  dates.gcm.fut = seq(as.Date('2020-01-01'),as.Date('2049-12-31'),by=1)
  gcmFut.dat.all = array(NA,dim=c(ncell,length(dates.gcm.fut)))
  for (j in 1:length(years.gcm.fut)) {
    for (k in 1:12)  {
      gcmFut = stack(paste(gcmFutDir,'/',gcm,'/2020_2049/by-month/',var,'_',years.gcm.fut[j],'_',months[k],'.nc',sep=''))
      gcmFut = mask(gcmFut, maskWFDLat)  #cut to Latin America
      gcmFut.dat = rasterToPoints(gcmFut)  #extract values (gridcells by days)
      n = dim(gcmFut.dat)[2] - 2  #calculate number of days in month
      dates.mo = seq(as.Date(paste(years.gcm.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.gcm.fut[j],'-',months[k],'-',n,sep='')),by=1)
      ind.mo = match(dates.mo,dates.gcm.fut)  #find indices of this month in big dates vector
      gcmFut.dat.all[,ind.mo] = gcmFut.dat[,3:(n+2)]  #PUT IN SUPER-MATRIX HERE
      print(paste(j,k))  #print month & year of loop
    }
  }
  
  
  batchs <- floor(ncell / 100) + 1

  month.hist = month(dates.gcm.hist)
  mday.hist = mday(dates.gcm.hist)
  month.hist2 = month(dates.gcm.fut)
  mday.hist2 = mday(dates.gcm.fut)
  dates_nonleap = seq(as.Date('2001-01-01'),as.Date('2001-12-31'),by=1)  #example year  
  
  for (n in 1:batchs){
    
    staCell <- ((n - 1) * 100) + 1
    if (n == batchs){endCell <- ncell} else {endCell <- staCell + 99}
    
    ncellArray <- endCell - staCell + 100
    
    #Apply bias-correction looping by gridcell (j) and day of year (k)
    gcmHistBC = array(NA,dim=c(ncellArray,length(dates.gcm.hist)))
    gcmFutBC = array(NA,dim=c(ncellArray,length(dates.gcm.fut)))  
    
    row <- 1
    
    if (!file.exists(paste(outDir, "/", gcm, "/2020_2049/", "bc_", var, "_2020_2049_cells_", staCell, '_', endCell, '.Rdat',sep=''))) {
    
      for (j in staCell:endCell){ #cells
        
        for (k in 1:365) {  #loop through days of year, selecting 30-day moving window around each day
          #do not fit Feb. 29 (NA for GCM's)
          
          #Need to find indices of all days "k" and then 15 days before and after by year
          ind_k = which(month.hist == month(dates_nonleap[k]) & mday.hist == mday(dates_nonleap[k]))  #historical
          ind_k2 = which(month.hist2 == month(dates_nonleap[k]) & mday.hist2 == mday(dates_nonleap[k]))  #future
          for (d in 1:length(ind_k))  {
            if (d==1) {ind.all = (ind_k[d]-15):(ind_k[d]+15)}  else{
              ind.all = c(ind.all,(ind_k[d]-15):(ind_k[d]+15))
            }
          }
          ind.all = ind.all[ind.all>0 & ind.all<dim(hist.dat.all)[2]]  #get rid of values outside of historical range
          
          #Extract all years (for cell j and day k with moving window) for historical obs, GCM past 
          hist.dat.i = hist.dat.all[j,ind.all]
          gcmHist.dat.i = gcmHist.dat.all[j,ind.all]
          
          #Fit model b/w historical obs & GCM past
          #make sure years are consistent here between past obs & GCM hist!

          
          #Then apply bias correction to GCM past & future (just day k)
          error.p = tryCatch( { #keep going if can't apply model (all zeros in obs)  
            
            if (var == "prec"){
              qm.hist.fit = fitQmap(obs = hist.dat.i, mod = gcmHist.dat.i, method="RQUANT",qstep=0.01,wet.day=T, na.rm=T)  
            } else {
              qm.hist.fit = fitQmap(obs = hist.dat.i, mod = gcmHist.dat.i, method="RQUANT",qstep=0.01,wet.day=F, na.rm=T)  
            }
            
            gcmHistBC[row,ind_k] = doQmap(x=gcmHist.dat.all[j,ind_k],qm.hist.fit, type="linear")  #should this be j,ind_k for model input?  verify!
            gcmFutBC[row,ind_k2] = doQmap(x=gcmFut.dat.all[j,ind_k2],qm.hist.fit, type="linear")}
            
          , error=function(e) e
          )
          if(inherits(error.p,'Error'))  next
    
          print(paste(j,k))
        }
  
      row <- row + 1
      }
      # Saving Results
      save(gcmHistBC, file=paste(outDir, "/", gcm, "/1971_2000/", "bc_", var, "_1950_2000_cells_", staCell, '_', endCell, '.Rdat',sep=''))
      save(gcmFutBC, file=paste(outDir, "/", gcm, "/2020_2049/", "bc_", var, "_2020_2049_cells_", staCell, '_', endCell, '.Rdat',sep=''))
    }
  }
}

## calculate diagnostics
#wet-day frequency
#standard deviation of daily rainfall
#interannual variability of monthly sums
