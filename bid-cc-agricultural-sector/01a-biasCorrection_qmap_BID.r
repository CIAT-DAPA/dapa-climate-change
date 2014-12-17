#Perform quantile mapping bias correction of daily climate data

#Load libraries
library(qmap)
library(ncdf)
library(raster)
library(lubridate)

#set directories
#wfdDir = '//dapadfs/data_cluster_4/observed/gridded_products/wfd/'
wfdDir = '/mnt/data_cluster_4/observed/gridded_products/wfd/'
#gcmDir = "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/"
gcmDir = "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/gcm_0_5deg_lat/"
#outDir = "D:/ClimateModel_downscaling/qmap_test/"
outDir = '/home/smgourdji/qmap_downscaling/'
setwd(outDir)

#which GCM's?
gcmList = c("bcc_csm1_1")

#Load mask for Latin America
maskWFDLat <- raster(paste(wfdDir,"raw/mask_wfd_lat.nc",sep=''))

#Load historical observations (looping through months...)
#Load each month as raster stack with daily data
years.hist = 1950:2000  #goes from 1950 to 2001, should use this period eventually (needs to be consistent with GCM.hist)
dates.hist = seq(as.Date('1950-01-01'),as.Date('2000-12-31'),by=1)
months = c(paste(0,1:9,sep=''),10:12)
hist.dat.all = array(NA,dim=c(8199,length(dates.hist)))  #gridcells x dates
for (j in 1:length(years.hist))  {
  for (k in 1:12)  {
    WFD = stack(paste(wfdDir,'nc-files/wfd_0_5_deg_lat/Rainf_daily_WFD_GPCC/lat_Rainf_daily_WFD_GPCC_',years.hist[j],months[k],'.nc',sep='')) 
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
  }
}

#Load historical GCM data
years.gcm.hist = 1950:2000
dates.gcm.hist = seq(as.Date('1950-01-01'),as.Date('2000-12-31'),by=1)
gcmHist.dat.all = array(NA,dim=c(8199,length(dates.gcm.hist)))  #gridcells x days x months x years
for (j in 1:length(years.gcm.hist))  {
  for (k in 1:12)  {
    gcmHist = stack(paste(gcmDir,'bcc_csm1_1/1971_2000/by-month/prec_',years.gcm.hist[j],'_',months[k],'.nc',sep=''))  
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
gcmFut.dat.all = array(NA,dim=c(8199,length(dates.gcm.fut)))
for (j in 1:length(years.gcm.fut)) {
  for (k in 1:12)  {
    gcmFut = stack(paste(gcmDir,'bcc_csm1_1/2020_2049/by-month/prec_',years.gcm.fut[j],'_',months[k],'.nc',sep=''))
    gcmFut = mask(gcmFut, maskWFDLat)  #cut to Latin America
    gcmFut.dat = rasterToPoints(gcmFut)  #extract values (gridcells by days)
    n = dim(gcmFut.dat)[2] - 2  #calculate number of days in month
    dates.mo = seq(as.Date(paste(years.gcm.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.gcm.fut[j],'-',months[k],'-',n,sep='')),by=1)
    ind.mo = match(dates.mo,dates.gcm.fut)  #find indices of this month in big dates vector
    gcmFut.dat.all[,ind.mo] = gcmFut.dat[,3:(n+2)]  #PUT IN SUPER-MATRIX HERE
    print(paste(j,k))  #print month & year of loop
  }
}

#Apply bias-correction looping by gridcell (j) and day of year (k)
gcmHistBC = array(NA,dim=c(8199,length(dates.gcm.hist)))
gcmFutBC = array(NA,dim=c(8199,length(dates.gcm.fut)))
month.hist = month(dates.gcm.hist)
mday.hist = mday(dates.gcm.hist)
month.hist2 = month(dates.gcm.fut)
mday.hist2 = mday(dates.gcm.fut)
dates_nonleap = seq(as.Date('2001-01-01'),as.Date('2001-12-31'),by=1)  #example year
for (j in 1:8199)  {  #cells
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
    qm.hist.fit = fitQmap(obs = hist.dat.i, mod = gcmHist.dat.i, method="RQUANT",qstep=0.01,wet.day=T, na.rm=T)
    
    #Then apply bias correction to GCM past & future (just day k)
    gcmHistBC[j,ind_k] = doQmap(x=gcmHist.dat.all[j,ind_k],qm.hist.fit, type="linear")  #should this be j,ind_k for model input?  verify!
    gcmFutBC[j,ind_k] = doQmap(x=gcmFut.dat.all[j,ind_k2],qm.hist.fit, type="linear")
    
    print(paste(j,k))
  }
}

## calculate diagnostics
#wet-day frequency
#standard deviation of daily rainfall
#interannual variability of monthly sums
