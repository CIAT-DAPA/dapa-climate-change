# Carlos Navarro, CIAT
# February 2017
# Perform quantile mapping bias correction of daily climate data

# # #Set variables
# iDir <- "Z:/DATA/WP2/06_Clustering_analyses"
# # obsDir <- "S:/observed/gridded_products/chirps/daily/32bits"
# obsDir <- "U:/cropdata/agmerra/daily/nc-files/by-month"
# region <- "amz"
# obsPer <- "1981_2010"
# gcmHistPer <- "1981_2005"
# gcmFutPerLs <- c("2020_2049", "2040_2069")
# county <- "Napo"
# outDir <- "Z:/DATA/WP2/06_Clustering_analyses/data/bc_quantile_0_05deg_amz"
# var <- "tmin"
# gcm="bcc_csm1_1"
# rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")

BC_Qmap <- function(iDir="", obsDir="", region="", obsPer="", gcmHistPer="", gcmFutPerLs="", county="", outDir="", var="", gcmList=c(""), rcpLs=c(""), i="") {
  
  #Load libraries
  library(qmap)
  library(ncdf)
  library(ncdf4)
  library(raster)
  library(lubridate)
  
  # Define gcm
  gcm <- gcmList[i]
    
  if(var == "rsds"){varmod <- "dswrf"} else {varmod = var}
  
  # Load county mask
  maskCnt <- raster(paste(iDir,"/data/", region, "_regions_rst/", region, "_base_lr.tif", sep=''))
  
  #Load RData for coordinates 
  load(paste0(iDir, "/data/input_tables_lr/", county, "/", varmod, "/", varmod, ".RData"))
  
  if (var == "prec"){
    hist.obs <- chirps_year
    rm(chirps_year)
  } else {
    hist.obs <- agmerra_year
    rm(agmerra_year)
  }
  
  #Load historical observations (looping through months...)
  #Load each month as raster stack with daily data
  
  cellId <- hist.obs[[1]]$cellID
  lat <- hist.obs[[1]]$lat
  lon <- hist.obs[[1]]$lon
  pts <- cbind(lon, lat)
  
  years.hist = strsplit(obsPer, "_")[[1]][1]:strsplit(obsPer, "_")[[1]][2]  
  dates.hist = seq(as.Date(paste0(strsplit(obsPer, "_")[[1]][1], '-01-01')),as.Date(paste0(strsplit(obsPer, "_")[[1]][2], '-12-31')),by=1)
  months = c(paste(0,1:9,sep=''),10:12)
  ncell <- nrow(pts)
  
  hist.dat.all <- do.call("cbind", hist.obs)
  hist.dat.all <- hist.dat.all[, -grep("cellID|lat|lon", names(hist.dat.all))]

  names(hist.dat.all) <- paste(dates.hist)
  rm(hist.obs)
  
#   hist.dat.all = array(NA,dim=c(ncell,length(dates.hist)))  #gridcells x dates
  
#   if (var == "prec"){
#     chirps_data <- list.files(path=obsDir, pattern='*.tif$', full.names=TRUE)
#   }
#   
#   for (j in 1:length(years.hist)){
#     for (k in 1:12){
#       
#       if (var == "prec"){
#         OBS <- raster::stack(chirps_data[grep(pattern=paste0(years.hist[[j]], ".", months[k]), x=chirps_data)])
#       } else {
#         OBS <- stack(paste(obsDir,'/', years.hist[j], '/', varmod, '_', years.hist[j], '_', months[k],'.nc',sep=''))
#         OBS <- resample(x=crop(OBS, maskCnt), y=maskCnt, method='ngb')        
#       }  
#       
#       hist.dat <- extract(OBS, as.matrix(pts))
#       n = nlayers(OBS)  #calculate number of days in month
#       dates.mo = seq(as.Date(paste(years.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.hist[j],'-',months[k],'-',n,sep='')),by=1)
#       ind.mo = match(dates.mo,dates.hist)  #find indices of this month in big dates vector
#       hist.dat.all[,ind.mo] = hist.dat[,1:n] /10  #convert to mm before putting in big matrix
#       print(paste("Obs yr", j, "mth", k))  #print month & year of loop
#       
#     }
#   }
#   
  
  #Load historical GCM data
  years.gcm.hist = strsplit(gcmHistPer, "_")[[1]][1]:strsplit(gcmHistPer, "_")[[1]][2]  

  if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
    comb <- expand.grid(Day= sprintf("%02d", 1:30), Month= paste(sprintf("%02d", 1:12)), Year= years.gcm.hist)
    dates.gcm.hist <- paste(comb$Year, '-', comb$Month, '-', comb$Day, sep="")
  } else {
    dates.gcm.hist = seq(as.Date(paste0(strsplit(gcmHistPer, "_")[[1]][1], '-01-01')),as.Date(paste0(strsplit(gcmHistPer, "_")[[1]][2], '-12-31')),by=1)
  }  
  gcmHist.dat.all = array(NA,dim=c(ncell,length(dates.gcm.hist)))  #gridcells x days x months x years
  
  for (j in 1:length(years.gcm.hist))  {
    for (k in 1:12)  {
      gcmHist = stack(paste(iDir, '/data/gcm_res_', region, '/',gcm,'/', gcmHistPer, '/by-month/',var,'_',years.gcm.hist[j],'_',months[k],'.nc',sep=''))  
      gcmHist.dat = extract(gcmHist, as.matrix(pts))
      
      n = nlayers(gcmHist)  #calculate number of days in month
      
      if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
        dates.mo = as.character(paste(years.gcm.hist[j],'-',months[k],'-', sprintf("%02d", 1:30) ,sep=''))
      } else {
        dates.mo = seq(as.Date(paste(years.gcm.hist[j],'-',months[k],'-01',sep='')),as.Date(paste(years.gcm.hist[j],'-',months[k],'-',n,sep='')),by=1)
      }
      
      ind.mo = match(dates.mo,dates.gcm.hist)  #find indices of this month in big dates vector
      gcmHist.dat.all[,ind.mo] = gcmHist.dat[,1:n]  #put in super-matrix
      print(paste("GCMH yr", j, "mth", k))
    }
  }

  for (rcp in rcpLs){
    
    for (gcmFutPer in gcmFutPerLs){
      
      
      #Load future GCM data
      years.gcm.fut = strsplit(gcmFutPer, "_")[[1]][1]:strsplit(gcmFutPer, "_")[[1]][2]  
      
      if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
        comb <- expand.grid(Day= sprintf("%02d", 1:30), Month= paste(sprintf("%02d", 1:12)), Year= years.gcm.fut)
        dates.gcm.fut <- paste(comb$Year, '-', comb$Month, '-', comb$Day, sep="")
      } else {
        dates.gcm.fut = seq(as.Date(paste0(strsplit(gcmFutPer, "_")[[1]][1], '-01-01')),as.Date(paste0(strsplit(gcmFutPer, "_")[[1]][2], '-12-31')),by=1)
      }  
      
      gcmFut.dat.all = array(NA,dim=c(ncell,length(dates.gcm.fut)))
      
      for (j in 1:length(years.gcm.fut)) {
        for (k in 1:12)  {
          
          gcmFut = stack(paste(iDir, '/data/gcm_res_', region, '/',gcm,'/', gcmFutPer, '/', rcp, '/by-month/',var,'_',years.gcm.fut[j],'_',months[k],'.nc',sep=''))
          gcmFut.dat = extract(gcmFut, as.matrix(pts))
          
          n = nlayers(gcmFut)  #calculate number of days in month
          
          if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
            dates.mo = as.character(paste(years.gcm.fut[j],'-',months[k],'-', sprintf("%02d", 1:30) ,sep=''))
          } else {
            dates.mo = seq(as.Date(paste(years.gcm.fut[j],'-',months[k],'-01',sep='')),as.Date(paste(years.gcm.fut[j],'-',months[k],'-',n,sep='')),by=1)
          }
          
          ind.mo = match(dates.mo,dates.gcm.fut)  
          gcmFut.dat.all[,ind.mo] = gcmFut.dat[,1:n]  
          print(paste(rcp, gcmFutPer, "GCMF yr", j, "mth", k))
        }
      }
      
      # Change measure units in solar radiation
      if(var=='rsds') # W/m-2 to MJ/m-2/day-1
      {
        gcmHist.dat.all <- gcmHist.dat.all*0.0864
        gcmFut.dat.all<- gcmFut.dat.all*0.0864
      }
      
      
      
      
      
      if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
        month.hist <- as.numeric(lapply(strsplit(dates.gcm.hist,"-"), function(x) x[2]))  
        mday.hist <- as.numeric(lapply(strsplit(dates.gcm.hist,"-"), function(x) x[3]))  
        month.hist2 <- as.numeric(lapply(strsplit(dates.gcm.fut,"-"), function(x) x[2]))  
        mday.hist2 <- as.numeric(lapply(strsplit(dates.gcm.fut,"-"), function(x) x[3]))  
        ndays = 360
        comb <- expand.grid(Day= sprintf("%02d", 1:30), Month= paste(sprintf("%02d", 1:12)))
        dates_nonleap <- paste('2001-', comb$Month, '-', comb$Day, sep="")
        month.notleap <- as.numeric(lapply(strsplit(dates_nonleap,"-"), function(x) x[2]))  
        mday.notleap <- as.numeric(lapply(strsplit(dates_nonleap,"-"), function(x) x[3]))  
        
      } else {
        month.hist = month(dates.gcmhist)
        mday.hist = mday(dates.gcm.hist)
        month.hist2 = month(dates.gcm.fut)
        mday.hist2 = mday(dates.gcm.fut)  
        ndays = 365
        dates_nonleap = seq(as.Date('2001-01-01'),as.Date('2001-12-31'),by=1)  #example year  
      }
      
      
      
      #Apply bias-correction looping by gridcell (j) and day of year (k)
      
      if (!file.exists(paste(outDir, "/", gcm, "/", gcmHistPer, "/", county, "/", varmod, sep=""))) {
        dir.create(paste(outDir, "/", gcm, "/", gcmHistPer, "/", county, "/", varmod, sep=""), recursive=T)
      }
      if (!file.exists(paste(outDir, "/", gcm, "/", gcmFutPer, "/", rcp, "/", county, "/", varmod, sep=""))) {
        dir.create(paste(outDir, "/", gcm, "/", gcmFutPer, "/", rcp, "/", county, "/", varmod, sep=""), recursive=T)
      }
      
      
      gcmHistBC = array(NA,dim=c(ncell,length(dates.gcm.hist)))
      gcmFutBC = array(NA,dim=c(ncell,length(dates.gcm.fut)))  
      
      row <- 1
      
      if (!file.exists(paste(outDir, "/", gcm, "/", gcmFutPer, "/", rcp, "/", county, "/", varmod, "/bc_qmap_", varmod, "_", gcmFutPer, '.Rdat',sep=''))) {
        
        for (j in 1:ncell){ #cells
          
          for (k in 1:ndays) {  #loop through days of year, selecting 30-day moving window around each day
            #do not fit Feb. 29 (NA for GCM's)
            
            
            if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
              #Need to find indices of all days "k" and then 15 days before and after by year
              ind_k = which(month.hist == month.notleap[k] & mday.hist == mday.notleap[k])  #historical
              ind_k2 = which(month.hist2 == month.notleap[k] & mday.hist2 == mday.notleap[k])  #future
              
              for (d in 1:length(ind_k))  {
                if (d==1) {
                  ind.all = (ind_k[d]-15):(ind_k[d]+15)
                } else {
                  if (k >=346 && d >= 25){
                    ind.all = c(ind.all,(ind_k[d]-15):length(dates.gcm.hist))
                  } else {
                    ind.all = c(ind.all,(ind_k[d]-15):(ind_k[d]+15))
                  }
                  
                }
              }
            } else {
              #Need to find indices of all days "k" and then 15 days before and after by year
              ind_k = which(month.hist == month(dates_nonleap[k]) & mday.hist == mday(dates_nonleap[k]))  #historical
              ind_k2 = which(month.hist2 == month(dates_nonleap[k]) & mday.hist2 == mday(dates_nonleap[k]))  #future
              for (d in 1:length(ind_k))  {
                if (d==1) {
                  ind.all = (ind_k[d]-15):(ind_k[d]+15)
                } else {
                  if (k >=351 && d >= 25){
                    ind.all = c(ind.all,(ind_k[d]-15):length(dates.gcm.hist))
                  } else {
                    ind.all = c(ind.all,(ind_k[d]-15):(ind_k[d]+15))
                  }
                  
                }
              }
            }

            
            ind.all = ind.all[ind.all>0 & ind.all<dim(hist.dat.all)[2]]  #get rid of values outside of historical range
            
            #Extract all years (for cell j and day k with moving window) for historical obs, GCM past 
            hist.dat.i = as.vector(as.matrix(hist.dat.all)[j,ind.all])
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
              
              gcmHistBC[row,ind_k] = doQmap(x=gcmHist.dat.all[j,ind_k],qm.hist.fit, type="linear")  
              gcmFutBC[row,ind_k2] = doQmap(x=gcmFut.dat.all[j,ind_k2],qm.hist.fit, type="linear")}
              
              , error=function(e) e
            )
            if(inherits(error.p,'Error'))  next
            
            print(paste(rcp, gcmFutPer, "ncell", j, "doy", k))
          }
          
          row <- row + 1
        }
        
        
        gcmHistBC <- cbind(cellId, pts, gcmHistBC)
        gcmFutBC  <- cbind(cellId, pts, gcmFutBC)
        
        colnames(gcmHistBC) <- c('cellID', 'lon', 'lat', paste(dates.gcm.hist))
        colnames(gcmFutBC)  <- c('cellID', 'lon', 'lat', paste(dates.gcm.fut))
        
        
        # Saving results
        if(!file.exists(paste(outDir, "/", gcm, "/", gcmHistPer, "/", county, "/", varmod, "/bc_qmap_", var, "_", gcmHistPer, ".RData", sep="")))
        {
          save(gcmHistBC, file=paste(outDir, "/", gcm, "/", gcmHistPer, "/", county, "/", varmod, "/bc_qmap_", var, "_", gcmHistPer, ".RData", sep=""))
        } else {
          cat('GCM historical data with bias correction has been done!\n')
        }
        
        save(gcmFutBC, file=paste(outDir, "/", gcm, "/", gcmFutPer, "/", rcp, "/", county, "/", varmod, "/bc_qmap_", varmod, "_", gcmFutPer, '.RData',sep=''))
        
        
      }
      
    }
  }
  
}
