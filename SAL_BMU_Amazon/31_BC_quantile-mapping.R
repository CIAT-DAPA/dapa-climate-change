### Perform quantile-mapping bias correction of daily climate data
### H. Achicanoy - C. Navarro
### CIAT, 2016

# Load libraries
library(qmap)
library(ncdf4)
library(raster)
library(lubridate)
library(reshape)
library(compiler)

# Quantile-mapping bias correction function
BC_Qmap <- function(county="Busia", rcp="rcp45", gcm="bcc_csm1_1", var="prec", period="2021_2045", iDir="/mnt"){
  
  cat(' *** Performing Quantile-mapping bias correction for', county, 'in the period', period, ', using: RCP', rcp, ', GCM', gcm, 'and variable', var, '***\n')
  
  # Establish directories
  obsDir     <- paste0(iDir, "/data/input_tables_lr")
  obsDir     <- paste0(obsDir, "/", county)
  
  gcmHistDir <- paste0(iDir, "/data/gcm_res_0_25_amz/", gcm, "/1981_2005")
  gcmFutDir  <- paste0(iDir, "/data/gcm_res_0_25_amz/", gcm, "/", period, "/", rcp)
  outDir     <- paste0(iDir, "/data/bc_0_25deg_amz")
  
  if(!file.exists(outDir)){dir.create(outDir)}
  years_analysis <- paste('y', 1981:2010, sep='')
  
  # Verify if output file exists
  if(!file.exists(paste(outDir, "/", gcm, "/", period, "/", rcp, "/", county, "/", var, "/bc_qmap_", var, "_", period, ".RData", sep=""))){
    
    # Changes in climatic variables names
    if(var == "dswrf"){varmod <- "rsds"} else {varmod <- var}
    
    # Establish directories to save climatic information by period
    if(!file.exists(paste(outDir, "/", gcm, "/1981_2005", sep=""))){dir.create(paste(outDir, "/", gcm, "/1981_2005", sep=""), recursive=T)}
    if(!file.exists(paste(outDir, "/", gcm, "/", period, sep=""))){dir.create(paste(outDir, "/", gcm, "/", period, sep=""), recursive=T)}
    
    # Load mask for each county
    cat('Loading county mask for:', county, '\n')
    countyMask <- raster(paste(iDir, "/data/amz_regions_rst/", county, "_base_lr.tif", sep=""))
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    # Load historical observed data
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    
    cat('Loading historical information for:', county, '\n')
    if(var=='prec')
    {
      load(paste(obsDir, '/', var, '/', var, '.RData', sep=''))
      prec <- chirps_year; rm(chirps_year)
      prec <- prec[years_analysis]
      prec <- lapply(1:length(prec), function(i){z <- as.data.frame(prec[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(prec)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
      names(prec) <- years_analysis
      hist.dat.all <- merge_recurse(prec); rm(prec)
    } else {
      if(var=='tmax')
      {
        load(paste(obsDir, '/', var, '/', var, '.RData', sep=''))
        tmax <- agmerra_year; rm(agmerra_year)
        tmax <- tmax[years_analysis]
        tmax <- lapply(1:length(tmax), function(i){z <- as.data.frame(tmax[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmax)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
        names(tmax) <- years_analysis
        hist.dat.all <- reshape::merge_recurse(tmax); rm(tmax)
      } else {
        if(var=='tmin')
        {
          load(paste(obsDir, '/', var, '/', var, '.RData', sep=''))
          tmin <- agmerra_year; rm(agmerra_year)
          tmin <- tmin[years_analysis]
          tmin <- lapply(1:length(tmin), function(i){z <- as.data.frame(tmin[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmin)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
          names(tmin) <- years_analysis
          hist.dat.all <- reshape::merge_recurse(tmin); rm(tmin)
        } else {
          if(var=='dswrf')
          {
            load(paste(obsDir, '/', var, '/', var, '.RData', sep=''))
            dswrf <- agmerra_year; rm(agmerra_year)
            dswrf <- dswrf[years_analysis]
            dswrf <- lapply(1:length(dswrf), function(i){z <- as.data.frame(dswrf[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(dswrf)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
            names(dswrf) <- years_analysis
            hist.dat.all <- reshape::merge_recurse(dswrf); rm(dswrf)
          }
        }
      }
    }
    hist.dat.all <- as.matrix(hist.dat.all)
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    # Load historical GCM data
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    
    cat('Loading GCM historical information for:', county, '\n')
    years.gcm.hist <- 1981:2005 # Originally is 1971
    # dates.gcm.hist <- seq(as.Date('1971-01-01'), as.Date('2000-12-31'), by=1)
    gcmDataProcess <- function(yr)
    {
      monthProcess <- function(mth)
      {
        gcmHist <- stack(paste(gcmHistDir, '/by-month/', varmod, '_', yr, '_', mth, '.nc', sep=''))
        gcmHist <- mask(resample(crop(gcmHist, countyMask), countyMask), countyMask) 
        gcmHist.dat <- as.data.frame(rasterToPoints(gcmHist))
        names(gcmHist.dat)[1:2] <- c('lon', 'lat')
        gcmHist.dat <- cbind(data.frame(cellID=cellFromXY(object=gcmHist, xy=gcmHist.dat[,c('lon', 'lat')])), gcmHist.dat)
        n <- dim(gcmHist.dat)[2] - 3 # Calculate number of days in month
        
        if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
          names(gcmHist.dat)[4:length(names(gcmHist.dat))] <- as.character(paste(yr, '-', mth, '-', sprintf("%02d", 1:30), sep=""))  
        } else {
          names(gcmHist.dat)[4:length(names(gcmHist.dat))] <- as.character(seq(as.Date(paste(yr, '-', mth,'-01', sep='')), as.Date(paste(yr, '-', mth, '-', n, sep='')), by=1))  
        }
        return(gcmHist.dat)
      }
      monthProcess <- Vectorize(monthProcess, vectorize.args='mth')
      monthData <- as.list(monthProcess(mth=c(paste('0', 1:9, sep=''), 10:12)))
      monthData <- reshape::merge_recurse(monthData)
      return(monthData)
    }
    library(foreach)
#     library(doMC)
#     registerDoMC(length(years.gcm.hist))
    gcmHist.dat.all <- foreach(i=1:length(years.gcm.hist)) %dopar% {
      cat(i)
      gcmHist.dat.all <- gcmDataProcess(yr=years.gcm.hist[i])
    }
    gcmHist.dat.all <- as.matrix(reshape::merge_recurse(gcmHist.dat.all))
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    # Load future GCM data
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    
    cat('Loading GCM future information for:', county, '\n')
    inYear <- strsplit(period, split='_'); inYear <- as.numeric(inYear[[1]][1])
    enYear <- strsplit(period, split='_'); enYear <- as.numeric(enYear[[1]][2])
    years.gcm.fut = inYear:enYear
    # dates.gcm.fut = seq(as.Date(paste(inYear, '-01-01', sep='')), as.Date(paste(enYear, '-12-31', sep='')), by=1)
    gcmDataFutureProcess <- function(yr)
    {
      monthProcess <- function(mth)
      {
        gcmFut <- stack(paste(gcmFutDir, '/by-month/', varmod, '_', yr, '_', mth, '.nc', sep=''))
        gcmFut <- mask(resample(crop(gcmFut, countyMask), countyMask), countyMask) 
        gcmFut.dat <- as.data.frame(rasterToPoints(gcmFut))
        names(gcmFut.dat)[1:2] <- c('lon', 'lat')
        gcmFut.dat <- cbind(data.frame(cellID=cellFromXY(object=gcmFut, xy=gcmFut.dat[,c('lon', 'lat')])), gcmFut.dat)
        n <- dim(gcmFut.dat)[2] - 3 # Calculate number of days in month
        
        if(gcm == "mohc_hadgem2_es" || gcm == "nimr_hadgem2_ao"){
          names(gcmFut.dat)[4:length(names(gcmFut.dat))] <- as.character(paste(yr, '-', mth, '-', sprintf("%02d", 1:30), sep=""))  
        } else {
          names(gcmFut.dat)[4:length(names(gcmFut.dat))] <- as.character(seq(as.Date(paste(yr, '-', mth,'-01', sep='')), as.Date(paste(yr, '-', mth, '-', n, sep='')), by=1))  
        }
        
        return(gcmFut.dat)
      }
      monthProcess <- Vectorize(monthProcess, vectorize.args='mth')
      monthData <- as.list(monthProcess(mth=c(paste('0', 1:9, sep=''), 10:12)))
      monthData <- reshape::merge_recurse(monthData)
      return(monthData)
    }
    library(foreach)
    library(doMC)
    registerDoMC(length(years.gcm.fut))
    gcmFut.dat.all <- foreach(i=1:length(years.gcm.fut)) %dopar% {
      gcmFut.dat.all <- gcmDataFutureProcess(yr=years.gcm.fut[i])
    }
    gcmFut.dat.all <- as.matrix(reshape::merge_recurse(gcmFut.dat.all))
    
    # Change measure units in solar radiation
    if(varmod=='rsds') # W/m-2 to MJ/m-2/day-1
    {
      gcmHist.dat.all[, 4:ncol(gcmHist.dat.all)] <- gcmHist.dat.all[,4:ncol(gcmHist.dat.all)]*0.0864
      gcmFut.dat.all[, 4:ncol(gcmFut.dat.all)] <- gcmFut.dat.all[,4:ncol(gcmFut.dat.all)]*0.0864
    }
    
    # It is possible that historical data frame does not have all cells because problems of data source
    ncell <- length(Reduce(intersect, list(hist.dat.all[,'cellID'], gcmHist.dat.all[,'cellID'], gcmFut.dat.all[,'cellID'])))
    pixelList <- Reduce(intersect, list(hist.dat.all[,'cellID'], gcmHist.dat.all[,'cellID'], gcmFut.dat.all[,'cellID']))
    
    month.hist    <- month(as.Date(colnames(gcmHist.dat.all)[-c(1:3)]))
    mday.hist     <- mday(as.Date(colnames(gcmHist.dat.all)[-c(1:3)]))
    month.hist2   <- month(as.Date(colnames(gcmFut.dat.all)[-c(1:3)]))
    mday.hist2    <- mday(as.Date(colnames(gcmFut.dat.all)[-c(1:3)]))
    dates_nonleap <- seq(as.Date('2001-01-01'),as.Date('2001-12-31'),by=1) # Example year
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    # Make Quantile-mapping bias correct process by pixel
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
    
    gcmHistBC <- array(NA, dim=c(ncell, length(colnames(gcmHist.dat.all)[-c(1:3)]))); colnames(gcmHistBC) <- as.character(colnames(gcmHist.dat.all)[-c(1:3)])
    gcmFutBC  <- array(NA, dim=c(ncell, length(colnames(gcmFut.dat.all)[-c(1:3)]))); colnames(gcmFutBC) <- as.character(colnames(gcmFut.dat.all)[-c(1:3)])
    
    # GCM historical path
    if(!file.exists(paste(outDir, "/", gcm, "/1981_2005/", county, "/", var, sep=""))){ dir.create(path=paste(outDir, "/", gcm, "/1981_2005/", county, "/", var, sep=""), recursive=TRUE) }
    # GCM future path
    if(!file.exists(paste(outDir, "/", gcm, "/", period, "/", rcp, "/", county, "/", var, sep=""))){ dir.create(path=paste(outDir, "/", gcm, "/", period, "/", rcp, "/", county, "/", var, sep=""), recursive=TRUE) }
    
    if(!file.exists(paste(outDir, "/", gcm, "/", period, "/", rcp, "/", county, "/", var, "/bc_qmap_", var, "_", period, ".RData", sep="")))
    {
      
      # Loop through days of year, selecting 30-day moving window around each day
      # Do not fit Feb. 29 (NA for GCM's)
      
      cat('Making Quantile-mapping bias correction\n')
      
      dailyProcess <- function(k){
        
        # Need to find indices of all days "k" and then 15 days before and after by year
        ind_k  <- which(month.hist == month(dates_nonleap[k]) & mday.hist == mday(dates_nonleap[k])) # Historical
        ind_k2 <- which(month.hist2 == month(dates_nonleap[k]) & mday.hist2 == mday(dates_nonleap[k])) # Future
        
        for (d in 1:length(ind_k))  {
          if (d==1) {
            ind.all = (ind_k[d]-15):(ind_k[d]+15)
          } else {
            if (k >=351 && d >= 25){
              ind.all = c(ind.all,(ind_k[d]-15):length(dates_nonleap))
            } else {
              ind.all = c(ind.all,(ind_k[d]-15):(ind_k[d]+15))
            }
            
          }
        }
        #         
        #         for(d in 1:length(ind_k))
        #         {
        #           if(d==1)
        #           {
        #             ind.all <- (ind_k[d]-15):(ind_k[d]+15)
        #           } else {
        #             ind.all <- c(ind.all,(ind_k[d]-15):(ind_k[d]+15))
        #           }
        #         }
        # Get rid of values outside of historical range
        ind.all <- ind.all[ind.all>0 & ind.all<(dim(hist.dat.all)[2]-3)]
        
        # Extract all years (for cell j and day k with moving window) for historical obs, GCM past
        # Verify that j cell is the same for each matrix
        
        hist.dat.i <- hist.dat.all[match(pixelList, hist.dat.all[,'cellID']), ind.all+3]
        hist.dat.i <- t(hist.dat.i); colnames(hist.dat.i) <- paste('px', pixelList, sep='')
        gcmHist.dat.i <- gcmHist.dat.all[match(pixelList, gcmHist.dat.all[,'cellID']), ind.all+3]
        gcmHist.dat.i <- t(gcmHist.dat.i); colnames(gcmHist.dat.i) <- paste('px', pixelList, sep='')
        
        # Fit model b/w historical obs & GCM past
        # Keep going if can't apply model (all zeros in obs)
        error.p <- tryCatch(
{
  
  if(var=="prec")
  {
    qm.hist.fit <- fitQmap(obs=hist.dat.i, mod=gcmHist.dat.i, method="RQUANT", qstep=0.01, wet.day=TRUE, na.rm=TRUE)
  } else {
    qm.hist.fit <- fitQmap(obs=hist.dat.i, mod=gcmHist.dat.i, method="RQUANT", qstep=0.01, wet.day=FALSE, na.rm=TRUE)
  }
  
  gcmHist_base           <- t(gcmHist.dat.all[match(pixelList, gcmHist.dat.all[,'cellID']), ind_k+3])
  # gcmHist_base           <- t(gcmHist.dat.all[match(pixelList, gcmHist.dat.all[,'cellID']), (ind_k+3)[-which(ind_k+3>ncol(gcmHist.dat.all))]])
  colnames(gcmHist_base) <- paste('px', pixelList, sep='')
  
  gcmFut_base           <- t(gcmFut.dat.all[match(pixelList, gcmFut.dat.all[,'cellID']), ind_k2+3])
  colnames(gcmFut_base) <- paste('px', pixelList, sep='')
  
  gcmHistBC_day <- doQmap(x=gcmHist_base, qm.hist.fit, type="linear")
  gcmHistBC_day <- t(gcmHistBC_day)
  gcmFutBC_day  <- doQmap(x=gcmFut_base, qm.hist.fit, type="linear")
  gcmFutBC_day  <- t(gcmFutBC_day)
  
}, error = function(e) e

        )

# if(inherits(error.p, 'Error')) next

return(list(historical=gcmHistBC_day,
            future=gcmFutBC_day))

      }
library(compiler)
dailyProcessCMP <- cmpfun(dailyProcess)
dailyProcessRes <- foreach(i=1:365) %dopar% {
  dailyProcessCMP(k=i)
}

for(i in 1:length(dailyProcessRes)){
  gcmHistBC[,match(colnames(dailyProcessRes[[i]][[1]]), colnames(gcmHistBC))] <- dailyProcessRes[[i]][[1]]
}; rm(i)

for(i in 1:length(dailyProcessRes)){
  gcmFutBC[,match(colnames(dailyProcessRes[[i]][[2]]), colnames(gcmFutBC))] <- dailyProcessRes[[i]][[2]]
}; rm(i)

gcmHistBC <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), gcmHistBC)
gcmFutBC  <- cbind(pixelList, xyFromCell(object=countyMask, cell=pixelList), gcmFutBC)

colnames(gcmHistBC) <- c('cellID', 'lon', 'lat', colnames(gcmHistBC)[-c(1:3)])
colnames(gcmFutBC)  <- c('cellID', 'lon', 'lat', colnames(gcmFutBC)[-c(1:3)])

# Saving results
if(!file.exists(paste(outDir, "/", gcm, "/1981_2005/", county, "/", var, "/bc_qmap_", var, "_1981_2005.RData", sep="")))
{
  save(gcmHistBC, file=paste(outDir, "/", gcm, "/1981_2005/", county, "/", var, "/bc_qmap_", var, "_1981_2005.RData", sep=""))
} else {
  cat('GCM historical data with bias correction has been done!\n')
}

save(gcmFutBC, file=paste(outDir, "/", gcm, "/", period, "/", rcp, "/", county, "/", var, "/bc_qmap_", var, "_", period, ".RData", sep=""))

    }

  } else {
    cat('Quantile-mapping process has been done. It is not necessary recalculate all again.\n')
  }

}

# This process would be done for each county, but for now I ran this process only for four counties with high importance
iDir <- "/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses" 
# iDir <-  "Z:/DATA/WP2/06_Clustering_analyses"
rcpList    <- paste("rcp", c(26, 45, 60, 85), sep="")
periodList <- c("2020_2049",'2040_2069')
countyList <- c("Ucayali")
gcmList <- c("bcc_csm1_1", "bcc_csm1_1_m","csiro_mk3_6_0", "gfdl_cm3", "ipsl_cm5a_lr","miroc_esm", "miroc_esm_chem","miroc_miroc5", "ncc_noresm1_m", "mri_cgcm3", "mohc_hadgem2_es", "nimr_hadgem2_ao")
varList    <- c('prec', 'tmax', 'tmin', 'dswrf')

# iDir <- "Z:/DATA/WP2/06_Clustering_analyses"
# source("31_BC_quantile-mapping.R")


lapply(1:length(countyList), function(z){
  lapply(1:length(varList), function(i){  
    cat('Processing variable:', varList[[i]], '\n')
    lapply(1:length(gcmList), function(j){
      cat('Processing GCM:', gcmList[[j]], '\n')
      lapply(1:length(periodList), function(l){       
        cat('Processing period:',periodList[[l]] , '\n')
        lapply(1:length(rcpList), function(k){
          cat('Processing RCP:', rcpList[[k]], '\n')
          
          BC_Qmap(county=countyList[[z]], rcp=rcpList[[k]], gcm=gcmList[[j]], var=varList[[i]], period=periodList[[l]], iDir)
        })
      })
    })
  })
})




