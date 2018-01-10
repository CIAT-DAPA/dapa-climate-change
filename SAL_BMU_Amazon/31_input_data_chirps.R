# Processing historical data in order to calculate climatic indices by counties in Kenya
# KACCAL project
# H. Achicanoy
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                            Processing data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# ===================================================================== #
# CHIRPS process
# ===================================================================== #

# Load packages
options(warn=-1); options(scipen = 999)
suppressMessages(if(!require(raster)){install.packages('raster'); library(raster)} else {library(raster)})
suppressMessages(if(!require(ncdf)){install.packages('ncdf'); library(ncdf)} else {library(ncdf)})
suppressMessages(if(!require(ncdf4)){install.packages('ncdf4'); library(ncdf4)} else {library(ncdf4)})
suppressMessages(if(!require(maptools)){install.packages('maptools'); library(maptools)} else {library(maptools)})
suppressMessages(if(!require(ff)){install.packages('ff'); library(ff)} else {library(ff)})
suppressMessages(if(!require(data.table)){install.packages('data.table'); library(data.table)} else {library(data.table)})
suppressMessages(if(!require(miscTools)){install.packages('miscTools'); library(miscTools)} else {library(miscTools)})
suppressMessages(if(!require(rgdal)){install.packages('rgdal'); library(rgdal)} else {library(rgdal)})
suppressMessages(if(!require(foreach)){install.packages('foreach'); library(foreach)} else {library(foreach)})
suppressMessages(if(!require(doMC)){install.packages('doMC'); library(doMC)} else {library(doMC)})

# Define Kenya cou???nties
# countyList <- data.frame(Cluster=c(rep('Cluster 1', 1),
#                                    rep('Cluster 1', 1),
#                                    rep('Cluster 3', 4),
#                                    rep('Cluster 4', 4)),
#                          County=c('Kilifi', 'Tana River', 'Garissa',
#                                   'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
#                                   'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
#                                   'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
# countyList$Cluster <- as.character(countyList$Cluster)
# countyList$County <- as.character(countyList$County)
countyList <- data.frame(Cluster=c(rep('Cluster 1', 1),
                                   rep('Cluster 2', 1)), County=c('Napo', 'Ucayali')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# Count number of pixels per county
# setwd('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/')
# rList <- lapply(paste(gsub(pattern = " ", replacement = "_", countyList$County), "_base.tif", sep = ""), raster)
# data.frame(County = countyList$County, Pixels = unlist(lapply(rList, ncell)), Pixels_data = unlist(lapply(rList, function(r){length(r[!is.na(r)])})))

# Create individual shapefiles per county
# setwd('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp')
# lapply(1:nrow(countyList), function(i){
#   cat('Creating a individual shapefile for:', countyList$County[[i]], '\n')
#   kenya <- readShapeSpatial('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp/County.shp', proj4string=CRS("+proj=longlat +datum=WGS84"))
#   shp <- kenya[kenya@data$COUNTY==countyList$County[[i]],]
#   proj4string(shp) <-CRS("+proj=longlat +datum=WGS84")
#   writeSpatialShape(shp, paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp/', countyList$County[[i]], '.shp', sep = ''))
#   writeOGR(shp, ".", countyList$County[[i]], driver="ESRI Shapefile", overwrite_layer = T)
# })

# Load CHIRPS data
iDir <- "/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses"
# iDir <- "Z:/DATA/WP2/06_Clustering_analyses"
rTempDir <- paste0(iDir, "/data/rTemp2")
# rTempDir <- 'D:/cenavarro/rTemp'
region <- "amz"
chirps_dir <- '/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/02_Gridded_data/chirps_0_25deg_amz'
# chirps_dir <- "S:/observed/gridded_products/chirps/daily/32bits"

chirps_data <- list.files(path=chirps_dir, pattern='*.tif$', full.names=TRUE)
chirps_date <- list.files(path=chirps_dir, pattern='*.tif$', full.names=FALSE)
chirps_date <- gsub(pattern='chirps-v2.0.', replacement='', chirps_date)
chirps_date <- gsub(pattern='.tif', replacement='', chirps_date)
chirps_date <- strsplit(x=chirps_date, split='.', fixed=TRUE)
chirps_date <- lapply(1:length(chirps_date), function(i)
{
  df <- as.data.frame(t(chirps_date[[i]]))
  colnames(df) <- c('Year', 'Month', 'Day')
  return(df)
})
chirps_date <- do.call(rbind, chirps_date)
year <- as.numeric(as.character(unique(chirps_date$Year)))
# year <- year[-length(year)]

# Create individual rasters per county

# Function to identify leap years
is.leapyear <- function(year){ return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) }

# Processing CHIRPS data by county in a table ***
chirps_county <- lapply(1:nrow(countyList), function(i)
{
  
  cat('***Processing county:', countyList$County[[i]], '***\n')
  
  # Define temporal directory to save temporary rasters created during process
  rasterOptions(tmpdir=rTempDir)
  if(!file.exists(rTempDir)){ dir.create(rTempDir, recursive = T) }
  
  # Read shapefile by county
  shp <- readShapeSpatial(paste(iDir, '/data/', region, '_regions_shp/', countyList$County[[i]], '.shp', sep=''),
                          proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Calculing extet of shapefile
  ext_shp <- extent(shp)
  # rs_ext@xmin <- rs_ext@xmin - xres(rs_ref)*2; rs_ext@xmax <- rs_ext@xmax + xres(rs_ref)*2
  # rs_ext@ymin <- rs_ext@ymin - yres(rs_ref)*2; rs_ext@ymax <- rs_ext@ymax + yres(rs_ref)*2
  
  # Run process by year in parallel
  library(parallel)
  chirps_year <- mclapply(1:length(year), function(j){
    
    cat('Processing year:', year[[j]], '\n')
    
    # Read daily rasters in one year
    data_year <- raster::stack(chirps_data[grep(pattern=year[[j]], x=chirps_data)])
    # Clipping daily rasters using extent's shapefile
    # data_year <- raster::crop(data_year, ext_shp)
    
    # Create a rasterized version of shapefile
    if(!file.exists(paste(iDir, '/data/', region, '_regions_rst/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_base_lr.tif', sep=''))){
      template.shp <- mask(crop(data_year[[1]], ext_shp), shp)
#       template.shp <- rasterize(shp, data_year[[1]], getCover=T)    
#       template.shp[which(template.shp[] <= 0)] <- NA
      template.shp[which(!is.na(template.shp[]))] <- 1
      writeRaster(template.shp, paste(iDir, '/data/', region, '_regions_rst/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_base_lr.tif', sep=''), format='GTiff', overwrite=TRUE)
    } else {
      cat('Shapefile rasterized for:', countyList$County[[i]], 'exists. Read it\n')
      template.shp <-raster(paste(iDir, '/data/', region, '_regions_rst/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_base_lr.tif', sep=''))
    }
    
    # Mask clipping raster using rasterized shapefile
    data_year <- mask(crop(data_year/10, template.shp), template.shp)
    
    # Verify if we have a leap year
    LeapYear <- is.leapyear(year[[j]])
    
    if(LeapYear==TRUE){ # Cases where we have leap years
      temp.dt <- ff(1, dim=c(ncell(data_year), 369), vmode="double")
      lapply(1:366, function(i)
      {
        z <- data_year[[i]]
        t <- getValues(z)
        t[which(t==-9999)] <- NA
        cat('Processing: Day', i, '\n')
        temp.dt[,1] <- 1:nrow(temp.dt)
        temp.dt[,2:3] <- xyFromCell(object=z, cell=1:nrow(temp.dt))
        temp.dt[,i+3] <- t[]
        return(cat("Done\n"))
      })
      temp.dt <- as.ffdf(temp.dt)
      names(temp.dt) <- c('cellID', 'lon', 'lat', paste0("d",1:366))
      temp.dt <- as.data.frame(temp.dt)
      temp.dt <- data.table(temp.dt)
      temp.dt <- temp.dt[complete.cases(temp.dt),]
    } else { # Cases where we have normal years
      temp.dt <- ff(1, dim=c(ncell(data_year), 368), vmode="double")
      lapply(1:365, function(i)
      {
        z <- data_year[[i]]
        t <- getValues(z)
        t[which(t==-9999)] <- NA
        cat('Processing: Day', i, '\n')
        temp.dt[,1] <- 1:nrow(temp.dt)
        temp.dt[,2:3] <- xyFromCell(object=z, cell=1:nrow(temp.dt))
        temp.dt[,i+3] <- t[]
        return(cat("Done\n"))
      })
      temp.dt <- as.ffdf(temp.dt)
      names(temp.dt) <- c('cellID', 'lon', 'lat', paste0("d",1:365))
      temp.dt <- as.data.frame(temp.dt)
      temp.dt <- data.table(temp.dt)
      temp.dt <- temp.dt[complete.cases(temp.dt),]
    }
    
    return(temp.dt)
    
  }, mc.cores=35)
  
  removeTmpFiles(h=0)
  
  names(chirps_year) <- paste('y', year, sep='')
  counDir <- paste(iDir, '/data/input_tables_lr/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec', sep='')
  if(!file.exists(counDir)){ dir.create(counDir, recursive = T) } else { cat('County folder exists\n') }
  save(chirps_year, file=paste(counDir, '/prec.RData', sep=''))
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                 First season estimates from CHIRPS
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# iDir <- "/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses"
iDir <- "Z:/DATA/WP2/06_Clustering_analyses"

# Define  counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 1), rep('Cluster 2', 1)), County=c('Napo', 'Ucayali')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# Important functions
rsum.lapply <- function(x, n=3L) # Calculate rollin sum
{
  lapply(1:(length(x)-n+1), function(i)
  {
    # Sum for n consecutive days
    z <- sum(x[i:(i+n-1)])
    # Indices used to calculate the sum
    seq.sum <- as.numeric(i:(i+n-1))
    # List with SUM and INDICES
    results <- list(z, seq.sum)
    return(results)
  })
}
cumulative.r.sum <- function(results){ unlist(lapply(results, function(x){z <- x[[1]]; return(z)})) } # Extract the SUM
cumulative.r.ind <- function(results){ lapply(results, function(x){z <- x[[2]]; return(z)}) } # Extract INDICES related to SUM
is.leapyear <- function(year){ return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) } # Function to identify leap years

# First semester: 181-182 days (depending on leap years)

library(data.table)
chirps_county <- lapply(1:nrow(countyList), function(i)
{
  
  cat('***Processing county:', countyList$County[[i]], '***\n')
  
  load(paste(iDir, '/data/input_tables_lr/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'/prec/prec.RData', sep=''))
  year <- as.numeric(gsub(pattern='y', replacement='', names(chirps_year), fixed=TRUE))
  
  library(parallel)
  chirps_wet_days <- mclapply(1:length(chirps_year), function(j)
  {
    df <- chirps_year[[j]]
    Leap <- is.leapyear(year[[j]])
    if(Leap==TRUE)
    {
      wet100fs_county <- lapply(1:nrow(df), function(k)
      {
        first_season <- rsum.lapply(x=as.numeric(df[k,])[-c(1:3)][1:182], n=100)
        sum.ind <- cumulative.r.ind(first_season)[[which.max(cumulative.r.sum(first_season))]]
        df_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(df[k,])[-c(1:3)][1:182][sum.ind])))
        names(df_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        sum.ind_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(sum.ind))))
        names(sum.ind_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        wet100fs <- list(prec=df_upd,
                         ind=sum.ind_upd)
        return(wet100fs)
      })
      wet_days <- lapply(1:length(wet100fs_county), function(k){z <- wet100fs_county[[k]][[1]]; return(z)})
      wet_indx <- lapply(1:length(wet100fs_county), function(k){z <- wet100fs_county[[k]][[2]]; return(z)})
      wet_days <- do.call(rbind, wet_days)
      wet_indx <- do.call(rbind, wet_indx)
      wet100fs_county <- list(prec=wet_days,
                              ind=wet_indx)
      return(wet100fs_county)
    } else {
      wet100fs_county <- lapply(1:nrow(df), function(k)
      {
        first_season <- rsum.lapply(x=as.numeric(df[k,])[-c(1:3)][1:181], n=100)
        sum.ind <- cumulative.r.ind(first_season)[[which.max(cumulative.r.sum(first_season))]]
        df_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(df[k,])[-c(1:3)][1:181][sum.ind])))
        names(df_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        sum.ind_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(sum.ind))))
        names(sum.ind_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        wet100fs <- list(prec=df_upd,
                         ind=sum.ind_upd)
        return(wet100fs)
      })
      wet_days <- lapply(1:length(wet100fs_county), function(k){z <- wet100fs_county[[k]][[1]]; return(z)})
      wet_indx <- lapply(1:length(wet100fs_county), function(k){z <- wet100fs_county[[k]][[2]]; return(z)})
      wet_days <- do.call(rbind, wet_days)
      wet_indx <- do.call(rbind, wet_indx)
      wet100fs_county <- list(prec=wet_days,
                              ind=wet_indx)
      return(wet100fs_county)
    }
  }, mc.cores=1)
  
  indexs_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[2]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[1]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]]; z <- data.table(z); return(z)})
  names(chirps_wet_days) <- paste('y', year, sep='')
  names(indexs_wet_days) <- paste('y', year, sep='')
  
  counDir <- paste(iDir, '/data/input_tables_lr/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec', sep='')
  if(!file.exists(counDir)){ dir.create(counDir, recursive = T) } else { cat('County folder exists\n') }
  save(chirps_wet_days, file=paste(counDir, '/prec_fs_wet_days.RData', sep=''))
  save(indexs_wet_days, file=paste(counDir, '/indx_fs_wet_days.RData', sep=''))
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})



# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                 Second season estimates from CHIRPS
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# iDir <- "/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses"
iDir <- "Z:/DATA/WP2/06_Clustering_analyses"

# Define  counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 1), rep('Cluster 2', 1)), County=c('Napo', 'Ucayali')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# Important functions
rsum.lapply <- function(x, n=3L) # Calculate rollin sum
{
  lapply(1:(length(x)-n+1), function(i)
  {
    # Sum for n consecutive days
    z <- sum(x[i:(i+n-1)])
    # Indices used to calculate the sum
    seq.sum <- as.numeric(i:(i+n-1))
    # List with SUM and INDICES
    results <- list(z, seq.sum)
    return(results)
  })
}
cumulative.r.sum <- function(results){ unlist(lapply(results, function(x){z <- x[[1]]; return(z)})) } # Extract the SUM
cumulative.r.ind <- function(results){ lapply(results, function(x){z <- x[[2]]; return(z)}) } # Extract INDICES related to SUM
is.leapyear <- function(year){ return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) } # Function to identify leap years

# Second semester: 184 days (but depends on leap years too)
library(data.table)
chirps_county <- lapply(1:nrow(countyList), function(i)
{
  
  cat('***Processing county:', countyList$County[[i]], '***\n')
  
  load(paste(iDir, '/data/input_tables_lr/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'/prec/prec.RData', sep=''))
  year <- as.numeric(gsub(pattern='y', replacement='', names(chirps_year), fixed=TRUE))
  
  library(parallel)
  chirps_wet_days <- mclapply(1:length(chirps_year), function(j)
  {
    df <- chirps_year[[j]]
    Leap <- is.leapyear(year[[j]])
    if(Leap==TRUE)
    {
      wet100ss_county <- lapply(1:nrow(df), function(k)
      {
        second_season <- rsum.lapply(x=as.numeric(df[k,])[-c(1:3)][183:length(as.numeric(df[k,])[-c(1:3)])], n=100)
        sum.ind <- cumulative.r.ind(second_season)[[which.max(cumulative.r.sum(second_season))]]
        df_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(df[k,])[-c(1:3)][183:length(as.numeric(df[k,])[-c(1:3)])][sum.ind])))
        names(df_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        sum.ind_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], (183:length(as.numeric(df[k,])[-c(1:3)]))[sum.ind])))
        names(sum.ind_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        wet100ss <- list(prec=df_upd,
                         ind=sum.ind_upd)
        return(wet100ss)
      })
      wet_days <- lapply(1:length(wet100ss_county), function(k){z <- wet100ss_county[[k]][[1]]; return(z)})
      wet_indx <- lapply(1:length(wet100ss_county), function(k){z <- wet100ss_county[[k]][[2]]; return(z)})
      wet_days <- do.call(rbind, wet_days)
      wet_indx <- do.call(rbind, wet_indx)
      wet100ss_county <- list(prec=wet_days,
                              ind=wet_indx)
      return(wet100ss_county)
    } else {
      wet100ss_county <- lapply(1:nrow(df), function(k)
      {
        second_season <- rsum.lapply(x=as.numeric(df[k,])[-c(1:3)][182:length(as.numeric(df[k,])[-c(1:3)])], n=100)
        sum.ind <- cumulative.r.ind(second_season)[[which.max(cumulative.r.sum(second_season))]]
        df_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], as.numeric(df[k,])[-c(1:3)][182:length(as.numeric(df[k,])[-c(1:3)])][sum.ind])))
        names(df_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        sum.ind_upd <- data.frame(t(c(as.numeric(df[k,])[1:3], (182:length(as.numeric(df[k,])[-c(1:3)]))[sum.ind])))
        names(sum.ind_upd) <- c('cellID', 'lon', 'lat', paste('d', 1:100, sep=''))
        
        wet100ss <- list(prec=df_upd,
                         ind=sum.ind_upd)
        return(wet100ss)
      })
      wet_days <- lapply(1:length(wet100ss_county), function(k){z <- wet100ss_county[[k]][[1]]; return(z)})
      wet_indx <- lapply(1:length(wet100ss_county), function(k){z <- wet100ss_county[[k]][[2]]; return(z)})
      wet_days <- do.call(rbind, wet_days)
      wet_indx <- do.call(rbind, wet_indx)
      wet100ss_county <- list(prec=wet_days,
                              ind=wet_indx)
      return(wet100ss_county)
    }
  }, mc.cores=1)
  
  indexs_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[2]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[1]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]]; z <- data.table(z); return(z)})
  names(chirps_wet_days) <- paste('y', year, sep='')
  names(indexs_wet_days) <- paste('y', year, sep='')
  
  counDir <- paste(iDir, '/data/input_tables_lr/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '/prec', sep='')
  if(!file.exists(counDir)){ dir.create(counDir, recursive = T) } else { cat('County folder exists\n') }
  save(chirps_wet_days, file=paste(counDir, '/prec_ss_wet_days.RData', sep=''))
  save(indexs_wet_days, file=paste(counDir, '/indx_ss_wet_days.RData', sep=''))
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})
