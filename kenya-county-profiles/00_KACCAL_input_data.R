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
options(warn=-1)
library(raster)
library(ncdf)
library(ncdf4)
library(maptools)
library(ff)
library(data.table)
library(miscTools)

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# Load CHIRPS data
chirps_dir <- '/mnt/data_cluster_4/observed/gridded_products/chirps/daily'
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
year <- year[-length(year)]

# Function to identify leap years
is.leapyear <- function(year){ return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) }

# Processing CHIRPS data by county in a table ***
chirps_county <- lapply(1:nrow(countyList), function(i)
{
  
  cat('***Processing county:', countyList$County[[i]], '***\n')
  
  # Define temporal directory to save temporary rasters created during process
  rasterOptions(tmpdir='/mnt/workspace_cluster_8/Kenya_KACCAL/data/rTemp')
  
  # Read shapefile by county
  shp <- readShapeSpatial(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp/', countyList$County[[i]], '.shp', sep=''),
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
    data_year <- raster::crop(data_year, ext_shp)
    
    # Create a rasterized version of shapefile
    if(j==1){
      template.shp <- rasterize(shp, data_year[[1]], getCover=T)
      template.shp[which(template.shp[] <= 0)] <- NA
      template.shp[which(!is.na(template.shp[]))] <- 1
      writeRaster(template.shp, paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_base.tif', sep=''), format='GTiff', overwrite=TRUE)
    } else {
      cat('Shapefile rasterized for:', countyList$County[[i]], 'exists. Read it\n')
      template.shp <-raster(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_base.tif', sep=''))
    }
    
    # Mask clipping raster using rasterized shapefile
    data_year <- mask(x=data_year, mask=template.shp)
    
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
    
  }, mc.cores=20)
  
  removeTmpFiles(h=0)
  
  names(chirps_year) <- paste('y', year, sep='')
  counDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/CHIRPS_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!dir.exists(counDir)){ dir.create(counDir) } else { cat('County folder exists\n') }
  save(chirps_year, file=paste(counDir, '/prec.RData', sep=''))
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})


# ===================================================================== #
# CH2014 process
# ===================================================================== #

# Load packages
options(warn=-1)
library(raster)
library(ncdf)
library(ncdf4)
library(maptools)
library(ff)
library(data.table)
library(miscTools)

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# Load Sheffield data
shff_dir <- '/mnt/data_cluster_4/observed/gridded_products/ncep-cru-srb-gsod-merge-for-east-west-africa/daily-files'
shff_data <- list.files(path=shff_dir, full.names=TRUE) # Listed by years
shff_data <- unlist(lapply(1:length(shff_data), function(i){z <- list.files(path=shff_data[[i]], pattern='*.nc$', full.names=TRUE); return(z)}))
# Variables that we need to analyze
varList <- c('tmin', 'tmax', 'dswrf')
shff_data <- lapply(1:length(varList), function(i){z <- grep(pattern=varList[[i]], x=shff_data, fixed=TRUE); z <- shff_data[z]; return(z)})
names(shff_data) <- c('tmin', 'tmax', 'dswrf')
year <- as.numeric(list.files(path=shff_dir, full.names=FALSE))

# Function to identify leap years
is.leapyear <- function(year){ return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) }


# Processing Sheffield data by county in a table for each variable
ch2014_var <- lapply(1:length(varList), function(i)
{
  
  cat('Processing:', varList[[i]], 'variable through years\n')
  
  # Select all layers by variable
  shff_dt_yr <- shff_data[[varList[[i]]]]
  
  # Define temporal directory to save temporary rasters created during process
  rasterOptions(tmpdir='/mnt/workspace_cluster_8/Kenya_KACCAL/data/rTemp')
  
  ch2014_county <- lapply(1:nrow(countyList), function(j)
  {
    
    cat('Processing:', countyList$County[[j]], 'county\n')
    
    # Read shapefile by county
    shp <- readShapeSpatial(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp/', countyList$County[[j]], '.shp', sep=''),
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
    # Calculing extet of shapefile
    ext_shp <- extent(shp)
    # rs_ext@xmin <- rs_ext@xmin - xres(rs_ref)*2; rs_ext@xmax <- rs_ext@xmax + xres(rs_ref)*2
    # rs_ext@ymin <- rs_ext@ymin - yres(rs_ref)*2; rs_ext@ymax <- rs_ext@ymax + yres(rs_ref)*2
    # Load rasterized shapefile
    template.shp <- raster(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/', gsub(pattern=' ', replacement='_', countyList$County[[j]]), '_base.tif', sep=''))
    
    library(parallel)
    ch2014_year <- mclapply(1:length(year), function(k)
    {
      
      cat('Processing year:', year[[k]], '\n')
      
      # Read daily rasters in one year
      data_year <- raster::stack(shff_dt_yr[grep(pattern=year[[k]], x=shff_dt_yr)])
      
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      # Resample process
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      # Load Kenya's raster
      kenya <- readShapeSpatial('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp/County.shp', proj4string=CRS("+proj=longlat +datum=WGS84"))
      # Kenya's extent
      kenya_ext <- extent(kenya)
      # Clipping rasters according to Kenya's extent
      data_year_cr <- crop(data_year, kenya_ext)
      # Load Kenya's shape rasterized
      kenya_shp <- raster('/mnt/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Kenya_base.tif')
      # Do resample
      data_year_cr_rs <- raster::resample(x=data_year_cr, y=kenya_shp, method='ngb')
      # Apply mask
      data_year_cr_rs <- mask(x=data_year_cr_rs, mask=kenya_shp)
      
      # Clipping daily rasters using extent's shapefile
      data_year <- raster::crop(data_year_cr_rs, ext_shp)
      
      # Mask clipping raster using rasterized shapefile
      data_year <- mask(x=data_year, mask=template.shp)
      
      # Verify if we have a leap year
      LeapYear <- is.leapyear(year[[k]])
      
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
      
    }, mc.cores=20)
    
    removeTmpFiles(h=0)
    
    names(ch2014_year) <- paste('y', year, sep='')
    counDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/CH2014_tables/', gsub(pattern=' ', replacement='_', countyList$County[[j]]), sep='')
    if(!dir.exists(counDir)){ dir.create(counDir) } else { cat('County folder exists\n') }
    save(ch2014_year, file=paste(counDir, '/', varList[[i]], '.RData', sep=''))
    
    return(cat('Process has been done for:', countyList$County[[j]], '\n'))
    
  })
  
  return(cat('Process has been done for:', varList[[i]], '\n'))
  
})


# ===================================================================== #
# AfSIS process
# ===================================================================== #

# Load packages
options(warn=-1)
library(raster)
library(ncdf)
library(ncdf4)
library(maptools)
library(ff)
library(data.table)
library(miscTools)

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# Soils directory
isric_dir <- '//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/ISRIC_soil'

# Rasterized Kenya shapefile
rs_adm <- raster('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Kenya_base.tif')

# Kenya shapefile
shp <- readShapeSpatial('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_shp/County.shp', proj4string=CRS("+proj=longlat +datum=WGS84"))

# Extract all coordinates different from NULL
loc_xy <- as.data.frame(xyFromCell(rs_adm, which(!is.na(rs_adm[]))))
names(loc_xy) <- c("lon","lat")

# Extract soil data, and calculate soil water holding capacity for selected sites
# soilcap = sum(af_AWCh2__M_sd[1-x]_1km) * x
root_depth <- raster(paste(isric_dir, "/af_ERDICM__M_1km.tif", sep=""))
rs_prj <- projectRaster(rs_adm, crs=root_depth@crs)
root_depth <- crop(root_depth, extent(rs_prj))
root_depth <- projectRaster(root_depth, crs=shp@proj4string)

# Calculate average of root depth for each big grid cell
rs_res <- resample(rs_adm, root_depth, method="ngb")
rs_ids <- rs_adm; rs_ids[which(!is.na(rs_ids[]))] <- which(!is.na(rs_ids[]))
rs_pts <- as.data.frame(xyFromCell(rs_res, which(!is.na(rs_res[]))))
rs_pts$id_coarse <- extract(rs_ids, cbind(x=rs_pts$x, y=rs_pts$y))
rs_pts$rdepth <- extract(root_depth, data.frame(x=rs_pts$x, y=rs_pts$y))
rs_pts <- aggregate(rs_pts[,c("x","y","rdepth")], by=list(id_coarse=rs_pts$id_coarse), FUN=function(x){mean(x,na.rm=T)})
rs_pts$x <- rs_pts$y <- NULL

# Put root_depth data on soil_data data.frame
soil_data <- loc_xy
soil_data$id_coarse <- extract(rs_ids, data.frame(x=soil_data$lon, y=soil_data$lat))
soil_data <- merge(soil_data, rs_pts, by="id_coarse")
rm(rs_pts)

# Extract soil water holding capacity data on soil_data data.frame
depths <- c(25,100,225,450,800,1500)
for (s_i in 1:6) {
  #s_i <- 1
  tdepth <- depths[s_i]
  cat("...extracting depth=",tdepth*.1,"cm\n")
  rs <- raster(paste(isric_dir,"/af_AWCh2__M_sd",s_i,"_1km.tif",sep=""))
  rs <- crop(rs, extent(rs_prj))
  rs <- projectRaster(rs, crs=shp@proj4string)
  rs_res <- resample(rs_adm, rs, method="ngb")
  rs_pts <- as.data.frame(xyFromCell(rs_res, which(!is.na(rs_res[]))))
  rs_pts$id_coarse <- extract(rs_ids, cbind(x=rs_pts$x, y=rs_pts$y))
  rs_pts$value <- extract(rs, data.frame(x=rs_pts$x, y=rs_pts$y))
  rs_pts <- aggregate(rs_pts[,c("x","y","value")],by=list(id_coarse=rs_pts$id_coarse),FUN=function(x) {mean(x,na.rm=T)})
  rs_pts$x <- rs_pts$y <- NULL
  soil_data <- merge(soil_data, rs_pts, by="id_coarse")
  names(soil_data)[ncol(soil_data)] <- paste("d.",tdepth,sep="")
  rm(list=c("rs","rs_pts","rs_res"))
}

soilcap_calc_mod <- function(x, minval, maxval) {
  if(!is.na(x[4])){
    rdepth <- max(c(x[4],minval)) #cross check
    rdepth <- min(c(rdepth,maxval)) #cross-check
    wc_df <- data.frame(depth=c(2.5,10,22.5,45,80,150),wc=(x[5:10])*.01)
    if (!rdepth %in% wc_df$depth) {
      wc_df1 <- wc_df[which(wc_df$depth < rdepth),]
      wc_df2 <- wc_df[which(wc_df$depth > rdepth),]
      y1 <- wc_df1$wc[nrow(wc_df1)]; y2 <- wc_df2$wc[1]
      x1 <- wc_df1$depth[nrow(wc_df1)]; x2 <- wc_df2$depth[1]
      ya <- (rdepth-x1) / (x2-x1) * (y2-y1) + y1
      wc_df <- rbind(wc_df1,data.frame(depth=rdepth,wc=ya),wc_df2)
    }
    wc_df <- wc_df[which(wc_df$depth <= rdepth),]
    wc_df$soilthick <- wc_df$depth - c(0,wc_df$depth[1:(nrow(wc_df)-1)])
    wc_df$soilcap <- wc_df$soilthick * wc_df$wc
    soilcp <- sum(wc_df$soilcap) * 10 #in mm
    return(soilcp)
  } else {
    soilcp <- NA
    return(soilcp)
  }
}

# Calculate soil water holding capacity in mm, minval and maxval taken from
# Fatondji et al. (2012) --in: Kihara, J. et al. Improving soil fert. recommendation using DSSAT
soil_data$soilcp <- apply(soil_data, 1, FUN=soilcap_calc_mod, minval=45, maxval=100)
save(soil_data, file='//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/ISRIC_tables/soil_data.RData')

# Create soil_data table for each county
lapply(1:nrow(countyList), function(i)
{
  cat('Load soil properties in Kenya\n')
  load('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/ISRIC_tables/soil_data.RData')
  
  cat('Load shapefile for:', countyList$County[[i]], '\n')
  shp_ras <- raster(paste('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), '_base.tif', sep=''))
  shp_tab <- as.data.frame(xyFromCell(object=shp_ras, cell=1:ncell(shp_ras)))
  shp_tab <- cbind(cellID=1:ncell(shp_ras), shp_tab)
  shp_tab <- shp_tab[which(!is.na(shp_ras[])),]
  names(shp_tab)[2:3] <- c('lon', 'lat')
  
  soil_data$cellID <- cellFromXY(object=shp_ras, xy=as.matrix(soil_data[, c('lon','lat')]))
  soil_data_county <- merge(x=shp_tab, y=soil_data, by='cellID')
  
  counDir <- paste('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/ISRIC_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!dir.exists(counDir)){ dir.create(counDir) } else { cat('County folder exists\n') }
  save(soil_data_county, file=paste(counDir, '/soil_data.RData', sep=''))
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})


# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                 First season estimates from CHIRPS
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
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
  
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/CHIRPS_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'/prec.RData', sep=''))
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
  }, mc.cores=20)
  
  indexs_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[2]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[1]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]]; z <- data.table(z); return(z)})
  names(chirps_wet_days) <- paste('y', year, sep='')
  names(indexs_wet_days) <- paste('y', year, sep='')
  
  counDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/CHIRPS_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!dir.exists(counDir)){ dir.create(counDir) } else { cat('County folder exists\n') }
  save(chirps_wet_days, file=paste(counDir, '/prec_fs_wet_days.RData', sep=''))
  save(indexs_wet_days, file=paste(counDir, '/indx_fs_wet_days.RData', sep=''))
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                 Second season estimates from CHIRPS
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
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
  
  load(paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/CHIRPS_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]),'/prec.RData', sep=''))
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
  }, mc.cores=20)
  
  indexs_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[2]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]][[1]]; return(z)})
  chirps_wet_days <- lapply(1:length(chirps_wet_days), function(i){z <- chirps_wet_days[[i]]; z <- data.table(z); return(z)})
  names(chirps_wet_days) <- paste('y', year, sep='')
  names(indexs_wet_days) <- paste('y', year, sep='')
  
  counDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/CHIRPS_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  if(!dir.exists(counDir)){ dir.create(counDir) } else { cat('County folder exists\n') }
  save(chirps_wet_days, file=paste(counDir, '/prec_ss_wet_days.RData', sep=''))
  save(indexs_wet_days, file=paste(counDir, '/indx_ss_wet_days.RData', sep=''))
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                               CH2014 first and second season estimates
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# Define Kenya counties
countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
                                   rep('Cluster 2', 4),
                                   rep('Cluster 3', 4),
                                   rep('Cluster 4', 4)),
                         County=c('Kilifi', 'Tana River', 'Garissa',
                                  'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
                                  'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
                                  'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

library(data.table)

lapply(1:nrow(countyList), function(i)
{
  
  cat('\n***Processing county:', countyList$County[[i]], '***\n\n')
  
  # Define county directory
  countyDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
  
  cat('Loading indexes by wet season/year/pixel\n')
  load(paste(countyDir, '/indx_fs_wet_days.RData', sep='')) # First 100-wettest season
  frs_wet_season <- indexs_wet_days; rm(indexs_wet_days)
  load(paste(countyDir, '/indx_ss_wet_days.RData', sep='')) # Second 100-wettest season
  scn_wet_season <- indexs_wet_days; rm(indexs_wet_days)
  
  varList <- c('tmax', 'tmin', 'dswrf')
  
  lapply(1:length(varList), function(j)
  {
    
    cat('Loading:', varList[[j]], 'data\n')
    
    countyVarDir <- paste(countyDir, '/', varList[[j]], sep='')
    load(paste(countyVarDir, '/', varList[[j]], '.RData', sep=''))
    ch2014_var <- ch2014_year; rm(ch2014_year)
    
    # Identify years to transform
    years_analysis <- intersect(names(frs_wet_season), names(ch2014_var))
    year <- as.numeric(gsub(pattern='y', replacement='', years_analysis))
    
    # Only select years to transform
    ch2014_var <- ch2014_var[years_analysis]
    frs_wet_season <- frs_wet_season[years_analysis]
    scn_wet_season <- scn_wet_season[years_analysis]
    
    library(parallel)
    ch2014_var_wet_days <- mclapply(1:length(ch2014_var), function(k)
    {
      df <- as.data.frame(ch2014_var[[k]])
      df_frs <- frs_wet_season[[k]]
      df_scn <- scn_wet_season[[k]]
      
      wet100_county <- lapply(1:nrow(df), function(m)
      {
        df_upd <- df[m,]
        cell   <- df_upd$cellID
        
        if(length(which(df_frs$cellID==cell)) > 0){
          
          indx_frs <- as.numeric(df_frs[which(df_frs$cellID==cell),-c(1:3)]) # Indexes related to first season
          df_upd_frs <- cbind(df_upd[,c('cellID', 'lon', 'lat')], df_upd[,-c(1:3)][,indx_frs])
          names(df_upd_frs)[4:length(df_upd_frs)] <- paste('d', 1:100, sep='')
          
          indx_scn <- as.numeric(df_scn[which(df_scn$cellID==cell),-c(1:3)]) # Indexes related to second season
          df_upd_scn <- cbind(df_upd[,c('cellID', 'lon', 'lat')], df_upd[,-c(1:3)][,indx_scn])
          names(df_upd_scn)[4:length(df_upd_scn)] <- paste('d', 1:100, sep='')
          
          wet100 <- list(first_season=df_upd_frs,
                         second_season=df_upd_scn)
          return(wet100)
        } else {
          return(cat('CellID:', cell, 'does not exists, continue with the process\n'))
        }
      })
      first_season <- lapply(1:length(wet100_county), function(k){z <- wet100_county[[k]][[1]]; return(z)})
      second_season <- lapply(1:length(wet100_county), function(k){z <- wet100_county[[k]][[2]]; return(z)})
      first_season <- do.call(rbind, first_season)
      second_season <- do.call(rbind, second_season)
      wet100_county <- list(f_season=first_season,
                            s_season=second_season)
      
      return(wet100_county)
      
    }, mc.cores=20)
    
    first_season_var <- lapply(1:length(ch2014_var_wet_days), function(k){z <- ch2014_var_wet_days[[k]][[1]]; z <- data.table(z); return(z)})
    names(first_season_var) <- years_analysis
    
    second_season_var <- lapply(1:length(ch2014_var_wet_days), function(k){z <- ch2014_var_wet_days[[k]][[2]]; z <- data.table(z); return(z)})
    names(second_season_var) <- years_analysis
    
    save(first_season_var, file=paste(countyVarDir, '/', varList[[j]], '_fs_wet_days.RData', sep=''))
    save(second_season_var, file=paste(countyVarDir, '/', varList[[j]], '_ss_wet_days.RData', sep=''))
    
  })
  
  return(cat('Process has been done for:', countyList$County[[i]], '\n'))
  
})

library(raster)
Kilifi <- raster('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Kilifi_base.tif')
plot(Kilifi)
xyFromCell(Kilifi, cell=c(275, 321, 322, 345, 501, 502, 592))
points(x=c(40.175, 40.175, 40.225, 40.225, 39.975, 40.025, 39.925), y=c(-2.875001, -2.975001, -2.975001, -3.025001, -3.375001, -3.375001, -3.575001), pch=20)
