# Processing historical data in order to calculate climatic indices by counties in Kenya
# KACCAL project
# H. Achicanoy
# CIAT, 2016

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                            Processing data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #


# ===================================================================== #
# agmerra process
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
countyList <- data.frame(Cluster=c(rep('Cluster 1', 1),
                                   rep('Cluster 2', 1)), County=c('Napo', 'Ucayali')) # Define counties to analyze by cluster
countyList$Cluster <- as.character(countyList$Cluster)
countyList$County <- as.character(countyList$County)

# Set working dirs 
iDir <- "/mnt/workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses"
# iDir <- "Z:/DATA/WP2/06_Clustering_analyses"
region <- "amz"
rTempDir <- paste0(iDir, "/data/rTemp")
year <- as.numeric(1981:2010)

# Load Sheffield data
shff_dir <- '/mnt/data_cluster_5/cropdata/agmerra/daily/nc-files/by-year'
# shff_dir <- 'U:/cropdata/agmerra/daily/nc-files/by-year' 
shff_data <- list.files(path=shff_dir, full.names=TRUE) # Listed by years
# shff_data <- unlist(lapply(1:length(shff_data), function(i){z <- list.files(path=shff_data[[i]], pattern='*.nc$', full.names=TRUE); return(z)}))

# Variables that we need to analyze
varList <- c('dswrf')
# shff_data <- lapply(1:length(varList), function(i){z <- grep(pattern=varList[[i]], x=shff_data, fixed=TRUE); z <- shff_data[z]; return(z)})
# names(shff_data) <- c('tmin', 'tmax', 'dswrf')

# Function to identify leap years
is.leapyear <- function(year){ return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)) }

# Processing Sheffield data by county in a table for each variable
agmerra_var <- lapply(1:length(varList), function(i)
{
 
  cat('Processing:', varList[[i]], 'variable through years\n')
  
  # Select all layers by variable
#   shff_dt_yr <- shff_data[[varList[[i]]]]
  
  # Define temporal directory to save temporary rasters created during process
  rasterOptions(tmpdir=rTempDir)
  
  agmerra_county <- lapply(1:nrow(countyList), function(j)
  {
    
    cat('Processing:', countyList$County[[j]], 'county\n')
    
    # Read shapefile by county
    shp <- readShapeSpatial(paste(iDir, '/data/', region, '_regions_shp/', countyList$County[[j]], '.shp', sep=''),
                            proj4string=CRS("+proj=longlat +datum=WGS84"))

    # Calculing extet of shapefile
    ext_shp <- extent(shp)
    # rs_ext@xmin <- rs_ext@xmin - xres(rs_ref)*2; rs_ext@xmax <- rs_ext@xmax + xres(rs_ref)*2
    # rs_ext@ymin <- rs_ext@ymin - yres(rs_ref)*2; rs_ext@ymax <- rs_ext@ymax + yres(rs_ref)*2
    # Load rasterized shapefile
    template.shp <- raster(paste(iDir, '/data/', region, '_regions_rst/', gsub(pattern=' ', replacement='_', countyList$County[[j]]), '_base.tif', sep=''))
    
    library(parallel)
    agmerra_year <- mclapply(1:length(year), function(k)
    {
      
      cat('Processing year:', year[[k]], '\n')
      
      # Read daily rasters in one year
#       data_year <- raster::stack(shff_dt_yr[grep(pattern=year[[k]], x=shff_dt_yr)])
      data_year <- raster::stack(paste0(shff_dir, "/", varList[[i]], "_", year[[k]], ".nc"))      

      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      # Resample process
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      # Load Kenya's raster
      kenya <- readShapeSpatial(paste0(iDir, '/data/', region, '_regions_shp/', region, '.shp'), proj4string=CRS("+proj=longlat +datum=WGS84"))
      
      # Kenya's extent
      kenya_ext <- extent(kenya)
      
      if(!file.exists(paste(iDir, '/data/', region, '_regions_rst/', region, '_base.tif', sep=''))){
        kenya_shp <- mask(crop(data_year[[1]], kenya_ext), kenya)
        #       template.shp <- rasterize(shp, data_year[[1]], getCover=T)    
        #       template.shp[which(template.shp[] <= 0)] <- NA
        kenya_shp[which(!is.na(kenya_shp[]))] <- 1
        writeRaster(kenya_shp, paste(iDir, '/data/', region, '_regions_rst/', region, '_base.tif', sep=''), format='GTiff', overwrite=TRUE)
      } else {
        cat('Shapefile rasterized for:', countyList$County[[i]], 'exists. Read it\n')
        kenya_shp <-raster(paste(iDir, '/data/', region, '_regions_rst/', region, '_base.tif', sep=''))
      }
      
      # Clipping rasters according to Kenya's extent
      data_year_cr <- crop(data_year, extent(kenya_shp))
      # # Load Kenya's shape rasterized
      # kenya_shp <- raster(paste0(iDir, '/data/', region, '_regions_rst/', region, '_base.tif'))
      # Do resample
      data_year_cr <- raster::resample(x=data_year_cr, y=kenya_shp, method='ngb')
      
      # Apply mask
      data_year_cr_rs <- mask(x=data_year_cr, mask=kenya_shp)
      
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
          # t <- getValues(z)
          t <- rasterToPoints(z)[,3]
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
          # t <- getValues(z)
          t <- rasterToPoints(z)[,3]
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
      
    }, mc.cores=30)
    
    removeTmpFiles(h=0)
    
    names(agmerra_year) <- paste('y', year, sep='')
    counDir <- paste(iDir, '/data/input_tables/', gsub(pattern=' ', replacement='_', countyList$County[[j]]), "/", varList[[i]], sep='')
    if(!file.exists(counDir)){ dir.create(counDir, recursive=T) } else { cat('County folder exists\n') }
    save(agmerra_year, file=paste(counDir, '/', varList[[i]], '.RData', sep=''))
    
    return(cat('Process has been done for:', countyList$County[[j]], '\n'))
    
  })
  
  return(cat('Process has been done for:', varList[[i]], '\n'))
  
})



# # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# #                                               agmerra first and second season estimates
# # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
# 
# # Define Kenya counties
# countyList <- data.frame(Cluster=c(rep('Cluster 1', 3),
#                                    rep('Cluster 2', 4),
#                                    rep('Cluster 3', 4),
#                                    rep('Cluster 4', 4)),
#                          County=c('Kilifi', 'Tana River', 'Garissa',
#                                   'Kwale', 'Makueni', 'Taita Taveta', 'Embu',
#                                   'Meru', 'Nyeri', 'Nyandarua', 'Nakuru',
#                                   'Homa Bay', 'Siaya', 'Busia', 'West Pokot')) # Define counties to analyze by cluster
# countyList$Cluster <- as.character(countyList$Cluster)
# countyList$County <- as.character(countyList$County)
# 
# library(data.table)
# 
# lapply(1:nrow(countyList), function(i)
# {
#   
#   cat('\n***Processing county:', countyList$County[[i]], '***\n\n')
#   
#   # Define county directory
#   countyDir <- paste('/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
#   
#   cat('Loading indexes by wet season/year/pixel\n')
#   load(paste(countyDir, '/indx_fs_wet_days.RData', sep='')) # First 100-wettest season
#   frs_wet_season <- indexs_wet_days; rm(indexs_wet_days)
#   load(paste(countyDir, '/indx_ss_wet_days.RData', sep='')) # Second 100-wettest season
#   scn_wet_season <- indexs_wet_days; rm(indexs_wet_days)
#   
#   varList <- c('tmax', 'tmin', 'dswrf')
#   
#   lapply(1:length(varList), function(j)
#   {
#     
#     cat('Loading:', varList[[j]], 'data\n')
#     
#     countyVarDir <- paste(countyDir, '/', varList[[j]], sep='')
#     load(paste(countyVarDir, '/', varList[[j]], '.RData', sep=''))
#     agmerra_var <- agmerra_year; rm(agmerra_year)
#     
#     # Identify years to transform
#     years_analysis <- intersect(names(frs_wet_season), names(agmerra_var))
#     year <- as.numeric(gsub(pattern='y', replacement='', years_analysis))
#     
#     # Only select years to transform
#     agmerra_var <- agmerra_var[years_analysis]
#     frs_wet_season <- frs_wet_season[years_analysis]
#     scn_wet_season <- scn_wet_season[years_analysis]
#     
#     library(parallel)
#     agmerra_var_wet_days <- mclapply(1:length(agmerra_var), function(k)
#     {
#       df <- as.data.frame(agmerra_var[[k]])
#       df_frs <- frs_wet_season[[k]]
#       df_scn <- scn_wet_season[[k]]
#       
#       wet100_county <- lapply(1:nrow(df), function(m)
#       {
#         df_upd <- df[m,]
#         cell   <- df_upd$cellID
#         
#         if(length(which(df_frs$cellID==cell)) > 0){
#           
#           indx_frs <- as.numeric(df_frs[which(df_frs$cellID==cell),-c(1:3)]) # Indexes related to first season
#           df_upd_frs <- cbind(df_upd[,c('cellID', 'lon', 'lat')], df_upd[,-c(1:3)][,indx_frs])
#           names(df_upd_frs)[4:length(df_upd_frs)] <- paste('d', 1:100, sep='')
#           
#           indx_scn <- as.numeric(df_scn[which(df_scn$cellID==cell),-c(1:3)]) # Indexes related to second season
#           df_upd_scn <- cbind(df_upd[,c('cellID', 'lon', 'lat')], df_upd[,-c(1:3)][,indx_scn])
#           names(df_upd_scn)[4:length(df_upd_scn)] <- paste('d', 1:100, sep='')
#           
#           wet100 <- list(first_season=df_upd_frs,
#                          second_season=df_upd_scn)
#           return(wet100)
#         } else {
#           return(cat('CellID:', cell, 'does not exists, continue with the process\n'))
#         }
#       })
#       first_season <- lapply(1:length(wet100_county), function(k){z <- wet100_county[[k]][[1]]; return(z)})
#       second_season <- lapply(1:length(wet100_county), function(k){z <- wet100_county[[k]][[2]]; return(z)})
#       first_season <- do.call(rbind, first_season)
#       second_season <- do.call(rbind, second_season)
#       wet100_county <- list(f_season=first_season,
#                             s_season=second_season)
#       
#       return(wet100_county)
#       
#     }, mc.cores=20)
#     
#     first_season_var <- lapply(1:length(agmerra_var_wet_days), function(k){z <- agmerra_var_wet_days[[k]][[1]]; z <- data.table(z); return(z)})
#     names(first_season_var) <- years_analysis
#     
#     second_season_var <- lapply(1:length(agmerra_var_wet_days), function(k){z <- agmerra_var_wet_days[[k]][[2]]; z <- data.table(z); return(z)})
#     names(second_season_var) <- years_analysis
#     
#     save(first_season_var, file=paste(countyVarDir, '/', varList[[j]], '_fs_wet_days.RData', sep=''))
#     save(second_season_var, file=paste(countyVarDir, '/', varList[[j]], '_ss_wet_days.RData', sep=''))
#     
#   })
#   
#   return(cat('Process has been done for:', countyList$County[[i]], '\n'))
#   
# })
# 
# library(raster)
# Kilifi <- raster('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties_rst/Kilifi_base.tif')
# plot(Kilifi)
# xyFromCell(Kilifi, cell=c(275, 321, 322, 345, 501, 502, 592))
# points(x=c(40.175, 40.175, 40.225, 40.225, 39.975, 40.025, 39.925), y=c(-2.875001, -2.975001, -2.975001, -3.025001, -3.375001, -3.375001, -3.575001), pch=20)
