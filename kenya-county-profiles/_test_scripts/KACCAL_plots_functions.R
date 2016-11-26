# Graphics and other functions
# H. Achicanoy
# CIAT, 2016

# ===================================================================== #
# CHIRPS data
# ===================================================================== #

# Plotting each time serie
plot(1:length(as.numeric(temp.dt[1,])), as.numeric(temp.dt[1,]), type='l', ylim=c(0,100), xlab='Days a year', ylab='Precipitation (mm)')
lapply(1:nrow(temp.dt), function(i){
  lines(1:length(as.numeric(temp.dt[1,])), as.numeric(temp.dt[i,]), col=1)
})

# Plotting cumulative time serie for each site in this year
plot(colMeans(temp.dt)[-1], type='l') # plot(colMedians(temp.dt)[-1], type='l')
plot(cumsum(colMeans(temp.dt)[-1]), type='l', col=2, xlab='Days a year', ylab='Cumulative precipitation (mm)')
lapply(2:nrow(temp.dt), function(i){
  lines(cumsum(as.numeric(temp.dt[i,])[-1]), col=2)
})

# ===================================================================== #
# CH2014 data
# ===================================================================== #

# Plotting each time serie
plot(1:length(as.numeric(temp.dt[1,])), as.numeric(temp.dt[1,]), type='l', ylim=c(0, 40), xlab='Days a year', ylab='Radiacion solar')
lapply(1:nrow(temp.dt), function(i){
  lines(1:length(as.numeric(temp.dt[1,])), as.numeric(temp.dt[i,]), col=1)
})

# ===================================================================== #
# IMPORTANT functions
# ===================================================================== #

### Estimate 100-day wettest period in each semester of year for each pixel ***

# Function to calculate sum for n consecutive days moving forward each index
rsum.lapply <- function(x, n=3L)
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

# Function to extract the SUM
cumulative.f.season <- function(results)
{
  unlist(lapply(results, function(x){z <- x[[1]]; return(z)}))
}

# Function to extract the INDICES
cumsum.f.season <- function(results)
{
  lapply(results, function(x){z <- x[[2]]; return(z)})
}

# First semester contains 181-182 days (depending on leap years) while second semester contains 184 days
# Example

# Table with data: rows correspond to pixels; columns correspond to days within the year. Each table represent a year. List of tables corresponds to a county
temp.dt <- chirps_year[[35]]
temp.dt <- chirps_year[[which(names(chirps_year)=='y1998')]]

# This object contains the SUM and list of INDICES used to calculate the SUM
pixel.first.season <- rsum.lapply(x=as.numeric(temp.dt[2,])[-1][1:181], n=100)
# This object contains list of SUMs
cumulative.test <- cumulative.f.season(pixel.first.season)
# This object contains list of INDICES
cumsum.test <- cumsum.f.season(pixel.first.season)

# Plotting cumulative curves for each sequence of 100 days within a semester
plot(1:100, cumsum(as.numeric(temp.dt[2,])[-1][cumsum.test[[1]]]), type='l', ylim=c(0,250), xlab='Days a year', ylab='Precipitation (mm)')
lapply(1:length(pixel.first.season), function(i){
  lines(1:100, cumsum(as.numeric(temp.dt[2,])[-1][cumsum.test[[i]]]), col=1)
})
lines(1:100, cumsum(as.numeric(temp.dt[2,])[-1][cumsum.test[[7]]]), col=2, lwd=5)

### Creating shapefiles for each county (DONE) ***
# df <-  readOGR("D:/ToBackup/Modelling/_data/Kenya_counties","County")
# lapply(1:nrow(countyList), function(i)
# {
#   library(rgdal)
#   library(PBSmapping)
#   
#   subset <- df[df$COUNTY==countyList$County[[i]],]
#   writeOGR(subset, "D:/ToBackup/Modelling/_data/Kenya_counties", countyList$County[[i]], driver="ESRI Shapefile")
#   return('Done!\n')
# }); rm(df)

### Clipping rasters using python ***
# Process for one shapefile
# command <- 'python'
# output <- system2(command, args=c('D:/ToBackup/Modelling/_scritps/ExtractByMask_modified.py',  # Python script
#                                   '//dapadfs/data_cluster_4/observed/gridded_products/chirps', # Input folder
#                                   'D:/ToBackup/Modelling/_data/CHIRPS',                        # Output folder
#                                   'D:/ToBackup/Modelling/_data/Kenya_counties/County.shp',     # Shapefile
#                                   'ALL'), stdout=TRUE)
# 
# # Recent version of R
# lapply(1:nrow(countyList), function(i)
# {
#   cat('Processing:', countyList$County[[i]], 'county\n')
#   outFolder <- paste('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/CHIRPS/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
#   ifelse(test=dir.exists(outFolder), yes=cat('Output folder exists\n'), no=dir.create(outFolder))
#   return(cat(countyList$County[[i]], 'done\n'))
# })
# 
# # Process for all shapefiles
# library(parallel)
# command <- 'python'
# mclapply2(1:nrow(countyList), function(i)
# {
#   cat('Processing:', countyList$County[[i]], 'county\n')
#   outFolder <- paste('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/CHIRPS/', gsub(pattern=' ', replacement='_', countyList$County[[i]]), sep='')
#   output <- system2(command, args=c('//dapadfs/workspace_cluster_8/Kenya_KACCAL/scripts/ExtractByMask_modified.py',  # Python script
#                                     '//dapadfs/data_cluster_4/observed/gridded_products/chirps',                     # Input folder
#                                     outFolder,                                                                       # Output folder
#                                     paste('//dapadfs/workspace_cluster_8/Kenya_KACCAL/data/Kenya_counties/', countyList$County[[i]], '.shp', sep=''),     # Shapefile
#                                     'ALL'), stdout=TRUE)
#   cat(countyList$County[[i]], 'done\n')
# })

