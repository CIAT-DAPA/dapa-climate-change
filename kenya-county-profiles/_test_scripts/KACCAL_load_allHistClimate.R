# Load all historical climate to calculate water balance
# H. Achicanoy
# CIAT, 2016

inputDir <- '/mnt/workspace_cluster_8/Kenya_KACCAL/data/input_tables'
countyDir <- paste(inputDir, '/', gsub(pattern=' ', replacement='_', county, fixed=TRUE), sep='')

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
cat('Loading: Solar radiation for first wet season\n')
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

load(paste(countyDir, '/dswrf/dswrf.RData', sep=''))
dswrfAll <- ch2014_year; rm(ch2014_year)
dswrfAll <- dswrfAll[years_analysis]
dswrfAll <- lapply(1:length(dswrfAll), function(i){z <- as.data.frame(dswrfAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(dswrfAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
names(dswrfAll) <- years_analysis
dswrfAll <- reshape::merge_recurse(dswrfAll)

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
cat('Loading: Daily precipitation for first wet season\n')
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

load(paste(countyDir, '/prec/prec.RData', sep=''))
precAll <- chirps_year; rm(chirps_year)
precAll <- precAll[years_analysis]
precAll <- lapply(1:length(precAll), function(i){z <- as.data.frame(precAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(precAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
names(precAll) <- years_analysis
precAll <- reshape::merge_recurse(precAll)

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
cat('Loading: Maximum temperature for first wet season\n')
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

load(paste(countyDir, '/tmax/tmax.RData', sep=''))
tmaxAll <- ch2014_year; rm(ch2014_year)
tmaxAll <- tmaxAll[years_analysis]
tmaxAll <- lapply(1:length(tmaxAll), function(i){z <- as.data.frame(tmaxAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tmaxAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
names(tmaxAll) <- years_analysis
tmaxAll <- reshape::merge_recurse(tmaxAll)

### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###
cat('Loading: Minimum temperature for first wet season\n')
### =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= ###

load(paste(countyDir, '/tmin/tmin.RData', sep=''))
tminAll <- ch2014_year; rm(ch2014_year)
tminAll <- tminAll[years_analysis]
tminAll <- lapply(1:length(tminAll), function(i){z <- as.data.frame(tminAll[[i]]); yr <- as.numeric(gsub(pattern='y', replacement='', x=names(tminAll)[i])); names(z)[4:length(names(z))] <- as.character(seq(as.Date(paste(yr, '-01-01', sep='')), as.Date(paste(yr, '-12-31', sep='')), by=1)); return(z)})
names(tminAll) <- years_analysis
tminAll <- reshape::merge_recurse(tminAll)
