# --------------------------------------------------------------------------------------------- #
# study on pines

source("src/init.analogues.R")

ccafs.params <- init.analogue(
x=-39.75,                           # x location of point for which dissimilarity is calculated
y=-18.66667,                           # y location
method="ccafs.generic",                 # method to calculate dissimilarity, currently only ccafs
scenario="",                 # emission scenario
gcms=c(NA),           # gcms to be used
year=2030,                      # year for the climate model
use.grass=F,                    # should a grass database with the climate data be used, grass.params need to be set
vars=c('tmean','prec'),
weights=c('dtr',1),
climate.data="pine_study/data",        # if no grass db is used, specify directory where climate data is located. Labels in lower case should be as follows:
direction="current",             # 3 possibilites: backwd -> take tmean,prec and dtr at location x,y in the futur for n gcms and look for similiarity in current conditions (i.e. project futur to current, projecting n possible futures to the present).
growing.season=1:12,
across.year=T,
keep.lag=F)
  
ccafs.data <- list()
  
# In the next step, the data is loaded
ccafs.data$training <- ccafs.load.data(ccafs.params)
ccafs.data$weights <- ccafs.load.weights(ccafs.params)
  
# top sites
ts <- read.csv('pine_study/sites/toptrials.csv')

system.time(first <- ccafs.dissimilarity(ccafs.data, ccafs.params))

# the direction of the model can be overwritten
results <- list()
for (i in 1:nrow(ts))
{
  x.new <- ts[i,5]
  y.new <- ts[i,4]
  ccafs.params$x <- x.new
  ccafs.params$y <- y.new
   results[[i]] <- ccafs.dissimilarity(ccafs.data, ccafs.params)

  print(c(x.new,y.new))
}

# apply a threshold
ccafs.data$results_with_threshold <- ccafs.apply.threshold(ccafs.data$results,from=0,to=100)

# now the results can be summarised
ccafs.data$results.summary <- ccafs.summary(ccafs.data$results, do.mean=T,do.std=T, do.cv=T)

# plot results - mean
plot(ccafs.data$results.summary$mean)
map("world", add=T)

# plot results - std
plot(ccafs.data$results.summary$std)
map("world", add=T)

# plot results - cv
plot(ccafs.data$results.summary$cv)
map("world", add=T)

# extract mean dissimilarity values at x,y pionts
my.points <- data.frame(x=c(-50,-55,60), y=c(0,4,50))
my.points$ mean <- extract(ccafs.data$results.summary$mean,cbind(my.points$x, my.points$y))

# crop to a specific region
my.region.extent <- extent(c(-80,-70,0,20)) # xmax,xmin,ymax,ymin
my.region <- crop(ccafs.data$results.summary$mean,my.region.extent)
plot(my.region)
map("world", add=T)

# Export results as ascii grids
# export mean
writeRaster(ccafs.data$results.summary$mean,filename="ccafs_mean.tif")

# export sd
writeRaster(ccafs.data$results.summary$std,filename="ccafs_std.tif")

# export cv
writeRaster(ccafs.data$results.summary$cv,filename="ccafs_cv.tif")

# export model n
n <- 1
writeRaster(ccafs.data$results[[n]],filename=str_c("ccafs_",n,".tif"))


