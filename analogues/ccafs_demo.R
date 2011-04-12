# --------------------------------------------------------------------------------------------- #
# ccafs analogues tool
# developed by josh hooker with some modifications and additions by johannes signer (jmsigner@gmail.com)
# cali 6.4.2011

# make sure the following libraries are installed: raster, maptools, stringr, maps, (spgrass)
# unzip data.zip 

# --------------------------------------------------------------------------------------------- #
# This is still a very early version and it is very likely there a plenty of bugs
# 
# To do:
# ------
# * more rigerouse chekcing of user input
# * removing some inconsistency with nameing funcitons, variables
# * porting the whole project to S4 classes
# * parellelazing the calculation of dissimilarities
#
# Note:
# -----
# * GRASS support is not fully implemented and requires a GRASS database with adjusted naming
# --------------------------------------------------------------------------------------------- #

# quick demo

# source first function
source("src/init.analogues.R")

ccafs.params <- init.analogue(
x=-76.4,                           # x location of point for which dissimilarity is calculated
y=3.8,                           # y location
method="ccafs",                 # method to calculate dissimilarity, currently only ccafs
scenario="a1b",                 # emission scenario
gcms=c("bccr_bcm2_0",           # gcms to be used
  "cccma_cgcm3_1_t47",
  "cccma_cgcm3_1_t63",
  "cnrm_cm3",
  "csiro_mk3_0",
  "csiro_mk3_5",
  "gfdl_cm2_0",
  "gfdl_cm2_1",
  "giss_aom",
  "giss_model_eh",
  "giss_model_er",
  "iap_fgoals1_0_g",
  "ingv_echam4",
  "inm_cm3_0",
  "ipsl_cm4",
  "miroc3_2_hires",
  "miroc3_2_medres",
  "miub_echo_g",
  "mpi_echam5",
  "mri_cgcm2_3_2a",
  "ncar_ccsm3_0",
  "ncar_pcm1",
  "ukmo_hadcm3",
  "ukmo_hadgem1"),
year=2030,                      # year for the climate model
use.grass=F,                    # should a grass database with the climate data be used, grass.params need to be set
climate.data="data",        # if no grass db is used, specify directory where climate data is located. Labels in lower case should be as follows:
                                # [sres]_[year]_[gcm]_[dtr|prec|tmean]_[1..12].asc
direction="backwd",             # 3 possibilites: backwd -> take tmean,prec and dtr at location x,y in the futur for n gcms and look for similiarity in current conditions (i.e. project futur to current, projecting n possible futures to the present).
                                #                 forwd -> take mean,prec and dtr at location x,y under current condition and look for similiarity in n gcms in future conditions (i.e. project current to futur, projecting 1 knwon truth into n possible futurs).
                                #                 current -> to do present dissimilarity
growing.season=1:12,            # specifiy a growing season for a crop (from, to) in months, currently it is not possible to specify growing seaon over e.g. nov - feb, must be within one year
grass.params=list(
  gisBase="/usr/local/grass-6.4.0",         # location of grass installation (needs a writeable .grassrc file)
  gisDbase="/home/johannes/GRASS/ciat_gdb", # location of the grass gisDbase
  location="ccafs",             # location containing the data
  mapset="PERMANENT",           # mapset, in which rasters are located
  override=T,
  res="1",                      # at which resolution should the model be calculated. in degree:min:seconds, e.g. 10 mins =00:10, 30 seconds=00:00:30
  region="world"))
  
  
# In the next step, the data is loaded
ccafs.data <- ccafs.load.data(ccafs.params)
  
# Once the data is loaded, reference values need to be extracted at location x,y. By default reference values are extracted at the location
# specified within the init function. These values can optionally be overwritten.
# By default values are extracted for the specified growing period in the init function, these can be overwritten too

# to save the reference values also into the parameters
ccafs.params$ref_values <- ccafs.make.ref(cdata=ccafs.data, params=ccafs.params, new.x=NA,new.y=NA, new.growing.season=NA)

# the direction of the model can be overwritten
ccafs.data$results <- ccafs.dissimilarity(ccafs.data, ccafs.params, new.direction=NA)

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


