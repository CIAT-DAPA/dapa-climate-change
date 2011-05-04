#----------------------------------------------------------------------------------------------#
# Function to initiate ccafs
#----------------------------------------------------------------------------------------------#

init.analogue <- function(x=10,   # x location of point for which dissimilarity is calculated
  y=48,                           # y location
  method="ccafs",                 # method to calculate dissimilarity, currently ccafs, ccafs.generic (i.e. bio variables and ele), hal
  hal.tmp=1,                      # If hallagate method is chosen, what is the max tolerable difference in temp between sites [°C]
  hal.prec.annual=0.3,            # If hallagate method is chosen, what is the max tolerable difference in annual precipitation [%]
  hal.prec.month=0.15,            # If hallagate method is chosen, what is the max tolerable difference in monthly precipitation [%]
  hal.ncond=3,                     # How many of the above specified conditions need to be fulfilled in order for a site to be suitable
  scenario="a1b",                 # emission scenario(s)
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
  vars=c("tmean", "prec", "dtr"), # Variables to calculate dissimilarity. For hal tmean and prec are needed for ccafs any can be chosen.
  ndivisions=12,                  # how are the measurements devided, this should be n equidistant time intervalls over a year. E.g. if there are monthly values the number would be 12. 
  climate.data=".",               # directory where climate data is located. Labels in lower case should be as follows:
                                  # [sres]_[year]_[gcm]_[dtr|prec|tmean]_[1..12].asc
  direction="backwd",             # 2 possibilites: backwd -> take tmean,prec and dtr at location x,y in the futur for n gcms and look for similiarity in current conditions (i.e. project futur to current, projecting n possible futures to the present).
                                  #                 forwd -> take mean,prec and dtr at location x,y under current condition and look for similiarity in n gcms in future conditions (i.e. project current to futur, projecting 1 knwon truth into n possible futurs).
  growing.season=1:12,            # specifiy a growing season for a crop (from, to) in months, currently it is not possible to specify growing seaon over e.g. nov - feb, must be within one year
  across.year=T,
  normalise=F,                    # should varaibles be normalised (mean=0,sd=1), takes some time
  keep.lag=F,                     # wether or not laged calculation for each month should be kept or not
  sumarize.lag=T,                 # wether or not lags should be sumarized, i.e. min searched
  pdf="test.pdf",                 # name of a pdf continaing main results, if empty no pdf will be produced
  paper="a4",                     # paper format: a4 or letter
  cores=1,                        # number of cores to be used on a multiple core computer
  sf.type="SOCK",                 # Type for parallelization
  grass.params=list(
    gisBase="/usr/local/grass-6.4.0",         # location of grass installation (needs a writeable .grassrc file)
    gisDbase="/home/johannes/GRASS/ciat_gdb", # location of the grass gisDbase
    location="ccafs",             # location containing the data
    mapset="PERMANENT",           # mapset, in which rasters are located
    override=T,
    res="1",                      # at which resolution should the model be calculated. in degree:min:seconds, e.g. 10 mins =00:10, 30 seconds=00:00:30
    region="world")){             # specify the region, continent etc.. for which the analysis is performed

  # required packages
  require(raster)
  require(stringr)
  require(maptools)
  require(maps)
  require(spgrass6)
  require(akima)
  require(grid)
  require(plotrix)
  
  options(warn=-1)
  
  # check wether point is terrestrial or not
  
  # either lags must be kept or sumarised
  
  if (!(keep.lag | sumarize.lag)) stop("no output options chosen")
  
  # Make a list with all parameters
  params <- list(x=x,
                  y=y,
                  scenario=tolower(scenario), 
                  method=method,
                  hal.tmp=hal.tmp,
                  hal.prec.annual=hal.prec.annual,
                  hal.prec.month=hal.prec.month,
                  gcms=tolower(gcms), 
                  year=year,
                  direction=tolower(direction),
                  across.year=across.year,
                  growing.season=growing.season,
                  keep.lag=keep.lag,
                  pdf=pdf,
                  sumarize.lag=sumarize.lag,
                  use.grass=use.grass,
                  climate.data=climate.data,
                  grass.params=grass.params,
                  vars=vars,
                  normalise=normalise,
                  ndivisions=ndivisions,
                  paper=paper,
                  hal.tmp=hal.tmp,
                  hal.prec.annual=hal.prec.annual,
                  hal.prec.month=hal.prec.month,
                  hal.ncond=hal.ncond)
                  
                  
  # source other functions
  source("src/ccafs.load.data.R")
  source("src/ccafs.make.ref.R")
  source("src/ccafs.dissimilarity.R")
  source("src/ccafs.function.R")
  source("src/ccafs.summary.R")
  source("src/ccafs.plot.R")
  source("src/ccafs.plot.grid.R")
   
  if(cores > 1) {
    require(snowfall)
    sfInit(parallel=TRUE, cpus=cores, type=sf.type)
    sfLibrary(raster)
    sfLibrary(spgrass6)
    sfLibrary(stringr)
  }
  
  # Additional Functions
  raster.n <- function(x) {
    r <- raster(x)
    r <- (r - cellStats(r,mean))/cellStats(r,sd)
    r <- r+10
    return(r)
  }
  
  return(params)
  

}
