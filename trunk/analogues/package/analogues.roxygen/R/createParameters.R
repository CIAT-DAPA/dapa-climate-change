#' Initiate parameters for analogue dissimilarity analysis.
#'
#' This function is the first step too find analogue climate sites.
#' Here all parameters for the later analysis are set. The initial 
#' parameters can be changed later. Make sure clmate data are name properly. 
#'
#' @param x coordinate of site under analysis
#' @param y coodrinate of site under analysis
#' @param to coordinates for which dissimilarity should be caluclated as matrix with. A \code{matrix} 
#' with two columns: x and y. If is set to \code{NA} dissimilarities are calculated for the whole grid. 
#' @param method select the method that is going to be use, either ccafs or hallegate
#' @param hal_rad maximum tolerable relative annual difference for hellegate method. A vector of \code{length(vars)}, 
#' if thre is a var that should not be considered, substitute the number with \code{NA}. 
#' @param hal_mad maximum tollerable difference in annual precipitation. A vector of \code{length(vars)}, 
#' if thre is a var that should not be considered, substitute the number with \code{NA}.
#' @param hal_mrd mean absolute difference between months difference. A vector of \code{length(vars)}, 
#' if thre is a var that should not be considered, substitute the number with \code{NA}.
#' @param hal_ncond specify how many of the above condition need to be true
#' @param screnario specify the SRES (e.g. A1B, A2..)
#' @param gcms which gcm model should be used (e.g. bccr_bcm2_0,cccma_cgcm3_1_t47)
#' @param year specify the year for which the dissimilarity should be calculated
#' @param vars specify which variables are being used in the calculation (e.g. tmean, precipitation)
#' @param weights specify how the variables are weighted. This can either be a \code{rasterLayer}
#' or a single number. Provide a list with the name of the raster or a value for weighting.
#' @param ndivision define how many devision the variables have per year (e.g. for monthly data use 12).
#' @param climate_data define directory where the climate data is located
#' @param ext Extension of raster format (must be supported by gdal)
#' @param direction which direction should the dissimilarity be calculated. Available 
#' options are forward, backward and current.
#' @param growing_season define the growing season of the crop that is to be modelled.
#' @param accross_year should the analogue method be looked accross years (i.e. should time lag be 
#' used to account for differences in seasons
#' @param normalise should the rasater be normalised to a mean of 0 and sd of 1 when being loaded.
#' @param keep_lag specify whether or not grids for each lag should be kept
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @return an object of AnalogueParameters
#' @export
#' @examples
#' ccafs_params <- createParams(x, z, )

createParameters <- function(x=10,   
  y=48, 
  to=NA,                       
  method="ccafs",                
  hal_rad=c(NA,0.15),                    
  hal_mad=c(NA,0.3),            
  hal_mrd=c(1,NA),           
  hal_ncond=2,                  
  scenario="a1b",               
  gcms=c("current"),
  year=2030,                      
  vars=c("tmean", "prec"),        
  weights=c("dtr",1),
  ndivisions=12,                  
  climate_data=".",              
  ext="asc",
  direction="backwd",             
  growing_season=1:12,           
  across_year=T,
  keep_lag=F,
  normalise=F){

  # required packages
  require(raster)
  require(stringr)
  require(maptools)
  require(maps)
  require(spgrass6)
  require(akima)
  require(grid)
  require(plotrix)
  require(rimage)

  
  # check wether point is terrestrial or not (waiting)
  
  # Make a list with all parameters
  params <- list(x=x,
                  y=y,
                  to=to,
                  scenario=tolower(scenario), 
                  method=method,
                  hal_rad=hal_rad,
                  hal_mad=hal_mad,
                  hal_mrd=hal_mrd,
                  hal_ncond=hal_ncond,
                  gcms=tolower(gcms), 
                  year=year,
                  direction=tolower(direction),
                  across_year=across_year,
                  growing_season=growing_season,
                  keep_lag=keep_lag,
                  climate_data=climate_data,
                  vars=vars,
                  weights=weights,
                  normalise=normalise,
                  ext=ext,
                  ndivisions=ndivisions)
                  
  # add idxs
  # indexing vars, ie saying to which gcm each variable belongs
  params$idx_vars <- rep(1:length(params$vars),length(params$gcms))      
  
  params$idx_gcms <- rep(1:(length(params$gcms)),each=length(params$vars)) 

  class(params) <- "AnalogueParameters"
  
  # load logos for plots
  data(logos)
    
  return(params)
}

