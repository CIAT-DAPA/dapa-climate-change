#' Method after Hallegatte
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )


  
calcHal <-
function(lag, params, training, weights, base, project) {
   UseMethod("calcHal", params$to)
}

#' Method after Hallegatte
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @method calcHal logic
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

calcHal.logical <- function (lag, params, training, weights, base, project)
{

 # progress
 cat(str_c("calculating dissimilarity for (hal): starting with ",lag[1], ". \n"))
 
 
 # to be as generic as possible we need for each variables
 
 # 1. relative annual difference (rad)
 # sum refernce values
 ref_sum <- lapply(params$ref_train,sum)   
 
 # sum projecting values and substract reference values
 rad <- lapply(1:length(params$vars),
  function(x) abs(sum(training[[which(params$idx_gcms==base & params$idx_vars == x)]]) - ref_sum[[x]]))
 
 # devide by summed reference values
 ref_sum_corrected <- lapply(ref_sum, function(x) ifelse(x==0,1,x))
 rad <- lapply(1:length(params$vars), function(x) rad[[x]] / ref_sum[[x]]) 
 
 # 2. Mean absolute difference between months
 # project - ref
 mrd <- lapply(1:length(params$vars), 
     function(x) training[[which(params$idx_gcms==base & params$idx_vars == x)]][[lag]] - params$ref_train[[which(params$idx_gcms==base & params$idx_vars == x)]][lag])
 
 # abs
 mrd <- lapply(mrd, abs)
 
 # devide by ref
 # account for zeros with devision
 ref_training_corrected <- lapply(params$ref_trai, function(x) abs(ifelse(x==0,1,x)))
 
 mrd <- lapply(1:length(params$vars), 
     function(x) training[[which(params$idx_gcms==base & params$idx_vars == x)]][[lag]] / ref_training_corrected[[which(params$idx_gcms==base & params$idx_vars == x)]][lag])
 
 # sum over a time period
 mrd <- lapply(mrd, function(x) sum(x))
 
 # devide by number of time periods
 mrd <- lapply(mrd, function(x) x/params$ndivisions)
 
 # 3. Sum over all ndivisons, but for each raster rist du project - ref/ref
 # substract base from project
 mad <- lapply(1:length(params$vars), 
     function(x) training[[which(params$idx_gcms==base & params$idx_vars == x)]][[lag]] - params$ref_train[[which(params$idx_gcms==base & params$idx_vars == x)]][lag])
 
 # take abs
 mad <- lapply(mad,abs)
 
 # sum up periods
 mad <- lapply(mad,function(x) sum(x))
 
 # devide by n divisions
 mad <- lapply(mad, function(x) x/params$ndivisions)
 
 
 # apply thresholds [redundant code, optimise)
 rad <- lapply(1:length(params$vars), 
  function(x) {
    if (!is.na(params$hal_rad[[x]]))
      return(rad[[x]] <= params$hal_rad[[x]])
  })
  
 mrd <- lapply(1:length(params$vars), 
  function(x) {
    if (!is.na(params$hal_mrd[[x]]))
      return(mrd[[x]] <= params$hal_mrd[[x]])
  })
  
 mad <- lapply(1:length(params$vars), 
  function(x) {
    if (!is.na(params$hal_mad[[x]]))
      return(mad[[x]] <= params$hal_mad[[x]])
  })
 
 # find site that match all three requirements, ugly recursive loop, replace with rapply
 hal <- list(rad,mad,mrd)
 hal <- unlist(hal)
 hal <- sum(stack(hal))
 
 hal <- hal >= params$hal_ncond
 
 return(hal)
}

#### When provided a matrix

#' Method after Hallegatte
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @method calcHal matrix
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

calcHal.matrix <- function (lag, params, training, weights, base, project)
{

 # progress
 cat(str_c("calculating dissimilarity for (hal): starting with ",lag[1], ". \n"))
 
 # extract values at points
 if (class(training[[1]]) == "RasterStack" | class(training[[1]]) == "RasterLayer") {
   if (exists(str_c(".ccafshal_ext",paste(as.vector(params$to[1:3,]), collapse="")), env=globalenv())) {
      ll <- get(str_c(".ccafshal_ext",paste(as.vector(params$to[1:3,]), collapse="")), env=globalenv())
    } else {
        ll <- lapply(training, 
          function(x) extract(x,params$to))
      assign(str_c(".ccafshal_ext",paste(as.vector(params$to[1:3,]), collapse="")), ll, env=globalenv())
    }
 }
 
 # to be as generic as possible we need for each variables
 
 # 1. relative annual difference (rad)
 # sum refernce values
 ref_sum <- lapply(params$ref_train,sum)   

 # sum projecting values and substract reference values
 rad <- lapply(1:length(params$vars),
  function(x) abs(rowSums(ll[[which(params$idx_gcms==base & params$idx_vars == x)]]) - ref_sum[[x]]))

 # devide by summed reference values
 ref_sum_corrected <- lapply(ref_sum, function(x) ifelse(x==0,1,x))
 rad <- lapply(1:length(params$vars), function(x) rad[[x]] / ref_sum[[x]]) 
 
 
 # 2. Mean absolute difference between months
 # project - ref
 mrd <- lapply(1:length(params$vars), 
     function(x) { 
      apply(ll[[which(params$idx_gcms==base & params$idx_vars == x)]][,params$growing_season],2, '-', 
        params$ref_train[[which(params$idx_gcms==base & params$idx_vars == x)]][lag])
     })
  
 # abs
 mrd <- lapply(mrd, abs)
 
 # devide by ref
 # account for zeros with devision
 ref_training_corrected <- lapply(params$ref_trai, function(x) abs(ifelse(x==0,1,x)))
 
 mrd <- lapply(1:length(params$vars), 
     function(x) 
      apply(mrd[[which(params$idx_gcms==base & params$idx_vars == x)]][,params$growing_season], 2, '/',
        ref_training_corrected[[which(params$idx_gcms==base & params$idx_vars == x)]][lag]))

 # sum over a time period
 mrd <- lapply(mrd, rowSums)

 # devide by number of time periods
 mrd <- lapply(mrd, function(x) x/params$ndivisions)
 
 # 3. Sum over all ndivisons, but for each raster rist du project - ref/ref
 # substract base from project
 mad <- lapply(1:length(params$vars), 
     function(x) 
      apply(ll[[which(params$idx_gcms==base & params$idx_vars == x)]][,params$growing_season], 2, '-', 
        params$ref_train[[which(params$idx_gcms==base & params$idx_vars == x)]][lag]))

 # take abs
 mad <- lapply(mad,abs)

 # sum up periods
 mad <- lapply(mad,rowSums)

 # devide by n divisions
 mad <- lapply(mad, function(x) x/params$ndivisions)
 
 # apply thresholds [redundant code, optimise)
 rad <- lapply(1:length(params$vars), 
  function(x) {
    if (!is.na(params$hal_rad[[x]]))
      return(rad[[x]] <= params$hal_rad[[x]])
  })

 mrd <- lapply(1:length(params$vars), 
  function(x) {
    if (!is.na(params$hal_mrd[[x]]))
      return(mrd[[x]] <= params$hal_mrd[[x]])
  })
 
 mad <- lapply(1:length(params$vars), 
  function(x) {
    if (!is.na(params$hal_mad[[x]]))
      return(mad[[x]] <= params$hal_mad[[x]])
  })
 
 # find site that match all three requirements, ugly recursive loop, replace with rapply
 hal <- unlist(mad) + unlist(rad) + unlist(mrd)
 

 hal <- hal >= params$hal_ncond

 return(hal)
}

