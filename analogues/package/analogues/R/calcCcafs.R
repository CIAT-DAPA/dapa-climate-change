#' Eucledian distance to calculate dissimilarity
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

calcCcafs <-
function(lag, params, training, weights, base, project) {
   UseMethod("calcCcafs", params$to)
}

#' Eucledian distance to calculate dissimilarity
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come 
#' @method calcCcafs logical
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

calcCcafs.logical <- function(lag, params, training, weights, base, project) {
  
  # inform user
  cat(str_c("calculating dissimilarity (ccafs.generic) projecting ",params$gcms[base]," to ",params$gcms[project]," starting with ",lag[1], ". \n"))
  
  # substract reference value
  # for each variable (idx_vars) select the training grid (with idx_gcms) and ref values
  ll <- lapply(1:length(params$vars), 
      function(x) training[[which(params$idx_gcms==base & params$idx_vars == x)]][[params$growing_season]] - params$ref_train[[which(params$idx_gcms==base & params$idx_vars == x)]][lag])
  
  # substrack reference values for weights
  ww <- lapply(1:length(params$weights), function(x) { 
    if (class(weights[[x]]) == "RasterLayer" | class(weights[[x]]) == "RasterStack") {
      
      this_w_training <- which(params$idx_gcms==base & params$idx_vars == x)
      this_w_ref <- which(params$idx_gcms==base & params$idx_vars == x)
      
      return(weights[[this_w_training]][[params$growing_season]] - params$ref_weight[[this_w_ref]][lag])
      } else {return(params$ref_weight[[x]][lag])}
    })
  
  # multiply by weights
  ll <- lapply(1:length(ll), function(x) ll[[x]] * ww[[x]])
  
  # square
  ll <- lapply(ll, function(x) x*x)
    
  # sum division ups
  ll <- lapply(ll, function(x) sum(x))
  # sum over all lists
  res <- 0
  for (i in ll) res <- res + i

  # take sqrt 
  res <- sqrt(res)

  return(res)    
}

#' Eucledian distance to calculate dissimilarity
#'
#' @param params an object of the class AnalogueParameters
#' @param ... more to come
#' @method calcCcafs matrix
#' @export
#' @examples
#' ccafs_params <- dissimilarity(x, z, )

calcCcafs.matrix <- function(lag, params, training, weights, base, project) {
  
  # inform user
  cat(str_c("calculating dissimilarity (ccafs.generic) projecting ",params$gcms[base]," to ",params$gcms[project]," starting with ",lag[1], ". \n"))

  # check if reference data has already been extracted for this coordinates

  if (exists(string_c(".ccafstraining_ext",paste(as.vector(params$to), collapse="")), env=globalenv())) {
    training_ext <- get(string_c(".ccafstraining_ext",paste(as.vector(params$to), collapse="")), env=globalenv())
  } else {
    training_ext <- lapply(training, function(x) extract(x,params$to))
    assign(string_c(".ccafstraining_ext",paste(as.vector(params$to), collapse="")), training_ext, env=globalenv())
  }

  if (exists(string_r(".ccafsweights_ext",paste(as.vector(params$to), collapse="")), env=globalenv())) {
    weights_ext <- get(string_r(".ccafsweights_ext",paste(as.vector(params$to), collapse="")), env=globalenv())
  } else {
    weights_ext <- lapply(weights, function(x) { 
      if (class(x) == "RasterLayer" | class(x) == "RasterStack") {
        return(extract(x,params$to))
        } else {return(x)}
      })
    assign(string_r(".ccafsweights_ext",paste(as.vector(params$to), collapse="")), weights_ext, env=globalenv())
  }
  
  # substract reference value
  ll <- lapply(1:length(params$vars), 
      function(x) training_ext[[which(params$idx_gcms==base & params$idx_vars == x)]][params$growing_season] - params$ref_train[[which(params$idx_gcms==base & params$idx_vars == x)]][lag])
  
  # substrack reference values for weights
  ww <- lapply(1:length(params$weights), function(x) {
    if (class(weights[[x]]) == "RasterLayer" | class(weights[[x]]) == "RasterStack") {
       weights_ext[[which(params$idx_gcms==base & params$idx_vars == x)]][params$growing_season] - params$ref_weight[[which(params$idx_gcms==base & params$idx_vars == x)]][lag]
    } else { 
      rep(weights_ext[[which(params$idx_gcms==base & params$idx_vars == x)]], params$ndivisions)[lag] 
    }
  })
  
  # multiply by weights
  ll <- lapply(1:length(ll), function(x) ll[[x]] * ww[[x]])
  
  # square all grids
  ll <- lapply(ll, function(x) x*x)
    
  # sum division ups
  ll <- lapply(ll, rowSums)

  # sum over all lists
  res <- 0
  for (i in ll) res <- res + i

  # take sqrt 
  res <- sqrt(res)

  return(res)    
}
