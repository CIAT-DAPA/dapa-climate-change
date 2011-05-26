#' Extract reference values from rasters
#'
#' @param params an object of the class AnalogueParameters
#' @param ... an object of the class AnalogueTraining, AnalogueTraining
#' @return an object of AnalogueResults
#' @export
#' @examples
#' load <- xy


loadDataGRASS <- function(params) {
  grass_params <- params$grass_params

  sites <- rbind(c(params$x,params$y),params$to)

  # connect to grass
  with(grass.params,initGRASS(gisBase=gisBase,gisDbase=gisDbase, location=location, mapset=mapset,override=override)) # tryCatch missing

  execGRASS("g.region", parameters=list(res=as.character(as.character(params$res))))

  # load current
  # mapset with the future data

  training <- list()
  count <- 1
  for (gcm in params$gcms) {
    for (var in params$vars) {
      tmp <- execGRASS("r.what", parameters=list(input=str_c(var,1:params$ndivisions), east_north=sites, fs=","), intern=TRUE)
      tmp <- ldply(str_c(tmp, ","))[,-(1:3)] # skip first three columns
      training[[count]] <- apply(tmp,2,as.numeric)

      # add count
      count <- count + 1
    }
  }

  return(training)
}

#' Load weights from GRASS
#'
#' @param params an object of the class AnalogueParameters
#' @param ... an object of the class AnalogueTraining, AnalogueTraining
#' @return an object of AnalogueResults
#' @export
#' @examples
#' load <- xy


loadWeightsGRASS <- function(params) {
  grass_params <- params$grass_params
  
  sites <- rbind(c(params$x,params$y),params$to)

  # connect to grass
  with(grass.params,initGRASS(gisBase=gisBase,gisDbase=gisDbase, location=location, mapset=mapset,override=override)) # tryCatch missing

  execGRASS("g.region", parameters=list(res=as.character(as.character(params$res))))

  # load current
  # mapset with the future data

  weigths <- list()
  count <- 1
  for (gcm in params$gcms) {
    for (weight in params$weights) {
      if (!is.na(as.numeric(weight))) {
        tmp <- execGRASS("r.what", parameters=list(input=str_c(weight,1:params$ndivisions), east_north=sites, fs=","), intern=TRUE)
        tmp <- ldply(str_c(tmp, ","))[,-(1:3)] # skip first three columns
        weights[[count]] <- apply(tmp,2,as.numeric)
      } else {
        weights[[count]] <- weight
        
      }
      # add count
      count <- count + 1
    }
  }

  return(training)
}
