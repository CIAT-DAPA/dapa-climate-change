ccafs.load.data <- function(params) {
    
  # load data
  cdata <- list()             # list that holds all data, as raster stacks
  
  if (params$use.grass==TRUE) {
    # get grass params
    grass.params <- params$grass.params
    
    # connect to grass
    with(grass.params,initGRASS(gisBase=gisBase,gisDbase=gisDbase, location=location, mapset=mapset,override=override)) # tryCatch missing
    
    # set region and mask in GRASS
    
    # some more ifs, on which region, if (char) -> set to predefined continent or country
    #                                 if (vector) -> def region by nswe
    #                                 if (Spatialpolygon) -> get bbx and then define region by nswe, later mask
    execGRASS("g.region", parameters=list(res=as.character(as.character(params$res)), region=params$region))
    
    # load current
    for (var in params$vars){
      cat(str_c("loading ",var," for current\n"))
      cdata[[str_c(var,"_b")]] <- do.call(stack,list(readRAST6(str_c("current_",var,"_",params$growing.season,"_10min"),ignore.stderr=T)))
    }
    
    # load futur data for all gcms, if current conditions need to be projected into the futur
    if (params$direction=="forwd"){
      for (gcm in params$gcms) {
        for (var in params$vars)
          cat(str_c("loading ",var," for ",gcm,"\n"))
          with(params,cdata[[str_c(var,"_",scenario)]] <- do.call(stack,list(readRAST6(str_c(scenario,"_",year,"s_",gcm,"_",var,"_",1:12,"_10min")))))
      }
    }
  }else {
    # if files need to be loaded from hd, significant improvment needed
    if (file.exists(params$climate.data)){
      # load current climate
      for (var in params$vars) {
        cat(str_c("loading ",var," for current\n"))
        cdata[[str_c(var,"_b")]] <- do.call(stack,lapply(with(params,str_c(climate.data,"/current_",var,"_", 1:12, ".asc")), raster))
      }
      
      # load future climate
      for (gcm in params$gcms) {
        for (var in params$vars) {
          cat(str_c("loading ",var," for ",gcm,"\n"))
          cdata[[str_c(var,"_",gcm)]] <- do.call(stack,lapply(with(params,str_c(climate.data,"/",scenario,"_",year,"_",gcm,"_",var,"_", 1:12, ".asc")), raster))
        }
      }
    } else {
      stop("Climate data not found")
    }
  }
  return(cdata)
}