ccafs.make.ref <- function(type,cdata=cdata, params=params, new.x=NA, new.y=NA, new.growing.season=NA) {
  
  # overwrite default x and y
  if (!is.na(new.x) & !is.na(new.y)) {
   params$x <- new.x
   params$y <- new.y
  }
 
  if (!is.na(new.growing.season)) {
   params$growing.season <- new.growing.season
  }
  
  
#----------------------------------------------------------------------------------------------#
# extract reference value for current
#----------------------------------------------------------------------------------------------#

# dont extract all if method is forward (todo)
# dont always extract it for layers, only the needed one, depending on the direction (and fill the others with NA)

if(type=="training")
  f_ref <- lapply(cdata$training,function(x){ 
      cat("extracting ref values ... \n")
      as.vector(extract(x,cbind(params$x,params$y)))})
  
if(type=="weights")
 f_ref <- lapply(cdata$weights,function(x) {
  cat("extracting ref values ... \n")
  if (class(x) == "RasterStack" | class(x) == "RasterLayer"){
    return(as.vector(extract(x,cbind(params$x,params$y))))
  } else {
    return(rep(1,params$ndivisions))
  }
  
 })
  
return(f_ref)
}
