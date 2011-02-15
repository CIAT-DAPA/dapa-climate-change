get.sp.swd <- function(path,all.pts=swd, log)
{
  tryCatch({  
  # read the file with the x,y coordinates
  points <- read.csv(paste(path,"/training/species.csv",sep=""))

  # extract the values of the environmental data, by using the uniqe location key x:y
  data <- all.pts[all.pts$xy %in% paste(points[,2],points[,3],sep=""),1:(ncol(all.pts)-1)]

  data <- cbind(points[1,1],data)
  
  names(data) <- c("sp","lon","lat","bio1","bio2","bio3","bio4","bio5","bio6","bio8","bio9","bio12","bio13","bio14","bio15","bio18","bio19")


  # remove points with NA's
  data <- data[!is.na(data[,4]),]

  # write the file again
  write.table(data,paste(path,"/training/training_swd.csv", sep=""), row.names=F, quote=F, sep=",")
  write(paste(date(), path, "successfull"), log, append=T)
  }, error=function(x) {
  write(paste(date(), path, "error:", x), log, append=T)
  })
}
