get.sp.swd <- function(path,all.pts=swd, log)
{
  tryCatch({  
  # read the file with the x,y coordinates
  points <- read.csv(paste(path,"/training/species.csv",sep=""))

  # extract the values of the environmental data, by using the uniqe location key x:y
  data <- cbind(points[,1:3],all.pts[match(paste(points[,2],":",points[,3],sep=""), paste(all.pts[,1], all.pts[,2], sep=":")),4:until])
  
  # remove points with NA's
  data <- data[!is.na(data[,4]),]

  # write the file again
  write.table(data,paste(path,"/training/training_swd.csv", sep=""), row.names=F, quote=F, sep=",")
  write(paste(date(), path, "successfull"), log, append=T)
  }, error=function(x) {
  write(paste(date(), path, "error:", x), log, append=T)
  })
}
