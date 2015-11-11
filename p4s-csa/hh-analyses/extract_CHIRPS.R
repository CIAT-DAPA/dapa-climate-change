#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#function to extract daily CHIRPS data for a single location
extract_CHIRPS_single <- function(in_dir,year,dates,lon,lat) {
  cat("...extracting daily CHIRPS data for lat=",lon,"lat=",lat,"\n")
  dates <- cbind.data.frame("date"=dates, "prec"=NA)
  for (jday in 1:nrow(dates)) {
    #jday <- 1
    tdate <- paste(dates$date[jday])
    tday <- unlist(strsplit(tdate,"-",fixed=T))[3]
    tmth <- unlist(strsplit(tdate,"-",fixed=T))[2]
    tyear <- unlist(strsplit(tdate,"-",fixed=T))[1]
    
    #chirps filename
    ch_fname <- paste("chirps-v2.0.",tyear,".",tmth,".",tday,".tif",sep="")
    ch_fname_gz <- paste("chirps-v2.0.",tyear,".",tmth,".",tday,".tif.gz",sep="")
    
    #decompress and load data
    twd <- getwd()
    setwd(paste(in_dir,"/",year,sep=""))
    system(paste("gunzip ",ch_fname_gz,sep=""))
    trs <- raster(ch_fname)
    dates$prec[jday] <- extract(trs, data.frame(x=lon,y=lat))
    rm(trs); x <- gc(); rm(x)
    system(paste("gzip ",ch_fname,sep=""))
    setwd(twd)
  }
  return(dates)
}


#function to extract daily CHIRPS data for multiple locations
extract_CHIRPS_mult <- function(in_dir,year,dates,xy) {
  cat("...extracting daily CHIRPS data for n=",nrow(xy),"sites\n")
  
  #create lists
  dates <- lapply(dates, FUN=function(x) {cbind.data.frame("date"=x, "prec"=NA)})
  dates_gen <- dates[[1]]
  
  for (jday in 1:nrow(dates_gen)) {
    #jday <- 1
    tdate <- paste(dates_gen$date[jday])
    tday <- unlist(strsplit(tdate,"-",fixed=T))[3]
    tmth <- unlist(strsplit(tdate,"-",fixed=T))[2]
    tyear <- unlist(strsplit(tdate,"-",fixed=T))[1]
    
    #chirps filename
    ch_fname <- paste("chirps-v2.0.",tyear,".",tmth,".",tday,".tif",sep="")
    ch_fname_gz <- paste("chirps-v2.0.",tyear,".",tmth,".",tday,".tif.gz",sep="")
    
    #decompress and load data
    twd <- getwd()
    setwd(paste(in_dir,"/",year,sep=""))
    system(paste("gunzip ",ch_fname_gz,sep=""))
    trs <- raster(ch_fname)
    extr_vals <- extract(trs, data.frame(x=xy$lon,y=xy$lat))
    rm(trs); x <- gc(); rm(x)
    system(paste("gzip ",ch_fname,sep=""))
    setwd(twd)
    
    #assign the values to the data.frames in dates list
    for (i_k in 1:nrow(xy)) {
      dates[[i_k]]$prec[jday] <- extr_vals[i_k]
    }
  }
  return(dates)
}


