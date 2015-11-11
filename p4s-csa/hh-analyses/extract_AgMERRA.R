#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#function to extract daily AgMERRA data for a given variable, single location
extract_AgMERRA_single <- function(in_dir,year=NULL,varname,year_range=c(1981,2010),hhid,lon,lat) {
  #input details
  tvar <- varname
  if (is.null(year)) {
    year_i <- min(year_range); year_f <- max(year_range)
  } else {
    year_i <- year; year_f <- year
  }
  if (lon < 0) {lon <- lon+360}
  
  cat("...processing AgMERRA variable=",tvar,"\n")
  
  #prepare output object
  data_agmerra <- data.frame()
  
  #loop years to extract the daily data
  for (yr in year_i:year_f) {
    #extract and load data
    setwd(in_dir)
    ncvar <- paste("AgMERRA_",yr,"_",tvar,".nc4",sep="")
    odat <- paste("AgMERRA_",tvar,"_hh",hhid,".tab",sep="")
    system(paste0("cdo -s -outputtab,date,year,value -remapnn,lon=", lon, "_lat=", lat, " ", ncvar, " > ", odat))
    datobs <- read.table(odat,header=F,sep="")
    names(datobs) <- c("date","year",tvar)
    system(paste("rm -f ",odat,sep=""))
    setwd("~")
    
    #append data
    data_agmerra <- rbind(data_agmerra,datobs)
  }
  
  #return object
  return(data_agmerra)
}


#function to extract daily AgMERRA data for a given variable, multiple locations
extract_AgMERRA_mult <- function(in_dir,year=NULL,varname,year_range=c(1981,2010),xy) {
  #input details
  tvar <- varname
  if (is.null(year)) {
    year_i <- min(year_range); year_f <- max(year_range)
  } else {
    year_i <- year; year_f <- year
  }
  
  cat("...processing AgMERRA variable=",tvar,"for n=",nrow(xy),"sites\n")
  
  #prepare output object
  data_agmerra <- lapply(1:nrow(xy), FUN=function(x) {y <- data.frame(); return(y)})
  
  #loop years to extract the daily data
  for (yr in year_i:year_f) {
    cat("...extracting year=",yr,"\n")
    
    #extract and load data
    ncvar <- paste(in_dir,"/AgMERRA_",yr,"_",tvar,".nc4",sep="")
    
    #open netcdf4 file and read data
    nc <- nc_open(ncvar)
    ncdata <- ncvar_get(nc,nc$var[[tvar]]) #get data from nc connection
    brs <- raster(nrow=720,ncol=1440,xmn=0,xmx=360,ymn=-90,ymx=90) #base raster
    nt <- dim(ncdata)[3]
    
    #create stack
    rstk <- c()
    for (i_t in 1:nt) {
      rs <- brs
      rs[] <- t(ncdata[,,i_t]) #tdata
      rstk <- c(rstk,rotate(rs))
      rm(rs); g=gc(); rm(g)
    }
    rm(ncdata); g=gc(); rm(g)
    rstk <- stack(rstk)
    
    #close netcdf4 file
    nc <- nc_close(nc)
    
    #extract data
    datobs <- extract(rstk, data.frame(x=xy$lon,y=xy$lat))
    
    #append the values to the data.frames in data_agmerra list
    for (i_k in 1:nrow(xy)) {
      tdf <- format(seq(as.Date(paste0(yr,"/1/1")), as.Date(paste0(yr,"/12/31")), "days") ,"%Y-%m-%d")
      tdf <- cbind.data.frame("date"=tdf, "value"=NA)
      tdf$value <- datobs[i_k,]
      names(tdf) <- c("date",tvar)
      tdf$year <- unlist(lapply(paste(tdf$date),FUN=function(x) {unlist(strsplit(x,"-",fixed=T))[1]}))
      tdf <- tdf[,c("date","year",tvar)]
      data_agmerra[[i_k]] <- rbind(data_agmerra[[i_k]], tdf)
      rm(tdf); g=gc(); rm(g)
    }
  }
  
  #return object
  return(data_agmerra)
}



