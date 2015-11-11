#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Oct 2015

#extract BCSD data for a set of locations
extract_bcsd <- function(sce, gcm, years, bbox, loc_xy, bcsd_dir) {
  #sce <- sce_list[1]
  #gcm <- gcm_list[1]
  #years <- c(2000:2004)
  #bbox <- rs_ext
  
  #correct bounding box
  if (bbox@xmin < 0)  bbox@xmin <- bbox@xmin+360
  if (bbox@xmax < 0)  bbox@xmax <- bbox@xmax+360
  
  #convert lon to 0-360
  xy_tmp <- loc_xy
  xy_tmp$lon_360 <- unlist(lapply(xy_tmp$lon, FUN=function(x) {if (x < 0) {xt <- x+360; return(xt)}}))
  
  #loop years to extract the data
  for (yr in years) {
    #yr <- years[1]
    cat("...extracting BCSD data for year=",yr,"\n")
    nc_indir <- paste(bcsd_dir,"/",sce,sep="")
    
    #create dates for this year
    tdates <- lapply(1:nrow(loc_xy), FUN=function(x) {tdates <- format(seq(as.Date(paste0(yr,"/1/1")), as.Date(paste0(yr,"/12/31")), "days") ,"%Y-%m-%d"); tdates <- data.frame(date=tdates); return(tdates)})
    
    for (vname in c("pr","tasmin","tasmax")) {
      #vname <- "pr"
      nc_file <- paste(vname,"_day_BCSD_",sce,"_r1i1p1_",gcm,"_",yr,".nc",sep="")
      
      #cut to study area
      setwd(nc_indir)
      if (file.exists("tempfile.nc")) {sytem("rm -f tempfile.nc")}
      system(paste("cdo sellonlatbox,",bbox@xmin,",",bbox@xmax,",",bbox@ymin,",",bbox@ymax," ",nc_file," tempfile.nc",sep=""))
      
      #read stack
      rstk <- stack("tempfile.nc",varname=vname)
      
      ## Convert units to mm/day and celsius Degrees
      if (vname == "pr") {rstk <- rstk * 86400} else {rstk <- rstk - 273.15}
      
      #extract data
      values <- extract(rstk, cbind(x=xy_tmp$lon_360, y=xy_tmp$lat))
      for (iloc in 1:nrow(loc_xy)) {
        if (nrow(tdates[[iloc]]) == 366 & length(values[iloc,]) == 365) {
          tdates[[iloc]] <- cbind(tdates[[iloc]], value=c(values[iloc,],values[iloc,365]))
        } else {
          tdates[[iloc]] <- cbind(tdates[[iloc]], value=values[iloc,])
        }
        names(tdates[[iloc]])[ncol(tdates[[iloc]])] <- vname
      }
      
      #clean up and return to home
      system("rm -f tempfile.nc")
      setwd("~")
      rm(list=c("values","rstk"))
    }
    
    #append to list of all locations
    if (yr == years[1]) {
      bcsd_data <- tdates
    } else {
      for (iloc in 1:nrow(loc_xy)) {
        #iloc <- 1
        bcsd_data[[iloc]] <- rbind(bcsd_data[[iloc]], tdates[[iloc]])
      }
    }
    rm(tdates)
  }
  rm(xy_tmp)
  
  #return object
  return(bcsd_data)
}

