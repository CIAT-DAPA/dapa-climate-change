#Julian Ramirez-Villegas
#March 2012
#CIAT / CCAFS / UoL

##################################################################
#functions to area-weighted aggregation of raster data

#wrapper to parallelise
agg_wrapper <- function(year) {
  library(raster); library(rgdal)
  #cat("\nProcessing year",year,"\n")
  for (mth in 1:12) { #cru_ts_3_10.1901.2009.tmn_1960_1.asc
    cat(mth," ")
    rs <- raster(paste(wd,"/monthly_grids/",variable,"/",variable,"_",year,"_",mth,sep=""))
    rs_oa <- awAggWrapper(rs,xt)
    rs_oa <- writeRaster(rs_oa,paste(outFolder,"/",variable,"_",year,"_",mth,".asc",sep=""),format="ascii")
    rm(rs_oa); g=gc()
  }
  cat("\n")
}


#wrapper function for aggregating data based on area-weighted average
#only works from 0.5 to 1d cells
awAggWrapper <- function(rs,xt) {
  rs <- crop(rs,xt)
  a <- area(rs); a[which(is.na(rs[]))] <- NA
  
  rs_ag <- aggregate(rs,fact=2)
  rs_ag[] <- 1:ncell(rs_ag)
  
  agm <- data.frame(CELL=which(!is.na(rs_ag[])))
  agm$X <- xFromCell(rs_ag,agm$CELL); agm$Y <- yFromCell(rs_ag,agm$CELL)
  
  x <- apply(agm,1,awBasicFunction,rs,a)
  
  rs_out <- raster(rs_ag)
  rs_out[agm$CELL] <- x
  return(rs_out)
}

#basic aggregating function
awBasicFunction <- function(x,rs,a) {
  cell <- x[1]
  xy <- expand.grid(c(x[2]-0.25,x[2]+0.25),c(x[3]-0.25,x[3]+0.25))
  wm <- data.frame(VALUE=extract(rs,xy),AREA=extract(a,xy))
  if (length(which(is.na(wm$VALUE))) == nrow(wm)) {
    out_val <- NA
  } else {
    wm$PROD <- wm$VALUE*wm$AREA
    out_val <- sum(wm$PROD,na.rm=T)/sum(wm$AREA,na.rm=T)
  }
  return(out_val)
}

# plot(rs_ag,xlim=c(-1,1),ylim=c(30,34))
# plot(rs,xlim=c(-1,1),ylim=c(30,34))
# points(xy,pch=20)
# points(agm$X[1100],agm$Y[1100])

