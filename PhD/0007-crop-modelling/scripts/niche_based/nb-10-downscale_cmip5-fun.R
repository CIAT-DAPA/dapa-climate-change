#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Jun 2013

#bias correct data for ecocrop and sdm runs
#x=obs, y=his, z=rcp, varname
biasfun <- function(x,y,z,varname) {
  xy <- as.data.frame(xyFromCell(x,1:ncell(x)))
  xy <- cbind(cell=1:ncell(x),xy)
  xy$obs <- extract(x,xy[,c("x","y")])
  xy$his <- extract(y,xy[,c("x","y")])
  xy$rcp <- extract(z,xy[,c("x","y")])
  if (varname == "prec") {
    #calculate factors
    xy$his <- sapply(xy$his,FUN=function(x) {if (x == 0) {x <- 1}; return(x)})
    xy$del <- (xy$rcp-xy$his)/(xy$his)
    #xy$bc <- (xy$obs-xy$his)/(xy$his)
    
    #calculate climate
    xy$del_rcp <- xy$obs * (1 + xy$del)
    xy$del_rcp <- sapply(xy$del_rcp,FUN=function(x) max(c(0,x)))
    #xy$bc_rcp <- xy$rcp * (1 + xy$bc)
    #xy$bc_rcp <- sapply(xy$bc_rcp,FUN=function(x) max(c(0,x)))
  } else {
    #calculate factors
    xy$del <- xy$rcp-xy$his
    #xy$bc <- xy$obs-xy$his
    
    #calculate climate
    xy$del_rcp <- xy$obs + xy$del
    #xy$bc_rcp <- xy$rcp + xy$bc
  }
  #bcr <- raster(x); bcr[xy$cell] <- xy$bc_rcp
  delr <- raster(x); delr[xy$cell] <- xy$del_rcp
  #obj <- list(delr,bcr)
  return(delr)
}

