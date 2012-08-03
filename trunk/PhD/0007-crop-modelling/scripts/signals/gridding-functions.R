#Julian Ramirez-Villegas
#February 2012
#CIAT / CCAFS / UoL


#####################################################################
#function wrapper for gridding a given grid
#inRs is the high resolution raster with gridvalues to be scaled out
#outRs is the name and path of output raster
#naVal is the NA value for those cells that are not already NA
#rs_dis is a high resolution raster with district IDs
#rs_c is the reference coarse resolution raster
#rs_a is a raster containing the area of each coarse gridcell
#xy is the list of xy values of cells
gridRaster <- function(inRs,outRs,naVal=-9999,rs_dis,rs_c,rs_a,xy) {
  library(raster)
  if (!file.exists(outRs)) {
    inRs[which(inRs[]==naVal)] <- 0
    rs_dis <- readAll(rs_dis)
    rs_c <- readAll(rs_c)
    rs_a <- readAll(rs_a)
    
    #apply the function to the cells 
    x <- apply(data.frame(CELLID=1:nrow(xy)),1,weightValues,rs_dis,rs_c,rs_a,inRs)
    
    #create and write output raster
    out_rs <- raster(dumm)
    out_rs[which(!is.na(dumm[]))] <- x
    out_rs <- writeRaster(out_rs,outRs,format="ascii")
  }
}


#Functions for yield data gridding
#####################################################################
#function wrapper for parallelising the process over years
controlGridding <- function(year) {
  if (tolower(format)=="ascii") {
    format <- "ascii"
    ext <- ".asc"
  } else if (tolower(format) == "gtiff") {
    format <- "GTiff"
    ext <- ".tif"
  } else {
    stop("invalid format: only gtiff or ascii are allowed")
  }
  
  library(raster)
  if (!file.exists(paste(outDir,"/",method,"-",year,".asc",sep=""))) {
    yield_rs <- raster(paste(yldDir,"/",method,"-",year,".asc",sep=""))
    yield_rs[which(yield_rs[]==-9999)] <- 0
    rs_dis <- readAll(rs_dis)
    rs_c <- readAll(rs_c)
    rs_a <- readAll(rs_a)
    #x <- weightValues(199,rs_dis,rs_c,rs_a,yield_rs) test!
    #setwd("H:")
    #write.csv(x,paste("test",year,".csv",sep=""))
    x <- apply(data.frame(CELLID=1:nrow(xy)),1,weightValues,rs_dis,rs_c,rs_a,yield_rs)
    
    #create and write output raster
    out_rs <- raster(dumm)
    out_rs[which(!is.na(dumm[]))] <- x
    out_rs <- writeRaster(out_rs,paste(outDir,"/",method,"-",year,ext,sep=""),format=format)
  }
}


#####################################################################
#function to get grid the yield data
#i=coarse gridcell
#dists=high resolution raster with district IDs
#rs_coarse=high resolution raster with coarse cell IDs
#rs_area=high resolution raster with area of cells
#rs_yield=high resolution raster with yield values per district
weightValues <- function(i,dists,rs_coarse,rs_area,rs_yield) { 
  wCells <- which(rs_coarse[]==i)
  xyCells <- as.data.frame(xyFromCell(rs_coarse,wCells))
  wDist <- unique(extract(dists,xyCells))
  wDist <- wDist[which(!is.na(wDist))]
  if (length(wDist)>0) {
    for (dis in wDist) {
      cat("Processing district",dis,"\n")
      #selecting all cells of district
      xyDist <- as.data.frame(xyFromCell(dists,which(dists[]==dis)))
      
      #selecting cells in the big-cell domain
      xyDist <- xyDist[which(xyDist$x>=min(xyCells$x) & xyDist$x<=max(xyCells$x)),]
      xyDist <- xyDist[which(xyDist$y>=min(xyCells$y) & xyDist$y<=max(xyCells$y)),]
      
      #get the areas of those pixels
      xyDist$AREA <- extract(rs_area,xyDist)
      yld <- unique(extract(rs_yield,xyDist[,c("x","y")]))
      if (length(yld)>1) {
        cat("More than one yield value, choosing the most frequent \n")
        yld.tmp <- extract(rs_yield,xyDist[,c("x","y")])
        nv <- 0
        for (val in yld) {
          nv2 <- length(which(yld.tmp==val))
          if (nv2>nv) {x <- val}
          nv <- nv2
        }
        yld <- x
      }
      
      outRow <- data.frame(DISID=dis,AREA.IN.CELL=sum(xyDist$AREA),YIELD=yld)
      if (dis==wDist[1]) {
        distOut <- outRow
      } else {
        distOut <- rbind(distOut,outRow)
      }
      #points(xyDist,col=cols[which(wDist==dis)],pch=20,cex=0.5)
    }
    #   2.5. Calculate the average by weighting the values of yield times 
    #        the area divided by sum of areas (per year)
    distOut$PRODUCT <- distOut$AREA.IN.CELL*distOut$YIELD
    outVal <- sum(distOut$PRODUCT,na.rm=T)/sum(distOut$AREA.IN.CELL)
  } else {
    outVal <- NA
  }
  return(outVal)
}
