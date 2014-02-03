#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#write a file that specifies the latitude and longitude of the whole domain for a given set
#of runs
write_gridFile <- function(x,outfile,fields=list(CELL="CELL",X="X",Y="Y",COL="COL",ROW="ROW")) {
  if (length(which(toupper(names(fields)) %in% c("CELL","X","Y","COL","ROW"))) != 5) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 5) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$X))] <- "X"
  names(x)[which(toupper(names(x)) == toupper(fields$Y))] <- "Y"
  names(x)[which(toupper(names(x)) == toupper(fields$COL))] <- "COL"
  names(x)[which(toupper(names(x)) == toupper(fields$ROW))] <- "ROW"
  
  gf <- file(outfile,"w")
  cat("GRID_CODE   LAT  LON  RLAT            RLON\n",file=gf)
  for (i in 1:nrow(cells)) {
    cat(paste(sprintf("%12d",x$CELL[i]),
              sprintf("%1$5d%2$5d",x$ROW[i],x$COL[i]),
              sprintf("%1$16.4f%2$16.4f",x$Y[i],x$X[i]),"\n",sep=""),file=gf)
  }
  close(gf)
  return(outfile)
}

