#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#### Write the file with sowing dates
#### a similar
write_sowdates <- function(x,outfile,cell=c(636),fields=list(CELL="CELL",COL="COL",ROW="ROW",SOW_DATE="SOW_DATE")) {
  
  if (length(which(toupper(names(fields)) %in% c("CELL","COL","ROW","SOW_DATE"))) != 4) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 4) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$COL))] <- "COL"
  names(x)[which(toupper(names(x)) == toupper(fields$ROW))] <- "ROW"
  names(x)[which(toupper(names(x)) == toupper(fields$SOW_DATE))] <- "SOW_DATE"
  
  fsg <- file(outfile,"w")
  col <- 0; row <- 1
  for (cll in cell) {
    if (col == 10) {
      col <- 1
      row <- row+1
    } else {
      col <- col+1
    }
    
    #col <- x$COL[which(x$CELL == cll)]
    #row <- x$ROW[which(x$CELL == cll)]
    dat <- round(x$SOW_DATE[which(x$CELL == cll)],0)
    
    cat(paste(sprintf("%1$4d%2$4d",row,col),
              sprintf("%6d",dat),"\n",sep=""),file=fsg)
  }
  close(fsg)
  return(outfile)
}


##############################
## write yield file
write_yield <- function(x,outfile,yld_stk,yri,yrf,cell=c(636),fields=list(CELL="CELL",X="X",Y="Y")) {
  
  if (length(which(toupper(names(fields)) %in% c("CELL","X","Y"))) != 3) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 3) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  if (nlayers(yld_stk) != length(yri:yrf)) {
    stop("number of yield layers in stack does not match with number of years")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$X))] <- "X"
  names(x)[which(toupper(names(x)) == toupper(fields$Y))] <- "Y"
  
  fsg <- file(outfile,"w")
  
  col <- 0; row <- 1
  for (cll in cell) {
    if (col == 10) {
      col <- 1
      row <- row+1
    } else {
      col <- col+1
    }
    #col <- x$COL[which(x$CELL == cll)]
    #row <- x$ROW[which(x$CELL == cll)]
    
    lon <- x$X[which(x$CELL == cll)]
    lat <- x$Y[which(x$CELL == cll)]
    
    yValues <- as.numeric(extract(yld_stk,cbind(X=lon,Y=lat)))
    
    for (i in 1:length(yValues)) {
      dat <- yValues[i]
      if (is.na(dat)) {dat <- -99}
      if (dat == 0) {dat <- -99}
      cat(paste(sprintf("%4d",(yri+i-1+1900)),
                sprintf("%1$4d%2$4d",row,col),
                sprintf("%8.1f",dat),"\n",sep=""),file=fsg)
    }
    
  }
  close(fsg)
  return(outfile)
}

