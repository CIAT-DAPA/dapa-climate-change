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



##########################################################################################
########### write the soil file, with each soil code belonging to a gridcell, 
########### to avoid the hassle of having to assign categories to each gridcell, and hence
########### using the actual gridcell soil data
##########################################################################################
write_soil_types <- function(x,outfile,fields=list(CELL="CELL",SAND="SAND",CLAY="CLAY",AREA_FRAC="AREA_FRAC")) {
  #note that x must be a data frame
  
  #checks on the existence of proper fields in the input command and in the data frame (x)
  if (length(which(toupper(names(fields)) %in% c("CELL","SAND","CLAY","AREA_FRAC"))) != 4) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 4) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  #get names right
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$SAND))] <- "SAND"
  names(x)[which(toupper(names(x)) == toupper(fields$CLAY))] <- "CLAY"
  names(x)[which(toupper(names(x)) == toupper(fields$AREA_FRAC))] <- "AREA_FRAC"
  
  #open the connection
  sfil <- file(outfile,"w")
  cat("SOIL_CODE  RLL    RLL_L  RLL_U  DUL   DUL_L   DUL_U  SAT    SAT_L  SAT_U  ASW_L  ASW_U  SOIL_TYPE\n",file=sfil)
  
  #loop through cells
  for (cll in unique(x$CELL)) {
    cellData <- x[which(x$CELL==cll),]
    
    #calculate hydro characteristics
    cellData$RLL <- apply(cbind(cellData$SAND,cellData$CLAY),1,function(x) hydroChars(x[1],x[2])$RLL)
    cellData$DUL <- apply(cbind(cellData$SAND,cellData$CLAY),1,function(x) hydroChars(x[1],x[2])$DUL)
    cellData$SAT <- apply(cbind(cellData$SAND,cellData$CLAY),1,function(x) hydroChars(x[1],x[2])$SAT)
    
    #get mean, max, min values of RLL, DUL and SAT
    rll_m <- sum(cellData$RLL*cellData$AREA_FRAC)/sum(cellData$AREA_FRAC)
    rll_n <- min(cellData$RLL)
    rll_x <- max(cellData$RLL)
    
    dul_m <- sum(cellData$DUL*cellData$AREA_FRAC)/sum(cellData$AREA_FRAC)
    dul_n <- min(cellData$DUL)
    dul_x <- max(cellData$DUL)
    
    sat_m <- sum(cellData$SAT*cellData$AREA_FRAC)/sum(cellData$AREA_FRAC)
    sat_n <- min(cellData$SAT)
    sat_x <- max(cellData$SAT)
    
    #available soil water (DUL-RLL)
    asw_l <- dul_n-rll_n
    asw_u <- asw_l*1.75
    
    #write the output line
    out_line <- paste(sprintf("%-11s",cll),
                      substr(rll_m,2,4),"    ",substr(rll_n,2,4),"    ",substr(rll_x,2,4),"    ",
                      substr(dul_m,2,4),"    ",substr(dul_n,2,4),"    ",substr(dul_x,2,4),"    ",
                      substr(sat_m,2,4),"    ",substr(sat_n,2,4),"    ",substr(sat_x,2,4),"    ",
                      substr(asw_l,2,4),"    ",substr(asw_u,2,4),"     ",paste("GridCell_",cll,sep=""),"\n",sep="")
    cat(out_line,file=sfil)
  }
  
  #end of loop, close the file, and return the name of output file
  close(sfil)
  return(outfile)
}


###### write soil codes
write_soilcodes <- function(x,outfile,cell=c(636),fields=list(CELL="CELL",COL="COL",ROW="ROW")) {
  
  if (length(which(toupper(names(fields)) %in% c("CELL","COL","ROW"))) != 3) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 3) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$COL))] <- "COL"
  names(x)[which(toupper(names(x)) == toupper(fields$ROW))] <- "ROW"
  
  fsg <- file(outfile,"w")
  for (cll in cell) {
    col <- x$COL[which(x$CELL == cll)]
    row <- x$ROW[which(x$CELL == cll)]
    
    cat(paste(sprintf("%1$4d%2$4d",row,col),
              sprintf("%6d",cll),"\n",sep=""),file=fsg)
  }
  close(fsg)
  return(outfile)
}






