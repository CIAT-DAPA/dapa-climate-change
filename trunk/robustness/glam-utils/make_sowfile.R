#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#testing run
#library(raster); library(rgdal)
#wd <- "~/Leeds-work/quest-for-robustness"
#yiDir <- paste(wd,"/data/yield_data_maize",sep="")
#sowDir <- paste(wd,"/data/crop_calendar_sacks",sep="")
#yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
#cellid <- 1792
#xy <- xyFromCell(yrs, cellid)
#lon <- xy[1]; lat <- xy[2]
#sdate <- raster(paste(sowDir,"/major_maize_plant.end.tif",sep=""))
#sdate <- as.numeric(extract(sdate, xy))
#x <- data.frame(CELL=cellid,X=lon,Y=lat,SOW_DATE=sdate)
#outfile <- paste(sowDir,"/sowing.txt",sep="")
#write_sowdates(x,outfile)

#### Write the file with sowing dates
#### a similar
write_sowdates <- function(x,outfile,fields=list(CELL="CELL",SOW_DATE="SOW_DATE")) {
  
  if (length(which(toupper(names(fields)) %in% c("CELL","SOW_DATE"))) != 2) {
    stop("field list incomplete")
  }
  
  if (length(which(toupper(names(x)) %in% toupper(unlist(fields)))) != 2) {
    stop("field list does not match with data.frame")
  }
  
  if (class(x) != "data.frame") {
    stop("x must be a data.frame")
  }
  
  names(x)[which(toupper(names(x)) == toupper(fields$CELL))] <- "CELL"
  names(x)[which(toupper(names(x)) == toupper(fields$SOW_DATE))] <- "SOW_DATE"
  
  cell <- x$CELL
  fsg <- file(outfile,"w")
  col <- 0; row <- 1
  for (cll in cell) {
    if (col == 10) {
      col <- 1
      row <- row+1
    } else {
      col <- col+1
    }
    
    dat <- round(x$SOW_DATE[which(x$CELL == cll)],0)
    
    cat(paste(sprintf("%1$4d%2$4d",row,col),
              sprintf("%5d",dat),"\n",sep=""),file=fsg)
  }
  close(fsg)
  return(outfile)
}

