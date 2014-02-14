#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#testing run
#library(raster); library(rgdal)
#wd <- "~/Leeds-work/quest-for-robustness"
#yiDir <- paste(wd,"/data/yield_data_maize",sep="")
#solDir <- paste(wd,"/data/soils",sep="")
#yrs <- raster(paste(yiDir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
#cellid <- 1792
#xy <- xyFromCell(yrs, cellid)
#lon <- xy[1]; lat <- xy[2]
#rll <- raster(paste(solDir,"/rll_lr.tif",sep=""))
#dul <- raster(paste(solDir,"/dul_lr.tif",sep=""))
#sat <- raster(paste(solDir,"/sat_lr.tif",sep=""))

#rll <- as.numeric(extract(rll, xy))
#dul <- as.numeric(extract(dul, xy))
#sat <- as.numeric(extract(sat, xy))

#x <- data.frame(CELL=cellid,X=lon,Y=lat,RLL=rll,DUL=dul,SAT=sat,ASW=(dul-rll))
#outfile <- paste(solDir,"/soiltypes.txt",sep="")
#write_soil_types(x,outfile)

#outfile <- paste(solDir,"/soilcodes.txt",sep="")
#write_soilcodes(outfile)

##########################################################################################
########### write the soil file, with each soil code belonging to a gridcell, 
########### to avoid the hassle of having to assign categories to each gridcell, and hence
########### using the actual gridcell soil data
##########################################################################################
make_soiltypes <- function(x,outfile,fields=list(CELL="CELL",RLL="RLL",DUL="DUL",SAT="SAT")) {
  #note that x must be a data frame
  
  #checks on the existence of proper fields in the input command and in the data frame (x)
  if (length(which(toupper(names(fields)) %in% c("CELL","RLL","DUL","SAT"))) != 4) {
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
  names(x)[which(toupper(names(x)) == toupper(fields$RLL))] <- "RLL"
  names(x)[which(toupper(names(x)) == toupper(fields$DUL))] <- "DUL"
  names(x)[which(toupper(names(x)) == toupper(fields$SAT))] <- "SAT"
  
  #open the connection
  sfil <- file(outfile,"w")
  cat("SOIL_CODE  RLL    RLL_L  RLL_U  DUL   DUL_L   DUL_U  SAT    SAT_L  SAT_U  ASW_L  ASW_U  SOIL_TYPE\n",file=sfil)
  
  #loop through cells
  cll <- x$CELL
  cellData <- x[which(x$CELL==cll),]
  
  #perturb values of RLL, DUL and SAT
  rll_m <- cellData$RLL
  rll_n <- rll_m * 0.9
  rll_x <- rll_m * 1.1
  
  dul_m <- cellData$DUL
  dul_n <- dul_m * 0.9
  dul_x <- dul_m * 1.1
  
  sat_m <- cellData$SAT
  sat_n <- sat_m * 0.9
  sat_x <- sat_m * 1.1
  
  #available soil water (DUL-RLL)
  asw_l <- dul_n-rll_n
  asw_u <- asw_l*1.75
  
  #write the output line
  for (cnt in 1:7) {
    out_line <- paste(sprintf("%-11s",cnt),
                      substr(sprintf("%3.2f",rll_m),2,4),"    ",substr(sprintf("%3.2f",rll_n),2,4),"    ",substr(sprintf("%3.2f",rll_x),2,4),"    ",
                      substr(sprintf("%3.2f",dul_m),2,4),"    ",substr(sprintf("%3.2f",dul_n),2,4),"    ",substr(sprintf("%3.2f",dul_x),2,4),"    ",
                      substr(sprintf("%3.2f",sat_m),2,4),"    ",substr(sprintf("%3.2f",sat_n),2,4),"    ",substr(sprintf("%3.2f",sat_x),2,4),"    ",
                      substr(sprintf("%3.2f",asw_l),2,4),"    ",substr(sprintf("%3.2f",asw_u),2,4),"     ",paste("gridcell_",cll,sep=""),"\n",sep="")
    cat(out_line,file=sfil)
    cnt <- cnt+1
  }
  
  #end of loop, close the file, and return the name of output file
  close(sfil)
  return(outfile)
}


###### write soil codes
# this will write a file with only one line, and all being 1 1 1
make_soilcodes <- function(outfile) {
  fsg <- file(outfile,"w")
  cnt <- 1; col <- 0; row <- 1
  cat(paste(sprintf("%1$4d%2$4d",1,1),
            sprintf("%4d",cnt),"\n",sep=""),file=fsg)
  cnt <- cnt+1
  close(fsg)
  return(outfile)
}

