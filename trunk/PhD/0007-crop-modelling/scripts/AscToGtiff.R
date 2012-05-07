#Julian Ramirez-Villegas
#CIAT/CCAFS/UoL
#May 2012
stop("Do not runt the whole thing")

#convert anything heavy to geoTiff and compress the asciis into gz

library(raster)

#Climate signals on yield for Indian wheat

#local dirs
bDir <- "F:/PhD-work/crop-modelling"
setwd(bDir)

###################################
#### GNUT
###################################
cropName <- "gnut"
cropDir <- paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),sep="")

#compressing base grids
bgDir <- paste(cropDir,"/0_base_grids",sep="")
AsctoGTiff(bgDir)

#Sacks et al. (2010) crop calendar
calDir <- paste(cropDir,"/calendar/gnut",sep="")
AsctoGTiff(calDir)

#Areas, yields and production summaries
smDir <- paste(cropDir,"/raster/summaries",sep="")
AsctoGTiff(smDir)

#yearly detrended high-resolution yield rasters
for (method in c("fou","lin","loe","qua","raw")) {
  yyDir <- paste(cropDir,"/raster/yearly/",method,sep="")
  AsctoGTiff(yyDir)
}


###################################
#### RICE
###################################
cropName <- "rice"
cropDir <- paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),sep="")

#compressing base grids
bgDir <- paste(cropDir,"/0_base_grids",sep="")
AsctoGTiff(bgDir)

#Sacks et al. (2010) crop calendar
calDir <- paste(cropDir,"/calendar/main",sep=""); AsctoGTiff(calDir)
calDir <- paste(cropDir,"/calendar/second",sep=""); AsctoGTiff(calDir)

#Areas, yields and production summaries
smDir <- paste(cropDir,"/raster/summaries",sep="")
AsctoGTiff(smDir)

#yearly detrended high-resolution yield rasters
for (method in c("fou","lin","loe","qua","raw")) {
  yyDir <- paste(cropDir,"/raster/yearly/",method,sep="")
  AsctoGTiff(yyDir)
}


###################################
#### SORG-KHARIFF
###################################
cropName <- "sorg-khariff"
cropDir <- paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),sep="")

#compressing base grids
bgDir <- paste(cropDir,"/0_base_grids",sep="")
AsctoGTiff(bgDir)

#Sacks et al. (2010) crop calendar
calDir <- paste(cropDir,"/calendar/main",sep=""); AsctoGTiff(calDir)
calDir <- paste(cropDir,"/calendar/second",sep=""); AsctoGTiff(calDir)

#Areas, yields and production summaries
smDir <- paste(cropDir,"/raster/summaries",sep="")
AsctoGTiff(smDir)

#yearly detrended high-resolution yield rasters
for (method in c("fou","lin","loe","qua","raw")) {
  yyDir <- paste(cropDir,"/raster/yearly/",method,sep="")
  AsctoGTiff(yyDir)
}


###################################
#### SORG-RABI
###################################
cropName <- "sorg-rabi"
cropDir <- paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),sep="")

#compressing base grids
bgDir <- paste(cropDir,"/0_base_grids",sep="")
AsctoGTiff(bgDir)

#Sacks et al. (2010) crop calendar
calDir <- paste(cropDir,"/calendar/main",sep=""); AsctoGTiff(calDir)
calDir <- paste(cropDir,"/calendar/second",sep=""); AsctoGTiff(calDir)

#Areas, yields and production summaries
smDir <- paste(cropDir,"/raster/summaries",sep="")
AsctoGTiff(smDir)

#yearly detrended high-resolution yield rasters
for (method in c("fou","lin","loe","qua","raw")) {
  yyDir <- paste(cropDir,"/raster/yearly/",method,sep="")
  AsctoGTiff(yyDir)
}


###################################
#### WHEAT
###################################
cropName <- "wheat"
cropDir <- paste(bDir,"/GLAM/climate-signals-yield/",toupper(cropName),sep="")

#compressing base grids
bgDir <- paste(cropDir,"/0_base_grids",sep="")
AsctoGTiff(bgDir)

#Sacks et al. (2010) crop calendar
calDir <- paste(cropDir,"/calendar/wwin",sep=""); AsctoGTiff(calDir)
calDir <- paste(cropDir,"/calendar/wunk",sep=""); AsctoGTiff(calDir)

#Areas, yields and production summaries
smDir <- paste(cropDir,"/raster/summaries",sep="")
AsctoGTiff(smDir)

#yearly detrended high-resolution yield rasters
for (method in c("fou","lin","loe","qua","raw")) {
  yyDir <- paste(cropDir,"/raster/yearly/",method,sep="")
  AsctoGTiff(yyDir)
}


##############################################################################
##############################################################################
#Function to process everything into a folder
AsctoGTiff <- function(this_dir) {
  ascList <- list.files(this_dir,pattern="\\.asc")
  if (length(grep("\\.gz",ascList)) > 0) {
    ascList <- ascList[-grep("\\.gz",ascList)]
  }
  
  if (length(ascList) == 0) {
    cat("This folder does not contain any raw ascii grid \n")
  } else {
    for (asc in ascList) {
      cat(asc,"\n")
      rs <- raster(paste(this_dir,"/",asc,sep=""))
      tifName <- gsub(".asc",".tif",asc)
      
      if (!file.exists(paste(this_dir,"/",tifName,sep=""))) {
        rs <- writeRaster(rs,paste(this_dir,"/",tifName,sep=""),format="GTiff")
      }
      
      if (!file.exists(paste(this_dir,"/",asc,".gz",sep=""))) {
        setwd(this_dir)
        system(paste("7z a -tgzip",paste(asc,".gz",sep=""),asc))
        setwd(bDir)
      }
      
      if (file.exists(paste(this_dir,"/",asc,sep=""))) {
        if (file.exists(paste(this_dir,"/",tifName,sep=""))) {
          if (file.exists(paste(this_dir,"/",asc,".gz",sep=""))) {
            x <- file.remove(paste(this_dir,"/",asc,sep=""))
          }
        }
      }
    }
  }
  return("Done!")
}

