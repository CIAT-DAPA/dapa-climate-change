#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

#source folder
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
source(paste(src.dir,"/readMonfreda-functions.R",sep=""))

bd <- "E:/PhD-work/crop-modelling/crop-distribution-data/Monfreda"
setwd(bDir)

id <- paste(bd,"/rasters",sep="")
od <- paste(bd,"/rasters_fixed",sep="")
if (!file.exists(od)) {dir.create(od)}

#list files in input folder and run function to correct data
fList <- list.files(id,pattern=".asc")

#loop through files
for (gn in fList) {
  cat("Processing",gn,"\n")
  convertMon(id,od,gn,ow=F)
}



