#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("error")

src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
source(paste(src.dir,"/cropDist-functions.R",sep=""))

#base directory
bDir <- "E:/PhD-work/crop-modelling"

#read IDs
ids <- read.csv(paste(bDir,"/crop-distribution-data/crop-IDs.csv",sep=""))
cList <- ids$ABRV[which(!is.na(ids$ABRV))]

for (cname in cList) {
  cat("Processing",paste(cname),"\n")
  createCropDist(bDir,ids,cname,ow=F)
}

