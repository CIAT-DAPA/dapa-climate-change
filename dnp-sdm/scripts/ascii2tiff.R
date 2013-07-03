#JRV 2013
#CIAT / CCAFS
stop("!")

#load libraries
library(raster)

#working directory
wd <- "/media/DATA/CIAT_work/DNP-biodiversity"
setwd(wd)

inDir <- "./env-data/bioclim"

outDir <- "./env-data/bioclim_gtiff"
if (!file.exists(outDir)) {dir.create(outDir)}

for (i in 1:19) {
  #i <- 1
  cat("bio",i,"\n")
  if (file.exists(paste(inDir,"/bio_",i,".asc",sep=""))) {
    rs <- raster(paste(inDir,"/bio_",i,".asc",sep=""))
    if (!file.exists(paste(outDir,"/bio_",i,".tif",sep=""))) {
      writeRaster(rs,paste(outDir,"/bio_",i,".tif",sep=""),format="GTiff")
    }
    setwd(inDir)
    system(paste("gzip bio_",i,".asc ",sep=""))
    setwd(wd)
  }
}

inDir <- "./env-data/prec_monthly"

outDir <- "./env-data/prec_monthly_gtiff"
if (!file.exists(outDir)) {dir.create(outDir)}

#loop through months
for (i in 1:12) {
  #i <- 1
  cat("prec",i,"\n")
  if (file.exists(paste(inDir,"/prec_",i,".asc",sep=""))) {
    rs <- raster(paste(inDir,"/prec_",i,".asc",sep=""))
    if (!file.exists(paste(outDir,"/prec_",i,".tif",sep=""))) {
      writeRaster(rs,paste(outDir,"/prec_",i,".tif",sep=""),format="GTiff")
    }
    setwd(inDir)
    system(paste("gzip prec_",i,".asc ",sep=""))
    setwd(wd)
  }
}





