#Julian Ramirez-Villegas
#November 2011

#Cut climate data to indian domain
require(rgdal); require(raster)

cutToDomain <- function(inFolder,outFolder,msk) {
  setwd(inFolder)
  fList <- list.files()
  
  if (!file.exists(outFolder)) {dir.create(outFolder,recursive=T)}
  
  for (fname in fList) {
    cat("Processing",fname,"\n")
    rs <- raster(fname)
    rt <- crop(rs,extent(raster(msk)))
    writeRaster(rt,paste(outFolder,"/",fname,sep=""),format='ascii',overwrite=T)
  }
  return("Done!")
}

msk <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analysis-mask/countries/ind_0.asc"
ifol <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/afasia_2_5min"
ofol <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_2_5min"

cutToDomain(ifol,ofol,msk)

