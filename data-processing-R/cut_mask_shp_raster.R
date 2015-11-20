require(raster)
library(maptools)

dirbase <- list.dirs(paste("D:/CIAT/Workspace/fnc", sep=""), recursive = FALSE, full.names = TRUE)
country <- "COL"
dept <- "Valle del Cauca" 

mask <- getData('GADM', country=country, level=0)

mask1 <- readOGR("D:/CIAT/_tools/AdminBoundaries/SHP_files/COL_adm/COL1.shp", layer= "COL1")
mask1 <- subset(mask1, mask1$NAME_1 == dept)


for (dir in dirbase){
  
  setwd(dir)
  
  rsList <- list.files(dir, pattern=".asc", full.names = TRUE)
  
  outDir <- paste(dir, "/cut_", country, sep="")
  if (!file.exists(outDir)) {dir.create(outDir)}
  
  for(rs in rsList){
    
    rsName <- basename(rs)
    
    if (!file.exists(paste0(outDir, "/", rsName, sep=""))) {
      rsCrop <- crop(raster(rs), extent(mask))
      rsMask <- mask(rsCrop, mask)
      
      ascWrite <- writeRaster(rsMask, paste0(outDir, "/", rsName, sep=""), overwrite=F)
      cat(paste0(" ", rsName, " cut done\n"))
    }
    
  }
  
  cat(" ..done\n")
  
  
  
  outDir <- paste(dir, "/cut_", dept, sep="")
  if (!file.exists(outDir)) {dir.create(outDir)}
  
  for(rs in rsList){
    
    rsName <- basename(rs)
    
    if (!file.exists(paste0(outDir, "/", rsName, sep=""))) {
      rsCrop <- crop(raster(rs), extent(mask1))
      rsMask <- mask(rsCrop, mask1)
      
      ascWrite <- writeRaster(rsMask, paste(outDir, "/", rsName, sep=""), overwrite=F)
      cat(paste0(" ", rsName, " cut done\n"))
    }
    
  }
  
  cat(" ..done\n")
  
}









