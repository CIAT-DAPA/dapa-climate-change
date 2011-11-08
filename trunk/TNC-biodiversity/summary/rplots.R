#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#2011

require(rgdal); require(raster)

#this will fix the resolution from rectangles to squares
#create dummy fixing raster for resample
fixRaster <- function(rs.src) {
  #create extent and target raster
  xt <- extent(c(-118,-33,-60,35))
  rs.tgt <- raster(xt,ncol=2040,nrow=2280)
  
  #fix the raster
  rs.fxd <- resample(rs.src,rs.tgt,method="ngb")
  
  return(rs.fxd)
}

fixData <- function(fileDir) {
  #listing files in folder
  cat("\n")
  cat("Processing within folder",fileDir,"\n")
  fList <- list.files(fileDir,pattern=".asc")
  fList <- fList[-grep("xml",fList)]
  
  for (inFile in fList) {
    cat("Fixing file",inFile,"\n")
    
    #load original raster
    rs.inp <- raster(paste(fileDir,"/",inFile,sep=""))
    rs.inp <- readAll(rs.inp)
    
    #apply
    rs.out <- fixRaster(rs.inp)
    
    #write fixed raster
    if (!file.exists(paste(fileDir,"/a_fixed_res",sep=""))) {
      dir.create(paste(fileDir,"/a_fixed_res",sep=""))
    }
    rs.out <- writeRaster(rs.out,paste(fileDir,"/a_fixed_res/",inFile,sep=""),format='ascii')
    rm(rs.out); rm(rs.inp); g=gc(T)
  }
  return("done!")
}

fixData("D:/CIAT_work/TNC_global_biodiversity/summaries")
