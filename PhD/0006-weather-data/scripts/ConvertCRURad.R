#Julian Ramirez-Villegas
#March 2012
#UoL / CCAFS / CIAT

wd <- "F:/PhD-work/crop-modelling/climate-data/CRU_CL_v1-1_data"

fName <- "crad6190.dat"

oDir <- paste(wd,"/srad",sep="")
if (!file.exists(oDir)) {dir.create(oDir)}

#get grid information
hd <- read.fortran(paste(wd,"/",fName,sep=""),format=c("5F10","3I10"),skip=1,n=1)
names(hd) <- c("grd_sz","xmin","ymin","xmax","ymax","n_cols","n_rows","n_months")

for (mth in 1:12) {
  cat("Month",mth,"\n")
  #get reading details
  lines_skip <- (mth-1)*hd$n_rows + 2
  
  #read data from text file
  srad <- read.fortran(paste(wd,"/",fName,sep=""),format=paste(hd$n_cols,"I",5,sep=""),skip=lines_skip,n=hd$n_rows)
  
  #create raster
  rs <- raster(as.matrix(srad),xmn=0,xmx=360,ymn=-90,ymx=90)
  rs[which(rs[]==-9999)] <- NA
  rs <- rotate(rs)
  #plot(rs)
  writeRaster(rs,paste(oDir,"/srad_",mth,".asc",sep=""),format="ascii")
  rm(rs); g=gc()
}
