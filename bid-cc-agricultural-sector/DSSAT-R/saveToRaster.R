
  outputPath<-"/home/jeisonmesa"
  cropName<-"Rice"
  riegoType<-"riego"
  climatDataName<-"WFD"
  startYear=1971
  endYear=1999
  periodeRun<-paste(startYear,endYear,sep="-")


saveToRaster<-function(cropData,runData,outputIndex,year){
  config()
  coord <-cropData[, c("x", "y")][1:length(runData),]
  data<-1:length(runData)
  for(x in 1:length(Run)){data[x]<-Run[[x]][year,outputIndex]}
  r<-createRaster(coord,data)
  saveRaster(r,outputPath,cropName,periodeRun,climatDataName,riegoType,startYear+year-1,colnames(runData[[1]])[outputIndex])
}

createRaster <-function(coord,data){
  library(raster)
  library(ncdf)
  basRs <- raster(nrows=178, ncols=180, xmn=-120, xmx=-30, ymn=-56, ymx=33) # This raster has the same configuration that temperature raster
  out <- rasterize(coord, basRs, field=data, fun='last')
  return(out)
}

saveRaster <-function(toSave,path,crop,periode,climateData,riego,year,output){
  dir<-paste(path,crop,periode,sep="/")
  if(!file.exists(dir)) { 
    dir.create(dir, showWarnings = TRUE, recursive = TRUE, mode = "7777")
  }
  name<-paste("_",climateData,riego,year,output,sep="_")
  name<-paste(name,".asc")
  finalPath<-paste(dir,name,sep="/")
  writeRaster(toSave, filename=finalPath, datatype='FLT8S', overwrite=TRUE)
}