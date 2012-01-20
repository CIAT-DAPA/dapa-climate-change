#Julian Ramirez-Villegas
#January 2012
#CIAT / CCAFS / UoL
stop("error")

library(raster)

bd <- "E:/PhD-work/crop-modelling/EcoCrop"

#Looping through the different crops
crops <- c("barl","bean","cass","chkp","cowp","gnut","maiz","milf","milp","pota","rice","whea","yams")
for (cn in crops) {
  readWriteLandrace(bd,cn)
}


##################################################################################################
#Function to read a file named $CROP.tab and then use the mask to select and finally plot the data
readWriteLandrace <- function(bDir,cropName) {
  wd <- paste(bDir,"/models/EcoCrop-",toupper(cropName),"/analyses/data",sep="")
  setwd(wd)
  inData <- read.delim(paste(cropName,".tab",sep=""),sep="\t")
  write.csv(inData,paste(cropName,".csv",sep=""),quote=T,row.names=F)
  
  #load mask
  msk <- raster(paste(bDir,"/analysis-mask/mask.asc",sep=""))
  
  #get only data that is inside mask
  inData$AFASIA <- extract(msk,cbind(X=as.numeric(inData$LONGITUDED),Y=as.numeric(inData$LATITUDED)))
  afasiaData <- inData[which(inData$AFASIA==1),]
  write.csv(afasiaData,paste(cropName,"-afasia.csv",sep=""),quote=T,row.names=F)
  
  #plot figure of data in mask and countries
  library(maptools); data(wrld_simpl)
  tiff(paste(cropName,".tif",sep=""),res=150,height=1200,width=1500,compression="lzw")
  plot(msk,legend=F); plot(wrld_simpl,add=T)
  points(inData$LONGITUDED,inData$LATITUDED,pch=20,col='black',cex=0.7)
  points(afasiaData$LONGITUDED,afasiaData$LATITUDED,pch=20,col='red',cex=0.7)
  grid()
  dev.off()
  
  return("Done!")
}
