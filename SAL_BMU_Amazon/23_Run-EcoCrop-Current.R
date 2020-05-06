
# CON EXTENSION ASC
#set variables
#nfs_base <- "/nfs" #"//dapadfs" "/mnt"
##nfs_base <- "//dapadfs" 

src.dir <- "D:/OneDrive - CGIAR/CIAT/Crops/EcoCrop-development/_scripts"

cropParamFile <- "D:/OneDrive - CGIAR/CIAT/Crops/EcoCrop-development/crop_parameters/crop-parameters-maize-bean-icc.csv"
cropDir <- "D:/OneDrive - CGIAR/CIAT/Crops/EcoCrop-development/outputs"
cDir <- "D:/OneDrive - CGIAR/CIAT/Climate & Geodata/cmip5_downscaled/gtm_wcl"
#sowDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/2000_sta_2.asc"
#harDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/end_date.asc"
#crop <- ""

source(paste(src.dir,"/src/EcoCrop-model_WCl.R",sep=""))


#######################################
#######          CURRENT         ######
#######################################

# Reading crop parameters from parameter file
cropPar <- read.csv(cropParamFile, header=T)
parNames <- paste(cropPar$X)
cropNames <- names(cropPar)[2:ncol(cropPar)]
cropPar <- as.data.frame(t(cropPar[,2:ncol(cropPar)]))
row.names(cropPar) <- 1:nrow(cropPar)
names(cropPar) <- parNames
cropPar <- cbind(Crop=cropNames, cropPar)

#number of crops
nTest <- nrow(cropPar) #Number of test into file crop parameters

#loop crops
for (n in 15:nTest){
  #n <- 1
  testName <- paste(cropPar$Crop[n])  #Name of the tests
  
  #Source scripts and libraries
  #stop("no")
  library(raster);library(maptools);library(rgdal);library(sp)
  
  if (!file.exists(paste(cropDir, "/" , testName, "/runs/", testName, "_suit.tif", sep=""))) {
    
    #Run principal function
    cat(paste("Processing : ",  testName, "\n", sep=""))
    
    eco <- suitCalc(climPath=cDir, 
                    Gmin=cropPar$Gmin[n], #Minimum lenght of the growing season 
                    Gmax=cropPar$Gmax[n], #Maximum lenght of the growing season
                    Tkmp=cropPar$Tkmp[n], #Killing temperature  
                    Tmin=cropPar$Tmin[n], #Minimum temperature
                    Topmin=cropPar$Topmin[n], #Minimum optimum temperature
                    Topmax=cropPar$Topmax[n], #Maximum optimum temperature
                    Tmax=cropPar$Tmax[n], #Maximum temperature
                    Rmin=cropPar$Rmin[n], #Minimum precipitation
                    Ropmin=cropPar$Ropmin[n], #Minimum optimum precipitation
                    Ropmax=cropPar$Ropmax[n], #Maximum optimum precipitation
                    Rmax=cropPar$Rmax[n], #Maximum precipitation
                    outfolder=paste(cropDir, "/", testName, "/runs", sep=""),
                    #sowDat=sowDat,
                    #harDat=harDat,
                    cropname=paste(testName, sep=""),
                    ext="",
                    cropClimate=F
                    )
    
  } else {
    cat(paste("Processed : ",  testName, "\n", sep=""))
  }
}