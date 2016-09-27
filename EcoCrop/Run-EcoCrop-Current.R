#set variables
#nfs_base <- "/nfs" #"//dapadfs" "/mnt"
nfs_base <- "//dapadfs" 

src.dir <- paste(nfs_base, "/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/_scripts", sep="")
cropParamFile <- paste(nfs_base, "/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/modelling/vulnerability-analysis/_model_data/parameters/calibration_param_allCrops.csv", sep="")
cropDir <- paste(nfs_base, "/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/modelling/_calibration", sep="")
cDir <- paste(nfs_base, "/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/WCl_world_10min_new", sep="")
#sowDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/2000_sta_2.asc"
#harDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/end_date.asc"
#crop <- ""


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
for (n in 1:nTest){
  #n <- 1
  testName <- paste(cropPar$Crop[n])  #Name of the tests
  
  #Source scripts and libraries
  #stop("no")
  library(raster);library(maptools);library(rgdal);library(sp)
  source(paste(src.dir,"/src/EcoCrop-model_WCl.R",sep=""))
  
  
  if (!file.exists(paste(cropDir, "/analyses/runs/",testName, "_suitability.tif", sep=""))) {
    
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
                    outfolder=paste(cropDir, "/", testName,"/analyses/runs", sep=""),
                    #sowDat=sowDat,
                    #harDat=harDat,
                    cropname=paste(testName, sep=""))
    
  } else {
    cat(paste("Processed : ",  testName, "\n", sep=""))
  }
}

