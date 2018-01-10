
# CON EXTENSION ASC
#set variables
#nfs_base <- "/nfs" #"//dapadfs" "/mnt"
##nfs_base <- "//dapadfs" 

src.dir <- "Z:/WORK_PACKAGES/WP2/00_scripts"
cropParamFile <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/crop-parameters/crop-parameters.csv"
cropDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/outputs"
cDir <- "Z:/WORK_PACKAGES/WP2/03_Future_data/downscaling_bsl_2_5min"
rcpLs <- c("rcp85")

#, "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069", "2070_2099")
#sowDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/2000_sta_2.asc"
#harDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/end_date.asc"
#crop <- ""

source(paste(src.dir,"/src-ecocrop/EcoCrop-model_WCl.R",sep=""))

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

#loop periods and rcps

for (rcp in rcpLs){

  
  gcmList <- list.dirs(paste0(cDir, "/", rcp), recursive = FALSE, full.names = FALSE)
  
  for (gcm in gcmList){
    
    for (period in periodLs){
      
      #loop crops
      for (n in 15:nTest){
        #n <- 1
        testName <- paste(cropPar$Crop[n])  #Name of the tests
        
        #Source scripts and libraries
        #stop("no")
        library(raster);library(maptools);library(rgdal);library(sp)
        
        if (!file.exists(paste(cropDir, "/", testName, "/runs-", rcp, "/", gcm , "/", period, "/", testName, "_suit.tif", sep=""))) {
          
          #Run principal function
          cat("Processing : ",  testName, rcp, gcm, "\n")
          
          eco <- suitCalc(climPath=paste0(cDir, "/", rcp, "/", gcm, "/", period), 
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
                          outfolder=paste(cropDir, "/", testName, "/runs-", rcp, "/", gcm, "/", period, sep=""),
                          #sowDat=sowDat,
                          #harDat=harDat,
                          cropname=paste(testName, sep=""),
                          ext=".tif",
                          cropClimate=F
          )
          
        } else {
          cat(paste("Processed : ",  testName, "\n", sep=""))
        }
      }  
    }  
    
  }
  
 
}

