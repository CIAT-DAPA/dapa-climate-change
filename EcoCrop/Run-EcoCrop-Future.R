# Set variables
src.dir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/_scripts"
cropParamFile <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_crop-parameters/FINAL_WCLIM.csv"
cropDir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/Additional/Cooper/Future_Suit/Fin/rcp_60"
#cDir <- "//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/climate/world_10min/_asciis"
#sowDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/2000_sta_2.asc"
#harDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/end_date.asc"
crop <- "crop"
fDir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/Wclim_Africa_cmip5_10min/rcp_60/Global_10min"


###LINUX directories
#src.dir <- "/home/urippke/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/_scripts"
#cropParamFile <- "/home/urippke/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/Uli_crop-parameters/FINAL_WCLIM.csv"
#cropDir <- "/home/urippke/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/Additional/Cooper/Future_Suit/Fin/rcp_60"
#cDir <- "/home/urippke/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/climate/world_10min/_asciis"
#sowDat="/home/urippke/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/2000_sta_2.asc"
#harDat="/home/urippke/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/end_date.asc"
#crop <- "crop"
#fDir <- "/home/urippke/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/Wclim_Africa_cmip5_10min/rcp_60/Global_10min"


########################################
########          FUTURE         #######
########################################

#Source scripts and libraries
stop("no")
library(raster);library(maptools);library(rgdal);library(sp)
source(paste(src.dir,"/src/EcoCrop-model_WCl.R",sep=""))
source(paste(src.dir,"/src/impacts.R",sep=""))
source(paste(src.dir,"/src/uncertainty.R",sep=""))


#Global climate models 
gls <- list.files("//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/Wclim_Africa_cmip5_10min/rcp_60/Global_10min") 
#Linux
#gls <- list.files("/home/urippke/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/climate/Wclim_Africa_cmip5_10min/rcp_60/Global_10min") 


#gls <- c("bccr_bcm2_0","cccma_cgcm3_1_t47","cccma_cgcm3_1_t63","cnrm_cm3","csiro_mk3_0",
         "csiro_mk3_5","gfdl_cm2_0","gfdl_cm2_1","giss_aom","giss_model_eh","giss_model_er","iap_fgoals1_0_g",
         "ingv_echam4","inm_cm3_0","ipsl_cm4","miroc3_2_hires","miroc3_2_medres","miub_echo_g",
         "mpi_echam5","mri_cgcm2_3_2a","ncar_ccsm3_0","ncar_pcm1","ukmo_hadcm3","ukmo_hadgem1")

# Reading crop parameters from parameter file
cropPar <- read.csv(cropParamFile, header=T)
nTest <- ncol(cropPar) #Number of test into file crop parameters



#############  Projection onto future  ##################
#Run principal function for each GCM
cat("Calculate Suitability projected onto future\n")

#########################################################
############ Loop through the tests #####################
for (n in 2:nTest){
  testName <- names(cropPar)[n] #Name of the last test
  cat(paste("Processing : ", crop, " ", testName, "\n", sep=""))

#Loop aroudn GCMs
for (gcm in gls) {
  
  rDir <- paste(cropDir, "/analyses/runs-future/", gcm, sep="") #Output folder for each GCM
  aDir <- paste(fDir, "/", gcm, "/r1i1p1/2040_2069/_asciis", sep="")  #Folder of future climate data

  
  
  if (!file.exists(paste(rDir, "/", crop, "_", testName, "_suitability.asc", sep=""))) {
    
    cat("\tFutSuitCalc ", gcm, "\n")
    
    #Run the function
    fut <- suitCalc(climPath = aDir, 
                    Gmin=cropPar[1,n],
                    Gmax=cropPar[2,n],
                    Tkmp=cropPar[3,n],
                    Tmin=cropPar[4,n],
                    Topmin=cropPar[5,n],
                    Topmax=cropPar[6,n],
                    Tmax=cropPar[7,n],
                    Rmin=cropPar[8,n],
                    Ropmin=cropPar[9,n],
                    Ropmax=cropPar[10,n],
                    Rmax=cropPar[11,n],
                    outfolder = rDir, 
                    #sowDat="NA",
                    #harDat="NA",
                    cropname = paste(crop, "_", testName, sep=""))
    
  } else { cat("\tFutSuitCalc", crop, " ", testName, "\t", gcm, "\tdone\n")}      
 }			
}