#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#August 2012

#Create daily weather data in the format of WTH files for CMIP5 baseline runs
#this script will first check if the GCM data are valid for the years that will be
#simulated. Similarly, this

#load packages
library(raster)

#load functions
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"

#sourcing needed functions
source(paste(src.dir,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
source(paste(src.dir2,"/scripts/11.process_rcp45-functions.R",sep=""))

#input directories
base_dir <- "/nfs/a102/eejarv"
gcm_dir <- paste(base_dir,"/CMIP5/rcp45",sep="")

#output directories
out_dir <- paste(base_dir,"/CMIP5/rcp45",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir,recursive=T)}

#load GCM characteristics
gcm_chars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
gcm_list <- unique(gcm_chars$GCM)

#processsing list of GCMs
for (i in 1:length(gcm_list)) {
  gcm <- gcm_list[i]
  
  gcm_odir <- paste(out_dir,"/",gcm,sep="")
  if (!file.exists(gcm_odir)) {dir.create(gcm_odir)}
  
  #reduce characteristics list for this GCM
  this_gcm <- gcm_chars[which(gcm_chars$GCM == gcm),]
  ens_list <- unique(this_gcm$Ensemble)
  
  for (ens in ens_list) {
    #ens <- ensList[1]
    cat("Processing ensemble",paste(ens),"\n")
    this_ens <- this_gcm[which(this_gcm$Ensemble == ens),]
    
    #create directory of ensemble
    ens_odir <- paste(gcm_odir,"/",ens,sep="")
    if (!file.exists(ens_odir)) {dir.create(ens_odir)}
    
    #list of variables depends on number of nc files (i.e. tas is not always available)
    patn <- gsub("%var%","",this_ens$naming[1])
    ncf <- list.files(ens_odir,pattern=patn)
    srn <- unique(thisEns$srad_naming)
    ncf <- ncf[which(!ncf %in% paste(srn))]
    if (length(list.files(outEnsDir,pattern="\\.control")) != 4) {
      if (length(list.files(outEnsDir,pattern="\\.control")) != 3) {
        if (length(ncf) == 4) {
          vnList <- c("pr","tasmin","tasmax","tas")
        } else if (length(ncf) == 3) {
          vnList <- c("pr","tasmin","tasmax")
        } else {
          stop("number of files not 3 or 4, check!")
        }
      }
    }
    
  }
  
  
}


