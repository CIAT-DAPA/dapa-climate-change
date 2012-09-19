#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Sept 2012

#load packages
library(raster)

#load functions
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
#src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0008-CMIP5"
src.dir3 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#sourcing needed functions
source(paste(src.dir,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))

#other details
crop_name <- "gnut"
yi <- 2020
yf <- 2049

#input directories
#model_dir <- "V:/eejarv"
#base_dir <- "W:/eejarv/PhD-work/crop-modelling"
model_dir <- "/nfs/a102/eejarv"
base_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT"
gcm_dir <- paste(model_dir,"/CMIP5/rcp45",sep="")
clim_dir <- paste(base_dir,"/climate-data/gridcell-data",sep="")
out_dir <- paste(clim_dir,"/IND_RCP45",sep="")

#load gridcells that i will extract data for
cells <- read.csv(paste(base_dir,"/GLAM/climate-signals-yield/",toupper(crop_name),"/signals/cells-process.csv",sep=""))

#load GCM characteristics
gcm_chars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
gcm_list <- unique(gcm_chars$GCM)
gcm_list <- gcm_list[-7] #temporary because ichec_ec_earth does not have rsds

#load the mask of india
drs <- raster(paste(src.dir2,"/data/mask.tif",sep=""))
drs[which(!is.na(drs[]))] <- 1


#here is when the GCM is specified. Either a function or a loop
#get the gcm stuff
#i=1
gcm <- gcm_list[i]

#output GCM dir
odir_gcm <- paste(out_dir,"/",gcm,sep="")
if (!file.exists(odir_gcm)) {dir.create(odir_gcm,recursive=T)}

#reduce characteristics list for this GCM
this_gcm <- gcm_chars[which(gcm_chars$GCM == gcm),]
ens_list <- unique(this_gcm$Ensemble)

#loop through ensembles
for (ens in ens_list) {
  #ens <- ens_list[1]
  cat("\nprocessing ensemble",paste(ens),"of model",paste(gcm),"\n")
  this_ens <- this_gcm[which(this_gcm$Ensemble == ens),]
  
  #create directory of ensemble
  odir_ens <- paste(odir_gcm,"/",ens,sep="")
  if (!file.exists(odir_ens)) {dir.create(odir_ens)}
  
  #list variables
  vn_list <- c("pr","tasmin","tasmax","rsds")
  
  #loop through variables
  for (vn in vn_list) {
    #vn <- vn_list[1]
    cat("variable:",vn,"\n")
    
    #output variable directory
    odir_var <- paste(odir_ens,"/",vn,sep="")
    if (!file.exists(odir_var)) {dir.create(odir_var)}
    
    flist <- list.files(odir_var,pattern="\\.csv")
    if (length(flist) != nrow(cells)) {
      #loop through years
      yrc <- 1
      for (year in yi:yf) {
        #year <- 2020
        cat("\nyear:",year,"\n")
        
        #input directory
        yr_dir <- paste(gcm_dir,"/",gcm,"/",ens,"/",vn,"_",year,sep="")
        #list of nc files in the year folder (could be monthly e.g. srad, or daily)
        nc_list <- list.files(yr_dir,pattern="\\.nc")
        
        ####!!!!!!!!!!
        ####!!!!!!!!!! here do the thing with GFDL
        ####!!!!!!!!!!
        
        #check if there is data for that year
        if (!file.exists(yrDir) | length(ncList) == 0) {
          #there is either no data or no folder for that year and variable, 
          #so need to generate dummy monthly files with all values being NA
          #this would be done only if the object does not exist (i.e. if it was not
          #created by a previous year)
          if (!exists("odf_all")) {
            odf_all <- list()
            for (cell in cells$CELL) {
              #create a matrix where to put all data, in the form that i need
              odf_all[[paste("c",cell,sep="")]] <- as.data.frame(matrix(NA,nrow=length(yi:yf),ncol=13))
              names(odf_all[[paste("c",cell,sep="")]]) <- c("YEAR",paste("MONTH",1:12,sep=""))
            }
          }
          
          #put the year to a value to the cell values
          for (cell in cells$CELL) {
            odf_all[[paste("c",cell,sep="")]][yrc,"YEAR"] <- year
          }
          
        }
        
      }
    }
    
    
  }
  
  
  
}


#load the GCM data


#resample GCM data


#put it into format, one file per gridcell "cell-153.csv"
#YEAR,1,2,3,4,5,6,7


#beware of
#    additional "2" in GFDL files (012 instead of 01 in month, and *.nc2 instead of *.nc)
#    some rsds data are monthly
#    dont modify calendar type, but convert units
#    missing years in miroc4, need to treat them accordingly


#write files into folder






