#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#September 2012

########################################################
#wrapper function for parallel processing of the climatology calculation
########################################################
wrapper_climatology_rcp45 <- function(i) {
  #libraries
  library(raster); library(rgdal)
  
  #sourcing needed functions
  source(paste(src.dir2,"/scripts/CMIP5-functions.R",sep=""))
  source(paste(src.dir2,"/scripts/13.02.calcClimGCM_rcp45-functions.R",sep=""))
  
  #get list of GCMs, and GCM name given i
  gcmChars <- read.table(paste(src.dir2,"/data/CMIP5gcms_rcp45.tab",sep=""),sep="\t",header=T)
  gcmList <- unique(gcmChars$GCM)
  gcm <- gcmList[i]
  
  #i/o directory with GCM data
  gcmDir <- paste(mdDir,"/",gcm,sep="")
  
  #list and loop through ensembles
  ensList <- list.files(gcmDir,pattern="_monthly")
  for (ens in ensList) {
    cat("\nensemble",paste(ens),"\n")
    #ens <- ensList[1]
    #output dir
    oClim <- paste(gcmDir,"/",gsub("_monthly","",ens),"_climatology",sep="")
    if (!file.exists(oClim)) {dir.create(oClim)}
    
    #list of variables
    vnList <- c("pr","rd","tas","dtr","tasmax","tasmin")
    #loop through variables
    for (vn in vnList) {
      #vn <- vnList[1]
      cat("\nvariable:",vn,"\n")
      
      #input folder (ensemble level)
      ensDir <- paste(gcmDir, "/", ens, sep="")
      m_seq <- 1:12; m_seq[which(m_seq < 10)] <- paste("0",m_seq[which(m_seq < 10)],sep="")
      for (mth in m_seq) {
        #mth <- m_seq[1]
        cat(mth,". ",sep="")
        
        #if the resulting file does not exist then process else do nothing
        if (!file.exists(paste(oClim,"/",vn,"_",mth,".tif",sep=""))) {
          #make, check list of files, and remove any NAs
          mfList <- paste(ensDir,"/",yi:yf,"/",vn,"_",mth,".tif",sep="")
          mfList <- as.character(sapply(mfList,checkExists)) #check existence of files
          mfList <- mfList[which(!is.na(mfList))]
          
          #if all files are NA, meaning there is nothing, don't calculate anything
          if (length(mfList) > 0) {
            #load stack of files
            #rstk <- stack(mfList)
            
            #here need to check the mean of these rasters and then load the raster stack
            rstk <- sapply(mfList,checkMaxMin,vn=vn)
            rstk <- stack(rstk)
            
            #calculate mean
            rs <- calc(rstk,fun = function(x) {mean(x,na.rm=T)})
            #write resulting raster
            rs <- writeRaster(rs,paste(oClim,"/",vn,"_",mth,".tif",sep=""),format="GTiff")
          }
        }
      }
    }
  }
}

