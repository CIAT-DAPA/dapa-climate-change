## 4- Resample GCM Daily
GCMDailyResample <- function(rcp="historical", tsLs="1981_2005", bbox=c(-78.55,-73.05,-10.9,2.05), reg_suf="amz", resolution=0.05, i=1, gcmDir="T:/gcm/cmip5/raw/daily", dirbase="W:/bid-cc-agricultural-sector/01-climate-data", dirout="D:/cenavarro/Request_oovalle"){
  
  require(raster)
  require(ncdf)
  require(rgdal)
  
  for (rcp in rcpLs){
    for (ts in tsLs){
      gcmList <- c("bcc_csm1_1","bcc_csm1_1_m","csiro_mk3_6_0","gfdl_cm3", "ipsl_cm5a_lr","miroc_esm", "miroc_esm_chem","miroc_miroc5","mohc_hadgem2_es","ncc_noresm1_m", "mri_cgcm3", "nimr_hadgem2_ao")
      
      gcm <- gcmList[i]
      
      #   dirraw <- paste0(dirbase, "/gcm_raw_res")
      #   dirres <- paste0(dirout, "/gcm_res_", reg_suf)
      
      bbox <- extent(bbox)
      
      varlist <- c("pr", "rsds", "tasmax", "tasmin")
      #   varlist <- c()  
      mthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
      
      metList <- c("avg", "std")
      
      # Get a list of month with and withour 0 in one digit numbers
      mthListMod <- c(1:12)
      ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
      mthMat <- as.data.frame(cbind(mthList, mthListMod, ndays))
      names(mthMat) <- c("Mth", "MthMod", "Ndays")
      
      yi <- substr(ts, 1, 4)
      yf <- substr(ts, 6, 9)
      
      # Create output directories
      if (rcp == "historical"){
        dirraw <- paste(dirbase, "/gcm_raw_res", "/", basename(gcm), "/", ts, sep="") 
        dirres <- paste(dirout, "/gcm_res_0_25_", reg_suf, "/", basename(gcm), "/", ts, sep="")
      } else {
        dirraw <- paste(dirbase, "/gcm_raw_res", "/", basename(gcm), "/", ts, "/", rcp, sep="")
        dirres <- paste(dirout, "/gcm_res_0_25_", reg_suf, "/", basename(gcm), "/", ts, "/", rcp, sep="")
      }
      
      
      if (!file.exists(paste(dirraw, "/by-month", sep=""))) {dir.create(paste(dirraw, "/by-month", sep=""), recursive=T)}
      if (!file.exists(paste(dirres, "/by-month", sep=""))) {dir.create(paste(dirres, "/by-month", sep=""), recursive=T)}
      
      
      ####### Process GCM #######
      
      cat(" Cutting : ", rcp, basename(gcm), " \n")
      var 
      ## Select dates and cut region
      for (var in varlist){
        
        if (!file.exists(paste(dirraw, "/", var, "_", ts, "_day_", reg_suf, ".nc", sep=""))) {
          
          ncList <- list.files(path=paste(gcmDir, "/", rcp, "/", basename(gcm), "/r1i1p1", sep=""), pattern=paste(var, "_day*", sep=""), full.names=TRUE)
          
          if (!file.exists(paste(dirraw, "/", var, "_", ts, "_day.nc", sep=""))) {
            system(paste("cdo seldate,", yi, "-01-01,", yf, "-12-31 ", ncList[1], " ", dirraw, "/", var, "_", ts, "_day.nc", sep=""))
          }
          
          system(paste("cdo sellonlatbox,",bbox@xmin+360-10,",",bbox@xmax+360+10,",",bbox@ymin-10,",",bbox@ymax+10," ", dirraw, "/", var, "_", ts, "_day.nc ", dirraw, "/", var, "_", ts, "_day_", reg_suf, ".nc",sep=""))
        }
        
        
        ## Split in years and then in months
        if (!file.exists(paste(dirraw, "/by-month/", var, "_", yf, "_12.nc", sep=""))) {
          
          system(paste("cdo splityear ", dirraw, "/", var, "_", ts, "_day_", reg_suf, ".nc ", dirraw, "/by-month/", var, "_", sep=""))
          
          for (yr in yi:yf){
            
            if (gcm == "gfdl_cm3"){
              
              system(paste("cdo -v -f nc -copy ", dirraw, "/by-month/", var, "_", yr, ".nc2 ", dirraw, "/by-month/", var, "_", yr, ".nc", sep=""))
              file.remove(paste(dirraw, "/by-month/", var, "_", yr, ".nc2", sep=""))  
            }
            
            system(paste("cdo splitmon ", dirraw, "/by-month/", var, "_", yr, ".nc ", dirraw, "/by-month/", var, "_", yr, "_", sep=""))
            file.remove(paste(dirraw, "/by-month/", var, "_", yr, ".nc", sep=""))
            
          }
          
        }
      }
      
      
      
      # Resample GCMs
      
      for (var in varlist){
        
        if (var == "tasmax"){varmod <- "tmax"}
        if (var == "tasmin"){varmod <- "tmin"}
        if (var == "pr"){varmod <- "prec"}
        if (var == "rsds"){varmod <- "rsds"}
        
        for (mth in mthList) {
          
          mthMod <- as.numeric(paste((mthMat$MthMod[which(mthMat$Mth == mth)])))
          ndayMth <- as.numeric(paste((mthMat$Ndays[which(mthMat$Mth == mth)])))
          
          if (!file.exists(paste(dirres, "/", varmod, "_", ts, "_", mth, "_std.nc", sep=""))) {
            
            for (yr in yi:yf){
              
              cat(" Resample daily: ", rcp, varmod, "_", yr, " ", mth, "\n")
              
              if (!file.exists(paste(dirres, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""))) {
                
                rs <- stack(paste(dirraw, "/by-month/", var, "_", yr, "_", mth, ".nc", sep=""))
                dayNcStackRes <- resample(rs, raster(nrows=nrow(raster(bbox, res=resolution)), ncols=ncol(raster(bbox, res=resolution)), xmn=bbox@xmin+360, xmx=bbox@xmax+360, ymn=bbox@ymin, ymx=bbox@ymax), method='bilinear')
                
                xmin(dayNcStackRes) <- xmin(dayNcStackRes)-360
                xmax(dayNcStackRes) <- xmax(dayNcStackRes)-360
                
                if (varmod == "tmax"){dayNcStackRes <- dayNcStackRes - 273.15}
                if (varmod == "tmin"){dayNcStackRes <- dayNcStackRes - 273.15}
                if (varmod == "prec"){dayNcStackRes <- dayNcStackRes * 86400}
                
                dayNcStackRes <- writeRaster(dayNcStackRes, paste(dirres, "/by-month/", varmod, "_", yr, "_", mth, ".nc", sep=""), format="CDF", overwrite=T)
                
              }
              
              cat(" Calculating avg and std daily: historical ", basename(gcm), " ", varmod, "_", yr, " ", mth, "\n")
              system(paste("cdo -s dayavg ", dirres, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  dirres, "/by-month/", varmod, "_", yr, "_", mth, "_avg.nc", sep=""))
              system(paste("cdo -s daystd ", dirres, "/by-month/", varmod, "_", yr, "_", mth, ".nc", " ",  dirres, "/by-month/", varmod, "_", yr, "_", mth, "_std.nc", sep=""))
              
            }
            
            if (!file.exists(paste(dirres, "/", varmod, "_", ts, "_", mth, "_avg.nc", sep=""))) {
              avgNcList <- paste(dirres, "/by-month/", varmod, "_", yi:yf, "_", mth, "_avg.nc", sep="")
              system(paste0("cdo -s ensavg ", paste(avgNcList, collapse=" "), " ", dirres, "/", varmod, "_", ts, "_", mth, "_avg.nc"))
              
            }
            
            if (!file.exists(paste(dirres, "/", varmod, "_", ts, "_", mth, "_std.nc", sep=""))) {
              stdNcList <- paste(dirres, "/by-month/", varmod, "_", yi:yf, "_", mth, "_std.nc", sep="")
              stdNcStack <- mean(stack(stdNcList))
              stdNcStack <- writeRaster(stdNcStack, paste(dirres, "/", varmod, "_", ts, "_", mth, "_std.nc", sep=""), format="CDF", overwrite=T)
            }
            
            for (nc in avgNcList){
              file.remove(paste(nc))
            }
            
            for (nc in stdNcList){
              file.remove(paste(nc))
            }
            
          }      
        }
      }  
    }
    
  }
  
  
}

### GCMDailyResample
sfStop()

## Parameters ###
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

require(raster); require(ncdf); require(rgdal)

stop("error")

rcpLs=c("historical")
tsLs=c("1981_2005")

# 1981_2005 2020_2049 2040_2069 2070_2099
# rcp26 rcp45 rcp60 rcp85
bbox=c(-78.55,-73.05,-10.95,2.05)
reg_suf="amz"
resolution=0.25
gcmDir="T:/gcm/cmip5/raw/daily"
# dirbase="W:/bid-cc-agricultural-sector/01-climate-data"
dirbase="Z:/DATA/WP2/06_Clustering_analyses/data"
dirout="Z:/DATA/WP2/06_Clustering_analyses/data"

# i <- 4
# gcm <- gcmList[i]
# GCMDailyResample(rcp, ts, bbox, reg_suf, resolution, i, gcmDir, dirbase, dirout)


# Export functions
sfExport("GCMDailyResample")

#export variables
sfExport("rcpLs")
sfExport("tsLs")
sfExport("bbox")
sfExport("reg_suf")
sfExport("resolution")
sfExport("gcmDir")
sfExport("dirbase")
sfExport("dirout")
# sfExport("gcmList")

# gcmList <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "cnrm_cm5", "csiro_mk3_6_0", "gfld_esm2g", "gfld_esm2m", "inm_cm4", "ipsl_cm5a_lr", "ipsl_cm5a_mr", "ipsl_cm5b_lr", "miroc_esm", "miroc_esm_chem", "miroc_miroc5", "mohc_hadgem2_cc", "mohc_hadgem2_es", "mpi_esm_lr", "mpi_esm_mr", "mri_cgcm3", "ncc_noresm1_m")

# for (i in 1:length(gcmList)){
  
# gcm <- paste(as.matrix(gcmList[i]))
# gcm <- gcmList[i]
# sfExport("gcm")    

control <- function(i) { #define a new function
  
  require(raster)
  require(ncdf)
  require(rgdal)
  
  cat(" .> ", paste("\t ", i, sep=""), "\t processing!\n")
  GCMDailyResample(rcpLs, tsLs, bbox, reg_suf, resolution, i, gcmDir, dirbase, dirout)
  
}

system.time(sfSapply(as.vector(1:12), control))

# }


#stop the cluster calculation
sfStop()

cat("GCM Resampling done!")
