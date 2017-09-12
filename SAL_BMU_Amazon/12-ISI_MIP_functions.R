require(raster)
require(ncdf)
require(rgdal)
require(ncdf4)

# source("12-ISI_MIP_functions.R")

GCMCut_Raw <- function(baseDir="T:/gcm/cmip5/isi_mip", region=extent(-80, -66, -16, 5), outDir="Z:/DATA/WP2/03_Future_data/isi_mip_ft_0_5deg") {
  
  setwd(baseDir)
  
  gcmList <- list("gfdl-esm2m"="gfdl_esm2m", "hadgem2-es"="mohc_hadgem2_es", "ipsl-cm5a-lr"="ipsl_cm5a_lr", "noresm1-m"="ncc_noresm1_m")
  varList <- list("pr"="prec", "tasmax"="tmax", "tasmin"="tmin")
  rcpList <- list("hist"="historical", "rcp2p6"="rcp26","rcp4p5"="rcp45", "rcp6p0"="rcp60", "rcp8p5"="rcp85")

  for (r in 1:length(rcpList)){
    
    if (rcpList[[r]] == "historical"){
      period <- "1961_2005"
    } else {
      period <- "2006_2099"
    }
    
    for (g in 1:length(gcmList)){
    
      for (v in 1:length(varList)){
        
        oDir <- paste0(outDir, "/", rcpList[[r]], "/", gcmList[[g]])
        if (!file.exists(oDir)) {dir.create(oDir, recursive = TRUE)}
#         
#         if(!file.exists(paste0(oDir, "/", varList[[v]], "_", period, ".nc"))){
#           
#           ncList <- Sys.glob(paste0(baseDir, "/", names(varList[v]),"*", names(gcmList[g]), "*", names(rcpList[r]), "*.nc"))
#           
#           for (nc in ncList){
#             
#             cat(rcpList[[r]], gcmList[[g]], basename(nc), "\n")
#             
#             oNc3 <- paste0(substring(basename(nc), 1, nchar(basename(nc))-3), "_tmp.nc")
#             
#             if (!file.exists(paste0(oDir, "/", basename(nc)))) {
#               
#               ## Convert to ncdf3
#               system(paste("ncks -3 ", basename(nc), " ", oNc3, sep=""))  
#               
#               ## Cut region
#               system(paste("cdo -f nc2 sellonlatbox,",region@xmin,",",region@xmax,",",region@ymin,",",region@ymax," ", oNc3, " ", oDir, "/", basename(nc), sep=""))
#               unlink(oNc3)
#               
#             }
#             
#           }
#           
#           oncList <- Sys.glob(paste0(oDir, "/", names(varList[v]), "*.nc"))
#           
#           ## Merge by rcp and period
#           cat("Merging ", varList[[v]], period, "\n")
#           
#           system(paste("cdo -s -f nc mergetime ", paste(oncList, collapse=" "), " ", oDir, "/", varList[[v]], "_", period, "_tmp.nc", sep=""))
#           for (onc in oncList){unlink(onc)}
#           
#           if (varList[[v]] == "prec"){
#             system(paste("cdo mulc,86400 ", oDir, "/", varList[[v]], "_", period, "_tmp.nc", " ", oDir, "/", varList[[v]], "_", period, ".nc", sep=""))
#             system(paste("cdo monsum ", oDir, "/", varList[[v]], "_", period, ".nc", " ", oDir, "/", varList[[v]], "_", period, "_monthly.nc", sep=""))
#           } else {
#             system(paste("cdo subc,273.15 ", oDir, "/", varList[[v]], "_", period, "_tmp.nc", " ", oDir, "/", varList[[v]], "_", period, ".nc", sep=""))          
#             system(paste("cdo monavg ", oDir, "/", varList[[v]], "_", period, ".nc", " ", oDir, "/", varList[[v]], "_", period, "_monthly.nc", sep=""))
#           }
#         }
#         
      
        cat("Clim calcs ", varList[[v]], period, "\n")
        system(paste("cdo -ymonavg -selyear,1986/2005 ", oDir, "/", varList[[v]], "_", period, "_monthly.nc", " ", oDir, "/", varList[[v]], "_1986_2005_climatology.nc", sep=""))
        system(paste("cdo -ymonavg -selyear,1971/2000 ", oDir, "/", varList[[v]], "_", period, "_monthly.nc", " ", oDir, "/", varList[[v]], "_1971_2000_climatology.nc", sep=""))
        system(paste("cdo -ymonavg -selyear,1976/2005 ", oDir, "/", varList[[v]], "_", period, "_monthly.nc", " ", oDir, "/", varList[[v]], "_1976_2005_climatology.nc", sep=""))
        
#         unlink(paste0(oDir, "/", varList[[v]], "_", period, "_tmp.nc"))
        
        
        
      }
      
    }
    
  }
  
}



## Cut GCM raw
baseDir <- "T:/gcm/cmip5/isi_mip"
region <- extent(-80, -66, -16, 5)
outDir <- "Z:/DATA/WP2/03_Future_data/isi_mip_ft_0_5deg"
otp <- GCMCut_Raw(baseDir, region, outDir)
