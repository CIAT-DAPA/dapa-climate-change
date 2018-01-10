### SPlit in years-months AgMerra datasets
## CArlos Navarro 
## Jan 2017


### SPLIT TIME AGMERRA

iDir <- '/mnt/data_cluster_5/cropdata/agmerra/daily/nc-files' 
# iDir <- 'U:/cropdata/agmerra/daily/nc-files'
# oDir <- "/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses/data/agmerra_raw"
oDir <- '/mnt/data_cluster_5/cropdata/agmerra/daily/nc-files/by-year'
ncLs <- list.files(iDir, pattern='*.nc$')
bbox <- extent(raster("/mnt/Workspace_cluster_9/Ecosystem_Services/SAL-project/DATA/WP2/06_Clustering_analyses/data/amz_regions_rst/amz_base.tif"))

for (nc in ncLs){
  
  var <- strsplit(nc, "_")[[1]][1]
  
  if (var %in% c("srad", "prec", "tmax", "tmin")){
    
    if (var == "srad"){
      varmod <- "dswrf"
    } else {
      varmod <- var
    }
    
    if (!file.exists(paste0(iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz.nc"))){
      system(paste("cdo sellonlatbox,",bbox@xmin+360-5,",",bbox@xmax+360+5,",",bbox@ymin-5,",",bbox@ymax+5," ", iDir, "/", nc, " ", iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz_tmp.nc",sep=""))
      
#       ncStk <- stack(paste0(iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz_tmp.nc"))
#       xmin(ncStk) <- xmin(ncStk)-360
#       xmax(ncStk) <- xmax(ncStk)-360
#       
#       writeRaster(ncStk, paste0(iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz.nc"))
#       file.remove(paste0(iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz_tmp.nc"))
      
      
      ncStk <- raster(paste0(iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz_tmp.nc"))[[1]]
      xmin(ncStk) <- xmin(ncStk)-360
      xmax(ncStk) <- xmax(ncStk)-360
      writeRaster(ncStk, paste0(iDir, "/base_amz.nc"), overwrite=T)
      
      system(paste0("cdo -f nc -setgrid,", iDir, "/base_amz.nc ", iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz_tmp.nc", " ", iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz.nc",sep=""))
      file.remove(paste0(iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz_tmp.nc"))
      file.remove(paste0(iDir, "/base_amz.nc"))
      
    }
    
    if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
    
    if (!file.exists(paste0(oDir, "/", varmod, "_2010.nc"))){
      system(paste("cdo splityear ", iDir, "/", strsplit(nc, "\\.")[[1]][1], "_amz.nc", " ", oDir, "/", varmod, "_", sep=""))  
    }
  }

}


    #     ncLsY <- list.files(oDir, pattern='*.nc$')
    ncLsY <- paste0(varmod, "_", 1980:2010, ".nc")
    
    for (ncY in ncLsY){
      
      year <- strsplit(strsplit(ncY, "_")[[1]][2], "\\.")[[1]][1]
      
      oDirY <- paste0(oDir, "/", year)
      if (!file.exists(oDirY)) {dir.create(oDirY, recursive=T)}
      
      if (!file.exists(paste0(oDirY, "/", varmod, "_2010_12.nc"))){
        system(paste("cdo splitmon ", oDir, "/", ncY, " ", oDirY, "/", varmod, "_", year, "_", sep=""))
        file.remove(paste0(oDir, "/", ncY))
      }
      
      
      if (!file.exists(paste0(oDirY, "/", varmod, "_12_31.nc"))){
        
        ncLsM <- paste0(varmod, "_", year, "_", c(paste0("0", 1:9), 10:12), ".nc")
        
        for (ncM in ncLsM){
          
          month <- strsplit(strsplit(ncM, "_")[[1]][3], "\\.")[[1]][1]
          
          system(paste("cdo splitday ", oDirY, "/", ncM, " ", oDirY, "/", varmod, "_", month, "_", sep=""))
          file.remove(paste0(oDirY, "/", ncM))
          
        }
        
      }
      
    }
    
  }
  
}
