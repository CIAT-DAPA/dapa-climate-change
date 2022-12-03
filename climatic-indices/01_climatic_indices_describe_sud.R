## Describe files

## Libraries
require(raster)
require(maptools)
require(rgdal)
require(netCDF)
require(ncdf4)
require(reshape2)
require(sp)
require(dplyr)
require(lubridate)
require(rasterVis)
require(RColorBrewer)
require(rgeos)
require(grid)

oIDir <- "D:/Workspace/yapu-gtm/indices_v2.4/historical"
enosCond <- c("elnino", "lanina", "normal")
vars <- c("cdd","drd", "fld", "frd")
vars <- c("p95") 

ctrName <- "gtm" 

desc <- data.frame()

for (var in vars){

  for (m in 1:12){
    
    for (enos in enosCond){
    
      #Shapefile magnitude

      
      shp_mag <- readOGR(paste0(oIDir, "/", var, "/", var, "_", ctrName, "_", m, "_", enos, "_mag.shp"), 
                         layer=paste0(var, "_", ctrName, "_", m, "_", enos, "_mag") )
      
      desc <- rbind(desc, cbind(paste0(var, "_", ctrName, "_", m, "_", enos, "_mag.shp"), 
                                round(xmin(shp_mag),6), round(xmax(shp_mag),6), round(ymin(shp_mag),6), round(ymax(shp_mag),6), nrow(shp_mag), "NA"))
      
      tif_mag <- raster(paste0(oIDir, "/", var, "/", var, "_", ctrName, "_", m, "_", enos, "_mag.tif"))
      
      desc <- rbind(desc, cbind(paste0(var, "_", ctrName, "_", m, "_", enos, "_mag.tif"), 
                                round(xmin(tif_mag),6), round(xmax(tif_mag),6), round(ymin(tif_mag),6), round(ymax(tif_mag),6), nrow(tif_mag), ncol(tif_mag)))
      
      
      if (var != "fld"){
        
        shp_val <- readOGR(paste0(oIDir, "/", var, "/", var, "_", ctrName, "_", m, "_", enos, ".shp"), 
                           layer= paste0(var, "_", ctrName, "_", m, "_", enos) )
        
        desc <- rbind(desc, cbind(paste0(var, "_", ctrName, "_", m, "_", enos, ".shp"), 
                                  round(xmin(shp_val),6), round(xmax(shp_val),6), round(ymin(shp_val),6), round(ymax(shp_val),6), nrow(shp_val), "NA"))
        
        
        tif_val <- raster(paste0(oIDir, "/", var, "/", var, "_", ctrName, "_", m, "_", enos, ".tif"))
        
        desc <- rbind(desc, cbind(paste0(var, "_", ctrName, "_", m, "_", enos, ".tif"), 
                                  round(xmin(tif_val),6), round(xmax(tif_val),6), round(ymin(tif_val),6), round(ymax(tif_val),6), nrow(tif_val), ncol(tif_val)))
        
        
      }
      
      
    }
    
  }
  
  
}


write.csv(desc, paste0(oIDir, "/_describe.csv"), row.names=F)


