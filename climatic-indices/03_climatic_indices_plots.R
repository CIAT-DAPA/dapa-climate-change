######################################################################################################################
#### Author : Carlos Navarro                                                                                      ####
#### Date   : Jan 2023                                                                                            ####
#### Contact: c.e.navarro@cgiar.org                                                                               ####
#### OS     : Windows                                                                                             ####
######################################################################################################################

######################################################################################################################
########################################## CLIMATIC INDICES PLOTS ####################################################
######################################################################################################################

## Libraries
require(raster)
require(maptools)
require(rgdal)
require(ncdf4)
require(reshape2)
require(sp)
require(lubridate)
require(rasterVis)
require(RColorBrewer)
require(stringr)
require(rgeos)
require(grid)

# Climate dirs
iDirAdm <- "E:/yapu_climate_risk/admin_boundaries"
iDirP <- "S:/observed/gridded_products/chirps/daily"
oIDir <- "F:/yapu_climate_risk/continental/latinamerica"
ctrName <- "LAC"

cat("Processing ", ctrName)

##################################################
## Check indices                               ###
##################################################

# Set params
# varList <- c("cdd", "drd", "p95", "frd", "fld", "hwd")
# scenarios <- c("historical", "recent-past")
# enosCond <- c("elnino", "lanina", "normal")
enosCond <- c("normal")
id <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# varList <- c("cdd")
# scenarios <- c("historical")


## Only consider continental part
ctrShpAdm0 <- paste0(iDirAdm, "/gadm41_", ctrName, "_0.shp")
ctrLyrAdm0 <- paste0("gadm41_", ctrName, "_0")
ctrShpAdm1 <- paste0(iDirAdm, "/gadm41_", ctrName, "_1.shp")
ctrLyrAdm1 <- paste0("gadm41_", ctrName, "_1")
ctrShpAdm2 <- paste0(iDirAdm, "/gadm41_", ctrName, "_2.shp")
ctrLyrAdm2 <- paste0("gadm41_", ctrName, "_2")
rsMsk <- paste0(iDirAdm, "/gadm41_", ctrName, "_rs_mask.tif")

if (!file.exists(rsMsk)) {
  ctrMsk <- readOGR(ctrShpAdm0,layer=ctrLyrAdm0)
  dts_dump <- raster(paste0(iDirP, "/chirps-v2.0.1981.01.01.tif"))
  ctrMsk_rs <- writeRaster(mask(crop(dts_dump, ctrMsk), ctrMsk) * 0 + 1, rsMsk)
}


for(scen in scenarios){
  
  ## General output directory
  oIDirH <- paste0(oIDir, "/", scen)
  oIDirHChk <- paste0(oIDir, "/", scen, "/_check")
  if (!file.exists(oIDirHChk)) {dir.create(oIDirHChk)}
  
  ## Load Mask (Adm0)
  ctrMsk <- readOGR(ctrShpAdm0,layer=ctrLyrAdm0)
  ctrMskAdm2 <- readOGR(ctrShpAdm2,layer=ctrLyrAdm2)
  
  for (var in varList){
    
    setwd(paste0(oIDirH, "/", var))
    
    ## Convert to shape
    for (enos in enosCond){
      
      stk_rec <- stack()
      stk_mag <- stack()
      
      ##Rasterize polygons
      for (m in 1:12){
        
        shp <- readOGR(paste0(var, "_", ctrName, "_", m, "_", enos, "_mun.shp"))
        r <- raster(rsMsk)
        extent(r) <- extent(shp)
        rp <- rasterize(shp, r, 'vuln')
        if (m==1){stk_rec <- stack(rp)} else {stk_rec <- addLayer(stk_rec, rp)}
        
        if (var != "fld"){
          rm <- rasterize(shp, r, var)
          if (m==1){stk_mag <- stack(rm)} else {stk_mag <- addLayer(stk_mag, rm)}
        }
        
      }
      
      if (var != "fld"){
        plot <- setZ(stk_mag, id)
        names(plot) <- id
      }
      
      plot_rec <- setZ(stk_rec, id)
      names(plot_rec) <- id
      
      
      ## Plot parms by var 
      if (var == "cdd" || var == "drd"){
        
        zvalues <- c(0, 5, 10, 15, 20, 25, 31) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"))(length(zvalues)-1) # Set new colors
        myTheme$strip.border$col = "white" # Eliminate frame from maps
        myTheme$axis.line$col = 'white' # Eliminate frame from maps
        unit <- "days"
        
      } else if ( var == "p95") {
        
        zvalues <- c(0, 5, 10, 15, 20, 100) # Define limits
        myTheme <- BuRdTheme()
        myTheme$regions$col=colorRampPalette(c("#d7191c", "#fdae61", "#ffffbf", "#abd9e9", "#2c7bb6"))(length(zvalues)-1) # Set new colors
        myTheme$strip.border$col = "white"
        myTheme$axis.line$col = 'white'
        unit <- "percent"
        
      } else if ( var == "frd") {
        
        zvalues <- c(0, 1, 2, 3, 4, 10) # Define limits
        myTheme <- BuRdTheme()
        myTheme$regions$col=colorRampPalette(c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac"))(length(zvalues)-1)
        myTheme$strip.border$col = "white"
        myTheme$axis.line$col = 'white'
        unit <- "days"
        
      } else if ( var == "fld") {
        
        zvalues <- c(0, 1, 2, 3, 4, 5) # Define limits
        myTheme <- BuRdTheme()
        myTheme$regions$col=colorRampPalette(c("#f0f9e8", "#bae4bc", "#7bccc4", "#43a2ca", "#0868ac"))(length(zvalues)-1)
        myTheme$strip.border$col = "white"
        myTheme$axis.line$col = 'white'
        unit <- "mag"
        
      } else if ( var == "hwd") {
        
        zvalues <- c(0, 1, 2, 3, 4, 10) # Define limits
        myTheme <- BuRdTheme()
        myTheme$regions$col=colorRampPalette(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"))(length(zvalues)-1) # Set new colors
        myTheme$strip.border$col = "white"
        myTheme$axis.line$col = 'white'
        unit <- "mag"
        
      }
      
      if (var != "fld"){

        ## Plot indices
        tiff(paste0(oIDirHChk, "/plot_monthly_", var, "_", enos, "_", unit, "_v1.tif"),
             width=3600, height=2400, pointsize=8, compression='lzw',res=200)
        print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, 
                        colorkey = list(space = "bottom", width=1.2, height=1)
        ) 
        + layer(sp.polygons(ctrMskAdm2, lwd=0.5))
        )
        dev.off()
        
      }
      
      ## Plot magnitudes
      zvalues_rec <- c(0, 1, 2, 3, 4, 5) # Define limits
      myTheme_rec <- BuRdTheme() # Define squeme of colors
      myTheme_rec$regions$col=colorRampPalette(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"))(length(zvalues)-1) # Set new colors
      myTheme_rec$strip.border$col = "white" # Eliminate frame from maps
      myTheme_rec$axis.line$col = 'white' # Eliminate frame from maps
      unit_rec <- "mag_reclass"
      
      
      tiff(paste0(oIDirHChk, "/plot_monthly_", var, "_", enos, "_", unit_rec, "_v1.tif"),
           width=3600, height=2400, pointsize=8, compression='lzw',res=200)
      print(levelplot(plot_rec, at = zvalues_rec, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme_rec, 
                      colorkey = list(space = "bottom", width=1.2, height=1)
      ) 
      + layer(sp.polygons(ctrMskAdm2, lwd=0.5))
      )
      dev.off()
      
      
    } 
    
  }
  
}

