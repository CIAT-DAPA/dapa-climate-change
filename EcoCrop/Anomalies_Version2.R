#Tomado de Antonio Pantoja (phyton)
#Analisis cambios futuro_presente
#################################################################################
require(raster);require(rgdal);require(maptools)
require(rasterVis)
#################################################################################

iDir <- "D:/CIAT/Projects/lat_bmu/05_EcoCrop_runs/outputs"
uDir <- "D:/CIAT/Projects/lat_bmu/05_EcoCrop_runs/uncertainties"
oDir <- "D:/CIAT/Projects/lat_bmu/05_EcoCrop_runs/impact"
cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "bean", "rice")
id <- c("Maize", "Cassava", "Plantain", "Bean", "Rice")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
# rcp <- "rcp45"
periodLs <- c("2020_2049", "2040_2069", "2070_2099")
# period <- "2040_2069"
mask <- readOGR("D:/CIAT/Projects/lat_bmu/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")


# Plot settings
zvalues <- seq(0, 5, 1) # Define limits
myTheme <- BuRdTheme() # Define squeme of colors
myTheme$regions$col=colorRampPalette(c("red", "orange", "yellow", "green", "darkgreen")) # Set new colors
myTheme$strip.border$col = "white" # Eliminate frame from maps
myTheme$axis.line$col = 'white' # Eliminate frame from maps
# myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  

for (rcp in rcpLs){
  
  for (period in periodLs){
    
    for (crop in cropLs){
      
      cat(crop, rcp, period, "\n")
      if(!file.exists(paste0(oDir, "/impacts-", crop, "/suitchg_", rcp, "-", period, ".tif"))){
        current = raster(paste0(iDir, "/", crop, "/analyses/runs/", crop, "_suit.tif", sep=""))
        future = raster(paste0(uDir, "/mean_", crop, "_", rcp, "_", period,".tif", sep=""))
        
        if (crop == "cassava"){
          thr <- 94
        } else if (crop == "cotton") {
          thr <- 60
        } else {
          thr <- 50
        }
        
        #Analysis
        outCon1 = ((current >= thr) & (future  <  thr)) #Areas nolong suitable (RED)
        outCon1[!outCon1]=NA
        outCon1[!is.na(outCon1)]=1
        
        outCon2 = (current >= thr) & (future >= thr) & ((future - current) < 0) #Areas suitable but less suitable inthe fut? (ORANGE)
        outCon2[!outCon2]=NA
        outCon2[!is.na(outCon2)]=2
        
        outCon3 = (current >= thr) & (future >= thr) & ((future - current) == 0) #Areas suitable and same suitability in the future (YELLOW)
        outCon3[!outCon3]=NA
        outCon3[!is.na(outCon3)]=3
        
        outCon4  = ((current < thr) & (future >= thr)) #New Areas of suitability (LIGHT GREEN)
        outCon4[!outCon4]=NA#Tells R to give all regions except specified region NA
        outCon4[!is.na(outCon4)]=4 #Gives each region a value of 1 
        
        outCon5 = (current >= thr) & (future >= thr) & ((future - current) > 0) #Areas Suitable and more suitable in the fut (DARK GREEN)
        outCon5[!outCon5]=NA
        outCon5[!is.na(outCon5)]=5
        
        ###Merge Layers
        
        pieced_fextent = merge(outCon1,outCon2,outCon3,outCon4,outCon5)
        #plot(pieced_fextent)
        writeRaster(pieced_fextent,paste0(oDir, "/impacts-", crop, "/suitchg_", rcp, "-", period, ".tif"))
        
      }
      
    }
    
    
    stk_crop <- stack(paste0(oDir, "/impacts-", cropLs, "/suitchg_", rcp, "-", period, ".tif"))

    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    tiff(paste(oDir, "/suitchg_", rcp, "-", period, ".tif", sep=""), width=1000, height=400, pointsize=8, compression='lzw',res=100)
    
    print(levelplot(plot, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"), main=paste0("Suitability change ", rcp, " ", period)) + layer(sp.polygons(mask)))
    
    dev.off()
    
    
  }
  
}




# Plot uncertainty
uDir <- "D:/CIAT/Projects/lat_bmu/05_EcoCrop_runs/uncertainties"
cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "bean", "rice")
oDir <- "D:/CIAT/Projects/lat_bmu/05_EcoCrop_runs/impact"
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069", "2070_2099")

stats <- c("mean", "mean-bottom25p", "mean-top25p", "agreement", "sd")


for (rcp in rcpLs){
  
  for (period in periodLs){
    
    for(stati in stats){
      
      if (stati == "agreement"){
        zvalues <- seq(0, 16, 1) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("yellow", "orange", "blue")) # Set new colors
        
      } else if (stati == "sd"){
        zvalues <- seq(0, 50, 5) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("white", "black")) # Set new colors
      
      } else {
        
        zvalues <- seq(0, 100, 10) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("red", "white", "darkgreen")) # Set new colors
      }
      
      stk_crop <- stack(paste0(uDir, "/", stati, "_", cropLs, "_", rcp, "_", period, ".tif"))
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      
      tiff(paste(oDir, "/", stati, "-", rcp, "-", period, ".tif", sep=""), width=1000, height=400, pointsize=8, compression='lzw',res=100)
      print(levelplot(plot, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"), main=paste0(stati, " ", rcp, " ", period)) + layer(sp.polygons(mask)))
      dev.off()
      
    }
  }
}






