#################################################################################
require(raster);require(rgdal);require(maptools); require(rasterVis)
#################################################################################

iDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/outputs"
uDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/uncertainties"
oDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/evaluation/suit_change_rec"
eDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/evaluation/suit_change_rec"
cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "cocoa", "sugar_cane", "panela_cane", "palmito")
id_crops <- c("Maize", "Cassava", "Plantain", "Cocoa", "Sugar Cane", "Panela Cane", "Palmito") #, " Beans", "Rice", "Cocoa", "Sugar Cane", "Panela Cane")
# id <- c("2070s")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
# rcpLs <- c("rcp45")
periodLs <- c("2020_2049", "2040_2069", "2070_2099")
# periodLs <- c("2070_2099")
mask <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")
mask_col <- extent(-79.5,-66.85,-5,7)
# mask_caqueta <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/MGN_ADM_MPIO_GRAFICO.shp", layer="MGN_ADM_MPIO_GRAFICO")
# adm_lim_col <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/COL1.shp", layer="COL1") 
# mask_ctr <- readOGR("Z:/WORK_PACKAGES/WP2/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")

if(!file.exists(eDir)){dir.create(eDir, recursive = T)}




### All crops in one plot

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
        
        current = raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif", sep=""))
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
        if(!file.exists(paste0(oDir, "/impacts-", crop))){
          dir.create(paste0(oDir, "/impacts-", crop), recursive = T)
          }
        writeRaster(pieced_fextent,paste0(oDir, "/impacts-", crop, "/suitchg_", rcp, "-", period, ".tif"))
        
      }
      
    }
    
    
    stk_crop <- stack(paste0(oDir, "/impacts-", cropLs, "/suitchg_", rcp, "-", period, ".tif"))
    stk_crop_msk <- crop(stk_crop, mask_col)
    
    plot <- setZ(stk_crop, id_crops)
    names(plot) <- id_crops
    
    tiff(paste(eDir, "/suitchg_", rcp, "-", period, ".tif", sep=""), width=600*length(cropLs), height=1000, pointsize=8, compression='lzw',res=100)
    
    print(levelplot(plot, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"), main=paste0("Suitability change ", rcp, " ", period)) + layer(sp.polygons(mask)) ) # + layer(sp.polygons(adm_lim_col)) + layer(sp.polygons(mask_caqueta, col = "red")) )
    
    dev.off()
    
    
  }
  
}



### Plot by 1 crop

for (rcp in rcpLs){
  
  for (c in 1:length(cropLs)){
    
    crop <- cropLs[c]
    crop_label <- id_crops[c]
    
    for (period in periodLs){
      
      cat(crop, rcp, period, "\n")
      
      if(!file.exists(paste0(oDir, "/impacts-", crop, "/suitchg_", rcp, "-", period, ".tif"))){
        
        current = raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif", sep=""))
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
        if(!file.exists(paste0(oDir, "/impacts-", crop))){
          dir.create(paste0(oDir, "/impacts-", crop), recursive = T)
        }
        writeRaster(pieced_fextent,paste0(oDir, "/impacts-", crop, "/suitchg_", rcp, "-", period, ".tif"))
        
      }
      
      
      ### Changes
      
      oPlot <- paste(eDir, "/suitchg_", rcp, "-", tolower(crop_label), "-", id, ".tif", sep="")
      if(!file.exists(oPlot)){
        
        # Plot settings
        zvalues <- seq(0, 5, 1) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("red", "orange", "yellow", "green", "darkgreen")) # Set new colors
        myTheme$strip.border$col = "white" # Eliminate frame from maps
        myTheme$axis.line$col = 'white' # Eliminate frame from maps
        # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
        
        
        stk_crop <- stack(paste0(oDir, "/impacts-", crop, "/suitchg_", rcp, "-", periodLs, ".tif"))
        stk_crop_msk <- mask(crop(stk_crop, mask_col), adm_lim_col)
        
        plot <- setZ(stk_crop_msk, id)
        names(plot) <- id
        
        tiff(paste(eDir, "/suitchg_", rcp, "-", tolower(crop_label), "-", id, ".tif", sep=""), width=900, height=800, pointsize=8, compression='lzw',res=100)
        
        print(levelplot(plot, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"), 
                        margin=FALSE,
                        labels=as.character(c("Areas no longer suitable", "Areas less suitable in the future", "Same suitability in the future", "New Areas of suitability", "Areas more suitable in the future")),
                        main=toupper(paste0("Suitability change ", rcp, " ", crop_label, " ", id))) + layer(sp.polygons(adm_lim_col)) + layer(sp.polygons(mask_caqueta, col = "red")) )
        dev.off()
      }
      
      
      
      ### Current
      oPlot <- paste(eDir, "/suit_current-", tolower(crop_label), ".tif", sep="")
      if(!file.exists(oPlot)){
        
      # Plot settings
      zvalues <- seq(0, 100, 10) # Define limits
      myTheme <- BuRdTheme() # Define squeme of colors
      myTheme$regions$col=colorRampPalette(c("white", "green3", "darkgreen")) # Set new colors
      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps
      # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
      
      
      stk_crop <- stack(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif"))
      stk_crop_msk <- mask(crop(stk_crop, mask_col), adm_lim_col)
      
      
      plot <- setZ(stk_crop_msk, c("Current"))
      names(plot) <- c("Current")
      
      tiff(oPlot, width=900, height=800, pointsize=8, compression='lzw',res=100)
      
      print(levelplot(plot, at = zvalues, scales = list(draw=FALSE),  xlab="", ylab="", par.settings = myTheme, colorkey = list(space = "bottom"), 
                      margin=FALSE,
                      main=toupper(paste0("Current Suitability ", crop_label))) + layer(sp.polygons(adm_lim_col)) + layer(sp.polygons(mask_caqueta, col = "red")) )
      dev.off()
      
      }
      
      
    }
    
  }
  
}


