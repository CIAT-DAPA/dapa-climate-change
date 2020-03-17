# Carlos Navarro 
# CIAT - CCAFS
# Feb 2020

##########################
#### Plots by months  ####
##########################

# Load libraries
install.packages("rasterVis")
install.packages("maptools")
install.packages("rgdal")
install.packages("raster")

require(rasterVis)
require(maptools)
require(rgdal)
require(raster)

# Set folders
bDir <- "D:/OneDrive - CGIAR/CIAT/Climate & Geodata/worldclim"
oDir <- "C:/Users/cenavarro/Dropbox/Training Materials/Week_2/R_examples"
setwd(bDir)
if (!file.exists(oDir)) {dir.create(oDir)}

# Variables and months
var <- "prec"
id <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
varList <- c("prec", "tavg", "tmin", "tmax")

# Masks (shapefiles)
mask <- readOGR("C:/Users/cenavarro/Dropbox/Training Materials/Week_2/R_examples/vnm_shp/gadm36_VNM_0.shp",layer="gadm36_VNM_0")
provinces <- readOGR("C:/Users/cenavarro/Dropbox/Training Materials/Week_2/R_examples/vnm_shp/gadm36_VNM_1.shp",layer="gadm36_VNM_1")

# WorldClim prefix
months <- c("01", "02","03", "04","05", "06", "07", "08", "09", "10", "11", "12")
wcl_pref <- "wc2.0_10m"

for (var in varList){

  # Load raster data

  # Load raster data in stack  
  rsStk <- stack(paste0(wcl_pref, "_", var, "_", months, ".tif"))
  stk_crop <- mask(crop(rsStk, extent(mask)), mask)
  
  # FLoat to integer
  if (var == "prec"){
    stk_crop <- round(stk_crop, digits = 0)
  } else {
    stk_crop <- round(stk_crop * 10, digits = 0)
  }
  
  # Plot parameters by variable
  if (var == "prec"){
    
    stk_crop[which(stk_crop[]>800)]=800
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    
    zvalues <- seq(0, 800, 25) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors from brewer pallete
    myTheme$regions$col=colorRampPalette(c("khaki1", "skyblue1", "blue", "darkblue", "darkmagenta"))(length(zvalues)-1) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
    
  } else {
    
    stk_crop <- stk_crop / 10
    stk_crop[which(stk_crop[]< 8 )]= 8
    stk_crop[which(stk_crop[]>38)]= 38
    
    plot <- setZ(stk_crop, id)
    names(plot) <- id
    zvalues <- seq(8, 38, 2)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkblue", "yellow", "orange", "red", "darkred"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    # myTheme=rasterTheme(region=brewer.pal('YlOrRd', n=9))  
    
  }
  
  # Save to file
  tiff(paste(oDir, "/climate_normal_", var, ".tif", sep=""), width=1600, height=2400, pointsize=8, compression='lzw',res=200)
  print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, 
                  colorkey = list(space = "right", width=1.2, height=1)
                  ) 
        + layer(sp.polygons(provinces, lwd=0.8))
        )
  dev.off()
  
}

  

################
## By seasons ##
################

# List of seasons
seasons <- list("djf"=c(12,1,2), "mam"=3:5, "jja"=6:8, "son"=9:11, "ann"=1:12)

for (var in varList){
  
  # Load raster data in stack  
  rsStk <- stack(paste0(wcl_pref, "_", var, "_", months, ".tif"))
  stk_crop <- mask(crop(rsStk, extent(mask)), mask)
  
  # FLoat to integer
  if (var == "prec"){
    stk_crop <- round(stk_crop, digits = 0)
  } else {
    stk_crop <- round(stk_crop * 10, digits = 0)
  }
  
  # Loop throught seasons 
  for (i in 1:length(seasons)){
    
    cat("Calcs ", var, names(seasons[i]), "\n")
    
    if (var == "prec"){
      
      # Sum for precipitation
      sAvg = calc(stk_crop[[c(seasons[i], recursive=T)]],fun=function(x){sum(x,na.rm=any(!is.na(x)))})
      
    } else {
      
      #Average for temperature
      sAvg = calc(stk_crop[[c(seasons[i], recursive=T)]],fun=function(x){mean(x,na.rm=T)})
      
    }
    
    # Write results
    writeRaster(sAvg, paste(bDir,'/', var, "_", names(seasons[i]), '.tif',sep=''),format="GTiff", overwrite=T, datatype='INT2S')
    
  } 
  
}


### Your task
### Plot the seasons....

