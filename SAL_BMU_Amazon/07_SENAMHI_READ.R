##########################################################################################
## Purpose: Merge daily INHAMI weather stations in a single csv file and convert to Monthly
## Author: Lizeth Llanos l.llanos@cgiar.org
## Modified by : Carlos Navarro c.e.navarro@cgiar.org
##########################################################################################


###########################################
### 00- Explore stations in Napo Region ###
###########################################

clim_calc <- function(bDir = "Z:/DATA/WP2/01_Weather_Stations/PER",st_loc = "Z:/DATA/WP2/01_Weather_Stations/PER/station_catalog.csv", rg=c(-80, -66, -16, 5), rgName="amazon", sY=1971, fY=2010){
  
  # CP	Climática principal
  # CO	Estaciones Climatológicas Ordinarias 
  # SIN	Sinópticas
  # PLU	Estaciones Pluviométricas
  # HLM	limnimétrica
  # HLG	limnigráfica
  # MAP	Estación Meteorológica Agrícola Principal 
  # PE	
  # EMA	
  
  library(raster)
  
  # Set region extent
  rgExt <- extent(rg)
  
  ## Add station info
  stInfo <- read.csv(st_loc, header=T)
  
  # Select stations inside region and combine in one matrix
  pos = stInfo$LAT_DD < ymin(rgExt) | stInfo$LAT_DD > ymax(rgExt) | stInfo$LON_DD < xmin(rgExt) | stInfo$LON_DD > xmax(rgExt)
  stSel <- stInfo[!pos, ]
  
  nyears = as.numeric(substr(stSel$PER_FIN, 1,4)) - as.numeric(substr(stSel$PER_INI, 1,4))
  stSel$NYEARS <- nyears
  
  pos = nyears < 15 | as.numeric(substr(stSel$PER_FIN, 1,4)) < 1985 | (as.numeric(substr(stSel$PER_FIN, 1,4)) - 1985) < 7 | stSel$CATEG == "HLM" | stSel$CATEG == "HLG"
  stSel <- stSel[!pos, ]
  
  # Remove columns with all NA's
  stSel <- stSel[rowSums(is.na(stSel)) < ncol(stSel),]
  
  # Write climatology 
  write.csv(stSel, paste0(bDir, "/stations.csv"), row.names=F)
  
}

