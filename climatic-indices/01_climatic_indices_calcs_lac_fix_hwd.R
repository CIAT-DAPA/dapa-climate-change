######################################################################################################################
#### Author : Carlos Navarro                                                                                      ####
#### Date   : Dec 2022                                                                                            ####
#### Contact: c.e.navarro@cgiar.org                                                                               ####
#### OS     : Windows                                                                                             ####
######################################################################################################################

######################################################################################################################
########################################## CLIMATIC INDICES CALCS ####################################################
######################################################################################################################

## Libraries
require(raster)
require(maptools)
require(rgdal)
require(ncdf4)
require(reshape2)
require(sp)
require(dplyr)
require(lubridate)
require(rasterVis)
require(RColorBrewer)
require(stringr)
require(rgeos)
require(grid)

rasterOptions(tmpdir="F:/temp")

# Years
yi <- 1981
yf <- 2021
yi_r <- 2003
yf_r <- 2021

# Climate dirs
iDirP <- "S:/observed/gridded_products/chirps/daily"
iDirT <- "U:/GLOBAL/Climate/observed/gridded_products/CHIRTS"
iDirTc <- "U:/observed/gridded_products/era5/sis-agromet/nc/2m_temperature"
iDirPm <- "S:/observed/gridded_products/chirps/monthly/world"
iDirAdm <- "E:/yapu_climate_risk/admin_boundaries"
iDir <- "E:/yapu_climate_risk"
oDir <- "F:/yapu_climate_risk"

# Climate params
dircdo <- "E:/yapu_climate_risk/cdo/cdo"
ensoFile <- "E:/yapu_climate_risk/enso_condition.csv"
ndays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
enosCond <- c("elnino", "lanina", "normal")
vars <- c("prec", "tmin", "tmax")
mv <- -999
resampling <- F
# probs_q <- c(.25,.45,.55,.75)
# probs_q <- c(.01,.35,.65,.99)
# probs_q <- c(.05,.35,.65,.95)
probs_q <- c(.159,.309,.691,.841)
mag_labels <- c("Month", "x-1sd", "x-0.5sd", "x+0.5sd", "x+1sd")
probs_q2 <- c(.023,.159,.841,.977)
mag_labels2 <- c("Month", "x-2sd", "x-1sd", "x+1sd", "x+2sd")

cly_global <- "U:/GLOBAL/Biofisico/SoilGrids250m/Physical soil properties/Clay content (0-2 micro meter) mass fraction/CLYPPT_M_sAvg_250m_ll.tif"
lco_global <- "U:/GISDATA/GLOBAL/Biofisico/LAND_COVER/GLOBCOVER_L4_200901_200912_V2.3_reclass.tif"
dem_global <- "S:/observed/gridded_products/srtm/srtm_v41_30s.tif"
wei_global <- "U:/GISDATA/GLOBAL/SAGA_wetness_index_global.tif"

# ## LAM splitted list
# ctrLs = c("ARG", "BRA", "CHL")
# ctrLs = c("BOL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV") #Errors ARG BRA CHL
# ctrLs = c("GUF", "GLP", "GTM", "GUY", "HTI", "HND", "MTQ", "MEX", "NIC", "PAN") 
# ctrLs = c("PRY", "PER", "SUR", "URY", "VEN", "VIR", "ATG")
# ctrLs = c("ABW", "AIA", "BHS", "BLZ", "BRB", "CUW", "CYM") #Errors ANT, FLK UMI
# ctrLs = c("DMA", "GRD", "JAM", "KNA", "LCA", "MSR", "PRI")
# ctrLs = c("SXM", "TCA", "TTO", "VCT", "VGB", "XCL")

# ctrName <- "BRA2"
bigctr <- "no"

cat("Processing ", ctrName)

##################################################
## Load masks by country                       ###
##################################################

## Only consider continental part
ctrShpAdm0 <- paste0(iDirAdm, "/gadm41_", ctrName, "_0.shp")
ctrLyrAdm0 <- paste0("gadm41_", ctrName, "_0")
ctrShpAdm0Buf <- paste0(iDirAdm, "/gadm41_", ctrName, "_0_buffer.shp")
ctrLyrAdm0Buf <- paste0("gadm41_", ctrName, "_0_buffer")
ctrShpAdm1 <- paste0(iDirAdm, "/gadm41_", ctrName, "_1.shp")
ctrLyrAdm1 <- paste0("gadm41_", ctrName, "_1")
ctrShpAdm1Sin <- paste0(iDirAdm, "/gadm41_", ctrName, "_1_sp.shp")
ctrLyrAdm1Sin <- paste0("gadm41_", ctrName, "_1_sp")
ctrShpAdm2 <- paste0(iDirAdm, "/gadm41_", ctrName, "_2.shp")
ctrLyrAdm2 <- paste0("gadm41_", ctrName, "_2")
ctrShpAdm2Sin <- paste0(iDirAdm, "/gadm41_", ctrName, "_2_sp.shp")
ctrLyrAdm2Sin <- paste0("gadm41_", ctrName, "_2_sp")
rsMsk <- paste0(iDirAdm, "/gadm41_", ctrName, "_rs_mask.tif")

oBDir <- paste0(iDir, "/basedata/", ctrName)
oIDir <- paste0(oDir, "/indices_v2/", ctrName)

if (!file.exists(rsMsk)) {
  ctrMsk <- readOGR(ctrShpAdm0Buf,layer=ctrLyrAdm0Buf)
  dts_dump <- raster(paste0(iDirP, "/chirps-v2.0.1981.01.01.tif"))
  ctrMsk_rs <- writeRaster(mask(crop(dts_dump, ctrMsk), ctrMsk) * 0 + 1, rsMsk)
}

ctrMsk0 <- raster(rsMsk)

##################################################
## HWD. Heat Wave duration                     ###
## Vulnerability type: Extreme heat            ###
## Data Source: CHIRPS daily                   ###
##################################################

## Identify historical ENOS condition
ensoCond <- read.csv(ensoFile, header=T, row.names = 1)
ensoCond <- as.vector(cbind(Year=rownames(ensoCond)[row(ensoCond)], setNames(melt(ensoCond), c('Month', 'Values'))))
ensoCond$Month <- gsub("X", "", ensoCond$Month)
elnino <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
lanina <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
normal <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)

## HWD output work directory
oBDirW <- paste0(oBDir, "/indices")
if (!file.exists(oBDirW)) {dir.create(oBDirW, recursive = TRUE)}


## Historical ##

## HWD output directory historical
oIDirH <- paste0(oIDir, "/historical")
oIDirHHwd <- paste0(oIDirH, "/hwd")
if (!file.exists(paste0(oIDirHHwd))) {dir.create(paste0(oIDirHHwd), recursive = TRUE)}

## HWD Calcs all years all months
cat(">. Calculating HWD Historical ", ctrName, "\n")

iNc <- paste0(oBDir, "/daily/Tmax.", yi, "-", yf, "_daily")
oTxWRef <- paste0(oBDirW, "/txavg_", yi, "-", yf)

oHwdW <- paste0(oBDirW, "/hwd_", ctrName)
oHwd <- paste0(oIDirHHwd, "/hwd_", ctrName)

## Load Mask (Adm0)
ctrMsk0 <- raster(rsMsk)

hwd_mag <- data.frame()

for (m in 1:12){
  
  if (!file.exists(paste0(oHwd, "_", m, "_normal.shp"))) {
    
    cat(" . HWD Month ", m, "processing\n")
    
    # ## Calc Tx average ref period
    # if (m == 2){
    #   # Calc yday Tmax ref. period
    #   system(paste0(dircdo," -s ydrunavg,5 ", iNc, "_", sprintf("%02d", m) ,".nc", " ",
    #                 oTxWRef, "_leap_", sprintf("%02d", m), ".nc"))
    #   system(paste0(dircdo," -s delete,month=2,day=29 ", oTxWRef, "_leap_", sprintf("%02d", m), ".nc", " ",
    #                 oTxWRef, "_",  sprintf("%02d", m),".nc"))
    # } else {
    #   # Calc yday Tmax ref. period
    #   system(paste0(dircdo," -s ydrunavg,5 ", iNc, "_", sprintf("%02d", m) ,".nc", " ",
    #                 oTxWRef, "_", sprintf("%02d", m), ".nc"))
    # }
    
    ## Calc heat waves for each year/month
    for (yr in yi:yf){
      
      if (!file.exists(paste0(oHwdW, "_", yr, "_", m, ".nc"))) {
        
        system(paste0(dircdo," -s -eca_hwdi,6,5 -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), ".nc", " ", 
                      iNc, "_", sprintf("%02d", m) ,".nc", " ", oHwdW, "_", yr, "_", m, "_tmp.nc"))
        
        system(paste0(dircdo," -s -select,name=heat_waves_per_time_period ", oHwdW, "_", yr, "_", m, "_tmp.nc", " ", oHwdW, "_", yr, "_", m, ".nc"))
        
        file.remove(paste0(oHwdW, "_", yr, "_", m, "_tmp.nc"))
        
      }
    }
    
    ## Load CHIRPS data and stack
    hwdStk_yrs <- stack(paste0(oHwdW, "_", yi:yf, "_", m, ".nc"))
    writeRaster(hwdStk_yrs, paste0(oHwdW, "_", yi, "-", yf, "_", m, ".nc"), format="CDF", overwrite=T)
    
    # Quantiles by month
    q <- paste(summary(quantile(hwdStk_yrs, probs = probs_q, names = FALSE, na.rm=TRUE)))[c(4,10,16,22)]
    hwd_mag <- rbind(hwd_mag, c(m, as.numeric(gsub("  ", "", gsub("Mean   :","", q)))))
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m <- subset(elnino, elnino$Month == m)
    # elnino_m_yrs <- gsub(" ","", toString(elnino_m$Year))
    lanina_m <- subset(lanina, lanina$Month == m)
    # lanina_m_yrs <- gsub(" ","", toString(lanina_m$Year))
    normal_m <- subset(normal, normal$Month == m)
    # normal_m_yrs <- gsub(" ","", toString(normal_m$Year))
    
    ## Calculate heat wave duration average by condition
    hwd_elnino <- mean(stack(paste0(oHwdW, "_", elnino_m$Year, "_", m, ".nc")))
    hwd_elnino[is.na(hwd_elnino[])] <- 0 
    hwd_elnino <- mask(hwd_elnino, ctrMsk0)
    
    hwd_lanina <- mean(stack(paste0(oHwdW, "_", lanina_m$Year, "_", m, ".nc")))
    hwd_lanina[is.na(hwd_lanina[])] <- 0 
    hwd_lanina <- mask(hwd_lanina, ctrMsk0)
    
    hwd_normal <- mean(stack(paste0(oHwdW, "_", normal_m$Year, "_", m, ".nc")))
    hwd_normal[is.na(hwd_normal[])] <- 0 
    hwd_normal <- mask(hwd_normal, ctrMsk0)
    
    writeRaster(hwd_elnino, paste0(oHwd, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(hwd_lanina, paste0(oHwd, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(hwd_normal, paste0(oHwd, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "hwd"
      varLn <- "heat.waves.index"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oHwd, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirHHwd, paste0("hwd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oHwdValsDf<- data.frame(getValues(dtsRs))
      # oHwdValsDf <- oHwdValsDf %>% mutate(vuln =
      #                                       case_when(oHwdValsDf <= as.numeric(hwd_mag[m, 2]) ~ "1", 
      #                                                 (oHwdValsDf > as.numeric(hwd_mag[m, 2]) & oHwdValsDf <= as.numeric(hwd_mag[m, 3])) ~ "2",
      #                                                 (oHwdValsDf > as.numeric(hwd_mag[m, 3]) & oHwdValsDf <= as.numeric(hwd_mag[m, 4])) ~ "3",
      #                                                 (oHwdValsDf > as.numeric(hwd_mag[m, 4]) & oHwdValsDf <= as.numeric(hwd_mag[m, 5])) ~ "4",
      #                                                 oHwdValsDf > as.numeric(hwd_mag[m, 5])  ~ "5")
      # )
      # 
      # 
      oHwdValsDf <- oHwdValsDf %>% mutate(vuln =
                                            case_when(oHwdValsDf >= as.numeric(hwd_mag[m, 5])  ~ "5", 
                                                      (oHwdValsDf < as.numeric(hwd_mag[m, 5]) & oHwdValsDf >= as.numeric(hwd_mag[m, 4])) ~ "4",
                                                      (oHwdValsDf < as.numeric(hwd_mag[m, 4]) & oHwdValsDf >= as.numeric(hwd_mag[m, 3])) ~ "3",
                                                      (oHwdValsDf < as.numeric(hwd_mag[m, 3]) & oHwdValsDf >= as.numeric(hwd_mag[m, 2])) ~ "2",
                                                      oHwdValsDf <= as.numeric(hwd_mag[m, 2]) ~ "1")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oHwdValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirHHwd, "/hwd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirHHwd, paste0("hwd_", ctrName, "_", m, "_", enos, "_mag"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      # ## Load Mask (Adm2)
      # ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      # 
      # ## Extract values inside polygons and calc avg
      # oHwdVals <- extract(dtsRs, ctrMsk)
      # # oHwdValsAvg <- round(unlist(lapply(oHwdVals, FUN=mean)))
      # 
      # ## Reclassify by magnitude ranges based on quantiles
      # ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      # oHwdVuln <- data.frame(oHwdVuln=unlist(lapply(oHwdVals, FUN=mean)))
      # 
      # oHwdVuln_mean <- oHwdVuln
      # 
      # oHwdVuln <- oHwdVuln %>% mutate(vuln =
      #                                   case_when(oHwdVuln >= as.numeric(hwd_mag[m, 5])  ~ "5", 
      #                                             (oHwdVuln < as.numeric(hwd_mag[m, 5]) & oHwdVuln >= as.numeric(hwd_mag[m, 4])) ~ "4",
      #                                             (oHwdVuln < as.numeric(hwd_mag[m, 4]) & oHwdVuln >= as.numeric(hwd_mag[m, 3])) ~ "3",
      #                                             (oHwdVuln < as.numeric(hwd_mag[m, 3]) & oHwdVuln >= as.numeric(hwd_mag[m, 2])) ~ "2",
      #                                             oHwdVuln <= as.numeric(hwd_mag[m, 2]) ~ "1")
      # )
      # 
      # ## Join mean values to polygon data and write shapefile
      # ctrMsk@data <- data.frame(ctrMsk@data, hwd=round(oHwdVuln$oHwdVuln), vuln=as.numeric(oHwdVuln$vuln))
      # writeOGR(ctrMsk, oIDirHHwd, paste0("hwd_", ctrName, "_", m, "_", enos, "_mun"),
      #          driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . HWD Month ", m, " ", enos, "done\n")
      
      names(hwd_mag) <- mag_labels
      write.csv(hwd_mag, paste0(oIDirHHwd, "/hwd_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
      
    }
    
    cat(" . HWD Month ", m, "done\n")
    
    
    
  } else {
    
    cat(" . HWD Month ", m, "done\n")
    
  }
  
}


## Recent past ##

## HWD output directory recent past
oIDirR <- paste0(oIDir, "/recent-past")
oIDirRHwd <- paste0(oIDirR, "/hwd")
if (!file.exists(paste0(oIDirRHwd))) {dir.create(paste0(oIDirRHwd), recursive = TRUE)}

## HWD Calcs all years all months
cat(">. Calculating HWD Recent Past ", ctrName, "\n")
oHwdR <- paste0(oIDirRHwd, "/hwd_", ctrName)

elnino_r <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
lanina_r <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
normal_r <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)

## Load Mask (Adm0)
ctrMsk0 <- raster(rsMsk)

for (m in 1:12){
  
  if (!file.exists(paste0(oHwdR, "_", m, "_normal.shp"))) {
    
    cat(" . HWD Month ", m, "processing\n")
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m_r <- subset(elnino_r, elnino_r$Month == m)
    lanina_m_r <- subset(lanina_r, lanina_r$Month == m)
    normal_m_r <- subset(normal_r, normal_r$Month == m)
    
    ## Calculate mean consecutive dry days length by condition
    hwd_elnino_r <- mean(stack(paste0(oHwdW, "_", elnino_m_r$Year, "_", m, ".nc")))
    hwd_elnino_r[is.na(hwd_elnino_r[])] <- 0 
    hwd_elnino_r <- mask(hwd_elnino_r, ctrMsk0)
    
    hwd_lanina_r <- mean(stack(paste0(oHwdW, "_", lanina_m_r$Year, "_", m, ".nc")))
    hwd_lanina_r[is.na(hwd_lanina_r[])] <- 0 
    hwd_lanina_r <- mask(hwd_lanina_r, ctrMsk0)
    
    hwd_normal_r <- mean(stack(paste0(oHwdW, "_", normal_m_r$Year, "_", m, ".nc")))
    hwd_normal_r[is.na(hwd_normal_r[])] <- 0 
    hwd_normal_r <- mask(hwd_normal_r, ctrMsk0)
    
    writeRaster(hwd_elnino_r, paste0(oHwdR, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(hwd_lanina_r, paste0(oHwdR, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(hwd_normal_r, paste0(oHwdR, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    hwd_mag <- read.csv(paste0(oIDirHHwd, "/hwd_", ctrName, "_", m, "_normal_mag_class", ".csv"))
    
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "hwd"
      varLn <- "heat.waves.index"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oHwdR, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirRHwd, paste0("hwd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oHwdValsDf<- data.frame(getValues(dtsRs))
      oHwdValsDf <- oHwdValsDf %>% mutate(vuln =
                                            case_when(oHwdValsDf >= as.numeric(hwd_mag[m, 5])  ~ "5", 
                                                      (oHwdValsDf < as.numeric(hwd_mag[m, 5]) & oHwdValsDf >= as.numeric(hwd_mag[m, 4])) ~ "4",
                                                      (oHwdValsDf < as.numeric(hwd_mag[m, 4]) & oHwdValsDf >= as.numeric(hwd_mag[m, 3])) ~ "3",
                                                      (oHwdValsDf < as.numeric(hwd_mag[m, 3]) & oHwdValsDf >= as.numeric(hwd_mag[m, 2])) ~ "2",
                                                      oHwdValsDf <= as.numeric(hwd_mag[m, 2]) ~ "1")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oHwdValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirRHwd, "/hwd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirRHwd, paste0("hwd_", ctrName, "_", m, "_", enos, "_mag"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      # ## Load Mask (Adm2)
      # ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      # 
      # ## Extract values inside polygons and calc avg
      # oHwdVals <- extract(dtsRs, ctrMsk)
      # # oHwdValsAvg <- round(unlist(lapply(oHwdVals, FUN=mean)))
      # 
      # ## Reclassify by magnitude ranges based on quantiles
      # ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      # oHwdVuln <- data.frame(oHwdVuln=unlist(lapply(oHwdVals, FUN=mean)))
      # oHwdVuln <- oHwdVuln %>% mutate(vuln =
      #                                   case_when(oHwdVuln >= as.numeric(hwd_mag[m, 5])  ~ "5", 
      #                                             (oHwdVuln < as.numeric(hwd_mag[m, 5]) & oHwdVuln >= as.numeric(hwd_mag[m, 4])) ~ "4",
      #                                             (oHwdVuln < as.numeric(hwd_mag[m, 4]) & oHwdVuln >= as.numeric(hwd_mag[m, 3])) ~ "3",
      #                                             (oHwdVuln < as.numeric(hwd_mag[m, 3]) & oHwdVuln >= as.numeric(hwd_mag[m, 2])) ~ "2",
      #                                             oHwdVuln <= as.numeric(hwd_mag[m, 2]) ~ "1")
      # )
      # 
      # ## Join mean values to polygon data and write shapefile
      # ctrMsk@data <- data.frame(ctrMsk@data, hwd=round(oHwdVuln$oHwdVuln), vuln=as.numeric(oHwdVuln$vuln))
      # writeOGR(ctrMsk, oIDirRHwd, paste0("hwd_", ctrName, "_", m, "_", enos, "_mun"),
      #          driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . HWD Month ", m, " ", enos, "done\n")
      
    }
    
    
    cat(" . HWD Month ", m, "done\n")
    
  } else {
    
    cat(" . HWD Month ", m, "done\n")
    
  }
  
}

cat(">. HWD calcs done", "\n")



