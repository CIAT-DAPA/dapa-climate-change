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
# vars <- c("tmax")
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

# ## Africa splitted list
# ctrLs = c("SDN", "DZA", "AGO", "BEN", "BWA", "BFA", "BDI", "CPV", "CMR", "CAF") 
# ctrLs = c("COM", "CIV", "DJI", "EGY", "GNQ", "ERI", "GAB", "GHA", "GIN", "GNB") 
# ctrLs = c("KEN", "LSO", "LBR", "LBY", "MDG", "MWI", "MLI", "MRT", "MUS", "MAR") 
# ctrLs = c("NGA", "COG", "MOZ", "NAM", "RWA", "SHN", "ZAF", "TZA", "COD", "TCD")
# ctrLs = c("TGO", "TUN", "UGA", "ESH", "ZMB", "ZWE", "STP", "SEN", "SYC", "SLE") 
# ctrLs = c("SOM", "MYT", "GMB", "REU", "NER", "ETH", "SWZ")
# 
# ## LAM splitted list
# ctrLs = c("ARG", "BRA", "CHL")
# ctrLs = c("BOL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV") #Errors ARG BRA CHL
# ctrLs = c("GUF", "GLP", "GTM", "GUY", "HTI", "HND", "MTQ", "MEX", "NIC", "PAN") 
# ctrLs = c("PRY", "PER", "SUR", "URY", "VEN", "VIR", "ATG")
# ctrLs = c("ABW", "AIA", "BHS", "BLZ", "BRB", "CUW", "CYM") #Errors ANT, FLK UMI
# ctrLs = c("DMA", "GRD", "JAM", "KNA", "LCA", "MSR", "PRI")
# ctrLs = c("SXM", "TCA", "TTO", "VCT", "VGB", "XCL")

ctrName <- "CHL"
bigctr <- "yes"

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
oIDir <- paste0(oDir, "/indices/", ctrName)

if (!file.exists(rsMsk)) {
  ctrMsk <- readOGR(ctrShpAdm0Buf,layer=ctrLyrAdm0Buf)
  dts_dump <- raster(paste0(iDirP, "/chirps-v2.0.1981.01.01.tif"))
  ctrMsk_rs <- writeRaster(mask(crop(dts_dump, ctrMsk), ctrMsk) * 0 + 1, rsMsk)
}

ctrMsk0 <- raster(rsMsk)


##################################################
## Extract CHRIPS by country                   ###
##################################################

## Loop across variables
for (var in vars){
  
  ## List files by years
  cat(">. Listing available climate files", "\n")
  
  if (var == "prec"){
    
    cat(" . Croping CHIRPS ", ctrName, "\n")
    
    dtsLs <-  list.files(path=iDirP, pattern=paste0("chirps-v2.0.*.tif"),full.names = T,ignore.case=F)
    prefix <- "chirps-v2.0"
    varLn <- "Precipitation"
    unit <- "mm/day"
    
  } else if (var == "tmin") {
    
    cat(" . Croping CHRITS/ERA5 ", ctrName, "\n")
    
    ## List files by years
    dtsLs <-  list.files(path=iDirT, pattern=paste0("Tmin.*.tif"),full.names = T,ignore.case=F)
    prefix <- "Tmin"
    varLn <- "MinimumTemperature"
    unit <- "CelsiusDegrees"
    
    ## List files by years
    dtsLsC <-  list.files(path=iDirTc, pattern=paste0("Temperature-Air-2m-Min-24h.*.nc"),full.names = T,ignore.case=F)
    prefix <- "Tmin"
    varLn <- "MinimumTemperature"
    unit <- "CelsiusDegrees"
    
  } else if (var == "tmax") {
    
    cat(" . Croping CHRITS/ERA5 ", ctrName, "\n")
    
    ## List files by years
    dtsLs <-  list.files(path=iDirT, pattern=paste0("Tmax.*.tif"),full.names = T,ignore.case=F)
    prefix <- "Tmax"
    varLn <- "MaximumTemperature"
    unit <- "CelsiusDegrees"
    
    ## List files by years
    dtsLsC <-  list.files(path=iDirTc, pattern=paste0("Temperature-Air-2m-Max-24h.*.nc"),full.names = T,ignore.case=F)
    prefix <- "Tmax"
    varLn <- "MinimumTemperature"
    unit <- "CelsiusDegrees"
    
  }
  
  years <- paste(yi:yf, sep="", collapse="|")
  dtsLs_yrs_leap <- dtsLs[grepl(years,dtsLs)]
  dtsLs_yrs <- dtsLs_yrs_leap
  dtsLs_yrs <- dtsLs_yrs_leap[!grepl(".2.29.tif",dtsLs_yrs_leap)] #no leap
  
  ## Output file
  oNc <- paste0(oBDir, "/daily/", prefix, ".", yi, "-", yf, "_daily")
  
  ## Last file processed
  if (!file.exists(paste0(oNc, "_12.nc"))){
    
    ## Output directories
    if (!file.exists(paste0(oBDir))) {dir.create(paste0(oBDir), recursive = TRUE)}
    if (!file.exists(paste0(oBDir, "/daily"))) {dir.create(paste0(oBDir, "/daily"), recursive = TRUE)}
    if (!file.exists(paste0(oBDir, "/daily/tmp"))) {dir.create(paste0(oBDir, "/daily/tmp"), recursive = TRUE)}
    
    ## Load Mask (Adm0)
    # ctrMsk <- readOGR(ctrShpAdm0,layer=ctrLyrAdm0)
    ctrMsk <- readOGR(ctrShpAdm0Buf,layer=ctrLyrAdm0Buf)
    
    for (tif in dtsLs_yrs){

      ## Output file
      oTif <- paste0(oBDir, "/daily/tmp/",  substr(basename(tif),1,nchar(basename(tif))-4), ".nc")

      if (!file.exists(oTif)){

        ## Load CHIRPS/CHIRTS data and cut by mask
        dtsMsk <- mask(crop(raster(tif), extent(ctrMsk)), ctrMsk)
        writeRaster(resample(dtsMsk, raster(rsMsk)), oTif,  format="CDF",overwrite=F)

      }

    }

    if (var == "tmax" || var == "tmin") {

      dtsLsC_yrs_leap <- dtsLsC[grepl(years,dtsLsC)]
      dtsLsC_yrs <- dtsLsC_yrs_leap

      ## Complementary ERA data
      for (tif in dtsLsC_yrs){

        ## Output file
        date <- str_split(basename(tif), "_")[[1]][4]
        oTif <- paste0(oBDir, "/daily/tmp/", prefix, ".", substr(date, 1, nchar(date)-4), ".", substr(date, 5, 6), ".", substr(date, 7, 8), ".nc")

        if (!file.exists(oTif)){

          ## Load CHIRPS data and cut by mask
          dtsMsk <- mask(crop(raster(tif), extent(ctrMsk)), ctrMsk)
          writeRaster(resample(dtsMsk, raster(rsMsk)) - 273.15, oTif,  format="CDF",overwrite=F)

        }

      }

    }
    
    
    
    if (!file.exists(paste0(oNc, "_12.nc"))) {
      
      # if (!file.exists(paste0(oBDir, "/daily-", ctrName, "/", tail(basename(dtsLs_yrs), n=1)))) {
      
      ## Load CHIRPS data and stack
      for (yr in yi:yf){
        
        if (!file.exists(paste0(oBDir, "/daily/", prefix, ".", yr, "_daily_12.nc"))){
          
          if (var == "prec"){
            
            dtsLs_out <-  list.files(path=paste0(oBDir, "/daily/tmp"), pattern=paste0("chirps-v2.0.*.nc"), full.names = T,ignore.case=F)
            
          } else if (var == "tmin") {
            
            dtsLs_out <-  list.files(path=paste0(oBDir, "/daily/tmp"), pattern=paste0("Tmin.*.nc"), full.names = T,ignore.case=F)
            
          } else if (var == "tmax") {
            
            dtsLs_out <-  list.files(path=paste0(oBDir, "/daily/tmp"), pattern=paste0("Tmax.*.nc"), full.names = T,ignore.case=F)
          }
          
          dtsLs_out_yr <- dtsLs_out[grepl(yr,dtsLs_out)]
          dtsStk_out <- stack(dtsLs_out_yr)
          
          writeRaster(dtsStk_out, paste0(oBDir, "/daily/", prefix, ".", yr, "_daily_temp.nc"), format="CDF", overwrite=T)
          
          ## Init date
          iDate <- as.Date(paste0(yr, "-01-01"))
          
          ## Add time component, variable name and unit
          system(paste0(dircdo," -s -settaxis,", iDate, ",12:00:00,1day -chname,variable,", varLn, " -chunit,,", unit, " ",
                        oBDir, "/daily/", prefix, ".", yr, "_daily_temp.nc", " ",
                        oBDir, "/daily/", prefix, ".", yr, "_daily.nc"))
          
          ## Remove temporal file
          unlink(paste0(oBDir, "/daily/", prefix, ".", yr, "_daily_temp.nc"))
          
        }
        
      }
      
      
      ## Split by months by year
      for (yr in yi:yf){
        
        oNc_yr <- paste0(oBDir, "/daily/", prefix, ".", yr, "_daily")
        oNc_mth <- oNc_yr
        
        if (!file.exists(paste0(oNc_mth, "_12.nc"))) {
          system(paste0(dircdo," -s -splitmon ", oNc_yr, ".nc ", oNc_mth, "_"))
          unlink(paste0(oNc_yr, ".nc"))
          
        }
        
      }
      
      
      if (bigctr == "yes"){
        
        for (yr in yi:yf){
          
          for (m in 1:12){
            
            if (!file.exists(paste0(oBDir, "/daily/", prefix, ".", yr, "_daily_", sprintf("%02d", m), "_b.nc"))) {
              
              system(paste0(dircdo," -s -sellonlatbox,", xmin(extent(ctrMsk0)), ",", (xmin(extent(ctrMsk0))+xmax(extent(ctrMsk0)))/2, ",", ymin(extent(ctrMsk0)), ",", ymax(extent(ctrMsk0)), " ", 
                            oBDir, "/daily/", prefix, ".", yr, "_daily_", sprintf("%02d", m), ".nc", " ", 
                            oBDir, "/daily/", prefix, ".", yr, "_daily_", sprintf("%02d", m), "_a.nc"))
              
              system(paste0(dircdo," -s -sellonlatbox,", (xmin(extent(ctrMsk0))+xmax(extent(ctrMsk0)))/2, ",", xmax(extent(ctrMsk0)), ",", ymin(extent(ctrMsk0)), ",", ymax(extent(ctrMsk0)), " ", 
                            oBDir, "/daily/", prefix, ".", yr, "_daily_", sprintf("%02d", m), ".nc", " ", 
                            oBDir, "/daily/", prefix, ".", yr, "_daily_", sprintf("%02d", m), "_b.nc"))
            }
            
          }
        }
        
        for (mth in months){
          
          if (!file.exists(paste0(oNc, "_", mth), "_b.nc")) {
            
            
            system(paste0(dircdo," -s mergetime ", gsub(", "," ", toString(paste0(oBDir, "/daily/", prefix, ".", yi:yf, "_daily_", mth, "_a.nc"))), " ",
                          paste0(oNc, "_", mth), "_a.nc"))
            
            system(paste0(dircdo," -s mergetime ", gsub(", "," ", toString(paste0(oBDir, "/daily/", prefix, ".", yi:yf, "_daily_", mth, "_b.nc"))), " ",
                          paste0(oNc, "_", mth), "_b.nc"))
            
          }
          
        }
        
        
        
        
      } else {
        
        # Merge time-series all years by month
        for (mth in months){
          system(paste0(dircdo," -s mergetime ", gsub(", "," ", toString(paste0(oBDir, "/daily/", prefix, ".", yi:yf, "_daily_", mth, ".nc"))), " ",
                        paste0(oNc, "_", mth), ".nc"))
          
        }
        
      }
      
      
      
      
      
      # # Merge time-series by 20-yr periods by month
      # for (yr in yi:yf){
      #
      #   if(yr<2003){
      #     for (mth in months){
      #       yrp <- yr+19
      #       system(paste0(dircdo," -s mergetime ", gsub(", "," ", toString(paste0(oBDir, "/daily-", ctrName, "-bymonth", "/", prefix, ".", yr:yrp, "_", ctrName, "_daily_", mth, ".nc"))), " ",
      #                     paste0(oBDir, "/daily-", ctrName, "-bymonth", "/", prefix, ".", yr, "-", yrp, "_", ctrName, "_daily_", mth), ".nc"))
      #     }
      #   }
      #
      # }
      
      
      ## Dissagregate to 0.025 and 0.01 deg (for smaller zones analyses)
      if (resampling == T && !file.exists(paste0(oNc, "_res001.nc"))) {
        
        dtsMskDiss_025 <- disaggregate(stack(paste0(oNc, ".nc")), fact=c(2,2), method='')
        oNcWrite <- writeRaster(dtsMskDiss_025, paste0(oNc, "_res0025.nc"), format="CDF", overwrite=T)
        dtsMskDiss_001 <- disaggregate(stack(paste0(oNc, ".nc")), fact=c(5,5), method='')
        oNcWrite <- writeRaster(dtsMskDiss_001, paste0(oNc, "_res001.nc"), format="CDF", overwrite=T)
        
        cat(" . Dissagregate done", ctrName, "\n")
        
      }
      
      
      # ## Split by months
      # if (!file.exists(paste0(oNc, "_12.nc"))) {
      #
      #   system(paste0(dircdo," -s -splitmon ", oNc, ".nc ", oNc, "_"))
      #
      #   if (resampling == T){
      #     system(paste0(dircdo," -s -splitmon ", oNc, "_res0025", ".nc ", oNc, "_res0025", "_"))
      #     system(paste0(dircdo," -s -splitmon ", oNc, "_res001", ".nc ", oNc, "_res001", "_"))
      #   }
      #
      # }
    }
    
    ## Remove temporal daily files
    # unlink(paste0(oBDir, "/daily/tmp"), recursive = T)
    
  }
  
}




##################################################
## CDD. Maximum number of consecutive dry days ###
## Vulnerability type: Drought                 ###
## Data Source: CHIRPS daily                   ###
##################################################

## Identify historical ENOS condition
ensoCond <- read.csv(ensoFile, header=T, row.names = 1)
ensoCond <- as.vector(cbind(Year=rownames(ensoCond)[row(ensoCond)], setNames(melt(ensoCond), c('Month', 'Values'))))
ensoCond$Month <- gsub("X", "", ensoCond$Month)
elnino <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
lanina <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
normal <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)

## CDD output work directory
oBDirW <- paste0(oBDir, "/indices")
if (!file.exists(oBDirW)) {dir.create(oBDirW, recursive = TRUE)}


## Historical ##

## CDD output directory historical
oIDirH <- paste0(oIDir, "/historical")
oIDirHCdd <- paste0(oIDirH, "/cdd")
if (!file.exists(paste0(oIDirHCdd))) {dir.create(paste0(oIDirHCdd), recursive = TRUE)}

## CDD Calcs all years all months
cat(">. Calculating CDD Historical ", ctrName, "\n")

iNc <- paste0(oBDir, "/daily/chirps-v2.0.", yi, "-", yf, "_daily")
oCddW <- paste0(oBDirW, "/cdd_", ctrName)
oCdd <- paste0(oIDirHCdd, "/cdd_", ctrName)

## Load Mask (Adm0)
ctrMsk0 <- raster(rsMsk)

cdd_mag <- data.frame()

for (m in 1:12){
  
  if (!file.exists(paste0(oCdd, "_", m, "_normal.shp"))) {
    
    cat(" . CDD Month ", m, "processing\n")
    
    for (yr in yi:yf){
      
      if (!file.exists(paste0(oCddW, "_", yr, "_", m, ".nc"))) {
        
        ## Calc consecutive dry days for each year/month
        system(paste0(dircdo," -s -eca_cdd,1 -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), ".nc", " ", oCddW, "_", yr, "_", m, ".nc"))
        
      }
      
      
    }
    
    ## Load CHIRPS data and stack
    cddStk_yrs <- stack(paste0(oCddW, "_", yi:yf, "_", m, ".nc"))
    writeRaster(cddStk_yrs, paste0(oCddW, "_", yi, "-", yf, "_", m, ".nc"), format="CDF", overwrite=T)
    
    # Quantiles by month
    q <- paste(summary(quantile(cddStk_yrs, probs = probs_q, names = FALSE, na.rm=TRUE)))[c(4,10,16,22)]
    cdd_mag <- rbind(cdd_mag, c(m, as.numeric(gsub("  ", "", gsub("Mean   :","", q)))))
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m <- subset(elnino, elnino$Month == m)
    # elnino_m_yrs <- gsub(" ","", toString(elnino_m$Year))
    lanina_m <- subset(lanina, lanina$Month == m)
    # lanina_m_yrs <- gsub(" ","", toString(lanina_m$Year))
    normal_m <- subset(normal, normal$Month == m)
    # normal_m_yrs <- gsub(" ","", toString(normal_m$Year))
    
    ## Calculate mean consecutive dry days length by condition
    cdd_elnino <- mean(stack(paste0(oCddW, "_", elnino_m$Year, "_", m, ".nc")))
    cdd_lanina <- mean(stack(paste0(oCddW, "_", lanina_m$Year, "_", m, ".nc")))
    cdd_normal <- mean(stack(paste0(oCddW, "_", normal_m$Year, "_", m, ".nc")))
    
    writeRaster(cdd_elnino, paste0(oCdd, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(cdd_lanina, paste0(oCdd, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(cdd_normal, paste0(oCdd, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "cdd"
      varLn <- "Consecutive.dry.days"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oCdd, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirHCdd, paste0("cdd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oCddValsDf<- data.frame(getValues(dtsRs))
      # oCddValsDf <- oCddValsDf %>% mutate(vuln =
      #                                       case_when(oCddValsDf <= as.numeric(cdd_mag[m, 2]) ~ "1", 
      #                                                 (oCddValsDf > as.numeric(cdd_mag[m, 2]) & oCddValsDf <= as.numeric(cdd_mag[m, 3])) ~ "2",
      #                                                 (oCddValsDf > as.numeric(cdd_mag[m, 3]) & oCddValsDf <= as.numeric(cdd_mag[m, 4])) ~ "3",
      #                                                 (oCddValsDf > as.numeric(cdd_mag[m, 4]) & oCddValsDf <= as.numeric(cdd_mag[m, 5])) ~ "4",
      #                                                 oCddValsDf > as.numeric(cdd_mag[m, 5])  ~ "5")
      # )
      # 
      # 
      oCddValsDf <- oCddValsDf %>% mutate(vuln =
                                            case_when(oCddValsDf >= as.numeric(cdd_mag[m, 5])  ~ "5", 
                                                      (oCddValsDf < as.numeric(cdd_mag[m, 5]) & oCddValsDf >= as.numeric(cdd_mag[m, 4])) ~ "4",
                                                      (oCddValsDf < as.numeric(cdd_mag[m, 4]) & oCddValsDf >= as.numeric(cdd_mag[m, 3])) ~ "3",
                                                      (oCddValsDf < as.numeric(cdd_mag[m, 3]) & oCddValsDf >= as.numeric(cdd_mag[m, 2])) ~ "2",
                                                      oCddValsDf <= as.numeric(cdd_mag[m, 2]) ~ "1")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oCddValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirHCdd, "/cdd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirHCdd, paste0("cdd_", ctrName, "_", m, "_", enos, "_mag"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg
      oCddVals <- extract(dtsRs, ctrMsk)
      # oCddValsAvg <- round(unlist(lapply(oCddVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oCddVuln <- data.frame(oCddVuln=unlist(lapply(oCddVals, FUN=mean)))
      
      oCddVuln_mean <- oCddVuln
      
      oCddVuln <- oCddVuln %>% mutate(vuln =
                                        case_when(oCddVuln >= as.numeric(cdd_mag[m, 5])  ~ "5", 
                                                  (oCddVuln < as.numeric(cdd_mag[m, 5]) & oCddVuln >= as.numeric(cdd_mag[m, 4])) ~ "4",
                                                  (oCddVuln < as.numeric(cdd_mag[m, 4]) & oCddVuln >= as.numeric(cdd_mag[m, 3])) ~ "3",
                                                  (oCddVuln < as.numeric(cdd_mag[m, 3]) & oCddVuln >= as.numeric(cdd_mag[m, 2])) ~ "2",
                                                  oCddVuln <= as.numeric(cdd_mag[m, 2]) ~ "1")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, cdd=round(oCddVuln$oCddVuln), vuln=as.numeric(oCddVuln$vuln))
      writeOGR(ctrMsk, oIDirHCdd, paste0("cdd_", ctrName, "_", m, "_", enos, "_mun"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . CDD Month ", m, " ", enos, "done\n")
      
      names(cdd_mag) <- mag_labels
      write.csv(cdd_mag, paste0(oIDirHCdd, "/cdd_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
      
    }
    
    cat(" . CDD Month ", m, "done\n")
    
    
    
  } else {
    
    cat(" . CDD Month ", m, "done\n")
    
  }
  
}


## Recent past ##

## CDD output directory recent past
oIDirR <- paste0(oIDir, "/recent-past")
oIDirRCdd <- paste0(oIDirR, "/cdd")
if (!file.exists(paste0(oIDirRCdd))) {dir.create(paste0(oIDirRCdd), recursive = TRUE)}

## CDD Calcs all years all months
cat(">. Calculating CDD Recent Past ", ctrName, "\n")
oCddR <- paste0(oIDirRCdd, "/cdd_", ctrName)

elnino_r <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
lanina_r <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
normal_r <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)

## Load Mask (Adm0)
ctrMsk0 <- raster(rsMsk)

for (m in 1:12){
  
  if (!file.exists(paste0(oCddR, "_", m, "_normal.shp"))) {
    
    cat(" . CDD Month ", m, "processing\n")
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m_r <- subset(elnino_r, elnino_r$Month == m)
    lanina_m_r <- subset(lanina_r, lanina_r$Month == m)
    normal_m_r <- subset(normal_r, normal_r$Month == m)
    
    ## Calculate mean consecutive dry days length by condition
    cdd_elnino_r <- mean(stack(paste0(oCddW, "_", elnino_m_r$Year, "_", m, ".nc")))
    cdd_lanina_r <- mean(stack(paste0(oCddW, "_", lanina_m_r$Year, "_", m, ".nc")))
    cdd_normal_r <- mean(stack(paste0(oCddW, "_", normal_m_r$Year, "_", m, ".nc")))
    
    writeRaster(cdd_elnino_r, paste0(oCddR, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(cdd_lanina_r, paste0(oCddR, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(cdd_normal_r, paste0(oCddR, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    cdd_mag <- read.csv(paste0(oIDirHCdd, "/cdd_", ctrName, "_", m, "_normal_mag_class", ".csv"))
    
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "cdd"
      varLn <- "Consecutive.dry.days"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oCddR, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirRCdd, paste0("cdd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oCddValsDf<- data.frame(getValues(dtsRs))
      oCddValsDf <- oCddValsDf %>% mutate(vuln =
                                            case_when(oCddValsDf >= as.numeric(cdd_mag[m, 5])  ~ "5", 
                                                      (oCddValsDf < as.numeric(cdd_mag[m, 5]) & oCddValsDf >= as.numeric(cdd_mag[m, 4])) ~ "4",
                                                      (oCddValsDf < as.numeric(cdd_mag[m, 4]) & oCddValsDf >= as.numeric(cdd_mag[m, 3])) ~ "3",
                                                      (oCddValsDf < as.numeric(cdd_mag[m, 3]) & oCddValsDf >= as.numeric(cdd_mag[m, 2])) ~ "2",
                                                      oCddValsDf <= as.numeric(cdd_mag[m, 2]) ~ "1")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oCddValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirRCdd, "/cdd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirRCdd, paste0("cdd_", ctrName, "_", m, "_", enos, "_mag"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg
      oCddVals <- extract(dtsRs, ctrMsk)
      # oCddValsAvg <- round(unlist(lapply(oCddVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oCddVuln <- data.frame(oCddVuln=unlist(lapply(oCddVals, FUN=mean)))
      oCddVuln <- oCddVuln %>% mutate(vuln =
                                        case_when(oCddVuln >= as.numeric(cdd_mag[m, 5])  ~ "5", 
                                                  (oCddVuln < as.numeric(cdd_mag[m, 5]) & oCddVuln >= as.numeric(cdd_mag[m, 4])) ~ "4",
                                                  (oCddVuln < as.numeric(cdd_mag[m, 4]) & oCddVuln >= as.numeric(cdd_mag[m, 3])) ~ "3",
                                                  (oCddVuln < as.numeric(cdd_mag[m, 3]) & oCddVuln >= as.numeric(cdd_mag[m, 2])) ~ "2",
                                                  oCddVuln <= as.numeric(cdd_mag[m, 2]) ~ "1")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, cdd=round(oCddVuln$oCddVuln), vuln=as.numeric(oCddVuln$vuln))
      writeOGR(ctrMsk, oIDirRCdd, paste0("cdd_", ctrName, "_", m, "_", enos, "_mun"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . CDD Month ", m, " ", enos, "done\n")
      
    }
    
    
    cat(" . CDD Month ", m, "done\n")
    
  } else {
    
    cat(" . CDD Month ", m, "done\n")
    
  }
  
}

cat(">. CDD calcs done", "\n")


##################################################
## DRD. Dry days calculation                   ###
## Vulnerability type: Drought                 ###
## Data Source: CHIRPS daily                   ###
##################################################

## Identify historical ENOS condition
ensoCond <- read.csv(ensoFile, header=T, row.names = 1)
ensoCond <- as.vector(cbind(Year=rownames(ensoCond)[row(ensoCond)], setNames(melt(ensoCond), c('Month', 'Values'))))
ensoCond$Month <- gsub("X", "", ensoCond$Month)
elnino <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
lanina <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
normal <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)

## DRD output work directory
oBDirW <- paste0(oBDir, "/indices")
if (!file.exists(oBDirW)) {dir.create(oBDirW, recursive = TRUE)}

## DRD output directory
oIDirH <- paste0(oIDir, "/historical")
oIDirHDrd <- paste0(oIDirH, "/drd")
if (!file.exists(paste0(oIDirHDrd))) {dir.create(paste0(oIDirHDrd), recursive = TRUE)}

ctrMsk0 <- raster(rsMsk)


## Historical ##

## DRD Calcs all years all months
cat(">. Calculating DRD ", ctrName, " historical\n")

iNc <- paste0(oBDir, "/daily/chirps-v2.0.", yi, "-", yf, "_daily")
oDrdW <- paste0(oBDirW, "/drd_", ctrName)
oWetW <- paste0(oBDirW, "/wet_", ctrName)
oDrd <- paste0(oIDirHDrd, "/drd_", ctrName)

## Load Mask (Adm0)

drd_mag <- data.frame()

for (m in 1:12){
  
  if (!file.exists(paste0(oDrd, "_", m, "_normal.shp"))) {
    
    for (yr in yi:yf){
      
      if (!file.exists(paste0(oWetW, "_", yr, "_", m, ".nc"))) {
        
        ## Calc wet days days for each year/month
        system(paste0(dircdo," -s -eca_pd,1 -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), ".nc", " ", oWetW, "_", yr, "_", m, ".nc"))
        
      }
    }
    
    ## Load CHIRPS data and stack
    drdStk_yrs <- abs( stack(paste0(oWetW, "_", yi:yf, "_", m, ".nc")) - ndays[m] )
    writeRaster(drdStk_yrs, paste0(oDrdW, "_", yi, "-", yf, "_", m, ".nc"), format="CDF", overwrite=T)
    
    # Quantiles by month
    q <- paste(summary(quantile(drdStk_yrs, probs = probs_q2, names = FALSE, na.rm=TRUE)))[c(4,10,16,22)]
    drd_mag <- rbind(drd_mag, c(m, round(as.numeric(gsub("  ", "", gsub("Mean   :","", q))))))
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m <- subset(elnino, elnino$Month == m)
    lanina_m <- subset(lanina, lanina$Month == m)
    normal_m <- subset(normal, normal$Month == m)
    
    ## Calculate mean consecutive dry days length by condition
    drd_elnino <- mean( abs(stack(paste0(oWetW, "_", elnino_m$Year, "_", m, ".nc")) - ndays[m]) )
    drd_lanina <- mean( abs(stack(paste0(oWetW, "_", lanina_m$Year, "_", m, ".nc")) - ndays[m]) )
    drd_normal <- mean( abs(stack(paste0(oWetW, "_", normal_m$Year, "_", m, ".nc")) - ndays[m]) )
    
    writeRaster(drd_elnino, paste0(oDrd, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(drd_lanina, paste0(oDrd, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(drd_normal, paste0(oDrd, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "drd"
      varLn <- "Dry.days"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oDrd, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirHDrd, paste0("drd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oDrdValsDf<- data.frame(getValues(dtsRs))
      oDrdValsDf <- oDrdValsDf %>% mutate(vuln =
                                            case_when(oDrdValsDf >= as.numeric(drd_mag[m, 5])  ~ "5", 
                                                      (oDrdValsDf < as.numeric(drd_mag[m, 5]) & oDrdValsDf >= as.numeric(drd_mag[m, 4])) ~ "4",
                                                      (oDrdValsDf < as.numeric(drd_mag[m, 4]) & oDrdValsDf >= as.numeric(drd_mag[m, 3])) ~ "3",
                                                      (oDrdValsDf < as.numeric(drd_mag[m, 3]) & oDrdValsDf >= as.numeric(drd_mag[m, 2])) ~ "2",
                                                      oDrdValsDf <= as.numeric(drd_mag[m, 2]) ~ "1")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oDrdValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirHDrd, "/drd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirHDrd, paste0("drd_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oDrdVals <- extract(raster(paste0(oDrd, "_", m, "_", enos, ".tif")), ctrMsk)
      # oDrdValsAvg <- round(unlist(lapply(oDrdVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oDrdVuln <- data.frame(oDrdVuln=unlist(lapply(oDrdVals, FUN=mean)))
      oDrdVuln <- oDrdVuln %>% mutate(vuln =
                                        case_when(oDrdVuln >= as.numeric(drd_mag[m, 5])  ~ "5", 
                                                  (oDrdVuln < as.numeric(drd_mag[m, 5]) & oDrdVuln >= as.numeric(drd_mag[m, 4])) ~ "4",
                                                  (oDrdVuln < as.numeric(drd_mag[m, 4]) & oDrdVuln >= as.numeric(drd_mag[m, 3])) ~ "3",
                                                  (oDrdVuln < as.numeric(drd_mag[m, 3]) & oDrdVuln >= as.numeric(drd_mag[m, 2])) ~ "2",
                                                  oDrdVuln <= as.numeric(drd_mag[m, 2]) ~ "1")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, drd=round(oDrdVuln$oDrdVuln), vuln=as.numeric(oDrdVuln$vuln))
      writeOGR(ctrMsk, oIDirHDrd, paste0("drd_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . DRD Month ", m, " ", enos, "done\n")
      
    }
    
    names(drd_mag) <- mag_labels
    write.csv(drd_mag, paste0(oIDirHDrd, "/drd_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
    
    
    cat(" . DRD Month ", m, "done\n")
    
  } else {
    
    cat(" . DRD Month ", m, "done\n")
    
  }
  
}



## Recent past ##


## DRD output directory
oIDirR <- paste0(oIDir, "/recent-past")
oIDirRDrd <- paste0(oIDirR, "/drd")
if (!file.exists(paste0(oIDirRDrd))) {dir.create(paste0(oIDirRDrd), recursive = TRUE)}

oDrdR <- paste0(oIDirRDrd, "/drd_", ctrName)
cat(">. Calculating DRD ", ctrName, " rencent-past\n")

elnino_r <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
lanina_r <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
normal_r <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)


## Load Mask (Adm0)
ctrMsk0 <- raster(rsMsk)

for (m in 1:12){
  
  if (!file.exists(paste0(oDrdR, "_", m, "_normal.shp"))) {
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m_r <- subset(elnino_r, elnino_r$Month == m)
    lanina_m_r <- subset(lanina_r, lanina_r$Month == m)
    normal_m_r <- subset(normal_r, normal_r$Month == m)
    
    ## Calculate mean consecutive dry days length by condition
    drd_elnino_r <- mean( abs(stack(paste0(oWetW, "_", elnino_m_r$Year, "_", m, ".nc")) - ndays[m]) )
    drd_lanina_r <- mean( abs(stack(paste0(oWetW, "_", lanina_m_r$Year, "_", m, ".nc")) - ndays[m]) )
    drd_normal_r <- mean( abs(stack(paste0(oWetW, "_", normal_m_r$Year, "_", m, ".nc")) - ndays[m]) )
    
    writeRaster(drd_elnino_r, paste0(oDrdR, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(drd_lanina_r, paste0(oDrdR, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(drd_normal_r, paste0(oDrdR, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    drd_mag <- read.csv(paste0(oIDirHDrd, "/drd_", ctrName, "_", m, "_normal_mag_class", ".csv"))
    
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "drd"
      varLn <- "Dry.days"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oDrdR, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirRDrd, paste0("drd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oDrdValsDf<- data.frame(getValues(dtsRs))
      oDrdValsDf <- oDrdValsDf %>% mutate(vuln =
                                            case_when(oDrdValsDf >= as.numeric(drd_mag[m, 5])  ~ "5", 
                                                      (oDrdValsDf < as.numeric(drd_mag[m, 5]) & oDrdValsDf >= as.numeric(drd_mag[m, 4])) ~ "4",
                                                      (oDrdValsDf < as.numeric(drd_mag[m, 4]) & oDrdValsDf >= as.numeric(drd_mag[m, 3])) ~ "3",
                                                      (oDrdValsDf < as.numeric(drd_mag[m, 3]) & oDrdValsDf >= as.numeric(drd_mag[m, 2])) ~ "2",
                                                      oDrdValsDf <= as.numeric(drd_mag[m, 2]) ~ "1")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oDrdValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirRDrd, "/drd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirRDrd, paste0("drd_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oDrdVals <- extract(raster(paste0(oDrdR, "_", m, "_", enos, ".tif")), ctrMsk)
      # oDrdValsAvg <- round(unlist(lapply(oDrdVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oDrdVuln <- data.frame(oDrdVuln=unlist(lapply(oDrdVals, FUN=mean)))
      oDrdVuln <- oDrdVuln %>% mutate(vuln =
                                        case_when(oDrdVuln >= as.numeric(drd_mag[m, 5])  ~ "5", 
                                                  (oDrdVuln < as.numeric(drd_mag[m, 5]) & oDrdVuln >= as.numeric(drd_mag[m, 4])) ~ "4",
                                                  (oDrdVuln < as.numeric(drd_mag[m, 4]) & oDrdVuln >= as.numeric(drd_mag[m, 3])) ~ "3",
                                                  (oDrdVuln < as.numeric(drd_mag[m, 3]) & oDrdVuln >= as.numeric(drd_mag[m, 2])) ~ "2",
                                                  oDrdVuln <= as.numeric(drd_mag[m, 2]) ~ "1")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, drd=round(oDrdVuln$oDrdVuln), vuln=as.numeric(oDrdVuln$vuln))
      writeOGR(ctrMsk, oIDirRDrd, paste0("drd_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . DRD Month ", m, " ", enos, "done\n")
      
    }
    
    names(drd_mag) <- mag_labels2
    write.csv(drd_mag, paste0(oIDirRDrd, "/drd_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
    
    
    cat(" . DRD Month ", m, "done\n")
    
  } else {
    
    cat(" . DRD Month ", m, "done\n")
    
  }
  
}


cat(">. DRD calcs done", "\n")




##################################################
## P95. Very Wet days (95th percentile)        ###
## Vulnerability type: Storms                  ###
## Data Source: CHIRPS daily                   ###
##################################################

## Identify historical ENOS condition
ensoCond <- read.csv(ensoFile, header=T, row.names = 1)
ensoCond <- as.vector(cbind(Year=rownames(ensoCond)[row(ensoCond)], setNames(melt(ensoCond), c('Month', 'Values'))))
ensoCond$Month <- gsub("X", "", ensoCond$Month)
elnino <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
lanina <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
normal <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)

## P95 output work directory
oBDirW <- paste0(oBDir, "/indices")
if (!file.exists(oBDirW)) {dir.create(oBDirW, recursive = TRUE)}

## P95 output directory
oIDirH <- paste0(oIDir, "/historical")
oIDirHP95 <- paste0(oIDirH, "/p95")
if (!file.exists(paste0(oIDirHP95))) {dir.create(paste0(oIDirHP95), recursive = TRUE)}


## P95 Calcs all years all months
cat(">. Calculating P95 ", ctrName, "\n")

iNc <- paste0(oBDir, "/daily/chirps-v2.0.", yi, "-", yf, "_daily")
oP95WRef <- paste0(oBDirW, "/p95_", yi, "-", yf, "_", ctrName)
oP95W <- paste0(oBDirW, "/p95_", ctrName)
oP95 <- paste0(oIDirHP95, "/p95_", ctrName)

ctrMsk0 <- raster(rsMsk)


if(bigctr == "yes"){
  
  if (!file.exists(paste0(oP95WRef, "_noleap", "_12.nc"))) {
    
    for (m in 1:12){
      
      if (!file.exists(paste0(iNc, "_wet", "_12.nc"))) {
        
        ## Calc time-series wet days
        system(paste0(dircdo," -s setrtomiss,0,0.999 ", iNc, "_", sprintf("%02d", m), ".nc", " ", iNc, "_wet_", sprintf("%02d", m), ".nc"))
        
      }
      
      system(paste0(dircdo," -s -sellonlatbox,", xmin(extent(ctrMsk0)), ",", (xmin(extent(ctrMsk0))+xmax(extent(ctrMsk0)))/2, ",", ymin(extent(ctrMsk0)), ",", ymax(extent(ctrMsk0)), " ", 
                    iNc, "_", sprintf("%02d", m), ".nc", " ", 
                    iNc, "_", sprintf("%02d", m), "_a.nc"))
      
      system(paste0(dircdo," -s -sellonlatbox,", (xmin(extent(ctrMsk0))+xmax(extent(ctrMsk0)))/2, ",", xmax(extent(ctrMsk0)), ",", ymin(extent(ctrMsk0)), ",", ymax(extent(ctrMsk0)), " ", 
                    iNc, "_", sprintf("%02d", m), ".nc", " ", 
                    iNc, "_", sprintf("%02d", m), "_b.nc"))
      
      
      system(paste0(dircdo," -s -sellonlatbox,", xmin(extent(ctrMsk0)), ",", (xmin(extent(ctrMsk0))+xmax(extent(ctrMsk0)))/2, ",", ymin(extent(ctrMsk0)), ",", ymax(extent(ctrMsk0)), " ", 
                    iNc, "_wet_", sprintf("%02d", m), ".nc", " ", 
                    iNc, "_wet_", sprintf("%02d", m), "_a.nc"))
      
      system(paste0(dircdo," -s -sellonlatbox,", (xmin(extent(ctrMsk0))+xmax(extent(ctrMsk0)))/2, ",", xmax(extent(ctrMsk0)), ",", ymin(extent(ctrMsk0)), ",", ymax(extent(ctrMsk0)), " ", 
                    iNc, "_wet_", sprintf("%02d", m), ".nc", " ", 
                    iNc, "_wet_", sprintf("%02d", m), "_b.nc"))
      
      if (m == 2){
        # Calc yday p95 ref. period of wet days time-series
        system(paste0(dircdo," -s ydrunpctl,95,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_a.nc", " ",
                      "-ydrunmin,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_a.nc", " ",
                      "-ydrunmax,5 ", iNc, "_wet_", sprintf("%02d", m), "_a.nc", " ",
                      oP95WRef, "_leap_", sprintf("%02d", m), "_a.nc"))
        
        # Calc yday p95 ref. period of wet days time-series
        system(paste0(dircdo," -s ydrunpctl,95,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_b.nc", " ",
                      "-ydrunmin,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_b.nc", " ",
                      "-ydrunmax,5 ", iNc, "_wet_", sprintf("%02d", m), "_b.nc", " ",
                      oP95WRef, "_leap_", sprintf("%02d", m), "_b.nc"))
        
        system(paste0(dircdo," -s delete,month=2,day=29 ", oP95WRef, "_leap_", sprintf("%02d", m), "_a.nc", " ",
                      oP95WRef, "_",  sprintf("%02d", m),"_a.nc"))
        
        system(paste0(dircdo," -s delete,month=2,day=29 ", oP95WRef, "_leap_", sprintf("%02d", m), "_b.nc", " ",
                      oP95WRef, "_",  sprintf("%02d", m),"_b.nc"))
        
        
      } else {
        system(paste0(dircdo," -s ydrunpctl,95,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_a.nc", " ",
                      "-ydrunmin,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_a.nc", " ",
                      "-ydrunmax,5 ", iNc, "_wet_", sprintf("%02d", m), "_a.nc", " ",
                      oP95WRef, "_", sprintf("%02d", m), "_a.nc"))
        
        system(paste0(dircdo," -s ydrunpctl,95,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_b.nc", " ",
                      "-ydrunmin,5 ", iNc, "_wet_", sprintf("%02d", m) ,"_b.nc", " ",
                      "-ydrunmax,5 ", iNc, "_wet_", sprintf("%02d", m), "_b.nc", " ",
                      oP95WRef, "_", sprintf("%02d", m), "_b.nc"))
        
      }
      
      
      
    }
    
  }
  
} else {
  
  if (!file.exists(paste0(oP95WRef, "_noleap", "_12.nc"))) {
    
    for (m in 1:12){
      
      if (!file.exists(paste0(iNc, "_wet", "_12.nc"))) {
        
        ## Calc time-series wet days
        system(paste0(dircdo," -s setrtomiss,0,0.999 ", iNc, "_", sprintf("%02d", m), ".nc", " ", iNc, "_wet_", sprintf("%02d", m), ".nc"))
        
      }
      
      
      if (m == 2){
        # Calc yday p95 ref. period of wet days time-series
        system(paste0(dircdo," -s ydrunpctl,95,5 ", iNc, "_wet_", sprintf("%02d", m) ,".nc", " ",
                      "-ydrunmin,5 ", iNc, "_wet_", sprintf("%02d", m) ,".nc", " ",
                      "-ydrunmax,5 ", iNc, "_wet_", sprintf("%02d", m), ".nc", " ",
                      oP95WRef, "_leap_", sprintf("%02d", m), ".nc"))
        system(paste0(dircdo," -s delete,month=2,day=29 ", oP95WRef, "_leap_", sprintf("%02d", m), ".nc", " ",
                      oP95WRef, "_",  sprintf("%02d", m),".nc"))
      } else {
        system(paste0(dircdo," -s ydrunpctl,95,5 ", iNc, "_wet_", sprintf("%02d", m) ,".nc", " ",
                      "-ydrunmin,5 ", iNc, "_wet_", sprintf("%02d", m) ,".nc", " ",
                      "-ydrunmax,5 ", iNc, "_wet_", sprintf("%02d", m), ".nc", " ",
                      oP95WRef, "_", sprintf("%02d", m), ".nc"))
      }
      
      
      
    }
    
  }
  
}



## Historical

p95_mag <- data.frame()

## Loop across months
for (m in 1:12){
  
  if (!file.exists(paste0(oIDirHP95, "/p95_", ctrName, "_", m, "_normal.shp"))) {
    
    ## Calc % p95 for every year in month m
    
    for (yr in yi:yf){
      
      if(bigctr == "yes"){
        
        if (leap_year(yr) == T && m == 2) {
          
          if (!file.exists(paste0(oP95W, "_", yr, "_", m, ".nc"))) {
            
            system(paste0(dircdo," -s eca_r95p -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), "_a.nc", " ",
                          " ", oP95WRef, "_leap_", sprintf("%02d", m), "_a.nc", " ", oP95W, "_", yr, "_", m, "_a.nc"))
            
            system(paste0(dircdo," -s eca_r95p -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), "_b.nc", " ",
                          " ", oP95WRef, "_leap_", sprintf("%02d", m), "_b.nc", " ", oP95W, "_", yr, "_", m, "_b.nc"))
            
          }
          
        } else {
          
          if (!file.exists(paste0(oP95W, "_", yr, "_", m, ".nc"))) {
            
            system(paste0(dircdo," -s eca_r95p -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), "_a.nc", " ",
                          " ", oP95WRef, "_", sprintf("%02d", m), "_a.nc", " ", oP95W, "_", yr, "_", m, "_a.nc"))
            
            system(paste0(dircdo," -s eca_r95p -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), "_b.nc", " ",
                          " ", oP95WRef, "_", sprintf("%02d", m), "_b.nc", " ", oP95W, "_", yr, "_", m, "_b.nc"))
            
          }
          
        }
        
        writeRaster(merge(raster(paste0(oP95W, "_", yr, "_", m, "_a.nc")), raster(paste0(oP95W, "_", yr, "_", m, "_b.nc"))), 
                    paste0(oP95W, "_", yr, "_", m, ".nc"), overwrite=T)
        file.remove(paste0(oP95W, "_", yr, "_", m, "_a.nc"))
        file.remove(paste0(oP95W, "_", yr, "_", m, "_b.nc"))
        
      } else {
        
        if (leap_year(yr) == T && m == 2) {
          
          if (!file.exists(paste0(oP95W, "_", yr, "_", m, ".nc"))) {
            
            system(paste0(dircdo," -s eca_r95p -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), ".nc", " ",
                          " ", oP95WRef, "_leap_", sprintf("%02d", m), ".nc", " ", oP95W, "_", yr, "_", m, ".nc"))
          }
          
        } else {
          
          if (!file.exists(paste0(oP95W, "_", yr, "_", m, ".nc"))) {
            
            system(paste0(dircdo," -s eca_r95p -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), ".nc", " ",
                          " ", oP95WRef, "_", sprintf("%02d", m), ".nc", " ", oP95W, "_", yr, "_", m, ".nc"))
          }
          
        }
        
      }
      
    }
    
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m <- subset(elnino, elnino$Month == m)
    lanina_m <- subset(lanina, lanina$Month == m)
    normal_m <- subset(normal, normal$Month == m)
    
    if (!file.exists(paste0(oP95, "_", m, "_normal.tif"))) {
      
      ## Calculate mean consecutive dry days length by condition
      p95_elnino <- mean(stack(paste0(oP95W, "_", elnino_m$Year, "_", m, ".nc")), na.rm=T)
      p95_lanina <- mean(stack(paste0(oP95W, "_", lanina_m$Year, "_", m, ".nc")), na.rm=T)
      p95_normal <- mean(stack(paste0(oP95W, "_", normal_m$Year, "_", m, ".nc")), na.rm=T)
      
      p95_elnino[is.na(p95_elnino[])] <- 0 
      p95_lanina[is.na(p95_lanina[])] <- 0 
      p95_normal[is.na(p95_normal[])] <- 0 
      
      writeRaster(p95_elnino * ctrMsk0, paste0(oP95, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      writeRaster(p95_lanina * ctrMsk0, paste0(oP95, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      writeRaster(p95_normal * ctrMsk0, paste0(oP95, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      
    }
    
    ## Convert to tif and shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "p95"
      varLn <- "Pecentile.95th.precipitation"
      unit <- "percent"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oP95, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirHP95, paste0("p95_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oP95ValsDf<- data.frame(getValues(dtsRs))
      oP95ValsDf <- oP95ValsDf %>% mutate(vuln =
                                            case_when(oP95ValsDf <= 5 ~ "1", 
                                                      (oP95ValsDf > 5 & oP95ValsDf <= 10) ~ "2",
                                                      (oP95ValsDf > 10 & oP95ValsDf <= 15) ~ "3",
                                                      (oP95ValsDf > 15 & oP95ValsDf <= 20) ~ "4",
                                                      oP95ValsDf > 20  ~ "5")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oP95ValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirHP95, "/p95_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirHP95, paste0("p95_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oP95Vals <- extract(raster(paste0(oP95, "_", m, "_", enos, ".tif")), ctrMsk)
      # oP95ValsAvg <- round(unlist(lapply(oP95Vals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      p95_mag <- c(m, 5, 10, 15, 20)
      oP95Vuln <- data.frame(oP95Vuln=unlist(lapply(oP95Vals, FUN=mean)))
      oP95Vuln <- oP95Vuln %>% mutate(vuln =
                                        case_when(is.na(oP95Vuln) ~ "1",
                                                  oP95Vuln <= 5 ~ "1", 
                                                  (oP95Vuln > 5 & oP95Vuln <= 10) ~ "2",
                                                  (oP95Vuln > 10 & oP95Vuln <= 15) ~ "3",
                                                  (oP95Vuln > 15 & oP95Vuln <= 20) ~ "4",
                                                  oP95Vuln > 20  ~ "5")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, p95=oP95Vuln$oP95Vuln, vuln=as.numeric(oP95Vuln$vuln))
      writeOGR(ctrMsk, oIDirHP95, paste0("p95_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . P95 Month ", m, " ", enos, "done\n")
      
    }
    
    names(p95_mag) <- c("Month", "5", "10", "15", "20")
    write.csv(p95_mag, paste0(oIDirHP95, "/p95_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
    
    cat(" . P95 Month ", m, "done\n")
    
  } else {
    
    cat(" . P95 Month ", m, "done\n")
    
  }
  
}

cat(">. P95 calcs done", "\n")



## Recent past

## P95 output directory
oIDirR <- paste0(oIDir, "/recent-past")
oIDirRP95 <- paste0(oIDirR, "/p95")
if (!file.exists(paste0(oIDirRP95))) {dir.create(paste0(oIDirRP95), recursive = TRUE)}

oP95R <- paste0(oIDirRP95, "/p95_", ctrName)

elnino_r <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
lanina_r <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
normal_r <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)

## Loop across months
for (m in 1:12){
  
  if (!file.exists(paste0(oIDirRP95, "/p95_", ctrName, "_", m, "_normal.shp"))) {
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m_r <- subset(elnino_r, elnino_r$Month == m)
    lanina_m_r <- subset(lanina_r, lanina_r$Month == m)
    normal_m_r <- subset(normal_r, normal_r$Month == m)
    
    if (!file.exists(paste0(oP95R, "_", m, "_normal.tif"))) {
      
      ## Calculate mean consecutive dry days length by condition
      p95_elnino_r <- mean(stack(paste0(oP95W, "_", elnino_m_r$Year, "_", m, ".nc")), na.rm=T)
      p95_lanina_r <- mean(stack(paste0(oP95W, "_", lanina_m_r$Year, "_", m, ".nc")), na.rm=T)
      p95_normal_r <- mean(stack(paste0(oP95W, "_", normal_m_r$Year, "_", m, ".nc")), na.rm=T)
      
      p95_elnino_r[is.na(p95_elnino_r[])] <- 0 
      p95_lanina_r[is.na(p95_lanina_r[])] <- 0 
      p95_normal_r[is.na(p95_normal_r[])] <- 0 
      
      writeRaster(p95_elnino_r * ctrMsk0, paste0(oP95R, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      writeRaster(p95_lanina_r * ctrMsk0, paste0(oP95R, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      writeRaster(p95_normal_r * ctrMsk0, paste0(oP95R, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      
    }
    
    ## Convert to tif and shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "p95"
      varLn <- "Pecentile.95th.precipitation"
      unit <- "percent"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oP95R, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirRP95, paste0("p95_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oP95ValsDf<- data.frame(getValues(dtsRs))
      oP95ValsDf <- oP95ValsDf %>% mutate(vuln =
                                            case_when(oP95ValsDf <= 5 ~ "1", 
                                                      (oP95ValsDf > 5 & oP95ValsDf <= 10) ~ "2",
                                                      (oP95ValsDf > 10 & oP95ValsDf <= 15) ~ "3",
                                                      (oP95ValsDf > 15 & oP95ValsDf <= 20) ~ "4",
                                                      oP95ValsDf > 20  ~ "5")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs
      values(dtsRs_vuln) <- as.numeric(oP95ValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln, paste0(oIDirRP95, "/p95_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirRP95, paste0("p95_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oP95Vals <- extract(raster(paste0(oP95R, "_", m, "_", enos, ".tif")), ctrMsk)
      # oP95ValsAvg <- round(unlist(lapply(oP95Vals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      p95_mag <- c(m, 5, 10, 15, 20)
      oP95Vuln <- data.frame(oP95Vuln=unlist(lapply(oP95Vals, FUN=mean)))
      oP95Vuln <- oP95Vuln %>% mutate(vuln =
                                        case_when(is.na(oP95Vuln) ~ "1",
                                                  oP95Vuln <= 5 ~ "1", 
                                                  (oP95Vuln > 5 & oP95Vuln <= 10) ~ "2",
                                                  (oP95Vuln > 10 & oP95Vuln <= 15) ~ "3",
                                                  (oP95Vuln > 15 & oP95Vuln <= 20) ~ "4",
                                                  oP95Vuln > 20  ~ "5")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, p95=oP95Vuln$oP95Vuln, vuln=as.numeric(oP95Vuln$vuln))
      writeOGR(ctrMsk, oIDirRP95, paste0("p95_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . P95 Month ", m, " ", enos, "done\n")
      
    }
    
    cat(" . P95 Month ", m, "done\n")
    
  } else {
    
    cat(" . P95 Month ", m, "done\n")
    
  }
  
}

cat(">. P95 calcs done", "\n")





##################################################
## FRD. Frost days calculation                 ###
## Vulnerability type: Frost                   ###
## Data Source: CHIRTS daily                   ###
##################################################

## Identify historical ENOS condition
ensoCond <- read.csv(ensoFile, header=T, row.names = 1)
ensoCond <- as.vector(cbind(Year=rownames(ensoCond)[row(ensoCond)], setNames(melt(ensoCond), c('Month', 'Values'))))
ensoCond$Month <- gsub("X", "", ensoCond$Month)
elnino <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
lanina <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
normal <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)

## FRD output work directory
oBDirW <- paste0(oBDir, "/indices")
if (!file.exists(oBDirW)) {dir.create(oBDirW, recursive = TRUE)}

## FRD output directory
oIDirH <- paste0(oIDir, "/historical")
oIDirHFrd <- paste0(oIDirH, "/frd")
if (!file.exists(paste0(oIDirHFrd))) {dir.create(paste0(oIDirHFrd), recursive = TRUE)}


## FRD Calcs all years all months
cat(">. Calculating FRD ", ctrName, "\n")

iNc <- paste0(oBDir, "/daily/Tmin.", yi, "-", yf, "_daily")
oFrdW <- paste0(oBDirW, "/frd_", ctrName)
oFrd <- paste0(oIDirHFrd, "/frd_", ctrName)

# ## Load Mask (Adm0)
# rsMsk <- raster(paste0(oBDir, "/daily-", ctrName, "/chirps-v2.0.", yi, ".", sprintf("%02d", 1), ".01.nc")) * 0 + 1
ctrMsk0 <- raster(rsMsk)


## Historical

# frd_nfeatures <- data.frame()

for (m in 1:12){
  
  if (!file.exists(paste0(oFrd, "_", m, "_normal.shp"))) {
    
    for (yr in yi:yf){
      
      if (!file.exists(paste0(oFrdW, "_", yr, "_", m, ".nc"))){
        
        ## Calc frost days each year/month (units in K)
        system(paste0(dircdo," -s -eca_fd -addc,273.15 -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), ".nc", " ", oFrdW, "_", yr, "_", m, ".nc"))
        
      }
      
    }
    
    ## Load CHIRTS data and stack
    # frdStk_yrs <- stack(paste0(oFrdW, "_", yi:yf, "_", m, ".nc"))
    frdStk_yrs <- resample(stack(paste0(oFrdW, "_", yi:yf, "_", m, ".nc")), ctrMsk0)
    
    frdStk_yrs[which(frdStk_yrs[]>15)]=0
    writeRaster(frdStk_yrs, paste0(oFrdW, "_", yi, "-", yf, "_", m, ".nc"), format="CDF", overwrite=T)
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m <- subset(elnino, elnino$Month == m)
    lanina_m <- subset(lanina, lanina$Month == m)
    normal_m <- subset(normal, normal$Month == m)
    
    ## Calculate mean frost days  by condition
    frd_elnino <- resample(mean( stack(paste0(oFrdW, "_", elnino_m$Year, "_", m, ".nc")) ), ctrMsk0)
    frd_lanina <- resample(mean( stack(paste0(oFrdW, "_", lanina_m$Year, "_", m, ".nc")) ), ctrMsk0)
    frd_normal <- resample(mean( stack(paste0(oFrdW, "_", normal_m$Year, "_", m, ".nc")) ), ctrMsk0)
    
    frd_elnino[which(frd_elnino[]>15)]=0
    frd_lanina[which(frd_lanina[]>15)]=0
    frd_normal[which(frd_normal[]>15)]=0
    
    frd_elnino[is.na(frd_elnino[])] <- 0 
    frd_lanina[is.na(frd_lanina[])] <- 0 
    frd_normal[is.na(frd_normal[])] <- 0 
    
    writeRaster(frd_elnino * ctrMsk0, paste0(oFrd, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(frd_lanina * ctrMsk0, paste0(oFrd, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(frd_normal * ctrMsk0, paste0(oFrd, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "frd"
      varLn <- "Frost.days"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oFrd, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirHFrd, paste0("frd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      # frd_nfeatures <- rbind(frd_nfeatures, paste0(" frd_ecu_", m, "_", enos, "_mag.shp has ", nrow(dtsRsShp), " features"))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      frd_mag <- c(m, 0, 2, 4, 6)
      
      dtsRs[is.na(dtsRs[])] <- 0
      
      oFrdValsDf<- data.frame(getValues(dtsRs* ctrMsk0))
      oFrdValsDf <- oFrdValsDf %>% mutate(vuln =
                                            case_when(oFrdValsDf == 0 ~ "1", 
                                                      (oFrdValsDf <= 2 ) ~ "2",
                                                      (oFrdValsDf > 2 & oFrdValsDf <= 4) ~ "3",
                                                      (oFrdValsDf > 4 & oFrdValsDf <= 6) ~ "4",
                                                      oFrdValsDf > 6  ~ "5")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs * ctrMsk0
      values(dtsRs_vuln) <- as.numeric(oFrdValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln * ctrMsk0, paste0(oIDirHFrd, "/frd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vuln[is.na(dtsRs_vuln[])] <- 1
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln * ctrMsk0)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirHFrd, paste0("frd_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oFrdVals <- extract(raster(paste0(oFrd, "_", m, "_", enos, ".tif")), ctrMsk)
      # oFrdValsAvg <- round(unlist(lapply(oFrdVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      oFrdVuln <- data.frame(oFrdVuln=unlist(lapply(oFrdVals, FUN=mean)))
      oFrdVuln <- oFrdVuln %>% mutate(vuln =
                                        case_when(oFrdVuln == 0 ~ "1", 
                                                  (oFrdVuln <= 2 ) ~ "2",
                                                  (oFrdVuln > 2 & oFrdVuln <= 4) ~ "3",
                                                  (oFrdVuln > 4 & oFrdVuln <= 6) ~ "4",
                                                  oFrdVuln > 6  ~ "5")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, frd=round(oFrdVuln$oFrdVuln), vuln=as.numeric(oFrdVuln$vuln))
      writeOGR(ctrMsk, oIDirHFrd, paste0("frd_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . FRD Month ", m, " ", enos, "done\n")
      
    }
    
    names(frd_mag) <- c("Month", "0", "2", "4", "6")
    write.csv(frd_mag, paste0(oIDirHFrd, "/frd_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
    
    cat(" . FRD Month ", m, "done\n")
    
  } else {
    
    cat(" . FRD Month ", m, "done\n")
    
  }
  
}

# write.csv(frd_nfeatures, paste0(oIDirHFrd, "/_nfeatures_describe.csv"), row.names=F)




## Recent past

## FRD output directory
oIDirR <- paste0(oIDir, "/recent-past")
oIDirRFrd <- paste0(oIDirR, "/frd")
if (!file.exists(paste0(oIDirRFrd))) {dir.create(paste0(oIDirRFrd), recursive = TRUE)}

oFrdR <- paste0(oIDirRFrd, "/frd_", ctrName)

elnino_r <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
lanina_r <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
normal_r <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)


for (m in 1:12){
  
  if (!file.exists(paste0(oFrdR, "_", m, "_normal.shp"))) {
    
    ## Load CHIRPS data and stack
    # frdStk_yrs <- stack(paste0(oFrdW, "_", yi:yf, "_", m, ".nc"))
    frdStk_yrs <- resample(stack(paste0(oFrdW, "_", yi:yf, "_", m, ".nc")), ctrMsk0)
    
    frdStk_yrs[which(frdStk_yrs[]>15)]=0
    writeRaster(frdStk_yrs, paste0(oFrdW, "_", yi, "-", yf, "_", m, ".nc"), format="CDF", overwrite=T)
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m_r <- subset(elnino_r, elnino_r$Month == m)
    lanina_m_r <- subset(lanina_r, lanina_r$Month == m)
    normal_m_r <- subset(normal_r, normal_r$Month == m)
    
    ## Calculate mean frost days  by condition
    frd_elnino_r <- resample(mean( stack(paste0(oFrdW, "_", elnino_m_r$Year, "_", m, ".nc")) ), ctrMsk0)
    frd_lanina_r <- resample(mean( stack(paste0(oFrdW, "_", lanina_m_r$Year, "_", m, ".nc")) ), ctrMsk0)
    frd_normal_r <- resample(mean( stack(paste0(oFrdW, "_", normal_m_r$Year, "_", m, ".nc")) ), ctrMsk0)
    
    frd_elnino_r[which(frd_elnino_r[]>15)]=0
    frd_lanina_r[which(frd_lanina_r[]>15)]=0
    frd_normal_r[which(frd_normal_r[]>15)]=0
    
    frd_elnino_r[is.na(frd_elnino_r[])] <- 0 
    frd_lanina_r[is.na(frd_lanina_r[])] <- 0 
    frd_normal_r[is.na(frd_normal_r[])] <- 0 
    
    writeRaster(frd_elnino_r * ctrMsk0, paste0(oFrdR, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(frd_lanina_r * ctrMsk0, paste0(oFrdR, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(frd_normal_r * ctrMsk0, paste0(oFrdR, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    ## Convert to shape
    for (enos in enosCond){
      
      # Var defs
      varNm <- "frd"
      varLn <- "Frost.days"
      unit <- "day"
      
      ## Create shapefile (index values)
      dtsRs <- raster(paste0(oFrdR, "_", m, "_", enos, ".tif"))
      dtsRsShp <- rasterToPolygons(dtsRs)
      dtsRsShp <- createSPComment(dtsRsShp)
      names(dtsRsShp) <- varNm
      writeOGR(dtsRsShp, oIDirRFrd, paste0("frd_", ctrName, "_", m, "_", enos), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      # frd_nfeatures <- rbind(frd_nfeatures, paste0(" frd_ecu_", m, "_", enos, "_mag.shp has ", nrow(dtsRsShp), " features"))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      # frd_mag <- c(m, 1, 2, 3, 4)
      
      dtsRs[is.na(dtsRs[])] <- 0
      
      oFrdValsDf<- data.frame(getValues(dtsRs* ctrMsk0))
      oFrdValsDf <- oFrdValsDf %>% mutate(vuln =
                                            case_when(oFrdValsDf == 0 ~ "1", 
                                                      (oFrdValsDf <= 2 ) ~ "2",
                                                      (oFrdValsDf > 2 & oFrdValsDf <= 4) ~ "3",
                                                      (oFrdValsDf > 4 & oFrdValsDf <= 6) ~ "4",
                                                      oFrdValsDf > 6  ~ "5")
      )
      
      ## Applied re-clasiffied values, cut and write raster (mag values)
      dtsRs_vuln <- dtsRs * ctrMsk0
      values(dtsRs_vuln) <- as.numeric(oFrdValsDf$vuln)
      crs(dtsRs_vuln) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
      writeRaster(dtsRs_vuln * ctrMsk0, paste0(oIDirRFrd, "/frd_", ctrName, "_", m, "_", enos, "_mag.tif"), overwrite=T)
      
      ## Create shapefile (magnitude values)
      dtsRs_vuln[is.na(dtsRs_vuln[])] <- 1
      dtsRs_vulnShp <- rasterToPolygons(dtsRs_vuln * ctrMsk0)
      dtsRs_vulnShp <- createSPComment(dtsRs_vulnShp)
      names(dtsRs_vulnShp) <- "vuln"
      writeOGR(dtsRs_vulnShp, oIDirRFrd, paste0("frd_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oFrdVals <- extract(raster(paste0(oFrdR, "_", m, "_", enos, ".tif")), ctrMsk)
      # oFrdValsAvg <- round(unlist(lapply(oFrdVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      oFrdVuln <- data.frame(oFrdVuln=unlist(lapply(oFrdVals, FUN=mean)))
      oFrdVuln <- oFrdVuln %>% mutate(vuln =
                                        case_when(oFrdVuln == 0 ~ "1", 
                                                  (oFrdVuln <= 2 ) ~ "2",
                                                  (oFrdVuln > 2 & oFrdVuln <= 4) ~ "3",
                                                  (oFrdVuln > 4 & oFrdVuln <= 6) ~ "4",
                                                  oFrdVuln > 6  ~ "5")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, frd=round(oFrdVuln$oFrdVuln), vuln=as.numeric(oFrdVuln$vuln))
      writeOGR(ctrMsk, oIDirRFrd, paste0("frd_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . FRD Month ", m, " ", enos, "done\n")
      
    }
    
    cat(" . FRD Month ", m, "done\n")
    
  } else {
    
    cat(" . FRD Month ", m, "done\n")
    
  }
  
}

# write.csv(frd_nfeatures, paste0(oIDirRFrd, "/_nfeatures_describe.csv"), row.names=F)





cat(">. FRD calcs done", "\n")



##################################################
## FLD. Flooding                               ###
## Vulnerability type: Flooding                ###
## Data Source: CHIRPS monthly and others      ###
##################################################

## Identify historical ENOS condition
ensoCond <- read.csv(ensoFile, header=T, row.names = 1)
ensoCond <- as.vector(cbind(Year=rownames(ensoCond)[row(ensoCond)], setNames(melt(ensoCond), c('Month', 'Values'))))
ensoCond$Month <- gsub("X", "", ensoCond$Month)
elnino <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
lanina <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)
normal <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi & as.vector(ensoCond$Year) <= yf)

## FLD output work directory
oBDirW <- paste0(oBDir, "/indices")
if (!file.exists(oBDirW)) {dir.create(oBDirW, recursive = TRUE)}

## Soils data work directory
oBDirS <- paste0(oBDir, "/soils")
if (!file.exists(paste0(oBDirS))) {dir.create(paste0(oBDirS), recursive = TRUE)}

## FLD output directory
oIDirH <- paste0(oIDir, "/historical")
oIDirHFld <- paste0(oIDirH, "/fld")
if (!file.exists(paste0(oIDirHFld))) {dir.create(paste0(oIDirHFld), recursive = TRUE)}

oFld <- paste0(oIDirHFld, "/fld_", ctrName)



#### Crop CHIRPS monthly
cat(">. Croping monthly CHIRPS ", ctrName, "\n")

## Load Mask (Adm0)
ctrMsk <- readOGR(ctrShpAdm0Buf,layer=ctrLyrAdm0Buf)
ctrMsk0 <- raster(rsMsk)

# Output directory
if (!file.exists(paste0(oBDir, "/monthly"))) {dir.create(paste0(oBDir, "/monthly"), recursive = TRUE)}


for (m in 1:12){
  
  for (yr in yi:yf){
    
    iNc <- paste0(iDirPm, "/v2p0chirps", yr,  sprintf("%02d", m), ".bil")
    oNc <- paste0(oBDir, "/monthly/chirps-v2.0.", yr,  sprintf("%02d", m), "_monthly.nc")
    
    if (!file.exists(oNc)){
      
      ## Load CHIRPS data and cut by mask
      dtsMsk <- mask(crop(raster(iNc), extent(ctrMsk0)), ctrMsk0)
      writeRaster(dtsMsk, oNc,  format="CDF",overwrite=T)
      
    }
    
  }
  
}


## Load and reclassify soil data
cly <- paste0(oBDirS, "/Clay_Percentage_rec.tif")
slp <- paste0(oBDirS, "/Slope_percentage_res_rec.tif")
wei <- paste0(oBDirS, "/Wei_res_rec.tif")
lco <- paste0(oBDirS, "/LU_LC_rec.tif")


if (!file.exists(cly)) {
  
  # if (!file.exists(paste0(cly_global, "/af_CLYPPT_T__M_sdAvg_250m_prj_res.tif"))) {
  # 
  #   sd1 <- raster(paste0(cly_global, "/CLYPPT_M_sl1_250m_ll.tif"))
  #   sd2 <- raster(paste0(cly_global, "/CLYPPT_M_sl2_250m_ll.tif"))
  #   sd3 <- raster(paste0(cly_global, "/CLYPPT_M_sl3_250m_ll.tif"))
  #   sd4 <- raster(paste0(cly_global, "/CLYPPT_M_sl4_250m_ll.tif"))
  #   sd5 <- raster(paste0(cly_global, "/CLYPPT_M_sl5_250m_ll.tif"))
  #   sd6 <- raster(paste0(cly_global, "/CLYPPT_M_sl6_250m_ll.tif"))
  #   sd7 <- raster(paste0(cly_global, "/CLYPPT_M_sl7_250m_ll.tif"))
  # 
  #   sdAvg <- ( ( (sd1+sd2)/2 * 5) + ((sd2+sd3)/2 * 10)  + ((sd3+sd4)/2 * 15) + ((sd4+sd5)/2 * 30) + ((sd5+sd6)/2 * 40) + ((sd6+sd7)/2 * 100) ) / 200
  #   # sdAvg <- ( (sd1 * 5) + (sd2 * 10)  + (sd3 * 15) + (sd4 * 30) + (sd5 * 40) + (sd6 * 100) ) / 200
  # 
  #   writeRaster(sdAvg, paste0(cly_global, "/CLYPPT_M_sAvg_250m_ll.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
  # 
  # }
  
  sdAvg <- raster(cly_global)
  sdAvg_crop <- crop(sdAvg, ctrMsk0)
  sdAvg_crop[is.na(sdAvg_crop)] <- 0
  sdAvg_res <- resample(sdAvg_crop, ctrMsk0)
  sdAvg_rec <- reclassify(sdAvg_res, c(-Inf,20,1, 20,40,2, 40,60,3, 60,80,4, 80,Inf,5))
  writeRaster(sdAvg_rec, cly, format="GTiff", overwrite=T, datatype='FLT4S')
  
}

if (!file.exists(lco)) {
  lco_crop <- crop(raster(lco_global), ctrMsk0)
  lco_crop[is.na(lco_crop)] <- 0
  lco_res <- resample(lco_crop, ctrMsk0)
  writeRaster(lco_res, lco, format="GTiff", overwrite=T, datatype='FLT4S')
  
}

if (!file.exists(slp)) {
  dem_crop <- crop(raster(dem_global), ctrMsk0)
  slp_crop <- terrain(dem_crop, opt='slope', unit='degrees')
  slp_crop[is.na(slp_crop)] <- 0
  slp_res <- resample(slp_crop, ctrMsk0)
  slp_rec <- reclassify(slp_res, c(-Inf,10,5, 10,20,4, 20,30,3, 30,40,2, 40,Inf,1))
  writeRaster(slp_rec, slp, format="GTiff", overwrite=T, datatype='FLT4S')
  
}

if (!file.exists(wei)) {
  # dem_crop <- crop(raster(dem_global), ctrMsk0)
  # writeRaster(dem_crop, paste0(oBDirS, "/srtm.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
  wei_crop <- crop(raster(wei_global), ctrMsk0)
  wei_res <- resample(wei_crop, ctrMsk0)
  wei_res[is.na(wei_res)] <- 0
  wei_rec <- reclassify(wei_res, c(-Inf,1,1, 1,2,2, 2,4,3, 4,8,4, 8,Inf,5))
  writeRaster(wei_rec, wei, format="GTiff", overwrite=T, datatype='FLT4S')
  
}



if (!file.exists(paste0(oIDirHFld, "/lco_", ctrName, ".tif"))) {
  
  cly_res <- mask(raster(cly), ctrMsk0, method='ngb')
  slp_res <- mask(raster(slp), ctrMsk0, method='ngb')
  wei_res <- mask(raster(wei), ctrMsk0, method='ngb')
  lco_res <- mask(raster(lco), ctrMsk0, method='ngb')
  
  writeRaster(cly_res, paste0(oIDirHFld, "/cly_", ctrName, ".tif"), format="GTiff", overwrite=T, datatype='FLT4S')
  writeRaster(slp_res, paste0(oIDirHFld, "/slp_", ctrName, ".tif"), format="GTiff", overwrite=T, datatype='FLT4S')
  writeRaster(wei_res, paste0(oIDirHFld, "/wei_", ctrName, ".tif"), format="GTiff", overwrite=T, datatype='FLT4S')
  writeRaster(lco_res, paste0(oIDirHFld, "/lco_", ctrName, ".tif"), format="GTiff", overwrite=T, datatype='FLT4S')
  
}



## Historical

## FLD Calcs all years all months
cat(">. Calculating FLD ", ctrName, " historical\n")

prc_mag <- data.frame()

for (m in 1:12){
  
  if (!file.exists(paste0("fld_", ctrName, "_", m, "_normal.shp"))) {
    
    ## Load CHIRPS data and stack
    prcStk_yrs <- mask(resample(stack(paste0(oBDir, "/monthly/chirps-v2.0.", yi:yf,  sprintf("%02d", m), "_monthly.nc")), ctrMsk0, method='ngb'), ctrMsk0)
    
    ## Remove -9999.9
    prcStk_yrs[which(prcStk_yrs[]<0)]=0
    
    # Var defs
    varNm <- "fld"
    varLn <- "Flooding.index"
    unit <- "mag"
    
    # Quantiles by month
    q <- paste(summary(quantile(prcStk_yrs, probs = probs_q, names = FALSE, na.rm=TRUE)))[c(4,10,16,22)]
    prc_mag <- rbind(prc_mag, c(m, as.numeric(gsub("  ", "", gsub("Mean   :","", q)))))
    prc_mag_mtx <- matrix(c(-1, prc_mag[m, 2], 1,
                            prc_mag[m, 2], prc_mag[m, 3], 2,
                            prc_mag[m, 3], prc_mag[m, 4], 3,
                            prc_mag[m, 4], prc_mag[m, 5], 4,
                            prc_mag[m, 5], 4000, 5), ncol=3, byrow=TRUE)
    
    # prc_mag_mtx <- matrix(c(4000, prc_mag[m, 5], 5,  
    #                         prc_mag[m, 5], prc_mag[m, 4], 4,  
    #                         prc_mag[m, 4], prc_mag[m, 3], 3, 
    #                         prc_mag[m, 3], prc_mag[m, 2], 2, 
    #                         prc_mag[m, 2], -1, 1), ncol=3, byrow=TRUE)
    
    prc_mag_mtx[is.na(prc_mag_mtx)] <- 0
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m <- subset(elnino, elnino$Month == m)
    # elnino_m_yrs <- gsub(" ","", toString(elnino_m$Year))
    lanina_m <- subset(lanina, lanina$Month == m)
    # lanina_m_yrs <- gsub(" ","", toString(lanina_m$Year))
    normal_m <- subset(normal, normal$Month == m)
    # normal_m_yrs <- gsub(" ","", toString(normal_m$Year))
    
    
    ## Calculate mean monthly prec by condition
    prc_elnino <- mask(resample(mean(stack(paste0(oBDir, "/monthly/chirps-v2.0.", elnino_m$Year, sprintf("%02d", m), "_monthly.nc"))), ctrMsk0, method='ngb'), ctrMsk0)
    prc_lanina <- mask(resample(mean(stack(paste0(oBDir, "/monthly/chirps-v2.0.", lanina_m$Year, sprintf("%02d", m), "_monthly.nc"))), ctrMsk0, method='ngb'), ctrMsk0)
    prc_normal <- mask(resample(mean(stack(paste0(oBDir, "/monthly/chirps-v2.0.", normal_m$Year, sprintf("%02d", m), "_monthly.nc"))), ctrMsk0, method='ngb'), ctrMsk0)
    
    prc_elnino[which(prc_elnino[]<0)]=0
    prc_lanina[which(prc_lanina[]<0)]=0
    prc_normal[which(prc_normal[]<0)]=0
    
    writeRaster(prc_elnino, paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(prc_lanina, paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(prc_normal, paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    ## Load rec soil data
    cly_rec <- raster(paste0(oIDirHFld, "/cly_", ctrName, ".tif"))
    slp_rec <- raster(paste0(oIDirHFld, "/slp_", ctrName, ".tif"))
    wei_rec <- raster(paste0(oIDirHFld, "/wei_", ctrName, ".tif"))
    lco_rec <- raster(paste0(oIDirHFld, "/lco_", ctrName, ".tif"))
    
    ## Convert to shape
    for (enos in enosCond){
      
      ## Reclassify monthly precipitation 
      prc_rec <- reclassify(raster(paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_", enos, ".tif")), prc_mag_mtx)
      # writeRaster(prc_rec, paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_", enos, "_mag.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      
      # ## Create shapefile (index values)
      # dtsRs <- raster(paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_", enos, ".tif"))
      # dtsRsShp <- rasterToPolygons(dtsRs)
      # dtsRsShp <- createSPComment(dtsRsShp)
      # names(dtsRsShp) <- varNm
      # writeOGR(dtsRsShp, oIDirHFld, paste0("prc_", ctrName, "_", m, "_", enos),
      #          driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Calc overlay
      indStk <- stack(cly_rec, slp_rec, wei_rec, lco_rec, prc_rec)
      indStkOvl <- overlay(indStk, fun=function(a,b,c,d,e) 0.15*a+0.15*b+0.15*c+0.15*d+0.4*e)
      # indStkOvl_025 <- disaggregate(indStkOvl, fact=c(2,2), method='')
      writeRaster(indStkOvl, paste0(oFld, "_", m, "_", enos, "_mag.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      
      ## Create shapefile (magnitude values)
      dtsRsShp <- rasterToPolygons(prc_rec)
      dtsRsShp@data <- data.frame(dtsRsShp@data, cly=data.frame(rasterToPolygons(mask(cly_rec, prc_rec))),
                                  slp=data.frame(rasterToPolygons(mask(slp_rec, prc_rec))), 
                                  wei=data.frame(rasterToPolygons(mask(wei_rec, prc_rec))), 
                                  lco=data.frame(rasterToPolygons(mask(lco_rec, prc_rec))), 
                                  vuln=round(data.frame(rasterToPolygons(indStkOvl)))
      )
      
      names(dtsRsShp) <- c("prc", "cly", "slp", "wei", "lco", "vuln")
      writeOGR(dtsRsShp, oIDirHFld, paste0("fld_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oFldVals <- extract(indStkOvl, ctrMsk)
      oFldVuln <- data.frame(oFldVuln=unlist(lapply(oFldVals, function(x) mean(x, na.rm=TRUE))))
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, vuln=as.numeric(round(oFldVuln$oFldVuln)) )
      writeOGR(ctrMsk, oIDirHFld, paste0("fld_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      names(prc_mag) <- mag_labels
      write.csv(prc_mag, paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
      
      cat(" . FLD Month ", m, " ", enos, "done\n")
      
    }
    
    cat(" . FLD Month ", m, "done\n")
    
  } else {
    
    cat(" . FLD Month ", m, "done\n")
    
  }
  
}

cat(">. FLD calcs done", "\n")



## Recent-past

## FLD Calcs all years all months
cat(">. Calculating FLD Recent Past ", ctrName, "\n")

## FLD output directory
oIDirR <- paste0(oIDir, "/recent-past")
oIDirRFld <- paste0(oIDirR, "/fld")
if (!file.exists(paste0(oIDirRFld))) {dir.create(paste0(oIDirRFld), recursive = TRUE)}
oFldR <- paste0(oIDirRFld, "/fld_", ctrName)

elnino_r <- subset(ensoCond, ensoCond$Values >= 0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
lanina_r <- subset(ensoCond, ensoCond$Values <= -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)
normal_r <- subset(ensoCond, ensoCond$Values < 0.5 & ensoCond$Values > -0.5 & as.vector(ensoCond$Year) >= yi_r & as.vector(ensoCond$Year) <= yf_r)

for (m in 1:12){
  
  if (!file.exists(paste0("fld_", ctrName, "_", m, "_normal.shp"))) {
    
    ## Load CHIRPS data and stack
    prcStk_yrs <- mask(resample(stack(paste0(oBDir, "/monthly/chirps-v2.0.", yi_r:yf_r,  sprintf("%02d", m), "_monthly.nc")), ctrMsk0, method='ngb'), ctrMsk0)
    
    ## Remove -9999.9
    prcStk_yrs[which(prcStk_yrs[]<0)]=0
    
    # Var defs
    varNm <- "fld"
    varLn <- "Flooding.index"
    unit <- "mag"
    
    # Quantiles by month
    prc_mag <- read.csv(paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_normal_mag_class", ".csv"))
    prc_mag_mtx <- matrix(c(-1, prc_mag[m, 2], 1,
                            prc_mag[m, 2], prc_mag[m, 3], 2,
                            prc_mag[m, 3], prc_mag[m, 4], 3,
                            prc_mag[m, 4], prc_mag[m, 5], 4,
                            prc_mag[m, 5], 4000, 5), ncol=3, byrow=TRUE)
    
    # prc_mag_mtx <- matrix(c(4000, prc_mag[m, 5], 5,  
    #                         prc_mag[m, 5], prc_mag[m, 4], 4,  
    #                         prc_mag[m, 4], prc_mag[m, 3], 3, 
    #                         prc_mag[m, 3], prc_mag[m, 2], 2, 
    #                         prc_mag[m, 2], -1, 1), ncol=3, byrow=TRUE)
    
    prc_mag_mtx[is.na(prc_mag_mtx)] <- 0
    
    ## El Nino, La Nina, Normal years selection 
    elnino_m_r <- subset(elnino_r, elnino_r$Month == m)
    lanina_m_r <- subset(lanina_r, lanina_r$Month == m)
    normal_m_r <- subset(normal_r, normal_r$Month == m)
    
    ## Calculate mean monthly prec by condition
    prc_elnino_r <- mask(resample(mean(stack(paste0(oBDir, "/monthly/chirps-v2.0.", elnino_m_r$Year, sprintf("%02d", m), "_monthly.nc"))), ctrMsk0, method='ngb'), ctrMsk0)
    prc_lanina_r <- mask(resample(mean(stack(paste0(oBDir, "/monthly/chirps-v2.0.", lanina_m_r$Year, sprintf("%02d", m), "_monthly.nc"))), ctrMsk0, method='ngb'), ctrMsk0)
    prc_normal_r <- mask(resample(mean(stack(paste0(oBDir, "/monthly/chirps-v2.0.", normal_m_r$Year, sprintf("%02d", m), "_monthly.nc"))), ctrMsk0, method='ngb'), ctrMsk0)
    
    prc_elnino_r[which(prc_elnino_r[]<0)]=0
    prc_lanina_r[which(prc_lanina_r[]<0)]=0
    prc_normal_r[which(prc_normal_r[]<0)]=0
    
    writeRaster(prc_elnino_r, paste0(oIDirRFld, "/prc_", ctrName, "_", m, "_elnino.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(prc_lanina_r, paste0(oIDirRFld, "/prc_", ctrName, "_", m, "_lanina.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    writeRaster(prc_normal_r, paste0(oIDirRFld, "/prc_", ctrName, "_", m, "_normal.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
    
    ## Load rec soil data
    cly_rec <- raster(paste0(oIDirHFld, "/cly_", ctrName, ".tif"))
    slp_rec <- raster(paste0(oIDirHFld, "/slp_", ctrName, ".tif"))
    wei_rec <- raster(paste0(oIDirHFld, "/wei_", ctrName, ".tif"))
    lco_rec <- raster(paste0(oIDirHFld, "/lco_", ctrName, ".tif"))
    
    ## Convert to shape
    for (enos in enosCond){
      
      ## Reclassify monthly precipitation 
      prc_rec <- reclassify(raster(paste0(oIDirRFld, "/prc_", ctrName, "_", m, "_", enos, ".tif")), prc_mag_mtx)
      # writeRaster(prc_rec, paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_", enos, "_mag.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      
      # ## Create shapefile (index values)
      # dtsRs <- raster(paste0(oIDirHFld, "/prc_", ctrName, "_", m, "_", enos, ".tif"))
      # dtsRsShp <- rasterToPolygons(dtsRs)
      # dtsRsShp <- createSPComment(dtsRsShp)
      # names(dtsRsShp) <- varNm
      # writeOGR(dtsRsShp, oIDirHFld, paste0("prc_", ctrName, "_", m, "_", enos),
      #          driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      
      ## Calc overlay
      indStk <- stack(cly_rec, slp_rec, wei_rec, lco_rec, prc_rec)
      indStkOvl <- overlay(indStk, fun=function(a,b,c,d,e) 0.15*a+0.15*b+0.15*c+0.15*d+0.4*e)
      # indStkOvl_025 <- disaggregate(indStkOvl, fact=c(2,2), method='')
      writeRaster(indStkOvl, paste0(oFldR, "_", m, "_", enos, "_mag.tif"), format="GTiff", overwrite=T, datatype='FLT4S')
      
      ## Create shapefile (magnitude values)
      dtsRsShp <- rasterToPolygons(prc_rec)
      dtsRsShp@data <- data.frame(dtsRsShp@data, cly=data.frame(rasterToPolygons(mask(cly_rec, prc_rec))),
                                  slp=data.frame(rasterToPolygons(mask(slp_rec, prc_rec))), 
                                  wei=data.frame(rasterToPolygons(mask(wei_rec, prc_rec))), 
                                  lco=data.frame(rasterToPolygons(mask(lco_rec, prc_rec))), 
                                  vuln=round(data.frame(rasterToPolygons(indStkOvl)))
      )
      
      names(dtsRsShp) <- c("prc", "cly", "slp", "wei", "lco", "vuln")
      writeOGR(dtsRsShp, oIDirRFld, paste0("fld_", ctrName, "_", m, "_", enos, "_mag"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg 
      oFldVals <- extract(indStkOvl, ctrMsk)
      oFldVuln <- data.frame(oFldVuln=unlist(lapply(oFldVals, function(x) mean(x, na.rm=TRUE))))
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, vuln=as.numeric(round(oFldVuln$oFldVuln)) )
      writeOGR(ctrMsk, oIDirRFld, paste0("fld_", ctrName, "_", m, "_", enos, "_mun"), 
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      names(prc_mag) <- mag_labels
      write.csv(prc_mag, paste0(oIDirRFld, "/prc_", ctrName, "_", m, "_", enos, "_mag_class", ".csv"), row.names=F)
      
      cat(" . FLD Month ", m, " ", enos, "done\n")
      
    }
    
    cat(" . FLD Month ", m, "done\n")
    
  } else {
    
    cat(" . FLD Month ", m, "done\n")
    
  }
  
}

cat(">. FLD calcs done", "\n")



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
    hwd_lanina <- mean(stack(paste0(oHwdW, "_", lanina_m$Year, "_", m, ".nc")))
    hwd_normal <- mean(stack(paste0(oHwdW, "_", normal_m$Year, "_", m, ".nc")))
    
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
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg
      oHwdVals <- extract(dtsRs, ctrMsk)
      # oHwdValsAvg <- round(unlist(lapply(oHwdVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oHwdVuln <- data.frame(oHwdVuln=unlist(lapply(oHwdVals, FUN=mean)))
      
      oHwdVuln_mean <- oHwdVuln
      
      oHwdVuln <- oHwdVuln %>% mutate(vuln =
                                        case_when(oHwdVuln >= as.numeric(hwd_mag[m, 5])  ~ "5", 
                                                  (oHwdVuln < as.numeric(hwd_mag[m, 5]) & oHwdVuln >= as.numeric(hwd_mag[m, 4])) ~ "4",
                                                  (oHwdVuln < as.numeric(hwd_mag[m, 4]) & oHwdVuln >= as.numeric(hwd_mag[m, 3])) ~ "3",
                                                  (oHwdVuln < as.numeric(hwd_mag[m, 3]) & oHwdVuln >= as.numeric(hwd_mag[m, 2])) ~ "2",
                                                  oHwdVuln <= as.numeric(hwd_mag[m, 2]) ~ "1")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, hwd=round(oHwdVuln$oHwdVuln), vuln=as.numeric(oHwdVuln$vuln))
      writeOGR(ctrMsk, oIDirHHwd, paste0("hwd_", ctrName, "_", m, "_", enos, "_mun"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
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
    hwd_lanina_r <- mean(stack(paste0(oHwdW, "_", lanina_m_r$Year, "_", m, ".nc")))
    hwd_normal_r <- mean(stack(paste0(oHwdW, "_", normal_m_r$Year, "_", m, ".nc")))
    
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
      
      
      ## Load Mask (Adm2)
      ctrMsk <- readOGR(ctrShpAdm2Sin, layer=ctrLyrAdm2Sin)
      
      ## Extract values inside polygons and calc avg
      oHwdVals <- extract(dtsRs, ctrMsk)
      # oHwdValsAvg <- round(unlist(lapply(oHwdVals, FUN=mean)))
      
      ## Reclassify by magnitude ranges based on quantiles
      ## 1 - Very low; 2 - Low; 3 - Medium; 4 - High; 5 - Very high
      oHwdVuln <- data.frame(oHwdVuln=unlist(lapply(oHwdVals, FUN=mean)))
      oHwdVuln <- oHwdVuln %>% mutate(vuln =
                                        case_when(oHwdVuln >= as.numeric(hwd_mag[m, 5])  ~ "5", 
                                                  (oHwdVuln < as.numeric(hwd_mag[m, 5]) & oHwdVuln >= as.numeric(hwd_mag[m, 4])) ~ "4",
                                                  (oHwdVuln < as.numeric(hwd_mag[m, 4]) & oHwdVuln >= as.numeric(hwd_mag[m, 3])) ~ "3",
                                                  (oHwdVuln < as.numeric(hwd_mag[m, 3]) & oHwdVuln >= as.numeric(hwd_mag[m, 2])) ~ "2",
                                                  oHwdVuln <= as.numeric(hwd_mag[m, 2]) ~ "1")
      )
      
      ## Join mean values to polygon data and write shapefile
      ctrMsk@data <- data.frame(ctrMsk@data, hwd=round(oHwdVuln$oHwdVuln), vuln=as.numeric(oHwdVuln$vuln))
      writeOGR(ctrMsk, oIDirRHwd, paste0("hwd_", ctrName, "_", m, "_", enos, "_mun"),
               driver="ESRI Shapefile", check_exists=TRUE, overwrite_layer=TRUE)
      
      cat(" . HWD Month ", m, " ", enos, "done\n")
      
    }
    
    
    cat(" . HWD Month ", m, "done\n")
    
  } else {
    
    cat(" . HWD Month ", m, "done\n")
    
  }
  
}

cat(">. HWD calcs done", "\n")





##################################################
## Check indices                               ###
##################################################

# Set params
varList <- c("cdd", "drd", "p95", "frd", "fld", "hwd")
id <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
# varList <- c("frd")


scenarios <- c("historical", "recent-past")

for(scen in scenarios){
  
  ## General output directory
  oIDirH <- paste0(oIDir, "/", scen)
  oIDirHChk <- paste0(oIDir, "/", scen, "/_check")
  if (!file.exists(oIDirHChk)) {dir.create(oIDirHChk)}
  
  ## Load Mask (Adm0)
  ctrMsk <- readOGR(ctrShpAdm0,layer=ctrLyrAdm0)
  ctrMskAdm2 <- readOGR(ctrShpAdm2Sin,layer=ctrLyrAdm2Sin)
  
  stk_reclass <- stack()
  
  for (var in varList){
    
    setwd(paste0(oIDirH, "/", var))
    
    ## Convert to shape
    for (enos in enosCond){
      
      ## Load data in stack
      if (var == "fld") {
        stk_crop <- stack(paste0(var, "_", ctrName, "_", 1:12, "_", enos, "_mag.tif"))
      } else {
        stk_crop <- stack(paste0(var, "_", ctrName, "_", 1:12, "_", enos, ".tif"))
      }
      
      
      ##Rasterize polygons
      for (m in 1:12){
        
        shp <- readOGR(paste0(var, "_", ctrName, "_", m, "_", enos, "_mun.shp"))
        r <- stk_crop[[1]]
        extent(r) <- extent(shp)
        rp <- rasterize(shp, r, 'vuln')
        if (m==1){stk_reclass <- stack(rp)} else {stk_reclass <- addLayer(stk_reclass, rp)}
        
      }
      
      plot <- setZ(stk_crop, id)
      names(plot) <- id
      
      plot_rec <- setZ(stk_reclass, id)
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
      
      
      ## Plot indices
      tiff(paste0(oIDirHChk, "/plot_monthly_", var, "_", enos, "_", unit, "_v1.tif"),
           width=1200, height=1200, pointsize=8, compression='lzw',res=200)
      print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme, 
                      colorkey = list(space = "bottom", width=1.2, height=1)
      ) 
      + layer(sp.polygons(ctrMsk, lwd=0.8))
      )
      dev.off()
      
      
      ## Plot magnitudes
      zvalues_rec <- c(0, 1, 2, 3, 4, 5) # Define limits
      myTheme_rec <- BuRdTheme() # Define squeme of colors
      myTheme_rec$regions$col=colorRampPalette(c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"))(length(zvalues)-1) # Set new colors
      myTheme_rec$strip.border$col = "white" # Eliminate frame from maps
      myTheme_rec$axis.line$col = 'white' # Eliminate frame from maps
      unit_rec <- "mag_reclass"
      
      
      tiff(paste0(oIDirHChk, "/plot_monthly_", var, "_", enos, "_", unit_rec, "_v1.tif"),
           width=1200, height=1200, pointsize=8, compression='lzw',res=200)
      print(levelplot(plot_rec, at = zvalues_rec, scales = list(draw=FALSE), layout=c(4, 3), xlab="", ylab="", par.settings = myTheme_rec, 
                      colorkey = list(space = "bottom", width=1.2, height=1)
      ) 
      + layer(sp.polygons(ctrMskAdm2, lwd=0.5))
      )
      dev.off()
      
      
    } 
    
  }
  
}


# 
# ### Liberate memory and remove temp dirs
# unlink(paste0(oBDir, "/indices"), recursive = T)
# unlink(paste0(oBDir, "/soils"), recursive = T)
# unlink(paste0(oBDir, "/monthly"), recursive = T)
# rm(list = ls())
# 
# ## Compress daily base files
# system(paste0("7za a -mmt=4", oBDir, "/daily.zip", " ", oBDir, "/daily"))
# # unlink(paste0(oBDir, "/daily"), recursive = T)

