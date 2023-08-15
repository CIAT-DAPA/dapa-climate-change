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
yf <- 2022
yi_r <- 2003
yf_r <- 2021

# Climate dirs
iDirP <- "S:/observed/gridded_products/chirps/daily"
iDirT <- "W:/GLOBAL/Climate/observed/gridded_products/CHIRTS"
iDirTc <- "W:/observed/gridded_products/era5/sis-agromet/nc/2m_temperature"
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

cly_global <- "W:/GLOBAL/Biofisico/SoilGrids250m/Physical soil properties/Clay content (0-2 micro meter) mass fraction/CLYPPT_M_sAvg_250m_ll.tif"
lco_global <- "W:/GISDATA/GLOBAL/Biofisico/LAND_COVER/GLOBCOVER_L4_200901_200912_V2.3_reclass.tif"
dem_global <- "S:/observed/gridded_products/srtm/srtm_v41_30s.tif"
wei_global <- "W:/GISDATA/GLOBAL/SAGA_wetness_index_global.tif"

# ## LAM splitted list
# ctrLs = c("ARG", "BRA", "CHL")
# ctrLs = c("BOL", "COL", "CRI", "CUB", "DOM", "ECU", "SLV") #Errors ARG BRA CHL
# ctrLs = c("GUF", "GLP", "GTM", "GUY", "HTI", "HND", "MTQ", "MEX", "NIC", "PAN") 
# ctrLs = c("PRY", "PER", "SUR", "URY", "VEN", "VIR", "ATG")
# ctrLs = c("ABW", "AIA", "BHS", "BLZ", "BRB", "CUW", "CYM") #Errors ANT, FLK UMI
# ctrLs = c("DMA", "GRD", "JAM", "KNA", "LCA", "MSR", "PRI")
# ctrLs = c("SXM", "TCA", "TTO", "VCT", "VGB", "XCL")

ctrName <- "SLV"
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
          
          writeRaster(dtsStk_out, paste0(oBDir, "/daily/", prefix, ".", yr, "_daily_temp.tif"), overwrite=T)
          
          writeRaster(dtsStk_out, paste0(oBDir, "/daily/", prefix, ".", yr, "_daily_temp.nc"), overwrite=T)
          
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

iNc <- paste0(oBDir, "/daily/chirps-v2.0.", yi, "_daily")
oCddW <- paste0(oBDirW, "/cdd_", ctrName)
oCdd <- paste0(oIDirHCdd, "/cdd_", ctrName)

## Load Mask (Adm0)
ctrMsk0 <- raster(rsMsk)

cdd_mag <- data.frame()

for (m in 1:12){
  
  # if (!file.exists(paste0(oCdd, "_", m, "_normal.shp"))) {
    
    cat(" . CDD Month ", m, "processing\n")
    
    for (yr in yi:yf){
      
      if (!file.exists(paste0(oCddW, "_", yr, "_", m, ".nc"))) {
        
        ## Calc consecutive dry days for each year/month
        system(paste0(dircdo," -s -eca_cdd,1 -selyear,", yr, " ", iNc, "_", sprintf("%02d", m), ".nc", " ", oCddW, "_", yr, "_", m, ".nc"))
        
      }
      
      
    }
  # }
}
  
cat(">. CDD calcs done", "\n")


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

iNc <- paste0(oBDir, "/daily/chirps-v2.0.", yi, "_daily")
oP95WRef <- paste0(oBDirW, "/p95_", 1981, "-", 2021, "_", ctrName)
oP95W <- paste0(oBDirW, "/p95_", ctrName)
oP95 <- paste0(oIDirHP95, "/p95_", ctrName)

ctrMsk0 <- raster(rsMsk)



  
  for (m in 1:12){
    
    if (!file.exists(paste0(iNc, "_wet", "_12.nc"))) {
      
      ## Calc time-series wet days
      system(paste0(dircdo," -s setrtomiss,0,0.999 ", iNc, "_", sprintf("%02d", m), ".nc", " ", iNc, "_wet_", sprintf("%02d", m), ".nc"))
      
    }
  }
    



## Historical

p95_mag <- data.frame()

## Loop across months
for (m in 1:12){
  
  # if (!file.exists(paste0(oIDirHP95, "/p95_", ctrName, "_", m, "_normal.shp"))) {
    
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
    
  }
  
# }

  
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


## Historical

## FLD Calcs all years all months
cat(">. Calculating FLD ", ctrName, " historical\n")


# ## Load rec soil data
# cly_rec <- raster(paste0(oIDirHFld, "/cly_", ctrName, ".tif"))
# slp_rec <- raster(paste0(oIDirHFld, "/slp_", ctrName, ".tif"))
# wei_rec <- raster(paste0(oIDirHFld, "/wei_", ctrName, ".tif"))
# lco_rec <- raster(paste0(oIDirHFld, "/lco_", ctrName, ".tif"))

cly_res <- mask(raster(cly), ctrMsk0, method='ngb')
slp_res <- mask(raster(slp), ctrMsk0, method='ngb')
wei_res <- mask(raster(wei), ctrMsk0, method='ngb')
lco_res <- mask(raster(lco), ctrMsk0, method='ngb')

prcStk_yrs <- stack()
prc_mag <- data.frame()


for (m in 1:12){
  ## Load CHIRPS data and stack
  prcStk_yrs <- stack(prcStk_yrs, mask(resample(stack(paste0(oBDir, "/monthly/chirps-v2.0.", 1981:2022,  sprintf("%02d", m), "_monthly.nc")), 
                                                ctrMsk0, method='ngb'), ctrMsk0))
}
## Remove -9999.9
prcStk_yrs[which(prcStk_yrs[]<0)]=0

m=1
# Quantiles by month
q <- paste(summary(quantile(prcStk_yrs, probs = probs_q, names = FALSE, na.rm=TRUE)))[c(4,10,16,22)]
prc_mag <- rbind(prc_mag, c(m, as.numeric(gsub("  ", "", gsub("Mean   :","", q)))))
prc_mag_mtx <- matrix(c(-1, prc_mag[m, 2], 1,
                        prc_mag[m, 2], prc_mag[m, 3], 2,
                        prc_mag[m, 3], prc_mag[m, 4], 3,
                        prc_mag[m, 4], prc_mag[m, 5], 4,
                        prc_mag[m, 5], 4000, 5), ncol=3, byrow=TRUE)
  

years <- 1981:2022

for (m in 1:12){
  
  ## Load CHIRPS data and stack
  prcStk_yrs <- mask(resample(stack(paste0(oBDir, "/monthly/chirps-v2.0.", 1981:2022,  sprintf("%02d", m), "_monthly.nc")), ctrMsk0, method='ngb'), ctrMsk0)
  
  ## Remove -9999.9
  prcStk_yrs[which(prcStk_yrs[]<0)]=0
  
  for (i in 1:length(1981:2022) ){
    
    
    prc_rec_i <- reclassify(prcStk_yrs[[i]], prc_mag_mtx)
    
    
    ## Calc overlay
    indStk <- stack(cly_res, slp_res, wei_res, lco_res, prc_rec_i)
    indStkOvl <- overlay(indStk, fun=function(a,b,c,d,e) 0.15*a+0.15*b+0.15*c+0.15*d+0.4*e)
    # indStkOvl_025 <- disaggregate(indStkOvl, fact=c(2,2), method='')
    indStkOvl[]=as.integer(indStkOvl[])
    writeRaster(indStkOvl, paste0(oBDirW, "/fld_", ctrName, "_", years[i], "_", m, ".nc"), overwrite=T)
    
  }
  
}
    
    
cat(">. FLD calcs done", "\n")




##################################################
## Merge CA Countries                          ###
##################################################
ctrLs = c("GTM", "HND", "NIC", "MEX", "SLV")
idxLs <- c("cdd", "p95", "fld")

for (idx in idxLs){
  
  for (m in 1:12){
    
    for (yr in yi:yf){
      
      idx_ctrs <- paste0(iDir, "/basedata/", ctrLs, "/indices/", idx, "_", ctrLs, "_", yr, "_", m, ".nc")
      # 
      # writeRaster(merge(raster(idx_ctrs[1]), raster(idx_ctrs[2]), raster(idx_ctrs[3]), raster(idx_ctrs[4]), raster(idx_ctrs[5])), 
      #             paste0("D:/cenavarro/torii_climate_risk/raw_indices/", idx, "_", yr, "_", m, ".tif"), overwrite=T)
      
      writeRaster(disaggregate(raster(paste0("D:/cenavarro/torii_climate_risk/raw_indices/", idx, "_", yr, "_", m, ".tif")), fact=c(5, 5)), 
                   paste0("D:/cenavarro/torii_climate_risk/raw_indices/", idx, "_", yr, "_", m, "_0_1.tif"), overwrite=T, datatype='INT1U')
      
    }
  }
  
}


