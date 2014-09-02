#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Aug 2014
stop("!")

#load packages
library(raster); library(maptools)

#calculate yearly JJA VPD using tasmin and tasmax from ISIMIP_wth for maize growing areas
#in IL, NE, and IA in the US and for France

#input directories
shp_dir <- "~/Leeds-work/datasets/shapefiles/Countries"
isimip_wth <- "/nfs/a101/earak/data/ISIMIP_wth"
base_dir <- "~/Leeds-work/AgMIP-maize-phase-2"
out_dir <- paste(base_dir,"/co2_resp_analysis",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#create masks
if (!file.exists(paste(out_dir,"/masks.RData",sep=""))) {
  ####
  #get global growing areas raster
  ahar <- raster("~/Leeds-work/scaling-effect/calendar/Maize.crop.calendar/harvested.area.fraction.asc")
  
  
  ####
  #load US state-level shapefile and make a mask with it
  shp_us <- readShapePoly(paste(shp_dir,"/USA_adm/USA1.shp",sep=""))
  shp_us <- shp_us[shp_us$HASC_1 == "US.IL" | shp_us$HASC_1 == "US.NE" | shp_us$HASC_1 == "US.IA",]
  ext_us <- extent(shp_us)
  ext_us@xmin <- ext_us@xmin - 1; ext_us@xmax <- ext_us@xmax + 1
  ext_us@ymin <- ext_us@ymin - 1; ext_us@ymax <- ext_us@ymax + 1
  rs_us <- rasterize(shp_us, crop(ahar, ext_us))
  rs_us[which(!is.na(rs_us[]))] <- 1
  rs_us <- rs_us * crop(ahar, ext_us)
  
  #load France shapefile and make a mask with it
  shp_fr <- readShapePoly(paste(shp_dir,"/FRA_adm/FRA0.shp",sep=""))
  ext_fr <- extent(shp_fr)
  ext_fr@xmin <- ext_fr@xmin - 1; ext_fr@xmax <- ext_fr@xmax + 1
  ext_fr@ymin <- ext_fr@ymin - 1; ext_fr@ymax <- ext_fr@ymax + 1
  rs_fr <- rasterize(shp_fr, crop(ahar, ext_fr))
  rs_fr[which(!is.na(rs_fr[]))] <- 1
  rs_fr <- rs_fr * crop(ahar, ext_fr)
  
  
  ####
  #load a mask from the ISIMIP wth data and produce 0.5x0.5 degree masks with it
  rs_wth <- raster(paste(isimip_wth, "/gfdl-esm2m/hist/pr_bced_1960_1999_gfdl-esm2m_hist_1950.nc",sep=""))
  rs_wth_us <- crop(rs_wth, ext_us)
  rs_wth_fr <- crop(rs_wth, ext_fr)
  
  us_df <- as.data.frame(xyFromCell(rs_us, which(!is.na(rs_us[]))))
  us_df$ahar <- extract(rs_us, us_df[,c("x","y")])
  us_df$cloc <- cellFromXY(rs_wth_us, us_df[,c("x","y")])
  us_df <- aggregate(us_df[,c(1:3)], by=list(cloc=us_df$cloc), FUN=function(x) {mean(x,na.rm=F)})
  
  msk_us <- rs_wth_us; msk_us[] <- NA
  msk_us[us_df$cloc] <- us_df$ahar
  msk_us[which(msk_us[] < 0.05)] <- NA #final US mask
  
  fr_df <- as.data.frame(xyFromCell(rs_fr, which(!is.na(rs_fr[]))))
  fr_df$ahar <- extract(rs_fr, fr_df[,c("x","y")])
  fr_df$cloc <- cellFromXY(rs_wth_fr, fr_df[,c("x","y")])
  fr_df <- aggregate(fr_df[,c(1:3)], by=list(cloc=fr_df$cloc), FUN=function(x) {mean(x,na.rm=F)})
  
  msk_fr <- rs_wth_fr; msk_fr[] <- NA
  msk_fr[fr_df$cloc] <- fr_df$ahar
  msk_fr[which(msk_fr[] < 0.05)] <- NA #final France mask
  
  #plot(rs_us); plot(shp_us, add=T)
  #plot(rs_fr); plot(shp_fr, add=T)
  #plot(msk_us); plot(shp_us,add=T)
  #plot(msk_fr); plot(shp_fr,add=T)
  
  save(list=c("msk_us","msk_fr"),file=paste(out_dir,"/masks.RData",sep=""))
} else {
  load(file=paste(out_dir,"/masks.RData",sep=""))
}

####
#calculate JJA VPD for historical period for each GCM
gcm_list <- c("gfdl-esm2m","hadgem2-es","ipsl-cm5a-lr","miroc-esm-chem","noresm1-m")
rcp_list <- c("rcp2p6","rcp4p5","rcp6p0","rcp8p5")

#data.frame of locations
loc_us <- as.data.frame(xyFromCell(msk_us, which(!is.na(msk_us[]))))
loc_fr <- as.data.frame(xyFromCell(msk_fr, which(!is.na(msk_fr[]))))

#loop GCMs for hist extraction
his_data <- list()
for (gcm_i in gcm_list) {
  #gcm_i <- gcm_list[1]
  cat("...processing gcm=",gcm_i,"\n")
  his_data[[gcm_i]] <- list()
  
  #list of files
  tmin_list <- list.files(paste(isimip_wth,"/",gcm_i,"/hist",sep=""), pattern="tasmin")
  tmax_list <- list.files(paste(isimip_wth,"/",gcm_i,"/hist",sep=""), pattern="tasmin")
  
  #loop files for data extraction
  i <- 1
  for (ifil in 1:length(tmin_list)) {
    #ifil <- 2
    cat("......processing file=",ifil,"\n")
    
    #pre-load data
    tmin_rs <- stack(paste(isimip_wth,"/",gcm_i,"/hist/",tmin_list[ifil],sep=""))
    tmax_rs <- stack(paste(isimip_wth,"/",gcm_i,"/hist/",tmax_list[ifil],sep=""))
    
    #derive years
    rsnames <- names(tmin_rs)
    rsnames <- substr(rsnames, 1, 5); rsnames <- as.numeric(gsub("X", "", rsnames))
    minyear <- min(rsnames); maxyear <- max(rsnames)
    
    #loop years for extraction of data
    for (year in minyear:maxyear) {
      #year <- c(minyear:maxyear)[1]
      cat(".........processing year=",year,"\n")
      tmin_trs <- tmin_rs[[which(rsnames %in% year)]]
      tmax_trs <- tmax_rs[[which(rsnames %in% year)]]
      
      #extract data for locations in both countries
      cat("............extracting data for US\n")
      tmin_us <- extract(tmin_trs, loc_us)
      tmax_us <- extract(tmax_trs, loc_us)
      
      cat("............extracting data for FR\n")
      tmin_fr <- extract(tmin_trs, loc_fr)
      tmax_fr <- extract(tmax_trs, loc_fr)
      
      #create/append total matrix
      if (i == 1) {
        atmin_us <- tmin_us; atmax_us <- tmax_us
        atmin_fr <- tmin_fr; atmax_fr <- tmax_fr
      } else {
        atmin_us <- cbind(atmin_us, tmin_us); atmax_us <- cbind(atmax_us, tmax_us)
        atmin_fr <- cbind(atmin_fr, tmin_fr); atmax_fr <- cbind(atmax_fr, tmax_fr)
      }
      i <- i+1
    }
  }
  
  #append into list
  hist_data[[gcm_i]][["tmin_us"]] <- atmin_us
  hist_data[[gcm_i]][["tmax_us"]] <- atmax_us
  hist_data[[gcm_i]][["tmin_fr"]] <- atmin_fr
  hist_data[[gcm_i]][["tmax_fr"]] <- atmax_fr
}


