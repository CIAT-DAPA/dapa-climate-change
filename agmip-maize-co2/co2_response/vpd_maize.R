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
#base_dir <- "~/Leeds-work/AgMIP-maize-phase-2"
base_dir <- "/nfs/a101/earjr/AgMIP-maize-phase-2"
out_dir <- paste(base_dir,"/co2_resp_analysis",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

scratch <- "/scratch/earjr/co2_resp_analysis"
if (!file.exists(scratch)) {dir.create(scratch)}

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
  
  #load Germany shapefile and make a mask with it
  shp_ge <- readShapePoly(paste(shp_dir,"/DEU_adm/DEU0.shp",sep=""))
  ext_ge <- extent(shp_ge)
  ext_ge@xmin <- ext_ge@xmin - 1; ext_ge@xmax <- ext_ge@xmax + 1
  ext_ge@ymin <- ext_ge@ymin - 1; ext_ge@ymax <- ext_ge@ymax + 1
  rs_ge <- rasterize(shp_ge, crop(ahar, ext_ge))
  rs_ge[which(!is.na(rs_ge[]))] <- 1
  rs_ge <- rs_ge * crop(ahar, ext_ge)
  
  ####
  #load a mask from the ISIMIP wth data and produce 0.5x0.5 degree masks with it
  rs_wth <- raster(paste(isimip_wth, "/gfdl-esm2m/hist/pr_bced_1960_1999_gfdl-esm2m_hist_1950.nc",sep=""))
  rs_wth_us <- crop(rs_wth, ext_us)
  rs_wth_fr <- crop(rs_wth, ext_fr)
  rs_wth_ge <- crop(rs_wth, ext_ge)
  
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
  
  ge_df <- as.data.frame(xyFromCell(rs_ge, which(!is.na(rs_ge[]))))
  ge_df$ahar <- extract(rs_ge, ge_df[,c("x","y")])
  ge_df$cloc <- cellFromXY(rs_wth_ge, ge_df[,c("x","y")])
  ge_df <- aggregate(ge_df[,c(1:3)], by=list(cloc=ge_df$cloc), FUN=function(x) {mean(x,na.rm=F)})
  msk_ge <- rs_wth_ge; msk_ge[] <- NA
  msk_ge[ge_df$cloc] <- ge_df$ahar
  msk_ge[which(msk_ge[] < 0.05)] <- NA #final Germany mask
  
  #plot(rs_us); plot(shp_us, add=T)
  #plot(rs_fr); plot(shp_fr, add=T)
  #plot(rs_ge); plot(shp_ge, add=T)
  #plot(msk_us); plot(shp_us,add=T)
  #plot(msk_fr); plot(shp_fr,add=T)
  #plot(msk_ge); plot(shp_ge,add=T)
  #points((10+27/60),(52+17/60+24/3600)) #Manderscheid experiment location
  
  #save data
  save(list=c("msk_us","msk_fr","msk_ge"),file=paste(out_dir,"/masks.RData",sep=""))
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
loc_ge <- as.data.frame(xyFromCell(msk_ge, which(!is.na(msk_ge[]))))

loc_all <- rbind(loc_us, loc_fr, loc_ge)

#constant for calculation of VPD, Tanner and Sinclair (1983) suggest values between 0.65-0.75
#also see page 182 of http://books.google.co.uk/books?id=xnHT6YOlk00C
vpd_cte <- 0.7

#### loop GCMs for his extraction
if (!file.exists(paste(out_dir,"/his_data.RData",sep=""))) {
  his_data <- list()
  for (gcm_i in gcm_list) {
    #gcm_i <- gcm_list[1]
    cat("...processing gcm=",gcm_i,"\n")
    his_data[[gcm_i]] <- list()
    
    #list of files
    tmin_list <- list.files(paste(isimip_wth,"/",gcm_i,"/hist",sep=""), pattern="tasmin")
    tmax_list <- list.files(paste(isimip_wth,"/",gcm_i,"/hist",sep=""), pattern="tasmax")
    
    #loop files for data extraction
    i <- 1
    for (ifil in 1:length(tmin_list)) {
      #ifil <- 1
      cat("   ...processing file=",ifil,"\n")
      
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
        cat("      ...processing year=",year,"\n")
        tmin_trs <- tmin_rs[[which(rsnames %in% year)]]
        tmax_trs <- tmax_rs[[which(rsnames %in% year)]]
        
        #extract data for locations in both countries
        cat("         ...extracting data\n")
        tmin_all <- extract(tmin_trs, loc_all)
        tmax_all <- extract(tmax_trs, loc_all)
        
        #get only JJA data for this year
        yrcal <- colnames(tmin_all); yrcal <- as.numeric(substr(yrcal, 7, 8))
        tmin_jja <- tmin_all[,which(yrcal %in% c(6:8))]
        tmax_jja <- tmax_all[,which(yrcal %in% c(6:8))]
        
        #create/append total matrix
        if (i == 1) {
          atmin_jja <- tmin_jja; atmax_jja <- tmax_jja
        } else {
          atmin_jja <- cbind(atmin_jja, tmin_jja); atmax_jja <- cbind(atmax_jja, tmax_jja)
        }
        i <- i+1
      }
    }
    
    #append into list
    his_data[[gcm_i]][["tmin"]] <- atmin_jja
    his_data[[gcm_i]][["tmax"]] <- atmax_jja
  }
  save(his_data, file=paste(out_dir,"/his_data.RData",sep=""))
} else {
  load(file=paste(out_dir,"/his_data.RData",sep=""))
}


#calculate his vpd
if (!file.exists(paste(out_dir,"/his_vpd.RData",sep=""))) {
  his_vpd <- list()
  for (gcm_i in gcm_list) {
    #gcm_i <- gcm_list[1]
    
    atmin_jja <- his_data[[gcm_i]][["tmin"]]
    atmax_jja <- his_data[[gcm_i]][["tmax"]]
    vpd_jja <- atmin_jja; vpd_jja[,] <- NA
    
    #loop locations
    for (i in 1:nrow(atmin_jja)) {
      #i <- 1 #i <- nrow(atmin_jja)
      tmin_val <- atmin_jja[i,] - 273.15
      tmax_val <- atmax_jja[i,] - 273.15
      
      #calculate daily vpd
      esat_min <- 0.61120 * exp((17.67 * tmin_val) / (tmin_val + 243.5))     
      esat_max <- 0.61120 * exp((17.67 * tmax_val) / (tmax_val + 243.5))
      vpd <- vpd_cte * (esat_max - esat_min) #kPa
      vpd_jja[i,] <- vpd
    }
    
    vpd_jja <- as.data.frame(vpd_jja)
    vpd_jja <- as.data.frame(t(vpd_jja))
    years <- row.names(vpd_jja); years <- as.numeric(substr(years, 2,5))
    vpd_jja <- cbind(year=years, vpd_jja)
    row.names(vpd_jja) <- 1:nrow(vpd_jja)
    vpd_jja <- aggregate(vpd_jja[,2:ncol(vpd_jja)],by=list(year=vpd_jja$year),FUN=function(x) {mean(x,na.rm=T)})
    
    #now calculate average vpd of all locations per region
    col_us <- c(2:(nrow(loc_us)+1))
    col_fr <- c((max(col_us)+1):(max(col_us)+nrow(loc_fr)))
    col_ge <- c((max(col_fr)+1):(max(col_fr)+nrow(loc_ge)))
    
    #unweighted
    #vpd_us <- rowMeans(vpd_jja[,col_us],na.rm=T)
    #vpd_fr <- rowMeans(vpd_jja[,col_fr],na.rm=T)
    #vpd_ge <- rowMeans(vpd_jja[,col_ge],na.rm=T)
    
    #weighted by growing area
    calc_wmean <- function(x) {
      #x <- as.numeric(vpd_jja[1,2:ncol(vpd_jja)])
      har_frac <- c(extract(msk_us, loc_us), extract(msk_fr, loc_fr), extract(msk_ge, loc_ge))
      vpd_us <- sum(x[col_us-1] * har_frac[col_us-1]) / sum(har_frac[col_us-1])
      vpd_fr <- sum(x[col_fr-1] * har_frac[col_fr-1]) / sum(har_frac[col_fr-1])
      vpd_ge <- sum(x[col_ge-1] * har_frac[col_ge-1]) / sum(har_frac[col_ge-1])
      return(c(vpd_us, vpd_fr, vpd_ge))
    }
    
    vpd_yearly <- as.data.frame(t(apply(vpd_jja[,2:ncol(vpd_jja)], 1, calc_wmean)))
    names(vpd_yearly) <- c("US","FR","GE")
    vpd_yearly <- cbind(year=vpd_jja$year, vpd_yearly)
    his_vpd[[gcm_i]] <- vpd_yearly
  }
} else {
  load(file=paste(out_dir,"/his_vpd.RData",sep=""))
}


#### loop RCPs and GCMs for fut extraction
if (!file.exists(paste(out_dir,"/fut_data.RData",sep=""))) {
  fut_data <- list()
  for (rcp_i in rcp_list) {
    #rcp_i <- rcp_list[1]
    rcp_i2 <- rcp_i; substring(rcp_i2, 5, 5) <- ""
    fut_data[[rcp_i]] <- list()
    
    for (gcm_i in gcm_list) {
      #gcm_i <- gcm_list[1]
      fut_data[[rcp_i]][[gcm_i]] <- list()
      
      #list of files
      tmin_list <- list.files(paste(isimip_wth,"/",gcm_i,"/",rcp_i2,sep=""), pattern="tasmin")
      tmax_list <- list.files(paste(isimip_wth,"/",gcm_i,"/",rcp_i2,sep=""), pattern="tasmax")
      
      #loop files for data extraction
      i <- 1
      for (ifil in 1:length(tmin_list)) {
        #ifil <- 1
        cat("...processing rcp=",rcp_i," gcm=",gcm_i," file=",ifil,"\n")
        
        #cut the data to an smaller extent per country
        for (iso in c("us","fr","ge")) {
          #iso <- "us"
          cat("processing iso=",iso,"\n")
          tmsk <- get(paste("msk_",iso,sep=""))
          infil <- paste(isimip_wth,"/",gcm_i,"/",rcp_i2,"/",tmin_list[ifil],sep="")
          oufil <- paste(scratch,"/fut_tmin_",iso,".nc",sep="")
          system(paste("cdo sellonlatbox,",tmsk@extent@xmin,",",tmsk@extent@xmax,",",tmsk@extent@ymin,",",tmsk@extent@ymax," ",infil," ",oufil,sep=""))
          
          infil <- paste(isimip_wth,"/",gcm_i,"/",rcp_i2,"/",tmax_list[ifil],sep="")
          oufil <- paste(scratch,"/fut_tmax_",iso,".nc",sep="")
          system(paste("cdo sellonlatbox,",tmsk@extent@xmin,",",tmsk@extent@xmax,",",tmsk@extent@ymin,",",tmsk@extent@ymax," ",infil," ",oufil,sep=""))
          
          #load data
          tmin_rs <- stack(paste(scratch,"/fut_tmin_",iso,".nc",sep="")) #; tmin_rs <- readAll(tmin_rs)
          tmax_rs <- stack(paste(scratch,"/fut_tmax_",iso,".nc",sep="")) #; tmax_rs <- readAll(tmax_rs)
          
          #extract data for locations in both countries
          tmin_all <- extract(tmin_rs, get(paste("loc_",iso,sep="")))
          tmax_all <- extract(tmax_rs, get(paste("loc_",iso,sep="")))
          
          #get only JJA data for this year
          yrcal <- colnames(tmin_all); yrcal <- as.numeric(substr(yrcal, 7, 8))
          itmin_jja <- tmin_all[,which(yrcal %in% c(6:8))]
          itmax_jja <- tmax_all[,which(yrcal %in% c(6:8))]
          
          if (iso == "us") {
            tmin_jja <- itmin_jja; tmax_jja <- itmax_jja
          } else {
            tmin_jja <- rbind(tmin_jja, itmin_jja); tmax_jja <- rbind(tmax_jja, itmax_jja)
          }
          
          #remove junk
          system(paste("rm -f ",scratch,"/fut_tmin_",iso,".nc",sep=""))
          system(paste("rm -f ",scratch,"/fut_tmax_",iso,".nc",sep=""))
        }
        
        #create/append total matrix
        if (i == 1) {
          atmin_jja <- tmin_jja; atmax_jja <- tmax_jja
        } else {
          atmin_jja <- cbind(atmin_jja, tmin_jja); atmax_jja <- cbind(atmax_jja, tmax_jja)
        }
        i <- i+1
      }
      
      #append into list
      fut_data[[rcp_i]][[gcm_i]][["tmin"]] <- atmin_jja
      fut_data[[rcp_i]][[gcm_i]][["tmax"]] <- atmax_jja
    }
  }
  save(fut_data, file=paste(out_dir,"/fut_data.RData",sep=""))
} else {
  load(file=paste(out_dir,"/fut_data.RData",sep=""))
}

#calculate fut vpd
if (!file.exists(paste(out_dir,"/fut_vpd.RData",sep=""))) {
  fut_vpd <- list()
  
  for (rcp_i in rcp_list) {
    #rcp_i <- rcp_list[1]
    fut_vpd[[rcp_i]] <- list()
    
    for (gcm_i in gcm_list) {
      #gcm_i <- gcm_list[1]
      
      atmin_jja <- fut_data[[rcp_i]][[gcm_i]][["tmin"]]
      atmax_jja <- fut_data[[rcp_i]][[gcm_i]][["tmax"]]
      vpd_jja <- atmin_jja; vpd_jja[,] <- NA
      
      #loop locations
      for (i in 1:nrow(atmin_jja)) {
        #i <- 1 #i <- nrow(atmin_jja)
        tmin_val <- atmin_jja[i,] - 273.15
        tmax_val <- atmax_jja[i,] - 273.15
        
        #calculate daily vpd
        esat_min <- 0.61120 * exp((17.67 * tmin_val) / (tmin_val + 243.5))     
        esat_max <- 0.61120 * exp((17.67 * tmax_val) / (tmax_val + 243.5))
        vpd <- vpd_cte * (esat_max - esat_min) #kPa
        vpd_jja[i,] <- vpd
      }
      
      vpd_jja <- as.data.frame(vpd_jja)
      vpd_jja <- as.data.frame(t(vpd_jja))
      years <- row.names(vpd_jja); years <- as.numeric(substr(years, 2,5))
      vpd_jja <- cbind(year=years, vpd_jja)
      row.names(vpd_jja) <- 1:nrow(vpd_jja)
      vpd_jja <- aggregate(vpd_jja[,2:ncol(vpd_jja)],by=list(year=vpd_jja$year),FUN=function(x) {mean(x,na.rm=T)})
      
      #now calculate average vpd of all locations per region
      col_us <- c(2:(nrow(loc_us)+1))
      col_fr <- c((max(col_us)+1):(max(col_us)+nrow(loc_fr)))
      col_ge <- c((max(col_fr)+1):(max(col_fr)+nrow(loc_ge)))
      
      #unweighted
      #vpd_us <- rowMeans(vpd_jja[,col_us],na.rm=T)
      #vpd_fr <- rowMeans(vpd_jja[,col_fr],na.rm=T)
      #vpd_ge <- rowMeans(vpd_jja[,col_ge],na.rm=T)
      
      #weighted by growing area
      calc_wmean <- function(x) {
        #x <- as.numeric(vpd_jja[1,2:ncol(vpd_jja)])
        har_frac <- c(extract(msk_us, loc_us), extract(msk_fr, loc_fr), extract(msk_ge, loc_ge))
        vpd_us <- sum(x[col_us-1] * har_frac[col_us-1]) / sum(har_frac[col_us-1])
        vpd_fr <- sum(x[col_fr-1] * har_frac[col_fr-1]) / sum(har_frac[col_fr-1])
        vpd_ge <- sum(x[col_ge-1] * har_frac[col_ge-1]) / sum(har_frac[col_ge-1])
        return(c(vpd_us, vpd_fr, vpd_ge))
      }
      
      vpd_yearly <- as.data.frame(t(apply(vpd_jja[,2:ncol(vpd_jja)], 1, calc_wmean)))
      names(vpd_yearly) <- c("US","FR","GE")
      vpd_yearly <- cbind(year=vpd_jja$year, vpd_yearly)
      fut_vpd[[rcp_i]][[gcm_i]] <- vpd_yearly
    }
  }
} else {
  load(file=paste(out_dir,"/fut_vpd.RData",sep=""))
}


#### here need to produce plot of VPD variations in time

