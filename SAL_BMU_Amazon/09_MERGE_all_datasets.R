########################################################################
## Purpose: Merge daily & monthly weather stations in a single csv file 
## Author: Carlos Navarro c.e.navarro@cgiar.org
########################################################################

###############################
### 01- Merge dataset daily ###
###############################

merge_dst_dly <- function(bDir="Z:/DATA/WP2/01_Weather_Stations", var="tmax", rg=c(-79.5, -72, -11.9, 3), sY=1971, fY=2010, rgname, oDir="Z:/DATA/WP2/04_Evaluations/WTH_ST"){
  
  library(reshape)
  require(raster)
  
  # Set region extent
  rgExt <- extent(rg)
  
  dstList <- c("COL", "ECU", "GHCN", "GSOD", "PER", "BRA")
  
  for (dst in dstList){
    
    cat(dst, var, "merging\n")
    # Read monthly weather file & select years
    monDt <- read.csv(paste0(bDir, "/", dst, "/", var, "_daily_all_qc.csv"), header = T)
    monDt <- monDt[ which(monDt$Year >= sY & monDt$Year <= fY),]
    monDt$year <- NULL
    monDt$month <- NULL
    monDt$day <- NULL
    
    # Read Station info file
    stInfo <- read.csv(paste0(bDir, "/", dst, "/stations.csv"), header = T)
    
    # Read name by dataset
    if (dst == "COL"){
      names(monDt) <- gsub("X", "", names(monDt))
      source = "IDEAM"
    } else if (dst == "GHCN"){
      names(monDt) <- gsub("X", "", names(monDt))
      names(stInfo) <- c("Station", "Lat", "Lon", "Alt", "Name")
      source = "GHCN"
    } else if (dst == "ECU") {
      source = "INAMHI"
    } else if (dst == "GSOD") {
      names(monDt) <- gsub("X", "", names(monDt))
      source = "GSOD"
    } else if (dst == "BRA") {
      source = "INMET"
    } else if (dst == "PER") {
      source = "SENAMHI"
    }
    
    stCod <- data.frame(Station=names(monDt[4:length(monDt)])) 
    
    # Select stations within region
    join <- merge(stCod, stInfo, by = "Station", all = FALSE)
    posXY = join$Lon < xmin(rgExt) | join$Lon > xmax(rgExt) | join$Lat < ymin(rgExt) | join$Lat > ymax(rgExt)
    join <- join[!posXY, ]
    
    if (dst == "COL"){
      joinSt <- cbind(join[ , c("Station", "Name","Lat", "Lon", "Alt")], "Source"=source)
      joinDt <- cbind("Year"=monDt$Year, "Month"=monDt$Month,"Day"=monDt$Day, monDt[, which(names(monDt) %in% as.vector(join$Station))], digits = 1 )
    } else {
      joinSt <- rbind(joinSt, cbind("Station"=as.character(join$Station), "Name"=as.character(join$Name), "Lat"=join$Lat, "Lon"=join$Lon, "Alt"=join$Alt, "Source"=source))
      joinDt <- cbind(joinDt, monDt[, which(names(monDt) %in% as.vector(join$Station))] )
    }
    
  }
  
  # Write daily quality controled merged
  write.csv(joinDt, paste0(oDir, "/", var, "_daily_all_qc_amazon.csv"), row.names = F)
  write.csv(joinSt, paste0(oDir, "/", var, "_stations_daily_amazon.csv"), row.names = F)
  
}


#################################
### 02- Merge dataset monthly ###
#################################

merge_dst_mth <- function(bDir="Z:/DATA/WP2/01_Weather_Stations", var="prec", rg=c(-79.5, -72, -11.9, 3), sY=1971, fY=2010, rgname, oDir="Z:/DATA/WP2/04_Evaluations/WTH_ST"){
  
  library(reshape)
  require(raster)
  
  # Set region extent
  rgExt <- extent(rg)
  
  dstList <- c("COL", "ECU", "GHCN", "GSOD", "PER", "BRA")
  
  for (dst in dstList){
    
    cat(dst, var, sy, "-", fY, "merging\n")
    # Read monthly weather file & select years
    monDt <- read.csv(paste0(bDir, "/", dst, "/", var, "_monthly_all.csv"), header = T)
    monDt <- monDt[ which(monDt$Year >= sY & monDt$Year <= fY),]
    monDt$year <- NULL
    monDt$month <- NULL
    monDt$day <- NULL
    
    # Read Station info file
    stInfo <- read.csv(paste0(bDir, "/", dst, "/stations.csv"), header = T)
    
    # Read name by dataset
    if (dst == "COL"){
      names(monDt) <- gsub("X", "", names(monDt))
      source = "IDEAM"
    } else if (dst == "GHCN"){
      names(monDt) <- gsub("X", "", names(monDt))
      names(stInfo) <- c("Station", "Lat", "Lon", "Alt", "Name")
      source = "GHCN"
    } else if (dst == "ECU") {
      source = "INAMHI"
    } else if (dst == "GSOD") {
      names(monDt) <- gsub("X", "", names(monDt))
      source = "GSOD"
    } else if (dst == "BRA") {
      source = "INMET"
      names(monDt) <- gsub("X", "", names(monDt))
    } else if (dst == "PER") {
      source = "SENAMHI"
      names(monDt) <- gsub("X", "", names(monDt))
      stInfo$Station <-  sprintf("%06d", stInfo$Station) 
    }
    
    stCod <- data.frame(Station=names(monDt[3:length(monDt)])) 
    
    # Select stations within region
    join <- merge(stCod, stInfo, by = "Station", all = FALSE)
    posXY = join$Lon < xmin(rgExt) | join$Lon > xmax(rgExt) | join$Lat < ymin(rgExt) | join$Lat > ymax(rgExt)
    join <- join[!posXY, ]
    
    if (dst == "COL"){
      joinSt <- cbind(join[ , c("Station", "Name","Lat", "Lon", "Alt")], "Source"=source, "Country"=dst)
      joinDt <- cbind("Year"=monDt$Year, "Month"=monDt$Month, monDt[, which(names(monDt) %in% as.vector(join$Station))])
      
      # Count number of station available
      # cat(ncol(monDt[, which(names(monDt) %in% as.vector(join$Station))]))
    } else {
      joinSt <- rbind(joinSt, cbind("Station"=as.character(join$Station), "Name"=as.character(join$Name), "Lat"=join$Lat, "Lon"=join$Lon, "Alt"=join$Alt, "Source"=source, "Country"=dst))
      joinDt <- cbind(joinDt, monDt[, which(names(monDt) %in% as.vector(join$Station))])
      
      # Count number of station available
      # cat(ncol(monDt[, which(names(monDt) %in% as.vector(join$Station))]))
    }
    
  }
  
  # Write daily quality controled merged
  write.csv(joinDt, paste0(oDir, "/", var, "_monthly_all_amazon.csv"), row.names = F)
  
  stSel <- as.matrix(names(joinDt)[3:length(names(joinDt))])
  colnames(stSel) <- "Station"
  
  # Select stations within region
  joinSt_f <- merge(stSel, joinSt, by = "Station", all = FALSE)
  
  write.csv(joinSt_f, paste0(oDir, "/", var, "_stations_monthly_amazon.csv"), row.names = F)
  
}


#############################
### 03- Fill monthly      ###
#############################

fill_dst_mth <- function(var="tmin",  bDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE", oDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE", dDir = "S:/observed/gridded_products/chirps/monthly/world",sY=1986, fY=2005){
  
  library(raster)
  
  # Read monthly and station adn year selection
  st_data <- read.csv(paste0(bDir, "/", var, "_monthly_all_amazon.csv"), header=T)
  st_coord <- read.csv(paste0(bDir, "/", var, "_stations_monthly_amazon.csv"), header=T)
  
  ## Return %NA and remove stations with lesser data
  na_per <- function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    x = na.x
    return(x)
  }
  st_data_na <- apply(st_data,2,na_per)
  st_data_filter <- st_data[,which(as.vector(st_data_na)<=0.50)]
  st_data_na_filter <- apply(st_data_filter,2,na_per)
  
  # Select period
  st_data_filter_earlier <- st_data_filter[ which(st_data_filter$Year < sY),]
  st_data_filter <- st_data_filter[ which(st_data_filter$Year >= sY & st_data_filter$Year <= fY),]
  
  ## Set coordinates only for the coordinates filtered 
  st_names_filter <- as.matrix(gsub("X", "", names(st_data_filter)[3:length(names(st_data_filter))]))
  colnames(st_names_filter) <- "Station"
  st_coord_filter <- merge(st_names_filter, st_coord[!duplicated(st_coord$Station), ], by = "Station", all = FALSE, sort=F)
  
  # Read stack and extract points
  if (var == "prec"){
    dts_all <- stack(paste0(dDir, "/v2p0chirps", format(seq(as.Date("1981/01/01"),as.Date("2010/12/31"),"months"), "%Y%m"), ".bil"))  
  } else {
    dst <- stack(paste0(dDir, "/", var, "_monthly_ts_agmerra_1980_2010.nc"))
    dts_all <- dst[[13:nlayers(dst)]]
  }
  
  
  cat("Extract values stack ", var, "\n")
  if (var == "prec"){
    st_dts_ext <- raster::extract(x=dts_all, y=cbind(st_coord_filter$Lon, st_coord_filter$Lat), method = 'bilinear')  
  } else {
    lonmod <- st_coord_filter$Lon
    lonmod[which(lonmod[]<0)] = lonmod[]+360
    st_dts_ext <- raster::extract(x=dts_all, y=cbind(lonmod, st_coord_filter$Lat), method = 'bilinear')
  }
  
  st_dts <- as.data.frame(t(st_dts_ext))[1:nrow(st_data_filter),]  
  
  if (var != "prec"){
    st_dts <- st_dts/10  
  }
  
  
  years_months <- st_data_filter[,1:2]
  st_data_filter$Year <- NULL
  st_data_filter$Month <- NULL

  names(st_dts) <- names(st_data_filter)
  st_dts_na = apply(st_dts,2,na_per)
  
  dates <- seq(as.Date(paste0(sY, "/01/01")),as.Date(paste0(fY, "/12/31")),"month")
  months <- months.Date(dates)
  
  add_legend <- function(...) {
    opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
                mar=c(0, 0, 0, 0), new=TRUE)
    on.exit(par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    legend(...)
  }
  
  # Filling stations
  for (i in 1:ncol(st_dts)){
    
    cat("Filling", var, "_", st_names_filter[i], "\n")
    
    if(st_dts_na[i]==0){
      
      # Set linear model
      data.model = as.data.frame(cbind("y"=st_data_filter[,i], "x"=st_dts[,i]))
      model = lm(data=data.model,formula = y~x)
      rmse <- round(sqrt(mean(resid(model)^2)), 2)
      coefs <- coef(model)
      b0 <- round(coefs[1], 2)
      b1 <- round(coefs[2],2)
      r2 <- round(summary(model)$r.squared, 2)
      
      eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~
                      R^2 == .(r2) * "," ~~ RMSE == .(rmse))
      
      new.data = as.data.frame(st_dts[,i])
      names(new.data) = "x"
      
      data_model.p = predict(model, new.data)
      
      if (var == "prec"){
        data_model.p[data_model.p<0] <- 0 
      } 
      
      # Plot comparisson
      tiff(paste0(oDir, "/", var, "_", st_names_filter[i],".tiff"),compression = 'lzw',height = 10,width = 10,units="in", res=200)
        
        par(mfrow=c(2,1))
        
        if (var == "prec"){
          plot(dates,st_data_filter[,i],lwd=1.5,type="l",xlab="",ylab="Precipitation (mm)")  
        } else {
          plot(dates,st_data_filter[,i],lwd=1.5,type="l",xlab="",ylab="Temperature (C deg)")  
        }
        
        lines(dates,st_dts[,i],col="blue",lty=2,lwd=1)
        lines(dates,data_model.p,col="red",lty=2)
        
        
        plot(st_data_filter[,i],st_dts[,i],xlab="Observed_stations",ylab="dts")
        abline(model,col="red")
        legend('bottomright', legend = eqn, bty = 'n')
        
        add_legend("topright",c("Observed","dts","Model"),
                   horiz=T, bty='n', cex=0.9,lty=c(1,2,2),lwd=c(1.5,1,1),col=c("black","blue","red"))
        
      dev.off()
      
      pos.na = which(is.na(st_data_filter[,i]))
      
      if(r2>=0.1){
        st_data_filter[pos.na,i] = as.numeric(data_model.p[pos.na])
      }else {
        st_data_filter[pos.na,i] = as.numeric(st_dts[pos.na,i])
      }
      
    }
    
    
  }
  
  # Write stations filled
  write.csv(rbind(st_data_filter_earlier, cbind(years_months, st_data_filter)) ,paste0(bDir, "/", var, "_monthly_all_amazon_filled.csv"),row.names = F)
  
}

#############################
### 04- Climatology Calcs ###
#############################

clim_calc <- function(var="prec",  bDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE", oDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE", stDir="Z:/DATA/WP2/01_Weather_Stations/MERGE", sY=1981, fY=2010){
  
  # Read monthly file
  if (var == "prec"){
    
    monthly_var <- read.csv(paste0(bDir, "/", var, "_monthly_all_amazon_filled.csv"), header=T) 
    
  } else {
    monthly_var <- read.csv(paste0(bDir, "/", var, "_monthly_all_amazon.csv"), header=T)  
  }
  
  
  # oDir_per <- paste0(oDir, "/", sY, "_", fY)
  # if (!file.exists(oDir_per)) {dir.create(oDir_per, recursive = TRUE)}
  
  if (var == "prec"){varmod <- "rain"} else { varmod = var}
  
  ## Climatology aggregation based on NA percent
  avg_var = function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    if(na.x>=0.33){
      x = NA
    }else{
      x = mean(a, na.rm = any(!is.na(a))) 
    }
    return(x)
  }
  
  ## Return %NA
  na_per <- function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    x = na.x
    return(x)
  }
  
  # Years selection
  monthly_var <- monthly_var[ which(monthly_var$Year >= sY & monthly_var$Year <= fY),]
  
  # Period climatology calc
  monthly_avg = aggregate(monthly_var[,3:length(monthly_var)], list(Month=monthly_var$Month),avg_var)
  na_p <- round( (1- apply(monthly_var[,3:length(monthly_var)], 2, na_per)) * (fY-sY), digits = 0)
  
  # Remove unnecesary columns if exists
  monthly_avg$month <- NULL;  monthly_avg$Month <- NULL;  monthly_avg$year <- NULL
  
  # Transpose
  st_names <- as.data.frame(names(monthly_avg))
  monthly_avg <- cbind(st_names, round(t(monthly_avg), 1))
  
  # Remove all rows with NA
  monthly_avg <- monthly_avg[rowSums(is.na(monthly_avg[,2:ncol(monthly_avg)])) < 12, ]
  
  # Add month names
  mths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  colnames(monthly_avg) <- c("Station", mths)
  monthly_avg$Station <- gsub("X", "", monthly_avg$Station)
  
  rownames(monthly_avg) <- NULL
  
  ## Add station info
  stInfo <- read.csv(paste0(stDir, "/", var, "_stations_monthly_amazon.csv"), header=T)
  join <- merge(monthly_avg, stInfo[!duplicated(stInfo$Station), ], by = "Station", all = FALSE, sort=F)
  
  ## NA percent
  na_p <- cbind(st_names, na_p)
  
  names(na_p) <- c("Station", "NYEARS")
  na_p$Station <-  gsub("X", "", na_p$Station)
  join <- merge(join, na_p, by = "Station", all = FALSE)
  join[is.na(join)] <- -9999.9
  
  
  # Combine info and data
  climData <- cbind(join[ , c("Station", "Source", "Station", "Name", "Country","Lon", "Lat", "Alt", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "NYEARS")])
  names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
  
  # Write climatology 
  write.csv(climData, paste0(oDir, "/", varmod, "_amz_", sY, "_", fY, ".csv"), row.names=F)
  
}


##################################
### 05 - Add Pseudo-stations #####
##################################

add_pseudost =function(dDir ="S:/observed/gridded_products/chirps/monthly/world", bDir ="Z:/DATA/WP2/01_Weather_Stations/MERGE/climatology", rDir ="Z:/DATA/WP2/00_zones", dst_name ="chrips", sY =1981, fY =2010, var ="prec"){
  
  require(raster)
  require(rgdal)
  require(dismo)
  require(som)
  
  # List of months
  mthLs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  ## Region mask
  mask <- raster(paste0(rDir, "/mask"))
  depLim <- raster(paste0(rDir, "/rg_poly_amz"))
  refExt <- extent(mask)
  
  ## Load original station data to 
  if (var == "prec"){
    raw_data <- read.csv(paste0(bDir, "/rain_amz_", sY, "_", fY, ".csv"))  
  } else {
    raw_data <- read.csv(paste0(bDir, "/", var, "_amz_", sY, "_", fY, ".csv"))  
  }
  
  
  ## Read CHRIPS output data, write in a friendly format
  # Cut border and calc 30yr avg
  if (var == "prec"){
    if (!file.exists(paste0(dDir, "/30yr_", sY, "_", fY, "/", var, "_12.tif"))) {
      dir.create(paste0(dDir, "/30yr_", sY, "_", fY), recursive=T)
      for (i in 1:12){
        rs <- mean(stack(paste0(dDir, "/v2p0chirps", sY:fY, sprintf("%02d", i), ".bil")))
        writeRaster(rs, paste0(dDir, "/30yr_", sY, "_", fY, "/", var, "_", i, ".tif"), format="GTiff", overwrite=T, datatype='INT2S')
      }
    }
  } else {
    if (!file.exists(paste0(dDir, "/30yr_", sY, "_", fY, "/", var, "_12.tif"))) {
      dir.create(paste0(dDir, "/30yr_", sY, "_", fY), recursive=T)
      for (i in 1:12){
        rs <- rotate(mean(stack(paste0(dDir, "/", var, "_monthly_ts_agmerra_1980_2010.nc"))[[seq(i + 12, 12*31, 12)]]))
        writeRaster(rs, paste0(dDir, "/30yr_", sY, "_", fY, "/", var, "_", i, ".tif"), format="GTiff", overwrite=T) #, datatype='INT2S')
      }
    }
  }
  
  rs_clip <- crop(stack(paste0(dDir, "/30yr_", sY, "_", fY, "/", var, "_", 1:12, ".tif")), refExt)
  rs_agg <- aggregate(rs_clip, fact=0.5/res(rs_clip)[1])

  # Gap mask calcs
  gap <- rasterize(cbind(raw_data$LONG, raw_data$LAT), rs_agg, 1, fun=mean)
  gap[is.na(gap)] <- 0 
  gap[gap==1] <- NA 

  ## Calc random points based on kernel-density function (computed in ArcGIS)
  wgRs <- setMinMax(raster(paste0(rDir, "/rg_weigth_1deg_", var, ".tif")))
  wgGap <- mask(1-(wgRs/(maxValue(wgRs)- minValue(wgRs))), resample(gap, wgRs))
  
  # Calculate the density within departaments based on raw_data
  # gapMsk <- mask(setExtent(crop(gap, extent(depLim)), extent(depLim), keepres = F) , depLim)
  # denPres <- nrow(raw_data)/length(depLim[!is.na(depLim)])
  # npts <- length(mask[!is.na(mask)]) * denPres
  if (var == "prec"){
    npts <- 0.3*(nrow(raw_data))  
  } else {
    npts <- 0.5*(nrow(raw_data))  
  }
  
  pts <- randomPoints(wgGap, n=npts, ext=extent(depLim), prob=T)
  
  rs_pts <- extract(rs_clip, pts)
  alt <- extract(raster(paste0(rDir, "/altitude")), pts)
  
  if (var == "prec"){
    rs_pts[rs_pts < 0] <- NA  
  } else {
    rs_pts[rs_pts < -50] <- NA  
  }
  
  alt[is.na(alt)] <- NA
  
  # Combine info and data
  climData <- as.data.frame(cbind(paste0(dst_name, 1:nrow(pts)), dst_name, paste0(dst_name, 1:nrow(pts)), paste0(dst_name, 1:nrow(pts)), "AMZ", round(pts, 3), alt, round(rs_pts, 1), fY-sY))
  names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
  
  combClimData <- na.omit(rbind(raw_data, climData))
  
  if(var == "prec"){
    write.csv(combClimData,  paste0(bDir, "/rain_amz_", sY, "_", fY, ".csv"), row.names = F)  
  } else {
    write.csv(combClimData,  paste0(bDir, "/", var, "_amz_", sY, "_", fY, ".csv"), row.names = F)
  }
  
} 

# ### 01- Merge dataset daily
# bDir="Z:/DATA/WP2/01_Weather_Stations"
# # rg=c(-79.5, -72, -11.9, 3)
# rg <- c(-80, -66, -16, 5) #Study Region region
# rgname <- "amazon"
# sY=1971
# fY=2010
# oDir="Z:/DATA/WP2/01_Weather_Stations/MERGE"
# varList <- c("prec", "tmax", "tmin")
# for (var in varList){
#   merge_dst_dly(bDir, var, rg, sY, fY, rgname, oDir)
# }


# ## 02- Merge dataset monthly
# bDir="Z:/DATA/WP2/01_Weather_Stations"
# # rg=c(-79.5, -70, -11, 2)
# rg <- c(-80, -66, -16, 5)
# rgname <- "amazon"
# oDir="Z:/DATA/WP2/01_Weather_Stations/MERGE"
# varList <- c("prec", "tmax", "tmin")
# 
# sY=1971
# fY=2010
# for (var in varList){
#   merge_dst_mth(bDir, var, rg, sY, fY, rgname, oDir)
# }

## 03- Fill monthly dataset
# bDir="Z:/DATA/WP2/01_Weather_Stations/MERGE"
# # rg=c(-79.5, -70, -11, 2)
# rg <- c(-80, -66, -16, 5)
# rgname <- "amazon"
# var <- "tmax"
# sY=1981
# fY=2010
# oDir="Z:/DATA/WP2/01_Weather_Stations/MERGE/filled"
# # dDir = "S:/observed/gridded_products/chirps/monthly/world"
# dDir <- "U:/cropdata/agmerra/monthly"
# # dst_stk = paste0("S:/observed/gridded_products/cru-ts-v3-21/proces_data/tmn/tmn_", sY:fY, ".nc")
# 
# fill_dst_mth(var, bDir, oDir, dDir, sY, fY)


# # 04 - Clim calcs
# rg=c(-79.5, -72, -11.9, 3)
# bDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE"
# oDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE/climatology"
# stDir= "Z:/DATA/WP2/01_Weather_Stations/MERGE"
# varList <- c("prec", "tmax", "tmin")
# periodLs <- c(1971, 1976, 1986, 1981)
# 
# for (sY in periodLs){
# 
#   if (sY == 1986){
#     fY = sY + 19
#   } else {
#     fY = sY + 29
#   }
# 
#   for (var in varList){
#     clim_calc(var, bDir, oDir, stDir, sY, fY)
#   }
# }


# 05 - Add pseudo-stations
# dDir <- "S:/observed/gridded_products/chirps/monthly/world"
dDir <- "U:/cropdata/agmerra/monthly"
bDir <- "Z:/DATA/WP2/01_Weather_Stations/MERGE/climatology"
rDir <- "Z:/DATA/WP2/00_zones"
dst_name <- "agmerra"
sY <- 1981
fY <- 2010
var <- "tmax"
add_pseudost(dDir, bDir, rDir, dst_name, sY, fY, var)


