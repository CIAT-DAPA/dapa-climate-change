# CNR

#############################
### 04- Climatology Calcs ###
#############################

clim_calc <- function(var="prec",  bDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE", oDir = "Z:/DATA/WP2/01_Weather_Stations/MERGE", stDir="Z:/DATA/WP2/01_Weather_Stations/MERGE", sY=1981, fY=2010, srtm="", dst_name=""){
  
  
  monthly_var <- read.csv(paste0(bDir, "/", var, "_merge_fill_", dst_name, ".csv"), header=T) 
  
  if (!file.exists(oDir)) {dir.create(oDir, recursive = TRUE)}
  
  if (var == "prec"){varmod <- "prec"} 
  
  ## Climatology aggregation based on NA percent
  avg_var = function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    if(na.x>=0.6667){
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
  
  for (yr in sY:fY){
    
    sY_m <- yr - 2
    fY_m <- yr + 2
    
    if (fY_m > fY){
      fY_m <- fY
    }
    
    # Years selection
    monthly_var_sel <- monthly_var[ which(monthly_var$year >= sY_m & monthly_var$year <= fY_m),]
    
    # Period climatology calc
    monthly_avg = aggregate(monthly_var_sel[,3:length(monthly_var_sel)], list(month=monthly_var_sel$month),avg_var)
    na_p <- round( (1- apply(monthly_var_sel[,3:length(monthly_var_sel)], 2, na_per)) * (fY_m - sY_m), digits = 0)
    
    # Remove unnecesary columns if exists
    monthly_avg$month <- NULL;  monthly_avg$Month <- NULL;  monthly_avg$year <- NULL
    
    # Transpose
    st_names <- as.data.frame(names(monthly_avg))
    colnames(st_names) <- "code"
    monthly_avg <- cbind(st_names, round(t(monthly_avg), 1))
    
    # Remove all rows with NA
    monthly_avg <- monthly_avg[rowSums(is.na(monthly_avg[,2:ncol(monthly_avg)])) < 12, ]
    
    rownames(monthly_avg) <- NULL
    
    # Add month names
    mths <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    colnames(monthly_avg) <- c("code", mths)
    monthly_avg$code <- unlist(lapply(strsplit(gsub("X", "", monthly_avg$code),"_"),"[[",1))
    
    ## Add station info
    stInfo <- read.csv(paste0(stDir, "/catalog_merge_", varmod, ".csv"), header=T)
    join <- merge(monthly_avg, stInfo, by = "code", all = FALSE, sort=F)
    
    # ## NA percent
    # na_p <- cbind(st_names, na_p)
    # 
    # names(na_p) <- c("code", "NYEARS")
    # na_p$code <-  unlist(lapply(strsplit(gsub("X", "",  na_p$code),"_"),"[[",1))
    
    # Reg selection
    rg_ext <- extent(rg)
    join <- join[ which(join$lon >= xmin(rg_ext) & join$lon <= xmax(rg_ext)),]
    join <- join[ which(join$lat >= ymin(rg_ext) & join$lat <= ymax(rg_ext)),]
    
    # join <- merge(join, na_p, by = "Station", all = FALSE)
    join[is.na(join)] <- -9999.9
    
    join$alt <- raster::extract(x=raster(srtm), y=cbind(join$lon, join$lat), method = 'bilinear')
    join$nyears <- 30
    
    # Combine info and data
    climData <- cbind(join[ , c("code", "inst", "code", "station", "inst","lon", "lat", "alt", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "nyears")])
    names(climData) <- c("ID", "SOURCE", "OLD_ID","NAME","COUNTRY","LONG","LAT","ALT","JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC","NYEARS")
    
    # Write climatology 
    if(var == "prec"){
      write.csv(climData, paste0(oDir, "/rain_", yr, ".csv"), row.names=F)
    } else {
      write.csv(climData, paste0(oDir, "/", var, "_", yr, ".csv"), row.names=F)
    }
    
    
    
  }
  
}
