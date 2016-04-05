iDir <- "D:/CIAT/Projects/ecu-hidroelectrica/01_station_data/st_data_inamhi_raw/MENSUAL"
st_loc <- "D:/CIAT/Projects/ecu-hidroelectrica/01_station_data/st_data_inamhi_raw/inamhi_all_stations_month_catalog.csv"
varList <- c("prec", "tmax", "tmin")

st_loc <- read.csv(st_loc)

st_coords <- as.data.frame(cbind("Station"=paste0(st_loc$cod_red_E_), "StName"= paste0(st_loc$nombre_E_M),"Lon"=st_loc$longitud, "Lat"=st_loc$latitud, "Alt"=st_loc$altitud))

for (var in varList){

  st_data <- read.table(paste0(iDir, "/", var, "_all_stations_proc.txt"), header=T)
  
  avg_var = function(a,na.rm=T){
    na.x = length(which(is.na(a))) / length(a)
    if(na.x>=0.20){
      x = NA
    }else{
      x = mean(a, na.rm = any(!is.na(a))) }
    return(x)}
  
  
  year_sel = function(x){
    pos = x[,2] <= 1981 | x[,2] >= 2010
    x   = x[!pos, ]
    return(x)}
  
  st_data_year_sel = year_sel(st_data)
  
  st_mth_30yr_avg <- aggregate(st_data_year_sel[,3:14], list(Station=st_data_year_sel$Station),avg_var)
  
  ## Add coordinates to the climatologies files
  st_mth_30yr_avg <- na.omit(merge(st_coords, st_mth_30yr_avg, by = "Station", all = TRUE))
  write.csv(st_mth_30yr_avg, paste0(iDir, "/", var, "_all_stations_1981-2010_avg.csv"), row.names=F)
  
  
}
