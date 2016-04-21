##########################################################################################
## Purpose: Calculate uncertainties in anomalies from IDEAM data
## Author: Carlos Navarro c.e.navarro@cgiar.org | Lizeth Llanos l.llanos@cgiar.org
##########################################################################################

### 1- Uncertainties inter-model

bDir <- "T:/gcm/cmip5/3era_comunicacion_ideam/point_based"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/04-uncertainties"
st_loc <- "S:/observed/weather_station/col-ideam/stations_location.txt"
rg <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average/prec_1.asc"
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

readIdeamProjectionsGCM <- function(bDir="T:/gcm/cmip5/3era_comunicacion_ideam/point_based", oDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias_ideam/by_stations"){
  
  library(raster)
  require(reshape2)
  require(ggplot2)
  require(rgdal)
  
  st_loc <- read.table(st_loc, header=T, sep="\t")
  region <- raster(rg)
  
  ## Lista de variables y de RCP
  rcpList <- c("rcp26", "rcp45", "rcp85")
  varList <- c("prec","tmin", "tmax")
  id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
  for (var in varList){
    
    if (!file.exists(paste0(oDir, "/anomalias_ideam_gcm_", var,".csv"))) {
      
      anomSts <- c()
      
      #Ruta donde se encuentran los archivos .txt
      iDir_h = paste0(bDir, "/", var, "/adjusted/historical") 
      files <- list.files(iDir_h, pattern="\\.txt$")
      
      for (i in 1:length(files)){
        
        cat(var, unlist(strsplit(files[i], "-"))[[1]], "\n")
        ## Leer todos los archivos
        data <- read.table(paste(iDir_h,"/", files[i],sep=""), header=T, sep="\t")
        
        ## Promediar los anhos por meses
        year_avg =function(a){
          a = aggregate(a, by = list(a$Mes), FUN = "mean")
          return(a)
        }
        
        hist_avg = year_avg(data)
        
        for (rcp in rcpList){
          
          #Ruta donde se encuentran los archivos .txt
          iDir_f = paste0(bDir, "/", var, "/adjusted/", rcp) 
          
          ## Leer todos los archivos
          data <- read.table(paste(iDir_f,"/", files[i],sep=""), header=T, sep="\t")
          
          ## Seleccionar los anhos que necesitamos
          year_sel = function(x){
            pos = x[,1] < 2030 | x[,1] > 2059 | is.na(x[,1])
            x   = x[!pos, ]
            return(x)}
          
          data_year_sel = year_sel(data)
          
          ## Promediar los anhos por meses
          year_avg =function(a){
            a = aggregate(a, by = list(a$Mes), FUN = "mean")
            return(a)
          }
          
          fut_avg = year_avg(data_year_sel)
          
          if (var == "prec"){
            anom_avg <- (fut_avg - hist_avg)/ (hist_avg + 1) * 100
          } else {
            anom_avg <- fut_avg - hist_avg  
          } 
          
          
          anomSts <- rbind(anomSts, cbind("Station"=unlist(strsplit(files[i], "-"))[[1]],"Month"=1:12,"RCP"=rcp,anom_avg[, 4:ncol(anom_avg)]))
          
        }
        
      }
      
      # Agregar latitud y longitud
      anom_avg_coords <- na.omit(merge(st_loc, anomSts, by = "Station", all = TRUE))
      
      # Seleccionar las estaciones para la región
      pos = anom_avg_coords$Lon < xmin(region) | anom_avg_coords$Lon > xmax(region) | anom_avg_coords$Lat < ymin(region) | anom_avg_coords$Lat > ymax(region)
      anom_avg_coords = anom_avg_coords[!pos, ]
      
      # Escribir el archivo de salida
      write.csv(anom_avg_coords, paste0(oDir, "/anomalias_ideam_gcm_", var,".csv"), row.names = F)
      
    }
  }
  
  
  ## By station 
  
  for (var in varList){
    
    anom_avg_coords <- read.csv(paste0(oDir, "/anomalias_ideam_gcm_", var, ".csv"), header = T)
    
    coords <- SpatialPointsDataFrame(coords = data.frame(anom_avg_coords[,2:3]), data = data.frame(anom_avg_coords[,1]))
    st_sel <- as.data.frame(coords[mask, ])[,1]
    stLs <- st_sel[!duplicated(st_sel)]
    
    anom_avg_coords$Lon <- NULL
    anom_avg_coords$Lat <- NULL
    anom_avg_coords$Alt <- NULL
    
    anom_agg <- melt(anom_avg_coords, id.vars = c("Station", "RCP", "Month"))
    anom_agg$Month <- NULL 
    # anom_agg <- anom_agg[which(anom_agg$RCP != "rcp60"),]
    anom_agg_sel <- anom_agg[anom_agg$Station %in% stLs,]
    
    if (var == "prec"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "darkblue") +  
        geom_boxplot(colour="darkblue", fill="white", outlier.size = 1, outlier.colour = "darkblue") + 
        facet_wrap( ~ Station, ncol = 8) +
        scale_y_continuous(limits = c(-50, 50))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Prec %")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_gcm_llanos_", var, ".tif", sep=""), width=1000, height=1400, pointsize=5, compression='lzw',res=80)      
      
    } else if (var == "tmin"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "orange") +  
        geom_boxplot(colour="orange", fill="white", outlier.size = 1, outlier.colour = "orange") + 
        facet_wrap( ~ Station, ncol = 6) +
        scale_y_continuous(limits = c(0, 4))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Tmin ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_gcm_llanos_", var, ".tif", sep=""), width=1000, height=600, pointsize=5, compression='lzw',res=80)
      
    } else if (var == "tmax"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "red") +  
        geom_boxplot(colour="red", fill="white", outlier.size = 1, outlier.colour = "red") + 
        facet_wrap( ~ Station, ncol = 6) +
        scale_y_continuous(limits = c(0, 4))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Tmax ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_gcm_llanos_", var, ".tif", sep=""), width=1000, height=600, pointsize=5, compression='lzw',res=80)
      
    }
    
    
    print(p)
    dev.off() 
    
  }
  
  
  
  # By monthly cycle
  
  for (var in varList){
    
    anom_avg_coords <- read.csv(paste0(oDir, "/anomalias_ideam_gcm_", var, ".csv"), header = T)
    
    coords <- SpatialPointsDataFrame(coords = data.frame(anom_avg_coords[,2:3]), data = data.frame(anom_avg_coords[,1]))
    st_sel <- as.data.frame(coords[mask, ])[,1]
    stLs <- st_sel[!duplicated(st_sel)]
    
    anom_avg_coords$Lon <- NULL
    anom_avg_coords$Lat <- NULL
    anom_avg_coords$Alt <- NULL
    
    anom_agg <- melt(anom_avg_coords, id.vars = c("Station", "RCP", "Month"))
    # anom_agg$Month <- NULL 
    anom_agg <- anom_agg[which(anom_agg$RCP == "rcp85"),]
    anom_agg_sel <- anom_agg[anom_agg$Station %in% stLs,]
    
    if (var == "prec"){
      
      p <- ggplot(anom_agg_sel, aes(factor(Month),value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "darkblue") +  
        geom_boxplot(colour="darkblue", fill="white", outlier.size = 1, outlier.colour = "darkblue") + 
        facet_wrap( ~ RCP, ncol = 1) +
        scale_y_continuous(limits = c(-50, 50))+
        scale_x_discrete(labels=id) +
        xlab("Months") + 
        theme(text = element_text(size=14), strip.text.x = element_text(size = 14)) +
        ylab("Cambio en Prec %")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_monthly_llanos_", var, ".tif", sep=""), width=1000, height=400, pointsize=5, compression='lzw',res=80)      
      
    } else if (var == "tmin"){
      
      p <- ggplot(anom_agg_sel, aes(factor(Month),value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "orange") +  
        geom_boxplot(colour="orange", fill="white", outlier.size = 1, outlier.colour = "orange") + 
        facet_wrap( ~ RCP, ncol = 1) +
        scale_y_continuous(limits = c(0, 4))+
        scale_x_discrete(labels=id) +
        xlab("Months") + 
        theme(text = element_text(size=14), strip.text.x = element_text(size = 14)) +
        ylab("Cambio en Tmin ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_monthly_llanos_", var, ".tif", sep=""), width=1000, height=400, pointsize=5, compression='lzw',res=80)
      
    } else if (var == "tmax"){
      
      p <- ggplot(anom_agg_sel, aes(factor(Month),value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "red") +  
        geom_boxplot(colour="red", fill="white", outlier.size = 1, outlier.colour = "red") + 
        facet_wrap( ~ RCP, ncol = 1) +
        scale_y_continuous(limits = c(0, 4)) + 
        scale_x_discrete(labels=id) +
        xlab("Months") + 
        theme(text = element_text(size=14), strip.text.x = element_text(size = 14)) +
        ylab("Cambio en Tmax ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_monthly_llanos_", var, ".tif", sep=""), width=1000, height=400, pointsize=5, compression='lzw',res=80)
      
    }
    
    
    print(p)
    dev.off() 
    
  }
  
  
  
  ## By RCP 
  
  for (var in varList){
    
    anom_avg_coords <- read.csv(paste0(oDir, "/anomalias_ideam_gcm_", var, ".csv"), header = T)
    
    coords <- SpatialPointsDataFrame(coords = data.frame(anom_avg_coords[,2:3]), data = data.frame(anom_avg_coords[,1]))
    st_sel <- as.data.frame(coords[mask, ])[,1]
    stLs <- st_sel[!duplicated(st_sel)]
    
    anom_avg_coords$Lon <- NULL
    anom_avg_coords$Lat <- NULL
    anom_avg_coords$Alt <- NULL
    
    anom_agg <- melt(anom_avg_coords, id.vars = c("Station", "RCP", "Month"))
    anom_agg$Month <- NULL 
    # anom_agg <- anom_agg[which(anom_agg$RCP != "rcp60"),]
    anom_agg_sel <- anom_agg[anom_agg$Station %in% stLs,]
    
    if (var == "prec"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "darkblue") +  
        geom_boxplot(colour="darkblue", fill="white", outlier.size = 1, outlier.colour = "darkblue") + 
        # facet_wrap( ~ Station, ncol = 8) +
        scale_y_continuous(limits = c(-50, 50))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Prec %")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_rcp_llanos_", var, ".tif", sep=""), width=400, height=400, pointsize=5, compression='lzw',res=80)      
      
    } else if (var == "tmin"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "orange") +  
        geom_boxplot(colour="orange", fill="white", outlier.size = 1, outlier.colour = "orange") + 
        # facet_wrap( ~ Station, ncol = 6) +
        scale_y_continuous(limits = c(0, 4))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Tmin ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_rcp_llanos_", var, ".tif", sep=""), width=400, height=400, pointsize=5, compression='lzw',res=80)
      
    } else if (var == "tmax"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "red") +  
        geom_boxplot(colour="red", fill="white", outlier.size = 1, outlier.colour = "red") + 
        # facet_wrap( ~ Station, ncol = 6) +
        scale_y_continuous(limits = c(0, 4))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Tmax ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_rcp_llanos_", var, ".tif", sep=""), width=400, height=400, pointsize=5, compression='lzw',res=80)
      
    }
    
    
    print(p)
    dev.off() 
    
  }
  
  
}


### 2- Uncertainties inter-model anual

bDir <- "T:/gcm/cmip5/3era_comunicacion_ideam/point_based"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/04-uncertainties"
st_loc <- "S:/observed/weather_station/col-ideam/stations_location.txt"
rg <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average/prec_1.asc"
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

readIdeamProjectionsGCM_yr <- function(bDir="T:/gcm/cmip5/3era_comunicacion_ideam/point_based", oDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias_ideam/by_stations"){
  
  library(raster)
  require(reshape2)
  require(ggplot2)
  require(rgdal)
  
  st_loc <- read.table(st_loc, header=T, sep="\t")
  region <- raster(rg)
  
  ## Lista de variables y de RCP
  rcpList <- c("rcp26", "rcp45", "rcp85")
  varList <- c("prec","tmin", "tmax")
  id <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
  for (var in varList){
    
    if (!file.exists(paste0(oDir, "/anomalias_ideam_gcm_yearly_", var,".csv"))) {
      
      anomSts <- c()
      
      #Ruta donde se encuentran los archivos .txt
      iDir_h = paste0(bDir, "/", var, "/adjusted/historical") 
      files <- list.files(iDir_h, pattern="\\.txt$")
      
      for (i in 1:length(files)){
        
        cat(var, unlist(strsplit(files[i], "-"))[[1]], "\n")
        ## Leer todos los archivos
        data <- read.table(paste(iDir_h,"/", files[i],sep=""), header=T, sep="\t")
        
        if(var == 'prec'){
          hist_avg <- aggregate(data, by = list(data$Anyo), FUN = "sum")
        } else {
          hist_avg <- aggregate(data, by = list(data$Anyo), FUN = "mean")
        }
        
        hist_avg <- aggregate(hist_avg, by = list(hist_avg$Mes), FUN = "mean")
        
        
        for (rcp in rcpList){
          
          #Ruta donde se encuentran los archivos .txt
          iDir_f = paste0(bDir, "/", var, "/adjusted/", rcp) 
          
          ## Leer todos los archivos
          data <- read.table(paste(iDir_f,"/", files[i],sep=""), header=T, sep="\t")
          
          ## Seleccionar los anhos que necesitamos
          year_sel = function(x){
            pos = x[,1] < 2030 | x[,1] > 2059 | is.na(x[,1])
            x   = x[!pos, ]
            return(x)}
          
          data_year_sel = year_sel(data)
          
          if(var == 'prec'){
            fut_avg <- aggregate(data_year_sel, by = list(data_year_sel$Anyo), FUN = "sum")
          } else {
            fut_avg <- aggregate(data_year_sel, by = list(data_year_sel$Anyo), FUN = "mean")
          }
          
          fut_avg <- aggregate(fut_avg, by = list(fut_avg$Mes), FUN = "mean")
          
          
          if (var == "prec"){
            anom_avg <- (fut_avg - hist_avg)/ (hist_avg) * 100
          } else {
            anom_avg <- fut_avg - hist_avg  
          } 
          
          
          anomSts <- rbind(anomSts, cbind("Station"=unlist(strsplit(files[i], "-"))[[1]],"RCP"=rcp,anom_avg[, 5:ncol(anom_avg)]))
          
        }
        
      }
      
      # Agregar latitud y longitud
      anom_avg_coords <- na.omit(merge(st_loc, anomSts, by = "Station", all = TRUE))
      
      # Seleccionar las estaciones para la región
      pos = anom_avg_coords$Lon < xmin(region) | anom_avg_coords$Lon > xmax(region) | anom_avg_coords$Lat < ymin(region) | anom_avg_coords$Lat > ymax(region)
      anom_avg_coords = anom_avg_coords[!pos, ]
      
      # Escribir el archivo de salida
      write.csv(anom_avg_coords, paste0(oDir, "/anomalias_ideam_gcm_yearly_", var,".csv"), row.names = F)
      
    }
  }
  
  
  
  ## By RCP 
  
  for (var in varList){
    
    anom_avg_coords <- read.csv(paste0(oDir, "/anomalias_ideam_gcm_yearly_", var, ".csv"), header = T)
    
    coords <- SpatialPointsDataFrame(coords = data.frame(anom_avg_coords[,2:3]), data = data.frame(anom_avg_coords[,1]))
    st_sel <- as.data.frame(coords[mask, ])[,1]
    stLs <- st_sel[!duplicated(st_sel)]
    
    anom_avg_coords$Lon <- NULL
    anom_avg_coords$Lat <- NULL
    anom_avg_coords$Alt <- NULL
    
    anom_agg <- melt(anom_avg_coords, id.vars = c("Station", "RCP"))
    # anom_agg <- anom_agg[which(anom_agg$RCP != "rcp60"),]
    anom_agg_sel <- anom_agg[anom_agg$Station %in% stLs,]
    
    if (var == "prec"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "darkblue") +  
        geom_boxplot(colour="darkblue", fill="white", outlier.size = 1, outlier.colour = "darkblue") + 
        # facet_wrap( ~ Station, ncol = 8) +
        scale_y_continuous(limits = c(-50, 50))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Prec %")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_rcp_yearly_llanos_", var, ".tif", sep=""), width=400, height=400, pointsize=5, compression='lzw',res=80)      
      
    } else if (var == "tmin"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "orange") +  
        geom_boxplot(colour="orange", fill="white", outlier.size = 1, outlier.colour = "orange") + 
        # facet_wrap( ~ Station, ncol = 6) +
        scale_y_continuous(limits = c(0, 4))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Tmin ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_rcp_yearly_llanos_", var, ".tif", sep=""), width=400, height=400, pointsize=5, compression='lzw',res=80)
      
    } else if (var == "tmax"){
      
      p <- ggplot(anom_agg_sel, aes(RCP,value)) + 
        stat_boxplot(geom = "errorbar", width = 0.5, colour = "red") +  
        geom_boxplot(colour="red", fill="white", outlier.size = 1, outlier.colour = "red") + 
        # facet_wrap( ~ Station, ncol = 6) +
        scale_y_continuous(limits = c(0, 4))+
        theme(text = element_text(size=14)) +
        xlab("") + 
        ylab("Cambio en Tmax ºC")
      tiff(paste(oDir, "/anomalies_ideam_boxplot_rcp_yearly_llanos_", var, ".tif", sep=""), width=400, height=400, pointsize=5, compression='lzw',res=80)
      
    }
    
    print(p)
    dev.off() 
    
  }
  
  
  
}





### 2a- Uncertainties inter-model seasonal

library(raster)
require(reshape2)
require(ggplot2)
require(rgdal)

bDir <- "T:/gcm/cmip5/3era_comunicacion_ideam/point_based"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/04-uncertainties"
st_loc <- "S:/observed/weather_station/col-ideam/stations_location.txt"
rg <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average/prec_1.asc"
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

readIdeamProjectionsGCM_seasons <- function(bDir="T:/gcm/cmip5/3era_comunicacion_ideam/point_based", oDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias_ideam/by_stations"){
  
  seasons <- list("def"=c(12, 1, 2), "mam"=3:5, "jja"=6:8, "son"=9:11, "anu"=1:12)
  st_loc <- read.table(st_loc, header=T, sep="\t")
  region <- raster(rg)
  
  ## Lista de variables y de RCP
  rcpList <- c("rcp26", "rcp45", "rcp85")
  varList <- c("prec","tmin", "tmax")
  # var <- varList[1]
  for (var in varList){
    
    downSts <- c()
    anomSts <- c()
    
    if (!file.exists(paste0(oDir, "/downscaling_ideam_gcm_seasons_", var,".csv"))) {

      #Ruta donde se encuentran los archivos .txt
      iDir_h = paste0(bDir, "/", var, "/adjusted/historical") 
      iDir_o = paste0(bDir, "/", var, "/adjusted/observations") 
      # files <- list.files(iDir_h, pattern="\\.txt$")
      a <- c(2113502, 2114001, 2114003, 2114012, 2116003, 2116004, 2116005, 2116008, 2116020, 2117002, 2118016, 2118502, 2118503, 2118504, 2119009, 2119021, 2119024, 2119029, 2119030, 2119031, 2119035, 2119506, 2119507, 2119510, 2120008, 2120011, 2120013, 2120019, 2120020, 2120023, 2120024, 2120026, 2120027, 2120031, 2120032, 2120034, 2120040, 2120043, 2120051, 2120052, 2120055, 2120060, 2120069, 2120074, 2120077, 2120080, 2120085, 2120086, 2120088, 2120103, 2120106, 2120107, 2120109, 2120110, 2120112, 2120114, 2120121, 2120122, 2120123, 2120124, 2120125, 2120154, 2120158, 2120164, 2120166, 2120172, 2120176, 2120196, 2120197, 2120212, 2120516, 2120524, 2120539, 2120540, 2120541, 2120542, 2120557, 2120558, 2120565, 2120566, 2120567, 2120569, 2120570, 2120571, 2120572, 2120574, 2120577, 2120579, 2120592, 2120602, 2120606, 2120614, 2120629, 2121508, 2123009, 2125043, 2125046, 2205503, 2301008, 2301504, 2302010, 2302502, 2303502, 2304003, 2304007, 2305025, 2305504, 2306011, 2306014, 2306016, 2306017, 2306018, 2306019, 2306511, 2306512, 2308076, 2308508, 2309002, 2309501, 2310003, 2310004, 2310503, 2312001, 2312005, 2312012, 2312020, 2312021, 2312024, 2312504, 2312505, 2314004, 2314502, 2317502, 2401007, 2401014, 2401015, 2401018, 2401023, 2401028, 2401039, 2401044, 2401051, 2401066, 2401080, 2401511, 2401512, 2401521, 2401522, 2401526, 2403012, 2403027, 2403030, 2403031, 2403034, 2403035, 2403038, 2403040, 2403047, 2403053, 2403054, 2403055, 2403057, 2403058, 2403061, 2403063, 2403064, 2403066, 2403067, 2403068, 2403069, 2403074, 2403077, 2403078, 2403079, 2403080, 2403082, 2403084, 2403085, 2403094, 2403104, 2403501, 2403512, 2403513, 2403514, 2403515, 2403517, 2403518, 2403525, 2403526, 2403527, 2403530, 2403531, 2403532, 2403533, 2403534, 2404005, 2404006, 2405006, 2405007, 2405010, 2405503, 2405504, 2406005, 2406006, 2406007, 3109001, 3109501, 3201001, 3203002, 3203502, 3204001, 3206002, 3206003, 3206006, 3206501, 3207001, 3207002, 3207003, 3207004, 3207006, 3207008, 3207009, 3207010, 3207011, 3207503, 3207504, 3207505, 3208001, 3209001, 3210507, 3215004, 3215006, 3220001, 3303501, 3305002, 3306001, 3401501, 3403501, 3403502, 3501001, 3501004, 3501006, 3501007, 3501008, 3501009, 3502001, 3502002, 3502006, 3502007, 3502024, 3502028, 3502029, 3502030, 3502031, 3502034, 3502038, 3502039, 3502041, 3502042, 3502502, 3502505, 3502506, 3503002, 3503003, 3503005, 3503008, 3503012, 3503014, 3503016, 3503017, 3503023, 3503026, 3503030, 3503501, 3503502, 3503503, 3503507, 3504001, 3505002, 3505501, 3506002, 3506005, 3506009, 3506010, 3506012, 3506013, 3506014, 3506015, 3506016, 3506017, 3506018, 3506020, 3506021, 3506022, 3506023, 3506024, 3506025, 3506030, 3506040, 3506501, 3507001, 3507002, 3507003, 3507004, 3507005, 3507006, 3507007, 3507008, 3507009, 3507010, 3507011, 3507012, 3507013, 3507018, 3507019, 3507020, 3507021, 3507022, 3507023, 3507026, 3507045, 3507047, 3507048, 3507049, 3507050, 3507052, 3507055, 3507501, 3507502, 3507503, 3507504, 3508001, 3508003, 3508005, 3508006, 3508007, 3508008, 3508010, 3508011, 3508013, 3508502, 3508504, 3508505, 3509001, 3509004, 3509005, 3509006, 3509007, 3509502, 3509503, 3510002, 3512001, 3512501, 3513001, 3518001, 3518003, 3518005, 3519001, 3519002, 3519004, 3519005, 3519007, 3519502, 3519503, 3519505, 3521501, 3522003, 3522502, 3523001, 3523501, 3525001, 3525002, 3525003, 3525004, 3526001, 3526002, 3526003, 3526005, 3526007, 3526008, 3603002, 3701502, 3801003, 3801503, 3802002, 3803001, 4601501, 4601502)
      files <- paste0(a, "-Todos.txt")
      
      for (i in 1:length(files)){
        
        cat(var, unlist(strsplit(files[i], "-"))[[1]], "\n")
        ## Leer todos los archivos
        data <- read.table(paste(iDir_h,"/", files[i],sep=""), header=T, sep="\t")
        data_o <- read.table(paste(iDir_o,"/", files[i],sep=""), header=T, sep="\t")
        
        for (s in 1:length(seasons)){

          data_sel <- data[data$Mes %in% seasons[[s]],]
          data_sel_o <- data_o[data_o$Mes %in% seasons[[s]],]
          
          if(var == 'prec'){
            hist_avg <- aggregate(data_sel, by = list(data_sel$Anyo), FUN = "sum")
            hist_avg_o <- aggregate(data_sel_o, by = list(data_sel_o$Anyo), FUN = "sum")
          } else {
            hist_avg <- aggregate(data_sel, by = list(data_sel$Anyo), FUN = "mean")
            hist_avg_o <- aggregate(data_sel_o, by = list(data_sel_o$Anyo), FUN = "mean")
          }
          
          hist_avg <- aggregate(hist_avg, by = list(hist_avg$Mes), FUN = "mean")
          hist_avg_o <- aggregate(hist_avg_o, by = list(hist_avg_o$Mes), FUN = "mean")
          
          
          for (rcp in rcpList){
            
            #Ruta donde se encuentran los archivos .txt
            iDir_f = paste0(bDir, "/", var, "/adjusted/", rcp) 
            
            ## Leer todos los archivos
            data_f <- read.table(paste(iDir_f,"/", files[i],sep=""), header=T, sep="\t")
            
            ## Seleccionar los anhos que necesitamos
            year_sel = function(x){
              pos = x[,1] < 2030 | x[,1] > 2059 | is.na(x[,1])
              x   = x[!pos, ]
              return(x)}
            
            data_year_sel = year_sel(data_f)
            data_sel_f <- data_year_sel[data_year_sel$Mes %in% seasons[[s]],]
            
            if(var == 'prec'){
              fut_avg <- aggregate(data_sel_f, by = list(data_sel_f$Anyo), FUN = "sum")
            } else {
              fut_avg <- aggregate(data_sel_f, by = list(data_sel_f$Anyo), FUN = "mean")
            }
            
            fut_avg <- aggregate(fut_avg, by = list(fut_avg$Mes), FUN = "mean")
            
            
            if (var == "prec"){
              anom_avg <- (fut_avg - hist_avg)/ (hist_avg) * 100
            } else {
              anom_avg <- fut_avg - hist_avg  
            } 
            
            anomSts <- rbind(anomSts, cbind("Station"=unlist(strsplit(files[i], "-"))[[1]],"Season"=names(seasons[s]),"RCP"=rcp,anom_avg[, 5:ncol(anom_avg)]))
            
            
            if (var == "prec"){
              down_avg <- hist_avg_o$Valor * abs(1 + anom_avg/100)
            } else {
              down_avg <- hist_avg_o + anom_avg
            } 
            
            downSts <- rbind(downSts, cbind("Station"=unlist(strsplit(files[i], "-"))[[1]],"Season"=names(seasons[s]),"RCP"=rcp,down_avg[, 5:ncol(down_avg)]))
            
            
          }
          
        }
        
        
      }

      # Agregar latitud y longitud
      anom_avg_coords <- na.omit(merge(st_loc, anomSts, by = "Station", all = TRUE))

      # Seleccionar las estaciones para la región
      pos = anom_avg_coords$Lon < xmin(region) | anom_avg_coords$Lon > xmax(region) | anom_avg_coords$Lat < ymin(region) | anom_avg_coords$Lat > ymax(region)
      anom_avg_coords = anom_avg_coords[!pos, ]

      # Escribir el archivo de salida
      write.csv(anom_avg_coords, paste0(oDir, "/anomalias_ideam_gcm_seasons_", var,".csv"), row.names = F)


      # Agregar latitud y longitud
      down_avg_coords <- na.omit(merge(st_loc, downSts, by = "Station", all = TRUE))
      
      # Seleccionar las estaciones para la región
      pos = down_avg_coords$Lon < xmin(region) | down_avg_coords$Lon > xmax(region) | down_avg_coords$Lat < ymin(region) | down_avg_coords$Lat > ymax(region)
      down_avg_coords = down_avg_coords[!pos, ]
      
      # Escribir el archivo de salida
      write.csv(down_avg_coords, paste0(oDir, "/downscaling_ideam_gcm_seasons_", var,".csv"), row.names = F)
      
    }
    
  }
  
}



### 3- Uncertainties inter-model CV

######################################################
##Función para Crear Mapas de Correlación de Pearson##
######Equipo de Modelación - CIAT Enero 2015##########
######################################################

require(raster)
require(grid)
require(ggplot2)
require(rgeos)
require(reshape2)
require(rgdal)

oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/04-uncertainties"
st_loc <- "S:/observed/weather_station/col-ideam/stations_location.txt"
mask <- readOGR("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/buffer_llanos/Llanos.shp", layer= "Llanos")

rcpList <- c("rcp26", "rcp45", "rcp85")
varList <- c("prec","tmin", "tmax")
st_loc <- read.table(st_loc, header=T, sep="\t")

for (var in varList){
  
  if (var == "prec"){
    anom_avg_coords <- read.csv(paste0(oDir, "/downscaling_ideam_gcm_seasons_", var, ".csv"), header = T)  
  } else {
    anom_avg_coords <- read.csv(paste0(oDir, "/anomalias_ideam_gcm_seasons_", var, ".csv"), header = T)  
  }
  
  coords <- SpatialPointsDataFrame(coords = data.frame(anom_avg_coords[,2:3]), data = data.frame(anom_avg_coords[,1]))
  st_sel <- as.data.frame(coords[mask, ])[,1]
  stLs <- st_sel[!duplicated(st_sel)]
  
  anom_avg_coords$Lon <- NULL
  anom_avg_coords$Lat <- NULL
  anom_avg_coords$Alt <- NULL
  
  anom_agg <- melt(anom_avg_coords, id.vars = c("Station", "RCP", "Season"))
  # anom_agg$Month <- NULL 
  anom_agg_sel <- anom_agg[anom_agg$Station %in% stLs,]
  
  
  meanfx = function(x){
    mean(x)
    return(x)
  }
  
  co.var = function(x){
    x = sd(x)
    return(x)
  }
  
  anom_agg_sel_avg = aggregate(anom_agg_sel, list(Station=anom_agg_sel$Station, RCP=anom_agg_sel$RCP, Season=anom_agg_sel$Season),mean)
  anom_agg_sel_std = aggregate(anom_agg_sel, list(Station=anom_agg_sel$Station, RCP=anom_agg_sel$RCP, Season=anom_agg_sel$Season),co.var)
  
  anom_agg_sel_avg$value[anom_agg_sel_avg$value < 1 & anom_agg_sel_avg$value > 0] <- 1
  anom_agg_sel_avg$value[anom_agg_sel_avg$value < 0 & anom_agg_sel_avg$value > -1] <- -1
  
  anom_agg_sel_cv <- cbind(anom_agg_sel_avg, "Std"=anom_agg_sel_std$value, "CV"=(anom_agg_sel_std$value/abs(anom_agg_sel_avg$value)))
  anom_agg_sel_cv <- cbind(anom_agg_sel_cv[,1:3],"CV"=anom_agg_sel_cv[,ncol(anom_agg_sel_cv)])
  anom_agg_sel_cv <- na.omit(merge(st_loc, anom_agg_sel_cv, by = "Station", all = TRUE))
  
  anom_agg_sel_cv$Alt <- NULL
  
  names(anom_agg_sel_cv)=c("Station","Lon","Lat","RCP", "Season","CV")
  
  anom_agg_sel_cv$Season <- factor(anom_agg_sel_cv$Season,
                         levels = c("def", "mam", "jja", "son", "anu"))
  # col2=extent(-78,-72,0,10)
  # colombia=crop(colombia,col2)
  
  mask@data$id <- rownames(mask@data)
  region <- fortify(mask)
  
  if (var == "prec"){
    uplimit <- 0; dwlimit <- 1; step <- 0.1; low <- "red"; mid <- "white"; high <- "blue"; uplimit_size <- 0; dwlimit_size <- 6; step_size <- 0.1
  } else if (var == "tmax") {
    uplimit <- 0; dwlimit <- 1; step <- 0.1 ; low <- "white"; mid <- "yellow"; high <- "red"; uplimit_size <- 0; dwlimit_size <- 6; step_size <- 0.1
  } else if (var == "tmin") {
    uplimit <- 0; dwlimit <- 1; step <- 0.1 ; low <- "white"; mid <- "yellow"; high <- "red"; uplimit_size <- 0; dwlimit_size <- 6; step_size <- 0.1
  }

  
  p <- ggplot(region, aes(x=long,y=lat)) + 
    geom_polygon(aes(fill=hole,group=group),fill="grey 80") + 
    scale_fill_manual(values=c("grey 80","grey 80")) +
    facet_grid(RCP ~ Season) +
    geom_path(aes(long,lat,group=group,fill=hole),color="white",size=0.3) + 
    geom_point(data=anom_agg_sel_cv, aes(x=Lon, y=Lat, map_id=Station, size=CV, col=CV)) + 
    geom_point(data=anom_agg_sel_cv, aes(x=Lon, y=Lat, size=CV), shape = 1, colour = "black") +
    scale_color_gradient2(name="CV", low = low, mid=mid, high = high,
                          limits=c(uplimit,dwlimit),guide="colourbar",
                          breaks=seq(uplimit,dwlimit,by=step), labels=paste(seq(uplimit,dwlimit,by=step))) +
    scale_size(limits=c(uplimit,dwlimit)) +
    coord_equal() + 
    theme(legend.text=element_text(size=10), #panel.background=element_rect(fill="white",colour="black"),
                 axis.text=element_text(colour="black",size=9), strip.text=element_text(colour="black",size=12)) +
    xlab("") + 
    ylab("")
  
  
  tiff(paste(oDir,"/anomalies_ideam_cv_rcp_seasons_llanos_",var,".tif",sep=""), height=600,width=1200,res=100, pointsize=10,compression="lzw")
    print(p)
  dev.off()
  
}









