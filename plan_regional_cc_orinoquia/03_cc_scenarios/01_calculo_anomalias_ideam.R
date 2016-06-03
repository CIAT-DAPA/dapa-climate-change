##########################################################################################
## Purpose: Calculate anomalies from IDEAM data
## Author: Carlos Navarro c.e.navarro@cgiar.org
##########################################################################################

bDir <- "T:/gcm/cmip5/3era_comunicacion_ideam/point_based"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias_ideam/by_stations"

readIdeamProjections <- function(bDir="T:/gcm/cmip5/3era_comunicacion_ideam/point_based", oDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias_ideam/by_stations"){
  
  ## Folders de entrada 
  
  ## Lista de variables y de RCP
  rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  varList <- c("prec", "tmin", "tmax")
  
  for (var in varList){
    
    #Ruta donde se encuentran los archivos .txt
    iDir = paste0(bDir, "/", var, "/anomalies") 
    files <- list.files(iDir, pattern="\\.txt$")
    st_list <- unlist(lapply(strsplit(files,"-"), function(x) x[1]))
    
    ## Leer todos los archivos
    data <- lapply(paste(iDir,"/", files,sep=""), function(x){read.table(x, header=T, sep="\t")})
    
    ## Seleccionar los anhos que necesitamos
    year_sel = function(x){
      pos = x[,1] < 2030 | x[,1] > 2059 | is.na(x[,1])
      x   = x[!pos, ]
      return(x)}
    
    data_year_sel = lapply(data, year_sel)
    
    ## Promediar los anhos por meses
    year_avg =function(a){
      a = aggregate(a, by = list(a$Mes), FUN = "mean")
      return(a)
    }
    
    anom_avg = lapply(data_year_sel, year_avg)
    
    ## Escribir los valores de las anomalias en un archivo por RCP
    for(i in 1:length(rcpList)){
      
      cat(var, rcpList[i], "\n")
      # Crear una matrix en blanco
      anom_avg_all_st = as.data.frame(matrix(NA,nrow=12,ncol=length(anom_avg)))
      
      # Rellenar la matrix con los valores de las anomalias
      for(j in 1:length(anom_avg)) {  
        anom_avg_all_st[,j] <- anom_avg[[j]][,i+3]
      }
      
      anom_avg_all_st = cbind(st_list, t(round(anom_avg_all_st, digits = 3)))
      colnames(anom_avg_all_st) = c("Estacion", 1:12)
      
      # Escribir el archivo de salida
      write.csv(anom_avg_all_st, paste0(oDir, "/anomalias_ideam_", var, "_", rcpList[i],".csv"), row.names = F)
      
    }
    
  }
  
}


bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias/ideam"
st_loc <- "S:/observed/weather_station/col-ideam/stations_location.txt"
## Interpolated region
rg <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/average/prec_1.asc"
# rcp <- "rcp26"
## Region with deparments
msk <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/mask.tif"

spatialIDW <- function(bDir="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/anomalias_ideam", st_loc="S:/observed/weather_station/col-ideam/stations_location.txt", rg="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/monthly-interpolations/_region/alt-prj-lla.asc", rcp="rcp26", mask="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/_masks/mask.tif"){
  
  library(raster)
  # library(ggplot2)
  library(gstat)
  library(sp)
  library(maptools)
  require(rgdal)
  
  ## Lista de variables y de RCP
  rcpList <- c("rcp26", "rcp45", "rcp60", "rcp85")
  varList <- c("prec", "tmin", "tmax")
  
  st_loc <- read.table(st_loc, header=T, sep="\t")
  region <- raster(rg)
  mask <- raster(msk)
  # mask <- readOGR(msk, layer= "Llanos")
  
  for (rcp in rcpList){
    
    for (var in varList){
      
      cat(var, rcp, "\n")
      
      oDir <- paste0(bDir, "/", rcp)
      if (!file.exists(oDir)) {dir.create(oDir, recursive=T)}
      
      # Leer el archivo de anomalías 
      anom_avg <- read.csv(paste0(bDir, "/by_stations/anomalias_ideam_", var, "_", rcp,".csv"), header=T)
      colnames(anom_avg)[1] <- "Station"
      
      # Eliminar valores negativos
      if (var == "prec"){

        pos=c()
        for(i in 1:nrow(anom_avg)){pos[i]=ifelse(any(anom_avg[i,-1]>40),i,NA)}
        
        if(length(pos[-which(is.na(pos))])==0){
          anom_avg=anom_avg
        }else{
            anom_avg = anom_avg[-pos[-which(is.na(pos))],]
        }
        
        pos=c()
        for(i in 1:nrow(anom_avg)){pos[i]=ifelse(any(anom_avg[i,-1]<40),i,NA)}
        
        if(length(pos[-which(is.na(pos))])==0){
          anom_avg=anom_avg
        }else{
          anom_avg = anom_avg[-pos[-which(is.na(pos))],]
        }
        
      } else {
        
        
        # Eliminar valores negativos & muy altos
        pos=c()
        for(i in 1:nrow(anom_avg)){pos[i]=ifelse(any(anom_avg[i,]<0),i,NA)}
        anom_avg = anom_avg[-pos[-which(is.na(pos))],]
        
        pos=c()
        for(i in 1:nrow(anom_avg)){pos[i]=ifelse(any(anom_avg[i,-1]>5),i,NA)}
        
        if(length(pos[-which(is.na(pos))])==0){
          anom_avg=anom_avg
        }else{
          anom_avg = anom_avg[-pos[-which(is.na(pos))],]
        }
        
      }
      
      
      # Agregar latitud y longitud
      anom_avg_coords <- na.omit(merge(st_loc, anom_avg, by = "Station", all = TRUE))
      
      # Seleccionar las estaciones para la región
      pos = anom_avg_coords$Lon < xmin(region) | anom_avg_coords$Lon > xmax(region) | anom_avg_coords$Lat < ymin(region) | anom_avg_coords$Lat > ymax(region)
      anom_avg_coords = anom_avg_coords[!pos, ]
      
      # Define x & y as longitude and latitude
      anom_region <- anom_avg_coords
      anom_region$x <- anom_avg_coords$Lon  
      anom_region$y <- anom_avg_coords$Lat
        
      for(i in 1:12){
        
        anom_region_i <- as.data.frame(cbind(anom_avg_coords$Lon, anom_avg_coords$Lat, anom_avg_coords[,4+i]))
        colnames(anom_region_i) <- c("x", "y", "z")
        
        #Set spatial coordinates to create a Spatial object:
        coordinates(anom_region_i) = ~x + y
        
        # Create a data frame from all combinations of the supplied vectors or factors. See the description of the return value for precise details of the way this is done. Set spatial coordinates to create a Spatial object. Assign gridded structure:
        # Expand points to grid
        grd <- expand.grid(x = seq(from = xmin(region), to = xmax(region), by = 0.008333334), y = seq(from = ymin(region), to = ymax(region), by = 0.008333334))  
        coordinates(grd) <- ~x + y
        gridded(grd) <- TRUE

        # Apply idw model for the data
        idw <- idw(formula = z ~ 1, locations = anom_region_i, newdata = grd, idp=3)
        
        oIdw <- setExtent(raster(idw), region)
        writeRaster(oIdw, paste0(oDir, "/", var, "_", i, ".asc"))
        
        oIdw_msk <- oIdw * 10
        oIdw_msk <- crop(oIdw_msk, extent(mask))
        oIdw_msk <- mask(oIdw_msk, mask)
        writeRaster(oIdw_msk, paste0(oDir, "/", var, "_", i, ".tif"), format="GTiff", overwrite=F, datatype='INT2S')
        

#         jpeg(paste(oDir, "/", var, "_", i, ".jpg", sep=""), width = 800, height = 500)
#           plot(oIdw)
#         dev.off()
        
        ## Plot ggplot
        ## ggplot() + geom_tile(data = idw.output, aes(x = Lon, y = Lat, fill = X1)) 
        
      }

    }
    
  }
  
}

spatialIDW(bDir, st_loc, rg, rcp, msk)





