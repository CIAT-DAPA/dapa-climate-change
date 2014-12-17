setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/01-climate-data/bc_0_5deg_lat/bcc_csm1_1/1971_2000/by_month")

library(snowfall)
sfInit( parallel=TRUE, cpus=7)
sfLibrary(snowfall)
sfLibrary(raster)
sfLibrary (sp)
sfLibrary (rgdal)
sfLibrary (maps)
sfLibrary (mapproj)
sfLibrary(stringr)    ## libreria necesaria para las funciones de tipo caracter
sfLibrary(date)       ## configuracion de fecha tipo dia juliano
sfLibrary(ncdf)

sfSource("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/_scripts/mainFunctions.R", encoding="latin1")
sfExportAll()

Raster_precipitacion=sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Prec[[j]][i])))
## Raster_precipitacion[[1]][[1]][[1]] 

Raster_TempMax=sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmax[[j]][i])))
##Raster_TempMax[[1]][[1]][[1]] 

Raster_TempMin=sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Tmin[[j]][i])))
##Raster_TempMin[[1]][[1]][[1]] 

Raster_Radiacion=sfLapply(1:30,function(j) sfLapply(1:12,function(i) lecturabandas(SerieAnual_Rad[[j]][i])))


setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/_scripts")

save(file="readclimate.Rdat")


sfStop()
coordenadas<-rasterToPoints(Raster_precipitacion[[1]][[1]][[1]])[,1:2]