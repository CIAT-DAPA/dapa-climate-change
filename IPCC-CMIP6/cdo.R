###############################
# Proposito: Pasos para downscaling CMIP6
# Autor: Jaime Tarapues
# Fechas: 2020
# jaime.tm8@gmail.com
# Se recomienda correr desde Linux, htCondor
############################


##########################
# 01 - organizar archivos
##########################

#dirbase="/dapadfs/workspace_cluster_13/ADAA/CMIP6/raw"
dirbase="X:/ADAA/CMIP6/raw"
outdir="X:/ADAA/CMIP6/monthly"
listNC <-  list.files(dirbase, recursive = FALSE, full.names = FALSE,pattern = ".nc") 
for(nc in listNC){
  var=tolower(sapply(strsplit(nc, '[_]'), "[[", 1))   
  gcm=tolower(sapply(strsplit(nc, '[_]'), "[[", 3))
  periodo=gsub(".nc","",tolower(sapply(strsplit(nc, '[_]'), "[[", 7)))
  yi=sapply(strsplit(periodo, '[-]'), "[[", 1)
  yf=sapply(strsplit(periodo, '[-]'), "[[", 2)    
  ens=tolower(sapply(strsplit(nc, '[_]'), "[[", 5))
  grilla=tolower(sapply(strsplit(nc, '[_]'), "[[", 6))
  rcp=tolower(sapply(strsplit(nc, '[_]'), "[[", 4))
  
  outrcp=paste0(outdir,"/",rcp)
  dir.create(outrcp,showWarnings=FALSE,recursive = T)
  file.copy(paste0(dirbase,"/",nc), outrcp,overwrite = F)
  #system(paste0("mv -r ",,)) # linux
  #system(paste0("robocopy ",,)) # win
  
}
#######################################
# 02 Invetario de variales y periodos
#################################

library(ncdf4)
dirbase="X:/ADAA/CMIP6/monthly"
outdir="X:/ADAA/CMIP6/monthly"
listNC <-  list.files(dirbase, recursive = T, full.names = T,pattern = ".nc") 
reporte=paste0(outdir,"/Inventory_monthly.txt")
opnFile <- file(reporte, open="a",encoding = "UTF-8")
cat(paste("file","rcp","gcm","ens","grilla","var","yi","yf","xres","yres","ndates",sep="\t"), file=opnFile)
for(nc in listNC){
  var=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 1))   
  gcm=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 3))
  periodo=gsub(".nc","",tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 7)))
  yi=sapply(strsplit(periodo, '[-]'), "[[", 1)
  yf=sapply(strsplit(periodo, '[-]'), "[[", 2)    
  ens=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 5))
  grilla=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 6))
  rcp=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 4))
  ncin <- nc_open(nc)
  lon <- ncvar_get(ncin,"lon")
  tiempo <- ncvar_get(ncin,"time")
  lat <- ncvar_get(ncin,"lat")
  xres=(lon[dim(lon)]-lon[1])/dim(lon)
  yres=(lon[dim(lat)]-lon[1])/dim(lat)
  cat("\n",file=opnFile)
  cat(paste(basename(nc),rcp,gcm,ens,grilla,var,yi,yf,xres,yres,dim(tiempo),sep="\t"), file=opnFile) 
}
close.connection(opnFile)

###########################
# 03 calcular los promedios
#    Se debe correr en linux con cdo
###########################

library(ncdf4)
require(raster)
require(maptools)
dirwork="X:"#"/dapadfs/workspace_cluster_13"
dirbase=paste0(dirwork,"/ADAA/CMIP6/monthly/")#"X:/ADAA/CMIP6/monthly"
outdir=paste0(dirwork,"/ADAA/CMIP6/monthly_test/")
mask1deg=paste0(dirwork,"/ADAA/CMIP6/_scripts/buff_md.nc")
monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")

listrcp <-  list.dirs(dirbase,recursive = F,full.names = F) 
for(rcp in listrcp){
  listNC <-  list.files(paste0(dirbase,"/",rcp), recursive = F, full.names = T,pattern = ".nc")   
  for(nc in listNC){
    var=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 1))   
    gcm=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 3))
    periodo=gsub(".nc","",tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 7)))
    yi=sapply(strsplit(periodo, '[-]'), "[[", 1)
    yf=sapply(strsplit(periodo, '[-]'), "[[", 2)    
    ens=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 5))
    grilla=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 6))
    rcp=tolower(sapply(strsplit(basename(nc), '[_]'), "[[", 4))  
    
    outrcp=paste0(outdir,"/",rcp,"/",gcm,"/",ens,"/",grilla,"/average")
    dir.create(outrcp,showWarnings=FALSE,recursive = T)
    
    if (rcp == "historical"){
      periodList <- c("1971") #"1961", 
    } else {
      periodList <- c("2020") # "2030", "2040", "2050", "2060", "2070"
    }  
    if(rcp=="historical" || rcp=="ssp585" || var=="tasmin"){
      for (period in periodList) {
        
        # Define start and end year
        staYear <- as.integer(period)
        endYear <- as.integer(period) + 29
        cat(".>Processing: ",basename(nc),staYear,endYear)
        selyearnc=paste0(outrcp,"/",var,"_",rcp,"_",gcm,"_",staYear,"_",endYear,"_sely.nc")
        conver=paste0(outrcp,"/",var,"_",rcp,"_",gcm,"_",staYear,"_",endYear,"_conver.nc")
        avgclim=paste0(outrcp,"/",var,"_",rcp,"_",gcm,"_",staYear,"_",endYear,".nc")
        ## calculo de promedio conversion de unidades y remapping a 1 grado
        if(!file.exists(avgclim)){
          system(paste0("cdo selyear,",staYear,"/",endYear," ",nc," ",selyearnc), intern = TRUE)

          ## conversion de unidades y enteros
          if (var=="tasmin" || var=="tasmax"){
            system(paste0("cdo -subc,273.15 ",selyearnc," ",conver), intern = TRUE)
            system(paste0("cdo ymonmean ",conver," ",avgclim), intern = TRUE) 
          }
          if (var=="pr"){
            system(paste0("cdo -mulc,86400 ",selyearnc," ",conver), intern = TRUE)
            system(paste0("cdo ymonsum ",conver," ",avgclim), intern = TRUE)   
          }
          if (var=="rsds"){
            system(paste0("cdo -mulc,0.0864 ",selyearnc," ",conver), intern = TRUE)
            system(paste0("cdo ymonmean ",conver," ",avgclim), intern = TRUE) 
          }    
          unlink(c(selyearnc,conver))
        }
        
        ### calculo de anomalias
        if (rcp != "historical"){
          periodListHist <- c("1971") #"1961", 
          for (periodH in periodListHist) {
            staYearHist <- as.integer(periodH)
            endYearHist <- as.integer(periodH) + 29  
            
            diranom=paste0(outdir,rcp,"/",gcm,"/",ens,"/",grilla,"/anomalies_",as.integer(periodH)+14,"/",staYear,"_",endYear)
            dir.create(diranom,showWarnings=FALSE,recursive = T)    
            avgclimHist=paste0(outdir,"/historical/",gcm,"/",ens,"/",grilla,"/average/",var,"_historical_",gcm,"_",staYearHist,"_",endYearHist,".nc")
            tempAnom=paste0(diranom,"/",var,"_anom.nc")
            tempAnomPr=paste0(diranom,"/",var,"_anomPr.nc")
            temphist=paste0(diranom,"/",var,"_anomHist.nc")
            tempbox=paste0(diranom,"/",var,"_box.nc")
            sumnc=paste0(diranom,"/",var,"_sum.nc")
            mulnc=paste0(diranom,"/",var,"_mul.nc")
            donenc=paste0(diranom,"/",var,".nc")
            remap1deg=paste0(diranom,"/",var,"_remp.nc")
            
            if(var=="pr"||var=="rsds"){
              system(paste0("cdo sub ",avgclim," ",avgclimHist," ",tempAnomPr), intern = TRUE)
              system(paste0("cdo addc,0.5 ",avgclimHist," ",temphist), intern = TRUE)
              system(paste0("cdo div ",tempAnomPr," ",temphist," ",tempAnom), intern = TRUE)
              
            }else{
              system(paste0("cdo sub ",avgclim," ",avgclimHist," ",tempAnom), intern = TRUE)
              
            }
            ## extract mask
            system(paste0("cdo remapbil,r360x180 ",tempAnom," ",remap1deg), intern = TRUE)
            system(paste0("cdo sellonlatbox,0,360,-60,90 ",remap1deg," ",tempbox))
            system(paste0("cdo mul ",mask1deg," ",tempbox," ",donenc), intern = TRUE)
            splitmonNC=paste0(diranom,"/",var,"_")
            system(paste0("cdo splitmon ",donenc," ",splitmonNC), intern = TRUE)
            
            diranomshp=paste0(outdir,"/",rcp,"/",gcm,"/",ens,"/",grilla,"/anomalies/",staYear,"_",endYear)
            
            dir.create(diranomshp,showWarnings=FALSE,recursive = T)  
            for (mth in monthList){
              # anom1km=paste0(outdir,"/",rcp,"/",gcm,"/",ens,"/",grilla,"/anomalies/",var,"_",mth,".nc")
              # system(paste0("cdo remapbil,r43200x18000 ",diranom,"/",var,"_",mth,".nc ",anom1km), intern = TRUE)
              
              # Se optiene los centroides del raster
              InFact=4 # translape entre puntos
              
                anom=rotate(raster(paste0(diranom,"/",var,"_2",mth,".nc ")))
                if(var="pr"){
                  anom=anom*10000
                }else{anom=anom*100}
                storage.mode(anom[]) = "integer" #object.size(anom) class(getValues(anom))
                
                listExt=c(extent(-180,0+InFact,0-InFact,90),extent(0-InFact,180,0-InFact,90),extent(-180,0+InFact,-60,0+InFact),extent(0-InFact,180,-60,0+InFact))
                
                for(i in 1:4){
                  outShp <- paste0(diranomshp,"/",var,"_",mth,"_A",i,".shp")
                  if (!file.exists(outShp)) {
                    anomNc <- writeRaster(crop(anom,listExt[[i]]), paste0(diranomshp,'/',var,"_",mth,"_A",i,".nc "), format='CDF', overwrite=FALSE)
                    anomPts <- rasterToPoints(crop(anom,listExt[[i]]))
                    
                    coords <- data.frame(anomPts[,1:2])
                    colnames(coords) <- c("LON", "LAT")
                    
                    values <- data.frame(anomPts[,3])
                    colnames(values) <- c("VALUE")
                    
                    anomPts <- SpatialPointsDataFrame(coords,values)
                    anomShp <- writePointsShape(anomPts, outShp)
                    
                    
                    #writeOGR(anomPts, outShp, "coords", driver="ESRI Shapefile", overwrite_layer=TRUE)
                 
                  } else {cat(" .> Anomalies ", paste("\t ", var, "_", mth, sep=""), "\tdone!\n")}   
                                  
                }
                cat(" .> Anomalies ", paste("\t ", var, "_", mth,"_A",i, sep=""), "\tdone!\n")          
            }
          }
        }
        cat("..done!\n")
      }    
    }
    
  }
}

###########################
# 04 Interpolacion de anomalias SOLO WINDOWS
#    Se debe correr en linux con cdo
###########################

library(gstat) # Use gstat's idw routine
library(sp)    # Used for the spsample function
library(raster)

dirwork="X:"#"/dapadfs/workspace_cluster_13"
dirbase=paste0(dirwork,"/ADAA/CMIP6/monthly_test/")#"X:/ADAA/CMIP6/monthly"
outdir=paste0(dirwork,"/ADAA/CMIP6/monthly_test/")
mask1deg=paste0(dirwork,"/ADAA/CMIP6/_scripts/buff_md.nc")
monthList <- c("01","02","03","04","05","06","07","08","09","10","11","12")



## interpolación en python
system(paste0("python X:/ADAA/CMIP6/_scripts/01_interpolation_mask.py D:/TEMP/2020_2049/tasmin_12_A3.shp C:/temp/interpo4.tif -180,-60,0,0 C:/temp/clip4.tif"),intern = T)


listrcp <-  list.dirs(dirbase,recursive = F,full.names = F) 
for(rcp in listrcp){
  listmodels <-  list.dirs(paste0(dirbase,"/",rcp),recursive = F,full.names = F) 
  for(model in listmodels){
    listens <-  list.dirs(paste0(dirbase,"/",rcp,"/",model),recursive = F,full.names = F) 
    for(ens in listens){
      listshp <-  list.files(paste0(dirbase,"/",rcp,"/",model,"/",ens,"/gn/anomalies"),recursive = F,full.names = T,pattern = ".shp$") 
      for(shp in listshp){
        shpi=shapefile(shp)
        proj4string(shpi)=CRS("+init=epsg:4326")
        
        grd              <- as.data.frame(spsample(shpi, "regular", n=50000))
        names(grd)       <- c("X", "Y")
        coordinates(grd) <- c("X", "Y")
        gridded(grd)     <- TRUE  # Create SpatialPixel object
        fullgrid(grd)    <- TRUE  # Create SpatialGrid object
        
        if(var=="pr"){
          
        }else{
          
        }        
        # Add P's projection information to the empty grid
        proj4string(grd) <- proj4string(shpi)
        
        # Interpolate the grid cells using a power value of 2 (idp=2.0)
        P.idw <- gstat::idw(VALUE ~ 1, shpi, newdata=grd, idp=2)
        
        # Convert to raster object then clip to Texas
        r       <- raster(P.idw)
        r.m     <- mask(r, W)
        
        # Plot
        tm_shape(r.m) + 
          tm_raster(n=10,palette = "RdBu",
                    title="Predicted precipitation \n(in inches)") + 
          tm_shape(P) + tm_dots(size=0.2) +
          tm_legend(legend.outside=TRUE)
        
        
      }
    }
  }  
}







