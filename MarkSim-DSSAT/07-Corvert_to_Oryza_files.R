## Author : Camilo Barrios & Carlos Navarro
## Date   : March 2016
## Purpose: Create Oryza files from MarkSim outputs 

require(raster)

iDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/datos_diarios"
rcp <- "baseline"
pts <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/04-Crop_Modelling/rice/00_mask/rice_points_llanos.txt"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/04-Crop_Modelling/rice/01_climate_data"
dem <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline/llanos/_region/altitude"
period <- 1995

convertOryza <- function(iDir, rcp, pts, oDir, period, alt, index, i){

  fd <- sprintf("%02d",index$Fold[i])
  id <- sprintf("%04d", index$Point[i])
  ptsDir <- paste0(oDir, "/", rcp, "/", fd, id)
  
  cat(rcp,"fold",fd,"id",id," point",i, "of",nrow(index),"\n")
  
  if(!file.exists(paste0(ptsDir, "/CORM99.", substr(period, 2,4)))){
    
    lon <- round(index$LONGITUD[i],2)
    lat <- round(index$LATITUD[i],2)
    
    if (!file.exists(ptsDir)) {dir.create(ptsDir, recursive=T)}
        
    wtgDir <- paste0(iDir, "/", rcp, "/fold-", fd, "/", id)
    # setwd(wtgDir)
    wtgLs <- list.files(wtgDir, full.names = T, pattern="\\.WTG$")
    
    if(length(wtgLs) == 99){
      
      wtgDt <- lapply(paste(wtgLs,sep=""), function(x){read.table(x, header=T, sep="", skip=5)})
      
      # year=list()
      # for(i in 1:99){ if (i%%4==0){year[[i]]=rep(i,366)} else {year[[i]]=rep(i,365)} } 
      
      substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x)) }
      sortCols = function(x){x=cbind(#paste0(fd, id),
        period,
        #sprintf("%04d",year), # anho
        as.numeric(substrRight(x$X.DATE, 3)), #doy
        # length(year), #ndays
        # "mes" = as.numeric(format(strptime(paste(year, ), format="%Y %j"), format="%m")),
        x$SRAD*1000, #srad
        x$TMIN, #tmin
        x$TMAX, #tmax
        -99, #windvar1
        -99, #windvar2
        x$RAIN #Rain
      )
      return(x)}
      
      wtgDtSort <- lapply(wtgDt,sortCols)
      # wtgDtSort = lapply(1:99, function(y)sortCols(wtgDt[[y]],year[[y]]))
      
      
      for (j in 1:length(wtgDtSort)){
        
        wtgDtSort_i = paste0(ptsDir, "/CORM",j, ".", substr(period, 2,4))
        
        sink(wtgDtSort_i)
        cat(paste(lon,paste0("0",lat),alt[i],"0,0",sep=","))
        cat("\n")
        
        write.table(cbind(j,wtgDtSort[[j]]),sep=",",row.names=F, col.names=F, quote = F)
        sink()
        
      }
      
    } else {
      
      write(paste0(fd, id), paste0(oDir, "/errors_", rcp, ".txt"),append=TRUE)
      
    }
    
  }
    
}

## Read index file
index <- read.table(pts, header = T)
alt <- extract(raster(dem), cbind(index$LONGITUD, index$LATITUD))
# index_mod <- cbind("ID"=paste0(sprintf("%02d",index$Fold), sprintf("%04d", index$Point)), index[,1:2], "ALTITUD"=alt)
# index_mod <- write.table(index_mod, paste0(oDir, "/index.txt"), row.names=F, col.names=T, quote = F)
# 
#Do the snowfall stuff here
library(snowfall)
sfInit(parallel=T,cpus=12) #initiate cluster

#export functions
sfExport("iDir")
sfExport("rcp")
sfExport("pts")
sfExport("oDir")
sfExport("period")
sfExport("alt")
sfExport("index")
sfExport("convertOryza")

controlconv <- function(i) { #define a new function
# for(i in 1:nrow(index)){
  tryCatch({
    convertOryza(iDir, rcp, pts, oDir, period, alt, index, i)
  }, error=function(e){
    write(paste0(sprintf("%02d",index$Fold[i]), sprintf("%04d", index$Point[i])), paste0(oDir, "/errors_", rcp, ".txt"),append=TRUE)
    cat("ERROR :",conditionMessage(e),paste0(sprintf("%02d",index$Fold[i]), sprintf("%04d", index$Point[i])), "\n")
  }
  )
# }

}

system.time(sfSapply(as.vector(1:nrow(index)), controlconv))






# ##Remove empty folder
# rcpList <- c("baseline","rcp26","rcp45", "rcp85")
# pts <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/04-Crop_Modelling/rice/00_mask/rice_points_llanos.txt"
# oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/04-Crop_Modelling/rice/01_climate_data"
# index <- read.table(pts, header = T)
# 
# for (rcp in rcpList){
#   
#   if (rcp == "baseline"){
#     period <- 1995    
#   } else {
#     period <- 2030
#   }
#   
#   for (i in 1:nrow(index)){
#     
#     fd <- sprintf("%02d",index$Fold[i])
#     id <- sprintf("%04d", index$Point[i])
#     ptsDir <- paste0(oDir, "/", rcp, "/", fd, id)
#     
#     cat(rcp,"fold",fd,"id",id," point",i, "of",nrow(index),"\n")
#     
#     if(!file.exists(paste0(ptsDir, "/CORM99.", substr(period, 2,4)))){
#       
#       write(paste0(fd, id), paste0(oDir, "/errors.txt"),append=TRUE)
#       
#     }
#   }
# }


# rcpList <- c("baseline","rcp26","rcp45", "rcp85")
# rmdirs <- read.table(paste0(oDir, "/errors.txt"), header = F)
# 
# for (rcp in rcpList){
#   
#   for(j in 1:nrow(rmdirs)){
#     
#     cat(rcp,"foldid",sprintf("%06d", rmdirs$V1[j])," point",j, "of",nrow(rmdirs),"\n")
#     ptsDir <- paste0(oDir, "/", rcp, "/", sprintf("%06d", rmdirs$V1[j]))
#     unlink(ptsDir, recursive = T)
#   }
#   
# }
