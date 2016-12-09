# May 2015
# Code to Generate fies .Soil Necessary to Run DSSAT in Latin America (resolution 0.5 degrees)
# Data from The latest version (1.1) of WISE Soil Database for Crop Simulation Models data and maps can be downloaded at:
# https://hc.box.net/shared/0404zn08js (Password: bHddsc)
# Developer layer Soil Jawoo Koo j.koo@cgiar.org
# Developer code R Jeison Mesa j.mesa@cgiar.org; jeison.mesa@correounivalle.edu.co 
# Updated by Harold Achicanoy, Eliana Vallejo and Julian Ramirez, Nov 2016

##########################################################################################
########### Write the soil file (SOIL.SOL); make_soilfile function #######################
##########################################################################################

# Load libraries
library(raster)
library(ncdf4)
library(dplyr)

# Source functions
source("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/_scripts/mainFunctions.R") ## File Functios Necessary
#source("~/Repositories/dapa-climate-change/bid-cc-agricultural-sector/mainFunctions_updated.R") ## JRV MBP

# Path to working directories in dapadfs
path <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/" ## Project Directory
#path <- "/nfs/workspace_cluster_3/bid-cc-agricultural-sector/" ## JRV MBP

# Read id_soil raster file (file with cell IDs)
id_soil <- raster(paste0(path, "02-Soil-data/","cell5m.asc")) ## Soil Type Identifier
#id_soil <- raster("~/CIAT-work/BID-impacts/rerun_analysis/soils/Soil-anexo/cell5m.asc") ## JRV MBP
proj4string(id_soil) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") ## Add to the Coordinate System

# Read soil profile matching table
Soil_profile <- read.table(paste0(path, "02-Soil-data/", "data_hc3ksol_5m_global.txt"), header = T) ## Soil Profile for Cell5m (Para utilizar en el QUERY)
#Soil_profile <- read.table("~/CIAT-work/BID-impacts/rerun_analysis/soils/Soil-anexo/data_hc3ksol_5m_global.txt", header=T) ## JRV MBP

# Read soil profiles in DSSAT format
wise <- readLines(paste0(path, "02-Soil-data/", "WI.SOL")) ## Soil File Wise
#wise <- readLines("~/CIAT-work/BID-impacts/rerun_analysis/soils/Soil-anexo/WI.SOL") ## JRV MBP
Soil_Generic <- readLines(paste0(path, "02-Soil-data/", "HC.SOL")) ## Soil File Generic
#Soil_Generic <- readLines("~/CIAT-work/BID-impacts/rerun_analysis/soils/Soil-anexo/HC.SOL") ## JRV MBP

# Load 30 arc-min reference raster
r30 <- raster(paste0(path, "02-Soil-data/","prec_1971_01.nc"))
proj4string(r30) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") ## Add to the Coordinate System

# Crop soil ID raster to LAC extent using reference raster
id_soil2 <- crop(x = id_soil, y = extend(r30, r30))

# Extract 30M cell ID and remove any grid cells which are not in LAC
Soil_profile <- cbind(xyFromCell(object = id_soil, cell = Soil_profile$CELL5M), Soil_profile)
Soil_profile <- cbind(Soil_profile, cellFromXY(object = r30, xy = Soil_profile[, c('x', 'y')]))
Soil_profile <- Soil_profile[complete.cases(Soil_profile),]
names(Soil_profile)[ncol(Soil_profile)] <- 'CELL30M'
Soil_profile <- Soil_profile[order(Soil_profile$CELL30M),]
rownames(Soil_profile) <- 1:nrow(Soil_profile)

# Computing proportion of soil profiles per 30-min pixel
test2 <- Soil_profile %>% group_by(CELL30M, SoilProfile) %>% summarise_each(funs(sum(SharePct), length(SoilProfile))) %>% data.frame()
test3 <- test2[,c("CELL30M", "SharePct_sum", "SharePct_length")] %>% group_by(CELL30M) %>% summarise_each(funs(sum(.))) %>% data.frame()
colnames(test3)[2:3]<-c("acum_rep", "n_celdas")
test1 <- merge(test2, test3, by="CELL30M")
test1$repre <- (test1$SharePct_sum/test1$acum_rep)
test1 <- test1[order(test1$repre, decreasing=T), c("CELL30M", "SoilProfile", "repre")]
test1 <- test1[,c("CELL30M", "SoilProfile", "repre")] %>% group_by(CELL30M) %>% mutate(acum = cumsum(repre)) %>% data.frame()
rownames(test1) <- 1:nrow(test1)

# Leave only those profiles which represent up to 70 % of the 30 min pixel
cellsID <- unique(test1$CELL30M)
subSetFun <- lapply(cellsID, function(i){
  subSet <- test1[test1$CELL30M==i,]
  if(sum(subSet$acum[1] < 0.7) > 0){
    subSet <- subSet[c(which(subSet$acum < 0.7), max(which(subSet$acum < 0.7))+1),]
  } else {
    if(subSet$acum[1] >= 0.7){
      subSet <- subSet[1,]
    }
  }
  return(subSet)
})
soils_30m <- do.call(rbind, subSetFun); rm(test1, test2, test3)
soils_30m <- soils_30m[order(soils_30m$CELL30M),]

# Rescale percentages so that selected pixels add up to 70 %
cellsID <- unique(soils_30m$CELL30M)
rescaleFun <- lapply(cellsID, function(i) {
  subSet <- soils_30m[which(soils_30m$CELL30M == i),]
  subSet$rep_res <- subSet$repre / sum(subSet$repre)
  subSet$cum_res <- cumsum(subSet$rep_res)
  return(subSet)
})
soils_30m <- do.call(rbind, rescaleFun)
rownames(soils_30m) <- 1:nrow(soils_30m)

# Reference raster
# r30[] <- 1:ncell(r30)

# Write outputs (reference raster and matching table)
write.table(soils_30m, file = paste0(path, "02-Soil-data/","data_hc3ksol_30m_lac.txt"), row.names=F)
writeRaster(r30, paste0(path, "02-Soil-data/","cell30m.asc"), format="ascii")

#########
# Soil functions
Type_soil_wise <- function(data, path){
  
  k=0
  j=0
  
  for(i in 1:length(data)){
    
    test <- try(strsplit(data[i], split=path), silent=TRUE)
    
    if(length(test[[1]]) == 1 && test[[1]][1] != "*"){
      
      j[i] = i
      
    } else(k[i] = i)
    
  }
  
  return(k)
  
}

## Function to get just the Soil profile code from Wise database
extract_tipe_soil <- function(data){
  
  typeWise <- substr(strsplit(data, " ")[[1]][1],2,nchar(strsplit(data," ")[[1]][1]))
  
  return(typeWise)
  
}

## Function to write soil file (data=Archivo Wise, Cell5m=identificador del archivo soilprofile)
read_oneSoilFile <- function(data, path){
  
  i = 1
  test <- try(strsplit(data[i], split=path), silent=TRUE)
  soils <- NULL
  
  while(length(test[[1]]) == 1){
    
    soils <- rbind(soils, data[i])
    i = i+1
    test <- try(strsplit(data[i], split=path), silent=TRUE)
    
  }
  
  return(soils)
  
}

#########
## Soils code data Wise
code_soil_wise <- Type_soil_wise(wise, getwd())
code_soil_generic <- Type_soil_wise(Soil_Generic, getwd())

## Header position Wise
Position_CodigoSueloWise <- which(code_soil_wise != "NA")
Position_CodigoSueloWise <- c(1,(Position_CodigoSueloWise[2:length(Position_CodigoSueloWise)] + 1))
Position_CodigoSueloWise <- Position_CodigoSueloWise[1:(length(Position_CodigoSueloWise) - 3)]
wise[Position_CodigoSueloWise] ## Header checking the Wise

## Header position Generic
Position_CodigoSueloGeneric<-which(code_soil_generic != "NA")
Position_CodigoSueloGeneric<-c(1,(Position_CodigoSueloGeneric[2:length(Position_CodigoSueloGeneric)] + 1))
Position_CodigoSueloGeneric<-Position_CodigoSueloGeneric[1:(length(Position_CodigoSueloGeneric) - 2)]
Soil_Generic[Position_CodigoSueloGeneric] ## Header checking the Generic

## Get reference for Wise codes
Cod_Ref <- sapply(1:length(Position_CodigoSueloWise), function(i) extract_tipe_soil(wise[Position_CodigoSueloWise[i]]))
Cod_Ref_Generic <- sapply(1:length(Position_CodigoSueloGeneric), function(i) extract_tipe_soil(Soil_Generic[Position_CodigoSueloGeneric[i]]))

## Data frame containing the code for wise and position that this is in the file Wise Soil contains the position in the file WISE
Cod_Ref_and_Position <- data.frame(Cod_Ref, Position_CodigoSueloWise) # wise[59933]
Cod_Ref_and_Position$Cod_Ref <- as.character(Cod_Ref_and_Position$Cod_Ref)
Cod_Ref_and_Position_Generic <- data.frame(Cod_Ref_Generic, Position_CodigoSueloGeneric) # Soil_Generic[473]
Cod_Ref_and_Position_Generic$Cod_Ref_Generic <- as.character(Cod_Ref_and_Position_Generic$Cod_Ref_Generic)

## Make Soil file (.SOIL)
# Preparing in_data
in_data <- list()
in_data$general <- data.frame(SITE=-99,COUNTRY="Generic",LAT=-99,LON=-99,SCSFAM="Generic") # Location

# Soil maker function
make_soilfile <- function(in_data, data, path){
  
  # Header construction
  y <- data
  y <- y[5]
  write(y, file="x.txt")
  y <- read.table("x.txt", sep="")
  in_data$properties <- data.frame(SCOM=paste(y[1,1]), SALB=y[1,2], SLU1=y[1,3], SLDR=y[1,4], SLRO=y[1,5], SLNF=y[1,6], SLPF=1, SMHB=y[1,8], SMPX=y[1,9], SMKE=y[1,10])
  
  sink("SOIL.SOL")
  cat("*SOILS: General DSSAT Soil Input File\n")
  cat("\n")
  cat("*BID0000001  WISE        SCL     140 GENERIC SOIL PROFILE\n")
  cat("@SITE        COUNTRY          LAT     LONG SCS Family\n")
  
  # General
  cat(paste(" ", sprintf("%1$-12s%2$-12s%3$8.3f%4$9.3f",
                         as.character(in_data$general$SITE), as.character(in_data$general$COUNTRY),
                         in_data$general$LAT, in_data$general$LON)," ",
            sprintf("%-12s", as.character(in_data$general$SCSFAM)),
            "\n", sep=""))
  
  # Properties
  cat("@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE\n")
  cat(paste(sprintf("%1$6s%2$6.2f%3$6.1f%4$6.2f%5$6.2f%6$6.2f%7$6.2f%8$6s%9$6s%10$6s",
                    as.character(in_data$properties$SCOM), in_data$properties$SALB,
                    in_data$properties$SLU1, in_data$properties$SLDR, in_data$properties$SLRO,
                    in_data$properties$SLNF, in_data$properties$SLPF, in_data$properties$SMHB,
                    in_data$properties$SMPX, in_data$properties$SMKE),"\n", sep=""))
  cat(paste(read_oneSoilFile(data[6:length(data)], path)), sep = "\n")
  sink()
  
}

make_soilfile(in_data, wise[59933:length(wise)], getwd())

make_soilfile2 <- function(in_data, i, data, path){
  
  # Header construction

  # in_data, 1, Soil_Generic[Generic_Position:length(wise)], path
  
  y <- data
  y <- y[5]
  write(y, file="x.txt")
  y <- read.table("x.txt", sep="")
  in_data$properties <- data.frame(SCOM=paste(y[1,1]), SALB=y[1,2], SLU1=y[1,3], SLDR=y[1,4], SLRO=y[1,5], SLNF=y[1,6], SLPF=1, SMHB=y[1,8], SMPX=y[1,9], SMKE=y[1,10])
  
  # sink("SOIL.SOL")
  cat("*SOILS: General DSSAT Soil Input File\n")
  cat("\n")
  cat(paste0("*BID000000", i, "  WISE        SCL     140 GENERIC SOIL PROFILE\n"))
  cat("@SITE        COUNTRY          LAT     LONG SCS Family\n")
  
  # General
  cat(paste(" ", sprintf("%1$-12s%2$-12s%3$8.3f%4$9.3f",
                         as.character(in_data$general$SITE), as.character(in_data$general$COUNTRY),
                         in_data$general$LAT, in_data$general$LON)," ",
            sprintf("%-12s", as.character(in_data$general$SCSFAM)),
            "\n", sep=""))
  
  # Properties
  cat("@ SCOM  SALB  SLU1  SLDR  SLRO  SLNF  SLPF  SMHB  SMPX  SMKE\n")
  cat(paste(sprintf("%1$6s%2$6.2f%3$6.1f%4$6.2f%5$6.2f%6$6.2f%7$6.2f%8$6s%9$6s%10$6s",
                    as.character(in_data$properties$SCOM), in_data$properties$SALB,
                    in_data$properties$SLU1, in_data$properties$SLDR, in_data$properties$SLRO,
                    in_data$properties$SLNF, in_data$properties$SLPF, in_data$properties$SMHB,
                    in_data$properties$SMPX, in_data$properties$SMKE),"\n", sep=""))
  cat(paste(read_oneSoilFile(data[6:length(data)], path)), sep = "\n")
  # sink()
  
}

## Test
## make_soilfile(in_data, wise[59933:length(wise)], getwd())

## Extracting Archive Soil
xy_Ref <- as.data.frame(rasterToPoints(r30))
xy_Ref$CELL30M <- as.numeric(unlist(cellFromXY(object = r30, xy = xy_Ref[,c('x', 'y')])))
profileMatrix <- soils_30m
values <- unique(xy_Ref$CELL30M)

Extraer.SoilDSSAT <- function(Codigo_identificadorSoil, profileMatrix, path) {
  
  position <- Codigo_identificadorSoil ## Where it coincides with the raster ID
  profileMatrix <- profileMatrix
  posicion <- which(profileMatrix[,'CELL30M'] == position)
  
  
  if(length(posicion) == 0){
    
    Wise_Position <- 239 # According to HC_GEN0014 soil profile by Myles recommendation
    return(make_soilfile(in_data, Soil_Generic[Wise_Position:length(wise)], path))
    
  } else {
    
    celdas_id_Wise <- profileMatrix[posicion,] ## Cells and percentage of soil File DSSAT 
    condicion <- match(celdas_id_Wise[,'SoilProfile'], Cod_Ref_and_Position[,'Cod_Ref']) ## Identify cells that accomplish the condition
    
    # Verify if exists some profiles of WISE and Generic
    pos_value_missing <- which(is.na(condicion))
    condicion <- as.numeric(na.omit(condicion))
    condicion_2 <- match(celdas_id_Wise[pos_value_missing, 'SoilProfile'], Cod_Ref_and_Position_Generic[, 'Cod_Ref_Generic'])
    if(length(condicion) > 0 && length(condicion_2) > 0){
      
      Wise_Position <- Cod_Ref_and_Position[which(Cod_Ref_and_Position[,'Cod_Ref'] == celdas_id_Wise[,'SoilProfile']),][1,2]
      Generic_Position <- match(celdas_id_Wise[pos_value_missing, 'SoilProfile'], Cod_Ref_and_Position_Generic[, 'Cod_Ref_Generic'])
      
      # Wise soils
      sink("SOIL.SOL")
      for(i in 1:length(condicion)){
        make_soilfile2(in_data, i, wise[Wise_Position:length(wise)], path)
        
      }
      
      # Generic soils
      for(j in 1:length(condicion_2)){
        make_soilfile2(in_data, i+j, Soil_Generic[Cod_Ref_and_Position_Generic[Generic_Position,2]:length(wise)], path)              
      }
      sink()
      
    }
    
    if(length(condicion) == 1 && length(condicion_2)==0){
      
      Wise_Position <- Cod_Ref_and_Position[which(Cod_Ref_and_Position[,'Cod_Ref'] == celdas_id_Wise[,'SoilProfile']),]
      return(make_soilfile(in_data, wise[Wise_Position[, 2]:length(wise)], path))
      
    } else {
      
      if(length(condicion) > 1 && length(condicion_2)==0){
        
        sink("SOIL.SOL")
        for(i in 1:length(condicion)){
          Wise_Position <- Cod_Ref_and_Position[which(Cod_Ref_and_Position[,'Cod_Ref'] == celdas_id_Wise[i,'SoilProfile']),]
          make_soilfile2(in_data, i, wise[Wise_Position[,2]:length(wise)], path)
        }
        sink()
        return('Done\n')
        
      }
      
    }
    
  }
  
}


Extraer.SoilDSSAT(Codigo_identificadorSoil = values[5], profileMatrix = profileMatrix, path = '/mnt/workspace_cluster_3/bid-cc-agricultural-sector/')

# Extraer.SoilDSSAT(values[972], getwd())

## Test
## The object values matches in the order of the coordinates for climate data for Latin America
## Extraer.SoilDSSAT(values[972],getwd())
path = '/mnt/workspace_cluster_3/bid-cc-agricultural-sector/'
save.image(file = paste0(path, "14-ObjectsR/Soil2.RData")) ## Save the file Soil
