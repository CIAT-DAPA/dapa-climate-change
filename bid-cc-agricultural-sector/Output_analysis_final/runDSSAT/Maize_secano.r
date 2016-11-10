

############### Parallel DSSAT ############################
########### Load functions necessary ###############
path_functions <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/_scripts/DSSAT-R/"
path_project <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"

# Cargar data frame entradas para DSSAT


load(paste0(path_project, "14-ObjectsR/Soil.RData"))

rm(list=setdiff(ls(), c("Extraer.SoilDSSAT", "values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile"
                        , "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", "Cod_Ref_and_Position")))


load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Maize_secano.Rdat"))
#cultivar = c('IB0056','FM6')  #fijar cultivar aqui
 cultivar = c('IB0006','INRA')
# cultivar = c('IB0059','H6')

source(paste0(path_functions, "main_functions.R"))     ## Cargar funciones principales
source(paste0(path_functions, "make_xfile.R"))         ## Cargar funcion para escribir Xfile DSSAT
source(paste0(path_functions, "make_wth.R"))           ## Cargar funcion para escribir WTH
source(paste0(path_functions, "dssat_batch.R"))        ## Cargar funcion para escibir el archivo batch
source(paste0(path_functions, "DSSAT_run.R"))          ## Funcion para correr DSSAT

day0 <- crop_secano$N.app.0d
day_aplication0 <- rep(0, length(day0))

day40 <- crop_secano$N.app.40d
day_aplication40 <- rep(40, length(day40))

amount <- data.frame(day0, day40)
day_app <- data.frame(day_aplication0, day_aplication40)


## configuracion Archivo experimental secano
years <- 69:97  ## la fecha es una 'trampa' para que DSSAT funcione a futuro
data_xfile <- list()
data_xfile$crop <- "MAIZE" 
data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ MAIZE LAC"
data_xfile$name <- "./JBID.MZX" 
data_xfile$CR <- "MZ"
data_xfile$INGENO <- rep(cultivar[1], length(crop_secano[, "variedad.1"])) ## el codigo del cultivar
data_xfile$CNAME <- "MZNA"
data_xfile$initation <- crop_secano$mirca.start
data_xfile$final <- crop_secano$mirca.end
data_xfile$system <- "rainfed"  
data_xfile$year <- years[1]  ## solo para indicar el primer año de la simulacion
data_xfile$nitrogen_aplication <- list(amount = amount, day_app = day_app)
data_xfile$smodel <- "MZCER045"     ##  Find Model
data_xfile$bname <- "DSSBatch.v45"  ## cual es el archivo batch
data_xfile$PPOP <- 7    
data_xfile$PPOE <- 5   
data_xfile$PLME <- "S"  
data_xfile$PLDS <- "R"  
data_xfile$PLRD <- 0
data_xfile$PLRS <- 80
data_xfile$PLDP <- 5


modelos <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr", "miroc_miroc5",
             "mpi_esm_mr", "ncc_noresm1_m")

i <- 9 ## variar i (no se puede involucrar en un ciclo for por que no funciona la funcion para paralelizar) 

gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[i],"/Futuro/")
load(paste0(gcm, "Precipitation.RDat"))
load(paste0(gcm, "Srad.Rdat"))
load(paste0(gcm, "Temperatura_2.RDat"))

# Climate Data Set for WFD or global model of climate change
climate_data <- list()
climate_data$year <- 69:97       
climate_data$Srad <- Srad        
climate_data$Tmax <- Tmax     
climate_data$Tmin <- Tmin     
climate_data$Prec <- Prec       
climate_data$lat <- crop_secano[,"y"]        ## You can include a vector of latitude
climate_data$long <- crop_secano[, "x"]         ## You can include a vector of longitude
climate_data$wfd <- "model"       ## Switch between "wfd" and "model"
climate_data$id <- crop_secano[, "Coincidencias"]

## Entradas para las corridas de DSSAT 

input_data <- list()
input_data$xfile <- data_xfile
input_data$climate <- climate_data

dir_dssat <- "/home/jmesa/csm45_1_23_bin_ifort/" ## directorio donde se encuentra las funciones fortran de DSSAT
dir_base <- "/home/jmesa/Scratch"                ## directorio donde se van a realizar las simulaciones

## librerias para el trabajo en paralelo
library(foreach)
library(doMC)

##  procesadores en su servidor
registerDoMC(8)

Run <- foreach(i = 1:dim(crop_secano)[1]) %dopar% {
  
  run_dssat(input_data, i, dir_dssat, dir_base)
  
}

tipo <- "Secano_"
cultivo <- "Maiz_"
cultivar.n <- cultivar[2]  ## nombre del cultivar


save(Run, file = paste("/home/jmesa/", "_", cultivo, tipo, cultivar.n, "_",modelos[i], "_",  ".RDat",sep=""))



