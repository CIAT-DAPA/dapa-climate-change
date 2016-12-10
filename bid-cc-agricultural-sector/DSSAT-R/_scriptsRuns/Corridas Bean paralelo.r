############### Parallel DSSAT ############################

#Some general config
scenario <- "historical" #historical, future
cul_list <- data.frame(CID=1:2,dsid=c("IB0118",'IB0010'),
                       culname=c("ICTAOstua","Manitou"))
cultivar <- 1

#cropping system
sys_type <- "riego" #riego, secano ##do not run
#GCMs
modelos <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
             "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
gcm_i <- 1 #which GCM will be run
#diagnostic run is only performed for irrigated systems
run_type <- "diagnostic" #diagnostic (to extract fertiliser dates) or final (final run once mgmt has been specified)
##############################################################################
##############################################################################

## librerias para el trabajo en paralelo
library(foreach)
library(doMC)

##procesadores en su servidor
registerDoMC(8)
#iterate cultivars
foreach(cultivar = 1:nrow(cul_list)) %do% {
  #Paths para scripts de funciones y workspace
  path_functions <- "~/Repositories/dapa-climate-change/bid-cc-agricultural-sector/DSSAT-R/"
  path_project <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"
 
  #Cargar data frame entradas para DSSAT
  load(paste0(path_project, "14-ObjectsR/Soil.RData"))
  rm(list=setdiff(ls(), c("Extraer.SoilDSSAT", "values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile"
                          , "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", "Cod_Ref_and_Position",
                          "scenario","cul_list","cultivar","run_type","sys_type")))

###load dataframe donde dice load hay q cambiar 


load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Bean_",sys_type,".RDat"))
assign("crop_mgmt", get(paste("crop_",sys_type,sep="")))

#Cargar funciones
#source(paste0(path_functions, "main_functions.R"))     ## Cargar funciones principales
source(paste0(path_functions, "make_xfile.R"))         ## Cargar funcion para escribir Xfile DSSAT
source(paste0(path_functions, "make_wth.R")) 
source(paste0(path_functions, "dssat_batch.R"))
source(paste0(path_functions, "DSSAT_run.R"))


##Definir rango de anos, linea base: 71:99; futuro: 69:97
if (scenario == "historical") {years <- 71:99}
if (scenario == "future") {years <- 69:97}

#Crear data.frame de aplicaciones de fertilizante
day0 <-  crop_mgmt$N.app.0
day_aplication0 <- rep(0, length(day0))

if (run_type == "diagnostic") {
  day30 <- rep(0, length(crop_mgmt$N.app.0))
} else {
  day30 <- crop_mgmt$N.app.30
}
day_aplication30 <- rep(30, length(day30))

amount <- data.frame(day0, day30)
day_app <- data.frame(day_aplication0, day_aplication30)

  ## configuracion Archivo experimental secano  
  data_xfile <- list()
  data_xfile$run_type <- run_type
  data_xfile$crop <- "BEAN" 
  data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ BEAN LAC"
  data_xfile$name <- "./JBID.BNX" 
  data_xfile$CR <- "BN"  ## Variable importante 
  data_xfile$INGENO <- rep(paste(cul_list$dsid[which(cul_list$CID == cultivar)]), length(crop_mgmt[, "variedad.1"]))
  data_xfile$CNAME <- "BZNA"
  data_xfile$initation <- crop_mgmt$mirca.start
  data_xfile$final <- crop_mgmt$mirca.end
  if (sys_type == "riego") {data_xfile$system <- "irrigation"} ## Irrigation or rainfed, if is irrigation then automatic irrigation
  if (sys_type == "secano") {data_xfile$system <- "rainfed"} ## Irrigation or rainfed, if is irrigation then automatic irrigation
  data_xfile$year <- years[1]
  data_xfile$nitrogen_aplication <- list(amount = amount, day_app = day_app)
  data_xfile$smodel <- "BNGRO045"     ##  Fin Model
  data_xfile$bname <- "DSSBatch.v45"
  data_xfile$PPOP <- 20   ## Plant population at planting
  data_xfile$PPOE <- 20   ## Plant population at emergence 
  data_xfile$PLME <- "S"  ## Planting method: dry seed (S)
  data_xfile$PLDS <- "R"  ## Seed distribution: by row (R)
  data_xfile$PLRD <- 0    ## Row direct## Row spacing (cm)ion (degrees from N)
  data_xfile$PLRS <- 50   ## Planting depth (cm)
  data_xfile$PLDP <- 2    ## Symbiosis (Y =  Yes, N = Not), "Y" only for bean and soy
  data_xfile$SYMBI <- 'Y' ## Y =  Yes, N = Not
  
  #load climate data
  if (scenario == "historical") {
    load(paste0(path_project, "14-ObjectsR/wfd/", "WDF_all_new.Rdat"))
  } else {
    gcm <-paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[gcm_i],"/Futuro/")
    load(paste0(gcm, "Precipitation.RDat"))
    load(paste0(gcm, "Srad.Rdat"))
    load(paste0(gcm, "Temperatura_2.Rdat"))
  }
  

  
  # Climate Data Set for WFD or global model of climate change
  climate_data <- list()
  climate_data$year <- years       ## Years where they will simulate yields change between 71:99 or 69:97
  climate_data$Srad <- Srad        ## [[year]][pixel, ]   
  climate_data$Tmax <- Tmax     ## [[year]][pixel, ]
  climate_data$Tmin <- Tmin     ## [[year]][pixel, ]
  climate_data$Prec <- Prec       ## [[year]][pixel, ]
  climate_data$lat <- crop_mgmt[,"y"]        ## You can include a vector of latitude
  climate_data$long <- crop_mgmt[, "x"]         ## You can include a vector of longitude
  climate_data$wfd <- "wfd"       ## Switch between "wfd" and "model"
  climate_data$id <- crop_mgmt[, "Coincidencias"]
  ##
 
  ## Entradas para las corridas de DSSAT
  input_data <- list()
  input_data$xfile <- data_xfile
  input_data$climate <- climate_data
  
  ## Carpetas necesarias donde se encuentra DSSAT compilado y un directorio para las corridas
  dir_dssat <- "~/csm45_1_23_bin_ifort/"
  dir_base <- "~/Scratch"
  
  #run dssat for one pixel (test)
  #run_dssat(input=input_data, pixel=1, dir_dssat, dir_base)
  Run <- foreach(i = 1:dim(crop_mgmt)[1]) %dopar% {
    run_dssat(input_data, i, dir_dssat, dir_base)
  }
  
  #create general output directory
  if (!file.exists(paste("~/bid_reruns/",run_type,sep=""))) {
    dir.create(paste("~/bid_reruns/",run_type,sep=""),recursive=T)
   }
  
  #save file
  if (scenario == "historical") {
    save(Run, file = paste("~/bid_reruns/",run_type,"/", data_xfile$crop,"_",data_xfile$system,"_", 
                           paste(cul_list$culname[which(cul_list$CID == cultivar)]), '_WFD', 
                           ".RDat", sep = ""))
  } else {
    save(Run, file = paste("~/bid_reruns/",run_type,"/", data_xfile$crop, "_", data_xfile$system, "_", 
                           paste(cul_list$culname[which(cul_list$CID == cultivar)]), "_", 
                           modelos[gcm_i], "_",  ".RDat",sep=""))
    

  }
  #clean up
  setwd("~")
  system("rm -rf ~/Scratch")
} 
 