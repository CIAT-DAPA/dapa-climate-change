##############################################################################
##############################################################################
######################## Parallel DSSAT for wheat ############################
##############################################################################
##############################################################################
#Some general config
scenario <- "historical" #historical, future

#cultivar list for wheat (based on CIMMYT Mega-environment work)
cul_list <- data.frame(CID=1:6,dsid=c("IB0010","IB0013","IB0016","IB0028","IB0022","IB0026"),
                       culname=c("Seri82BA","TajanBA","DonErnestoBA","Gerek79BA","HalconsnaBA","BrigadierBA"))

#diagnostic run is only performed for irrigated systems, for historical climate
run_type <- "diagnostic" #diagnostic (to extract fertiliser dates) or final (final run once mgmt has been specified)

#cropping system
sys_type <- "riego" #riego, secano

#GCMs, only if scenario == "future"
modelos <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
             "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")
gcm_i <- 10 #which GCM will be run

#if we want to clean up raw DSSAT files
cleanup_all <- F

##############################################################################
##############################################################################

#iterate cultivars
for (cultivar in 1:nrow(cul_list)) {
  #cultivar <- 1
  #Paths para scripts de funciones y workspace
  path_functions <- "~/Repositories/dapa-climate-change/bid-cc-agricultural-sector/DSSAT-R/"
  path_project <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"
  
  #Cargar data frame entradas para DSSAT
  #load(paste0(path_project, "14-ObjectsR/Soil.RData"))
  load(paste0(path_project, "14-ObjectsR/Soil2.RData"))
  rm(list=setdiff(ls(), c("values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile","xy_Ref",
                          "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", 
                          "Cod_Ref_and_Position", "profileMatrix","scenario","cul_list","cultivar","run_type","sys_type",
                          "modelos","gcm_i","cleanup_all")))
  load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Wheat_",sys_type,".RDat"))
  assign("crop_mgmt", get(paste("crop_",sys_type,sep="")))
  
  #Cargar funciones
  source(paste0(path_functions, "main_functions.R"))     ## Cargar funciones principales
  source(paste0(path_functions, "make_xfile.R"))         ## Cargar funcion para escribir Xfile DSSAT
  source(paste0(path_functions, "make_wth.R")) 
  source(paste0(path_functions, "dssat_batch.R"))
  source(paste0(path_functions, "DSSAT_run.R"))
  source(paste0(path_functions, "Extraer.SoilDSSAT.R"))  ## New extraer soil dssat function
  
  #Crear data.frame de aplicaciones de fertilizante
  if (run_type == "diagnostic") {
    day0 <-  crop_mgmt$N.app.0
    day_aplication0 <- rep(0, length(day0))
    
    day30 <- crop_mgmt$N.app.30
    day_aplication30 <- rep(30, length(day30))
    
    amount <- data.frame(day0, day30)
    day_app <- data.frame(day_aplication0, day_aplication30)
  } else {
    #here write update of mgmt matrix when first (diagnostic) run is available
  }
  
  ##Definir rango de anos, linea base: 71:99; futuro: 69:97
  if (scenario == "historical") {years <- 71:99}
  if (scenario == "future") {years <- 69:97}
  
  ##Configuracion Archivo experimental
  data_xfile <- list()
  data_xfile$run_type <- run_type
  data_xfile$crop <- "WHEAT"
  data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ WHEAT LAC"
  data_xfile$name <- "./JBID.WHX" 
  data_xfile$CR <- "WH"  ## Variable importante 
  data_xfile$INGENO <- rep(paste(cul_list$dsid[which(cul_list$CID == cultivar)]), length(crop_mgmt[, "variedad.1"]))
  data_xfile$CNAME <- "WHNA"
  data_xfile$initation <- crop_mgmt$mirca.start
  data_xfile$final <- crop_mgmt$mirca.end
  if (sys_type == "riego") {data_xfile$system <- "irrigation"} ## Irrigation or rainfed, if is irrigation then automatic irrigation
  if (sys_type == "secano") {data_xfile$system <- "rainfed"} ## Irrigation or rainfed, if is irrigation then automatic irrigation
  data_xfile$year <- years[1]
  data_xfile$nitrogen_aplication <- list(amount = amount, day_app = day_app) #need to take care of
  data_xfile$smodel <- "WHCER045"     ##  Fin Model
  data_xfile$bname <- "DSSBatch.v45"
  data_xfile$PPOP <- 200   ## Plant population at planting
  data_xfile$PPOE <- 200   ## Plant population at emergence
  data_xfile$PLME <- "S"  ## Planting method: dry seed (S); transplanting (T)
  data_xfile$PLDS <- "R"  ## Seed distribution: by row (R)
  data_xfile$PLRD <- 0 ## Row direction (degrees from N)
  data_xfile$PLRS <- 18 ## Row spacing (cm)
  data_xfile$PLDP <- 4 ## Planting depth (cm)
  data_xfile$SYMBI <- 'N' ## Symbiosis (Y =  Yes, N = Not), "Y" only for bean and soy
  
  #load climate data
  if (scenario == "historical") {
    load(paste0(path_project, "14-ObjectsR/wfd/", "WDF_all_new.Rdat"))
  } else {
    gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[gcm_i],"/Futuro/")
    load(paste0(gcm, "Precipitation.RDat"))
    load(paste0(gcm, "Srad.Rdat"))
    load(paste0(gcm, "Temperatura_2.Rdat"))
  }
  
  #Climate Data Set for WFD or global model of climate change
  climate_data <- list()
  climate_data$year <- years #71:99   ## Years where they will simulate yields change between 71:99 or 69:97
  climate_data$Srad <- Srad        ## [[year]][pixel, ]   
  climate_data$Tmax <- Tmax     ## [[year]][pixel, ]
  climate_data$Tmin <- Tmin     ## [[year]][pixel, ]
  climate_data$Prec <- Prec       ## [[year]][pixel, ]
  climate_data$lat <- crop_mgmt[,"y"]        ## You can include a vector of latitude
  climate_data$long <- crop_mgmt[, "x"]         ## You can include a vector of longitude
  climate_data$wfd <- "wfd"       ## Switch between "wfd" and "model"
  climate_data$id <- crop_mgmt[, "Coincidencias"]
  
  ## Entradas para las corridas de DSSAT
  input_data <- list()
  input_data$xfile <- data_xfile
  input_data$climate <- climate_data
  
  ## Carpetas necesarias donde se encuentra DSSAT compilado y un directorio para las corridas
  dir_dssat <- "~/csm45_1_23_bin_ifort/"
  dir_base <- "~/Scratch"
  
  #run dssat for one pixel (test)
  #run_dssat(input=input_data, pixel=250, dir_dssat, dir_base)
  
  ## librerias para el trabajo en paralelo
  library(foreach)
  library(doMC)
  
  ##procesadores en su servidor
  registerDoMC(8)
  
  Run <- foreach(i = 1:dim(crop_mgmt)[1]) %dopar% {
    run_dssat(input_data, i, dir_dssat, dir_base)
  }
  
  #create general output directory
  if (!file.exists(paste("~/bid_reruns/",run_type,sep=""))) {
    dir.create(paste("~/bid_reruns/",run_type,sep=""),recursive=T)
  }
  
  #save file
  if (scenario == "historical") {
    store_name <- paste0(data_xfile$crop,"_",data_xfile$system,"_", 
                         paste(cul_list$culname[which(cul_list$CID == cultivar)]), '_WFD')
  } else {
    store_name <- paste0(data_xfile$crop, "_", data_xfile$system, "_", 
                         paste(cul_list$culname[which(cul_list$CID == cultivar)]), "_",
                         modelos[gcm_i])
  }
  save(Run, file = paste("~/bid_reruns/",run_type, "/", store_name, ".RDat",sep=""))
  
  #clean up, else create a folder and store results in there
  if (cleanup_all) {
    setwd("~")
    system("rm -rf ~/Scratch")
  } else {
    setwd("~")
    system(paste0("mkdir ~/Scratch/",run_type,"_",store_name))
    system(paste0("mv -f ~/Scratch/",data_xfile$crop,"_",data_xfile$system,"_* ~/Scratch/",run_type,"_",store_name,"/."))
  }
}
