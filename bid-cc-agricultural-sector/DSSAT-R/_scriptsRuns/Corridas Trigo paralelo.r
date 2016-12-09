  ######################## Parallel DSSAT for wheat ############################
  ##############################################################################
  ##############################################################################
  #Some general config
  scenario <- "historical" #historical, future
  cul_list <- data.frame(CID=1:6,dsid=c("IB0010","IB0013","IB0016","IB0028","IB0022","IB0026"),
                         culname=c("Seri82BA","TajanBA","DonErnestoBA","Gerek79BA","HalconsnaBA","BrigadierBA"))
  cultivar <- 1
  run_type <- "diagnostic" #diagnostic (to extract fertiliser dates) or final (final run once mgmt has been specified)
  sys_type <- "riego" #riego, secano
  ##############################################################################
  ##############################################################################
  
  #Paths para scripts de funciones y workspace
  path_functions <- "~/Repositories/dapa-climate-change/bid-cc-agricultural-sector/DSSAT-R/"
  path_project <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"
  
  #Cargar data frame entradas para DSSAT
  load(paste0(path_project, "14-ObjectsR/Soil.RData"))
  rm(list=setdiff(ls(), c("Extraer.SoilDSSAT", "values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile"
                          , "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", "Cod_Ref_and_Position",
                          "scenario","cul_list","cultivar","run_type","sys_type")))
  load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Wheat_riego.RDat"))
  assign("crop_mgmt", get(paste("crop_",sys_type,sep="")))
  
  #Cargar funciones
  source(paste0(path_functions, "main_functions.R"))     ## Cargar funciones principales
  source(paste0(path_functions, "make_xfile.R"))         ## Cargar funcion para escribir Xfile DSSAT
  source(paste0(path_functions, "make_wth.R")) 
  source(paste0(path_functions, "dssat_batch.R"))
  source(paste0(path_functions, "DSSAT_run.R"))
  
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
  data_xfile$INGENO <- rep(paste(cul_list$dsid[which(cul_list$CID == cultivar)]), length(crop_riego[, "variedad.1"]))
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
    i <- 10 #which GCM will be run
  	modelos <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr", "miroc_miroc5",
	       			"mpi_esm_mr", "ncc_noresm1_m")
    gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[i],"/Futuro/")
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
  #run_dssat(input=input_data, pixel=1, dir_dssat, dir_base)
  
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
    save(Run, file = paste("~/bid_reruns/",run_type,"/", data_xfile$crop,"_",data_xfile$system,"_", 
                           paste(cul_list$culname[which(cul_list$CID == cultivar)]), '_WFD', 
                           ".RDat", sep = ""))
  } else {
    save(Run, file = paste("~/bid_reruns/",run_type,"/", data_xfile$crop, "_", data_xfile$system, "_", 
                           paste(cul_list$culname[which(cul_list$CID == cultivar)]), "_", 
                           modelos[i], "_",  ".RDat",sep=""))
  }
  
  
  ##############################################################################
  ##############################################################################
  # rainfed run
  
  
  ############### Parallel DSSAT ############################
  ########### Load functions necessary ###############
  # path_functions <- "/home/jeisonmesa/Proyectos/BID/DSSAT-R/"
  # path_project <- "/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/"
  
  path_functions <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/_scripts/DSSAT-R/"
  path_project <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"
  
  
  # Cargar data frame entradas para DSSAT
  
  
  load(paste0(path_project, "14-ObjectsR/Soil.RData"))
  rm(list=setdiff(ls(), c("Extraer.SoilDSSAT", "values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile"
                          , "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", "Cod_Ref_and_Position")))
  
  
  load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Wheat_secano.RDat"))

  
  source(paste0(path_functions, "main_functions.R"))     ## Cargar funciones principales
  source(paste0(path_functions, "make_xfile.R"))         ## Cargar funcion para escribir Xfile DSSAT
  source(paste0(path_functions, "make_wth.R")) 
  source(paste0(path_functions, "dssat_batch.R"))
  source(paste0(path_functions, "DSSAT_run.R"))
  
  
  day0 <-  crop_secano$N.app.0
  day_aplication0 <- rep(0, length(day0))
  
  
  day30 <- crop_secano$N.app.30
  day30[which(is.na(day30))] <- 0
  day_aplication30 <- rep(30, length(day30))
  
  amount <- data.frame(day0, day30)
  day_app <- data.frame(day_aplication0, day_aplication30)
  
## configuracion Archivo experimental secano
  years <- 71:99  ## Camabiar entre linea base 71:99 o futuro 69:97
  data_xfile <- list()
  data_xfile$crop <- "WHEAT" 
  data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ WHEAT LAC"
  data_xfile$name <- "./JBID.WHX" 
  data_xfile$CR <- "WH"  ## Variable importante 
  ## data_xfile$INGENO <- "IB0118"
  ##cultivars <- substr(crop_secano[, "variedad.1"], 1, 6)  ## Cambiar de acuerdo al cultivar a correr
  ##data_xfile$INGENO <- cultivars  ## Correr Cultivar por region
  data_xfile$INGENO <- rep('IB0024', length(crop_secano[, "variedad.1"]))
  data_xfile$CNAME <- "WHNA"
  data_xfile$initation <- crop_secano$mirca.start
  data_xfile$final <- crop_secano$mirca.end
  data_xfile$system <- "rainfed"  ## Irrigation or rainfed, if is irrigation then automatic irrigation
  data_xfile$year <- years[1]
  data_xfile$nitrogen_aplication <- list(amount = amount, day_app = day_app)
  data_xfile$smodel <- "WHCER045"     ##  Fin Model
  data_xfile$bname <- "DSSBatch.v45"
  data_xfile$PPOP <- 250   ## Investigar mas acerca de este parametro 
  data_xfile$PPOE <- 250   ## Investigar mas acerca de este parametro 
  data_xfile$PLME <- "S"  ## to rice S semilla T transplanting
  data_xfile$PLDS <- "R"  ## Investigar mas acerca de este parametro 
  data_xfile$PLRD <- 0
  data_xfile$PLRS <- 16
  data_xfile$PLDP <- 4
  data_xfile$SYMBI <- 'N' ## Y =  Yes, N = Not
  
  ## load(paste0(path_project, "14-ObjectsR/wfd/", "WDF_all_new.Rdat"))
  
	modelos <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr", "miroc_miroc5",
				"mpi_esm_mr", "ncc_noresm1_m")
  
  
  i<- 9
  gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[i],"/Futuro/")
  load(paste0(gcm, "Precipitation.RDat"))
  load(paste0(gcm, "Srad.Rdat"))
  load(paste0(gcm, "Temperatura_2.Rdat"))
  
  # Climate Data Set for WFD or global model of climate change
  climate_data <- list()
  climate_data$year <- 71:99      ## Years where they will simulate yields change between 71:99 or 69:97
  climate_data$Srad <- Srad        ## [[year]][pixel, ]   
  climate_data$Tmax <- Tmax     ## [[year]][pixel, ]
  climate_data$Tmin <- Tmin     ## [[year]][pixel, ]
  climate_data$Prec <- Prec       ## [[year]][pixel, ]
  climate_data$lat <- crop_secano[,"y"]        ## You can include a vector of latitude
  climate_data$long <- crop_secano[, "x"]         ## You can include a vector of longitude
  climate_data$wfd <- "wfd"       ## Switch between "wfd" and "model"
  climate_data$id <- crop_secano[, "Coincidencias"]
  
  ## Entradas para las corridas de DSSAT 
  
  input_data <- list()
  input_data$xfile <- data_xfile
  input_data$climate <- climate_data
  # Xfile(input_data$xfile, 158)
  
  ## Carpetas necesarias donde se encuentra DSSAT compilado y un directorio para las corridas
	dir_dssat <- "/home/jmesa/csm45_1_23_bin_ifort/"
	dir_base <- "/home/jmesa/Scratch"
  
  run_dssat(input_data, 104, dir_dssat, dir_base)
  run_dssat(input_data, 487, dir_dssat, dir_base)
  run_dssat(input_data, 1034, dir_dssat, dir_base)
  ## librerias para el trabajo en paralelo
  library(foreach)
  library(doMC)
  
    ##  procesadores en su servidor
  registerDoMC(8)
  Run <- foreach(i = 1:dim(crop_secano)[1]) %dopar% {
    
    run_dssat(input_data, i, dir_dssat, dir_base)
    
  }

  tipo <- "Secano_"
  cultivo <- "Trigo_"
  ##cultivar <- "KauzBA_"
  cultivar <- "KauzBA_"
##save(Run, file = paste("/home/jeisonmesa/Proyectos/BID/bid-cc-agricultural-sector/","_", cultivo,tipo,modelos[i],"_IC_.RDat",sep=""))
save(Run, file = paste("/home/jmesa/", "_", cultivo, tipo, cultivar, modelos[i], "_",  ".RDat",sep=""))
save(Run, file = paste("/home/jmesa/","_", cultivo, tipo, cultivar, 'WFD', ".RDat", sep = ""))



  



