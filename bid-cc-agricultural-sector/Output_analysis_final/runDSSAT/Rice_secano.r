 ############### Parallel DSSAT ############################
  ########### Load functions necessary
  path_functions <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/_scripts/DSSAT-R/"
  path_project <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"

  # Cargar data frame entradas para DSSAT
  
  
  load(paste0(path_project, "14-ObjectsR/Soil.RData"))
  rm(list=setdiff(ls(), c("Extraer.SoilDSSAT", "values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile"
                          , "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", "Cod_Ref_and_Position")))
  
  
  load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/Rice_secano.Rdat"))
  cultivar = c('IB0023','LOW.TEMP.TOL')  #fijar cultivar aqui
  #cultivar = c('IB0118','IR72')
  #cultivar = c('IB0001','IR8')
  
  source(paste0(path_functions, "main_functions.R"))     ## Cargar funciones principales
  source(paste0(path_functions, "make_xfile.R"))         ## Cargar funcion para escribir Xfile DSSAT
  source(paste0(path_functions, "make_wth.R")) 
  source(paste0(path_functions, "dssat_batch.R"))
  source(paste0(path_functions, "DSSAT_run.R"))
  
  day0 <- crop_secano$N.app.0d
  day_aplication0 <- rep(0, length(day0))
  
  day30 <- crop_secano$N.app.30d
  day_aplication30 <- rep(30, length(day30))
  
  amount <- data.frame(day0, day30)
  day_app <- data.frame(day_aplication0, day_aplication30)
  
  
## configuracion Archivo experimental secano
  years <- 69:97    
  data_xfile <- list()
  data_xfile$crop <- "RICE" 
  data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ RICE LAC"
  data_xfile$name <- "./JBID.RIX" 
  data_xfile$CR <- "RI"
  data_xfile$INGENO <- rep(cultivar[1], length(crop_secano[, "variedad.1"]))   
  data_xfile$CNAME <- "IRNA"
  data_xfile$initation <- crop_secano$mirca.start
  data_xfile$final <- crop_secano$mirca.end
  data_xfile$system <- "rainfed"  ## Irrigation or rainfed, if is irrigation then automatic irrigation
  data_xfile$year <- years[1]
  data_xfile$nitrogen_aplication <- list(amount = amount, day_app = day_app)
  data_xfile$smodel <- "RIXCER"     ##  Fin Model
  data_xfile$bname <- "DSSBatch.v45"
  data_xfile$PPOP <- 200   
  data_xfile$PPOE <- 175   
  data_xfile$PLME <- "S"  ## to rice S semilla T transplanting
  data_xfile$PLDS <- "B"   
  data_xfile$PLRD <- 0    ## Investigar mas acerca de este parametro
  data_xfile$PLDP <- 2    ## Investigar mas acerca de este parametro
  
  
	modelos <- c("bcc_csm1_1", "bnu_esm", "cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr", "miroc_miroc5",
             "mpi_esm_mr", "ncc_noresm1_m")
  
  
  i <- 4
  gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[i],"/Futuro/")
  load(paste0(gcm, "Precipitation.RDat"))
  load(paste0(gcm, "Srad.Rdat"))
  load(paste0(gcm, "Temperatura_2.Rdat"))
  
  # Climate Data Set for WFD or global model of climate change
  climate_data <- list()
  climate_data$year <- 69:97       
  climate_data$Srad <- Srad          
  climate_data$Tmax <- Tmax     
  climate_data$Tmin <- Tmin     
  climate_data$Prec <- Prec      
  climate_data$lat <- crop_secano[,"y"]       
  climate_data$long <- crop_secano[, "x"]        
  climate_data$wfd <- "model"      
  climate_data$id <- crop_secano[, "Coincidencias"]
  
  ## Entradas para las corridas de DSSAT 
  
  input_data <- list()
  input_data$xfile <- data_xfile
  input_data$climate <- climate_data
  
	dir_dssat <- "/home/jmesa/csm45_1_23_bin_ifort/"
	dir_base <- "/home/jmesa/Scratch"
	 
  
  ## librerias para el trabajo en paralelo
  library(foreach)
  library(doMC)
  
    ##  procesadores en su servidor
  registerDoMC(8)
  Run <- foreach(i = 1:dim(crop_secano)[1]) %dopar% {
    
    run_dssat(input_data, i, dir_dssat, dir_base)
    
  }

tipo <- "Secano_"
cultivo <- "Arroz_"
cultivar.n <- cultivar[2]  ## nombre del cultivar

save(Run, file = paste("/home/jmesa/", "_", cultivo, tipo, cultivar.n, "_",modelos[i], "_",  ".RDat",sep=""))

