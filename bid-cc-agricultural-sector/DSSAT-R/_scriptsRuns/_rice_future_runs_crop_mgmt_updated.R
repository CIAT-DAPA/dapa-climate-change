##############################################################################
##############################################################################
######################## Parallel DSSAT for rice ############################
##############################################################################
##############################################################################

options(warn = -1); options(scipen = 999); g <- gc(); rm(list = ls())

# Some general config
scenario <- "future" # historical, future

# Cultivar list for rice (based on CIMMYT Mega-environment work)
cul_list <- data.frame(CID = 1 : 3, dsid = c("IB0001", "IB0118", "IB0015"), culname = c("IR8", "IR72", "IR64"))

# Diagnostic run is only performed for irrigated systems, for historical climate
run_type <- "final" # diagnostic (to extract fertiliser dates) or final (final run once mgmt has been specified)

sys_type <- "secano" # riego, secano

# GCMs, only if scenario == "future"
modelos <- c("bcc_csm1_1", "bnu_esm","cccma_canesm2", "gfld_esm2g", "inm_cm4", "ipsl_cm5a_lr",
             "miroc_miroc5", "mpi_esm_mr", "ncc_noresm1_m")

# If we want to clean up raw DSSAT files
cleanup_all <- F

##############################################################################
##############################################################################

# Iterate GCM's
for (gcm_i in 1:length(modelos)) {
  
  cat(paste("Processing of:", modelos[gcm_i], "\n", sep = ""))
  
  # Load climate data
  if (scenario == "historical") {
    load(paste0(path_project, "14-ObjectsR/wfd/", "WDF_all_new.Rdat"))
  } else {
    gcm <- paste0("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/14-ObjectsR/14-ObjectsR/", modelos[gcm_i], "/Futuro/version2017/")
    load(paste0(gcm, "Precipitation.RDat"))
    load(paste0(gcm, "Srad.Rdat"))
    load(paste0(gcm, "Temperatura_2.Rdat"))
  }
  
  # Iterate cultivars
  for (cultivar in 1:nrow(cul_list)) {
    
    cat(paste("Processing of:", cul_list$culname[cultivar], "\n", sep = ""))
    
    # Paths para scripts de funciones y workspace
    path_functions <- "~/Repositories/dapa-climate-change/bid-cc-agricultural-sector/DSSAT-R/"
    path_project <- "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/"
    
    # Cargar data frame entradas para DSSAT
    # load(paste0(path_project, "14-ObjectsR/Soil.RData"))
    load(paste0(path_project, "14-ObjectsR/Soil2.RData"))
    rm(list=setdiff(ls(), c("values", "Soil_profile", "Cod_Ref_and_Position_Generic", "make_soilfile", "xy_Ref",
                            "Soil_Generic", "wise", "in_data", "read_oneSoilFile", "path_functions", "path_project", 
                            "Cod_Ref_and_Position", "profileMatrix", "scenario", "cul_list", "cultivar", "run_type", "sys_type",
                            "modelos", "gcm_i", "cleanup_all", "Tmax", "Tmin", "Prec", "Srad")))
    load(paste0(path_project, "/08-Cells_toRun/matrices_cultivo/version2017/rice_", sys_type, ".RDat"))
    assign("crop_mgmt", get(paste("crop_", sys_type, sep="")))
    
    # Updating planting dates using GGCMI data
    suppressMessages(library(ncdf4))
    suppressMessages(library(raster))
    if(sys_type == "riego"){
      ggcmi <- brick(paste(path_project, "/20-GGCMI-data/Rice_ir_growing_season_dates_v1.25.nc4", sep = ""), varname="planting day")
    } else {
      ggcmi <- brick(paste(path_project, "/20-GGCMI-data/Rice_rf_growing_season_dates_v1.25.nc4", sep = ""), varname="planting day")
    }
    ggcmi <- ggcmi[[1]]
    ggcmi[which(ggcmi[] == -99)] <- NA
    
    planting_dates <- raster::extract(x = ggcmi, y = crop_mgmt[, c('x', 'y')])
    crop_mgmt$mirca.start <- round(planting_dates, 0)
    
    # Cargar funciones
    source(paste0(path_functions, "main_functions.R"))    ## Cargar funciones principales
    source(paste0(path_functions, "make_xfile.R"))        ## Cargar funcion para escribir Xfile DSSAT
    source(paste0(path_functions, "make_wth.R"))
    source(paste0(path_functions, "dssat_batch.R"))
    source(paste0(path_functions, "DSSAT_run.R"))
    source(paste0(path_functions, "Extraer.SoilDSSAT.R")) ## New extraer soil dssat function
    
    # Crear data.frame de aplicaciones de fertilizante
    if (run_type == "diagnostic") {
      day0 <-  crop_mgmt$N.app.0d
      day_aplication0 <- rep(0, length(day0))
      
      day30 <- crop_mgmt$N.app.30d
      day_aplication30 <- rep(30, length(day30))
      
      amount <- data.frame(day0, day30)
      day_app <- data.frame(day_aplication0, day_aplication30)
    } else {
      # Here write update of mgmt matrix when first (diagnostic) run is available
      # Define dates of fertilizer second application
      if(!file.exists(paste(path_project, "/08-Cells_toRun/matrices_cultivo/version2017/_rice_crop_mgmt_", sys_type, ".Rdat", sep = ""))){
        
        day0 <-  crop_mgmt$N.app.0d
        day_aplication0 <- rep(0, length(day0))
        day_aplication30 <- unlist(lapply(1:dim(crop_mgmt)[1], function(p){
          
          if(sys_type == 'riego'){setwd(paste('/home/jmesa/Scratch/diagnostic_RICE_irrigation_', cul_list$culname[cultivar], '_WFD/RICE_irrigation_', p, sep = ''))}
          if(sys_type == 'secano'){setwd(paste('/home/jmesa/Scratch/diagnostic_RICE_rainfed_', cul_list$culname[cultivar], '_WFD/RICE_rainfed_', p, sep = ''))}
          NappDay <- read.NappDay(crop = "RICE")
          day30 <- round(mean(NappDay$Napp.day, na.rm = T), 0)
          return(day30)
          
        }))
        
        crop_mgmt$SecondAppDay <- day_aplication30
        save(crop_mgmt, file = paste(path_project, "08-Cells_toRun/matrices_cultivo/version2017/_rice_crop_mgmt_", sys_type, ".RDat", sep = ""))
        
        # Define amount of fertilizer to apply
        day0 <-  crop_mgmt$N.app.0d
        day30 <- crop_mgmt$N.app.30d
        amount <- data.frame(day0, day30)
        day_app <- data.frame(day_aplication0, day_aplication30)
        rm(day0, day30, day_aplication0, day_aplication30)
        
      } else {
        
        load(paste(path_project, "08-Cells_toRun/matrices_cultivo/version2017/_rice_crop_mgmt_", sys_type, ".RDat", sep = ""))
        day0 <-  crop_mgmt$N.app.0d
        day_aplication0 <- rep(0, length(day0))
        day0 <-  crop_mgmt$N.app.0d
        day30 <- crop_mgmt$N.app.30d
        
        amount <- data.frame(day0, day30)
        day_app <- data.frame(day_aplication0, day_aplication30=crop_mgmt$SecondAppDay)
        rm(day0, day30, day_aplication0)
        
      }
      
    }
    
    # Define years range, linea base: 71:99; futuro: 69:97
    if (scenario == "historical") {years <- 71:99}
    if (scenario == "future") {years <- 69:97}
    
    # Configuracion Archivo experimental
    data_xfile <- list()
    data_xfile$run_type <- run_type
    data_xfile$crop <- "RICE"
    data_xfile$exp_details <- "*EXP.DETAILS: BID17101RZ RICE LAC"
    data_xfile$name <- "./JBID.RIX" 
    data_xfile$CR <- "RI" # Variable importante 
    data_xfile$INGENO <- rep(paste(cul_list$dsid[which(cul_list$CID == cultivar)]), length(crop_mgmt[, "variedad.1"]))
    data_xfile$CNAME <- "IRNA"
    data_xfile$initation <- crop_mgmt$mirca.start
    data_xfile$final <- crop_mgmt$mirca.end
    if (sys_type == "riego") {data_xfile$system <- "irrigation"} # Irrigation or rainfed, if is irrigation then automatic irrigation
    if (sys_type == "secano") {data_xfile$system <- "rainfed"}   # Irrigation or rainfed, if is irrigation then automatic irrigation
    data_xfile$year <- years[1]
    data_xfile$nitrogen_aplication <- list(amount = amount, day_app = day_app) # Need to take care of
    data_xfile$smodel <- "RICER045"  # Fin Model
    data_xfile$bname <- "DSSBatch.v45"
    data_xfile$PPOP <- 100  # Plant population at planting
    data_xfile$PPOE <- 100  # Plant population at emergence
    data_xfile$PLME <- "S"  # Planting method: dry seed (S); transplanting (T)
    data_xfile$PLDS <- "R"  # Seed distribution: by row (R)
    data_xfile$PLRD <- 0    # Row direction (degrees from N)
    data_xfile$PLRS <- 18   # Row spacing (cm)
    data_xfile$PLDP <-  2 # Planting depth (cm)
    data_xfile$SYMBI <- 'N' # Symbiosis (Y =  Yes, N = Not), "Y" only for bean and soy
    
    # Climate Data Set for WFD or global model of climate change
    climate_data <- list()
    climate_data$year <- years # 71:99 # Years where they will simulate yields change between 71:99 or 69:97
    climate_data$Srad <- Srad  # [[year]][pixel, ]   
    climate_data$Tmax <- Tmax  # [[year]][pixel, ]
    climate_data$Tmin <- Tmin  # [[year]][pixel, ]
    climate_data$Prec <- Prec  # [[year]][pixel, ]
    climate_data$lat <- crop_mgmt[,"y"]   # You can include a vector of latitude
    climate_data$long <- crop_mgmt[, "x"] # You can include a vector of longitude
    if(scenario == "historical") {climate_data$wfd <- "wfd"} else {climate_data$wfd <- "model"} # Switch between "wfd" and "model"
    climate_data$id <- crop_mgmt[, "Coincidencias"]
    
    # Entradas para las corridas de DSSAT
    input_data <- list()
    input_data$xfile <- data_xfile
    input_data$climate <- climate_data
    
    # Carpetas necesarias donde se encuentra DSSAT compilado y un directorio para las corridas
    dir_dssat <- "~/csm45_1_23_bin_ifort/"
    if(run_type == "diagnostic"){dir_base <- "~/ScratchFut"} # It was "~/Scratch" originally
    if(run_type == "final"){dir_base <- "~/ScratchFinalFut"} # It was "~/ScratchFinal" originally
    
    # run dssat for one pixel (test)
    # run_dssat(input=input_data, pixel=250, dir_dssat, dir_base)
    
    # Librerias para el trabajo en paralelo
    suppressMessages(library(foreach))
    suppressMessages(library(doMC))
    
    # Procesadores en su servidor
    registerDoMC(8)
    
    # Run DSSAT in parallel
    Run <- foreach(i = 1:dim(crop_mgmt)[1]) %dopar% {
      run_dssat(input_data, i, dir_dssat, dir_base)
    }
    
    # # Create general output directory
    # if (!file.exists(paste("~/bid_reruns/", run_type, sep = ""))) {
    #   dir.create(paste("~/bid_reruns/", run_type, sep=""), recursive = T)
    # }
    
    # Save file
    if (scenario == "historical") {
      store_name <- paste0(data_xfile$crop, "_", data_xfile$system, "_", 
                           paste(cul_list$culname[which(cul_list$CID == cultivar)]), '_WFD')
    } else {
      store_name <- paste0(data_xfile$crop, "_", data_xfile$system, "_", 
                           paste(cul_list$culname[which(cul_list$CID == cultivar)]), "_",
                           modelos[gcm_i])
    }
    
    if(!dir.exists(paste(path_project, "19-BID-reanalysis/Rice/", scenario, "/", run_type, sep = ""))){
      dir.create(paste(path_project, "19-BID-reanalysis/Rice/", scenario, "/", run_type, sep = ""), recursive = TRUE)
    }
    save(Run, file = paste(path_project, "19-BID-reanalysis/Rice/", scenario, "/", run_type, "/", store_name, ".RDat", sep = ""))
    # save(Run, file = paste("~/bid_reruns/", run_type, "/", store_name, ".RDat", sep = ""))
    
    # Clean up, else create a folder and store results in there
    if (cleanup_all) {
      setwd("~")
      if(run_type == "diagnostic"){system("rm -rf ~/ScratchFut")}
      if(run_type == "final"){system("rm -rf ~/ScratchFinalFut")}
    } else {
      setwd("~")
      if(run_type == "diagnostic"){
        system(paste0("mkdir ~/ScratchFut/", run_type, "_", store_name))
        system(paste0("mv -f ~/ScratchFut/", data_xfile$crop, "_", data_xfile$system, "_* ~/ScratchFut/", run_type, "_", store_name, "/."))
      }
      if(run_type == "final"){
        system(paste0("mkdir ~/ScratchFinalFut/", run_type, "_", store_name))
        system(paste0("mv -f ~/ScratchFinalFut/", data_xfile$crop, "_", data_xfile$system, "_* ~/ScratchFinalFut/", run_type, "_", store_name, "/."))
      }
    }
  }
  
}

g <- gc(); rm(list = ls())
