##############################################################################
########################## initial_conditions #############################
##############################################################################

initial_conditions <- function(data, system) {
  
  if(system == "rainfed"){
    
    ## Extraer Condiciones iniciales de los archivos de suelos
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    
    pat <- "SLB|SDUL"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
	## Borrar easta linea cuando no se corra Papa Secano y Trigo Secano
    ##Initial_conditions <- data.frame(Initial_conditions[, 1], rep(-99, dim(Initial_conditions)[1]))
    colnames(Initial_conditions) <- headers
    
    return(Initial_conditions)
    
  }
  
  if(system == "irrigation"){
    
    ## Extraer Condiciones iniciales de los archivos de suelos
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    
    pat <- "SLB|SDUL"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
    
    Initial_conditions <- data.frame(Initial_conditions[, 1], rep(-99, dim(Initial_conditions)[1]))
    
    colnames(Initial_conditions) <- headers
    
    return(Initial_conditions)
  }

  
}

initial_conditions_mod <- function(data, system) {
  
  if(system == "rainfed"){
    
    ## Extraer Condiciones iniciales de los archivos de suelos
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6, -30, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    ## Aqui se multiplica por 2
	Initial_conditions$V3 <- Initial_conditions$V3 * 2
	
    pat <- "SLB|SLLL|SLOC"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
	
    ## Borrar easta linea cuando no se corra Papa Secano y Trigo Secano
   ## Initial_conditions <- data.frame(Initial_conditions[, 1], rep(-99, dim(Initial_conditions)[1]), Initial_conditions[, 3])
    colnames(Initial_conditions) <- headers
    
	
	

    return(Initial_conditions)
    
  }
  
  if(system == "irrigation"){
    
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6, -30, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    
    pat <- "SLB|SLLL|SLOC"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
    ## Borrar easta linea cuando no se corra Papa Secano y Trigo Secano
    Initial_conditions <- data.frame(Initial_conditions[, 1], rep(-99, dim(Initial_conditions)[1]), Initial_conditions[, 3])
    colnames(Initial_conditions) <- headers
    
    return(Initial_conditions)
    
    
  }
  
  
}

initial_conditions_wheat <- function(data, system) {
  
  if(system == "rainfed"){
    
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6, 6, -24, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    
    pat <- "SLB|SLLL|SDUL|SLOC"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
	
    ## Borrar easta linea cuando no se corra Papa Secano y Trigo Secano
   ## Initial_conditions <- data.frame(Initial_conditions[, 1], rep(-99, dim(Initial_conditions)[1]), Initial_conditions[, 3])
    colnames(Initial_conditions) <- headers
	
	prom_LWR_UPR <- apply(Initial_conditions[, c('SLLL', 'SDUL')],  1, mean)
	
    Initial_conditions <- data.frame(Initial_conditions[, -(2:4)], prom_LWR_UPR, Initial_conditions[, 4])
	colnames(Initial_conditions) <- c('SLB', 'SDUL', 'SLOC')
    
    return(Initial_conditions)
    
  }
  
  if(system == "irrigation"){
    
    SOIL <- readLines(paste(data))  ## Archivo de Suelo
    profiles <- grep("SLB", SOIL)  ## Posicion en la que coincide con las variables a extraer
    imp.head <- scan(paste(data), what = "character", skip = profiles[1] - 1, nlines = 1, quiet = T)  # Encabezado (titulo para las columnas)
    seps <- c(6, -6, 6, -30, 6)    ## Separadores para las variables
    Initial_conditions <- read.fwf("SOIL.SOL", width = seps, header = F, skip = profiles[1], n = profiles[2] - profiles[1] - 1)
    
    pat <- "SLB|SLLL|SLOC"
    headers <- imp.head[grep(pat, imp.head, perl = TRUE)]
    ## Borrar easta linea cuando no se corra Papa Secano y Trigo Secano
    Initial_conditions <- data.frame(Initial_conditions[, 1], rep(-99, dim(Initial_conditions)[1]), Initial_conditions[, 3])
    colnames(Initial_conditions) <- headers
    
    return(Initial_conditions)
    
    
  }
  
  
}


##############################################################################
########################## Convert day for DSSAT #############################
##############################################################################

convert_date <- function(date, year, initial) {
  ## esta es la funcion que se agrega pero sino sirve 60 dias antes dejar como estaba antes
  year_simul <- year - 1
  year <- year #+ 1
  if(date <= 0){
    date1 <- 365 + date
    date1 <- paste0(year_simul, date1)
  }
  ############################################# lo que funcionaba
  if(date < 10 & date > 0) {
    date1 <- paste0(year, "00", date)
  }
  if(date < 100 & date >= 10 & date > 0) {
    date1 <- paste0(year, "0", date)
  }
  if(date >= 100 & date > 0 & date < 305) {
    date1 <- paste0(year, date)
  }
	
  if(date >= 305 & initial == FALSE) {
  
    remains <- abs(365 - date)
	## sgte_year <- (60-remains)
	date1 <- paste0((year + 1) , '0', remains)
  
  }else{
  
      if(date >= 305 & initial == TRUE) {
  
	date1 <- paste0(year, date)
  
  }
  
  }
  

  
  return(date1)
}





##############################################################################
########################## Settings leap year ################################
##############################################################################

leap_year <- function(year) {
  ## Settings leap year
  ## yrs to create the label .WTH
  ## yrs2 Year Julian days
  if((year %% 4) == 0) {
    if((year %% 100) == 0) {
      if((year %% 400) == 0) {
        # print(paste(year,"is a leap year"))
        yrs <- year
        yrs2 <- (yrs * 100):((yrs * (100)) + 366)
        yrs <- (yrs * 1000):((yrs * (1000)) + 366)
        
        yrs <- yrs[-1]
        yrs2 <- yrs2[-1]
        
      } else {
        # print(paste(year,"is not a leap year"))
        yrs <- year
        yrs2 <- (yrs * 100):((yrs * (100)) + 365)
        yrs <- (yrs * 1000):((yrs * (1000)) + 365)
        
        yrs <- yrs[-1]               ## day 00 is not possible
        yrs2 <- yrs2[-1]             ## day 00 is not possible              
        
      }
    } else {
      # print(paste(year,"is a leap year"))
      yrs <- year
      yrs2 <- (yrs * 100):((yrs * (100)) + 366)
      yrs <- (yrs * 1000):((yrs * (1000)) + 366)
      
      yrs <- yrs[-1]                ## day 00 is not possible
      yrs2 <- yrs2[-1]              ## day 00 is not possible 
      
    }
  } else {
    # print(paste(year,"is not a leap year"))
    yrs <- year
    yrs2 <- (yrs * 100):((yrs * (100)) + 365)
    yrs <- (yrs * 1000):((yrs * (1000)) + 365)
    
    yrs <- yrs[-1]                  ## day 00 is not possible
    yrs2 <- yrs2[-1]                ## day 00 is not possible  
  }
  
  years <- list(yrs, yrs2)
  names(years) <- c("yrs", "yrs2")
  return(years)
  
}

##############################################################################
# Read stress data from OVERVIEW.OUT
##############################################################################

read.overview <- function(crop) {
  
  if(crop == "WHEAT") {
    data <- 'Overview.OUT'
    overview <- readLines(paste(data))
    stress <- suppressMessages(grep("ENVIRONMENTAL AND STRESS FACTORS", overview))
    stress_by_year <- 1:length(stress)
    
  } else{
    
    data <- 'OVERVIEW.OUT'
    overview <- readLines(paste(data))
    stress <- grep("ENVIRONMENTAL AND STRESS FACTORS", overview)
    stress_by_year <- 1:length(stress)
    error <- grep("SIMULATION ABORTED", overview)
    
    
    
    if(length(error)>0){
      
      stress <- c(stress, error)
      stress <- sort(stress)
      stress_by_year <- 1:length(stress)
      
    }
    
    
    
  }
  
  extract_stress <- function(crop, year){
    
    if(crop == "WHEAT"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- invisible(scan(paste(data), what = "character", skip = stress[year] + 9, nlines = 1, quiet = T) )
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 13, nlines = 1, quiet = T) 
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[15], value_stress1[17], value_stress_all[14], value_stress_all[16])
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
      
    }
    
    ## Ver el archivo overview para identificar cuales son los valores a extraer (revisar la ppt en la carpeta bid)
    if(crop == "RICE"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- scan(paste(data), what = "character", skip = stress[year] + 9, nlines = 1, quiet = T) 
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 13, nlines = 1, quiet = T) 
      
      if(length(value_stress1) >0){
        
        if(value_stress1[1] != 'Panicl'){
          value_stress1 <- NA
          value_stress_all <- NA
        }
        
      }
      
      
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[14], value_stress1[16], value_stress_all[13], value_stress_all[15])
      # values_of_stress <- data.frame(apply(values_of_stress, 2, as.numeric)) 
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
    }
    
    if(crop == "BEAN"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- scan(paste(data), what = "character", skip = stress[year] + 8, nlines = 1, quiet = T) 
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 12, nlines = 1, quiet = T) 
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[15], value_stress1[17], value_stress_all[13], value_stress_all[15])
      
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
      
    }
    
	
	  if(crop == "SOY"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- scan(paste(data), what = "character", skip = stress[year] + 8, nlines = 1, quiet = T) 
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 12, nlines = 1, quiet = T) 
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[13], value_stress1[15], value_stress_all[13], value_stress_all[15])
      
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
      
    }
    
    if(crop == "MAIZE"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- scan(paste(data), what = "character", skip = stress[year] + 9, nlines = 1, quiet = T) 
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 13, nlines = 1, quiet = T) 
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[14], value_stress1[16], value_stress_all[13], value_stress_all[15])
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
      
    }
    
    
  }
  
  y <- lapply(1:length(stress), function(i) suppressMessages(extract_stress(crop, i)))
  y <- do.call("rbind", y)
  
  if(dim(y)[1] == 1){
  
	for(i in 1:ncol(y)){
	y[,i] <- as.numeric(as.character(y[,i]))
	}
	
  
  } else{
  y <- data.frame(apply(y, 2, as.numeric))
  }
   
  
  return(y)
  
}

##############################################################################
# Read application day data for fertilizer from OVERVIEW.OUT
##############################################################################

read.NappDay <- function(crop) {
  # SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES
  
  if(crop == "WHEAT") {
    data <- 'Overview.OUT'
    overview <- readLines(paste(data))
    stress <- suppressMessages(grep("SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES", overview))
    stress_by_year <- 1:length(stress)
  } else{
    data <- 'OVERVIEW.OUT'
    overview <- readLines(paste(data))
    stress <- grep("SIMULATED CROP AND SOIL STATUS AT MAIN DEVELOPMENT STAGES", overview)
    stress_by_year <- 1:length(stress)
    error <- grep("SIMULATION ABORTED", overview)
    
    if(length(error)>0){
      stress <- c(stress, error)
      stress <- sort(stress)
      stress_by_year <- 1:length(stress)
    }
  }
  
  extract_application_date <- function(crop, year){
    
    if(crop == "WHEAT"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- invisible(scan(paste(data), what = "character", skip = stress[year] + 9, nlines = 1, quiet = T) )
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 13, nlines = 1, quiet = T) 
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[15], value_stress1[17], value_stress_all[14], value_stress_all[16])
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
      
    }
    
    ## Ver el archivo overview para identificar cuales son los valores a extraer (revisar la ppt en la carpeta bid)
    if(crop == "RICE"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- scan(paste(data), what = "character", skip = stress[year] + 9, nlines = 1, quiet = T) 
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 13, nlines = 1, quiet = T) 
      
      if(length(value_stress1) >0){
        
        if(value_stress1[1] != 'Panicl'){
          value_stress1 <- NA
          value_stress_all <- NA
        }
        
      }
      
      
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[14], value_stress1[16], value_stress_all[13], value_stress_all[15])
      # values_of_stress <- data.frame(apply(values_of_stress, 2, as.numeric)) 
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
    }
    
    if(crop == "BEAN"){
      # Read only the row corresponding to first flowering date per year
      application_date <- scan(paste(data), what = "character", skip = stress[year] + 12, nlines = 1, quiet = T)
      # Just capture the day of year when first flowering 
      application_date <- as.numeric(application_date[3])
      # Create a data.frame with crop, year and application day
      application_date <- data.frame(crop = crop, year=year, day=application_date)
      
      return(application_date)
    }
    
    if(crop == "SOY"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- scan(paste(data), what = "character", skip = stress[year] + 8, nlines = 1, quiet = T) 
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 12, nlines = 1, quiet = T) 
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[13], value_stress1[15], value_stress_all[13], value_stress_all[15])
      
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
      
    }
    
    if(crop == "MAIZE"){
      
      col.names <- c("Stress_water1", "Stress_nitrogen1", "Stress_water_all", "Stress_nitrogen_all")
      
      ## Se debe tener en cuenta que se debe leer para todo el tamaño del objeto Stress
      value_stress1 <- scan(paste(data), what = "character", skip = stress[year] + 9, nlines = 1, quiet = T) 
      value_stress_all <- scan(paste(data), what = "character", skip = stress[year] + 13, nlines = 1, quiet = T) 
      ## Se debe evaluar los puntos que se desean extraer
      ## Stress durante el llenado vital para los rendimientos
      ## value_stress1[15]
      ## value_stress1[17]
      ## Stress que considera al parecer el promedio de todas las etapas
      ## value_stress_all[14]
      ## value_stress_all[16]
      values_of_stress <- data.frame(value_stress1[14], value_stress1[16], value_stress_all[13], value_stress_all[15])
      colnames(values_of_stress) <- col.names
      
      return(values_of_stress)
      
    }
    
    
  }
  
  y <- lapply(1:length(stress), function(i) suppressMessages(extract_application_date(crop, i)))
  y <- do.call("rbind", y)
  app_day <- round(mean(y$day, na.rm = T))
  
  return(app_day)
  
}
read.overview2calc.appDay(crop = "BEAN") # To test

##############################################################################
## Cambiar fechas de Futuro 1969 == 2021
##############################################################################

change_date_to_fut <- function(data){

date_to_mod <- data.frame(pas = 1968:1997, fut = 2020:2049)
change <- as.numeric(substr(data, 1, 4))
post_to_change <- match(change, date_to_mod[, 'pas'])
change[] <- date_to_mod[post_to_change, 'fut']

return(change)

}

##Run[, 'SDAT'] <- as.numeric(paste0(change_date_to_fut(Run[, 'SDAT']), substr(Run[, 'SDAT'], 5, 7)))


