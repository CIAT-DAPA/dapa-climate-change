
g <- gc(); rm(list = ls())

# R options
options(warn = -1); options(scipen = 999); OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]

if(OSys == "Windows"){
  setwd("//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/22-Redo-BID-report")
} else {
  if(OSys == "Linux"){
    setwd("/mnt/workspace_cluster_3/bid-cc-agricultural-sector/22-Redo-BID-report")
  }
}

# Load libraries
suppressMessages(library(Hmisc))
suppressMessages(library(raster))
suppressMessages(library(ggplot2))
suppressMessages(library(reshape))
suppressMessages(library(RColorBrewer))

# Escoger parametros de interes
params <- c('HWAH','H.AM','IRCM','CWAM','TMAXA','TMINA')

crops <- c('Maize', 'Rice', 'Soybean', 'Bean', 'Wheat')

lapply(1:length(crops), function(i){
  
  cat(paste("\n\n========= Processing ", crops[i], " =========\n", sep = ""))
  
  if(crops[i] == "Maize"){variedades <- c("H6", "FM6", "MCCURDY6714")}
  if(crops[i] == "Rice"){variedades <- c("IR8", "IR72", "IR64")}
  if(crops[i] == "Soybean"){variedades <- c("Hutcheson", "DONMARIO")}
  if(crops[i] == "Bean"){variedades <- c("ICTAOstua", "Carioca", "A193", "BAT881", "Manitou", "Perola", "BRSRadiante")}
  if(crops[i] == "Wheat"){variedades <- c("Seri82BA", "TajanBA", "DonErnestoBA", "Gerek79BA", "HalconsnaBA", "BrigadierBA")}
  
  # Set paths
  if(OSys == "Windows"){
    path.res = "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis" # Results directory
    path.root = "//dapadfs/workspace_cluster_3/bid-cc-agricultural-sector" # Root path
  } else {
    if(OSys == "Linux"){
      path.res = "/mnt/workspace_cluster_3/bid-cc-agricultural-sector/19-BID-reanalysis" # Results directory
      path.root = "/mnt/workspace_cluster_3/bid-cc-agricultural-sector" # Root path
    }
  }
  carp.res.riego = "historical/final"
  carp.res.secano = "historical/final"
  
  # Otros datos fijos
  regions = c('MEX', 'CEN', 'AND', 'BRA', 'SUR')
  treat = c('riego', 'secano') # riego o secano (which to plot)
  treat.en = c('Irrigated', 'Rainfed')
  cultivos = c('maiz', 'arroz', 'soya', 'frijol', 'trigo')
  cultivos.en = c('Maize', 'Rice', 'Soybean', 'Bean', 'Wheat')
  anos = 1:28 # Fijar year para analizar aqui
  anos2 = 1971:1998
  
  # Load pixel id's
  eval(parse(text = paste('load("', path.root, '/08-Cells_toRun/matrices_cultivo/version2017/', cultivos.en[i], '_riego.Rdat")', sep = '')))  #08-Cells_toRun/
  eval(parse(text = paste('load("', path.root, '/08-Cells_toRun/matrices_cultivo/version2017/', cultivos.en[i], '_secano.Rdat")', sep = '')))
  
  # Load mapas de Latinoamerica
  eval(parse(text = paste('Map_LatinAmerica <- shapefile("', path.root, '/03-Map_LatinAmerica/Latino_America.shp")', sep = '')))
  Map_LatinAmerica1 <- fortify(Map_LatinAmerica)
  
  # Set graphing limits
  xlim.c = c(-116, -35)
  ylim.c = c(-54, 32)
  cex.c = 0.5
  
  # Get list of climate models
  models = read.table(paste(path.root, '/_documentos/ModelosGCM.csv', sep = ''), header = T, sep = ',', stringsAsFactors = F)
  models = rbind(models, 'Historical baseline', 'Future multi-GCM average')
  p = dim(models)[1]
  colnames(models) = 'Models'  #hack for now (machetazo)
  
  # load, unlist and extract yield data to arrays (gridcells x years x models)
  # initialize arrays
  yld_secano = array(NA, dim = c(dim(crop_secano)[1], 30, (p-1), length(variedades)))  #pixels x years x models x varieties
  yld_riego = array(NA, dim = c(dim(crop_riego)[1], 30, (p-1), length(variedades)))
  
  for(v in 1:length(variedades)){
    
    for(m in 1:10){ # fija indices de modelos para incluir
      
      # print(m)
      
      # Load & extract baseline data from list
      if(m <= 9){
        if(crops[i] == "Soybean"){
          load(paste(path.res, '/', crops[i], '/future/final/SOY_rainfed_', variedades[v], '_', models[m,], '.Rdat', sep = ''))
        } else {
          load(paste(path.res, '/', crops[i], '/future/final/', toupper(crops[i]), '_rainfed_', variedades[v], '_', models[m,], '.Rdat', sep = ''))
        }
      } else {
        if(crops[i] == "Soybean"){
          load(paste(path.res, '/', crops[i], '/', carp.res.secano, '/SOY_rainfed_', variedades[v], '_WFD.Rdat', sep = ''))
        } else {
          load(paste(path.res, '/', crops[i], '/', carp.res.secano, '/', toupper(crops[i]), '_rainfed_', variedades[v], '_WFD.Rdat', sep = ''))
        }
      }
      Run.secano = Run
      if(m <= 9){
        if(crops[i] == "Soybean"){
          load(paste(path.res, '/', crops[i], '/future/final/SOY_irrigation_', variedades[v], '_', models[m,], '.Rdat', sep = ''))
        } else {
          load(paste(path.res, '/', crops[i], '/future/final/', toupper(crops[i]), '_irrigation_', variedades[v], '_', models[m,], '.Rdat', sep = ''))
        }
      } else {
        if(crops[i] == "Soybean"){
          load(paste(path.res, '/', crops[i], '/', carp.res.riego, '/SOY_irrigation_', variedades[v], '_WFD.Rdat', sep = ''))
        } else {
          load(paste(path.res, '/', crops[i], '/', carp.res.riego, '/', toupper(crops[i]), '_irrigation_', variedades[v], '_WFD.Rdat', sep = ''))
        }
      }
      Run.riego = Run
      
      # Unlist everything into matrices
      secano = array(NA, dim = c(length(Run.secano), 30)) # Initialize arrays
      for(j in 1:length(Run.secano)){
        if(is.null(dim(Run.secano[[j]]))){
          secano[j,] = 0
        } else {
          if(m == 10){
            ind.s = as.numeric(substr(Run.secano[[j]][,1], 1, 4)) - 1970
          }  else {
            ind.s = as.numeric(substr(Run.secano[[j]][,1], 1, 4)) - 2020
          }
          if(ind.s[1] == 0){ind.s = ind.s +1}
          secano[j, ind.s] = Run.secano[[j]][,'HWAH']
        }
      }
      riego = array(NA, dim = c(length(Run.riego), 30))
      for(j in 1:length(Run.riego)){
        if(is.null(dim(Run.riego[[j]]))){
          riego[j,] = 0
        } else {
          if(m == 10){
            ind.r = as.numeric(substr(Run.riego[[j]][,1], 1, 4)) - 1970
          } else {
            ind.r = as.numeric(substr(Run.riego[[j]][,1], 1, 4)) - 2020
          }
          if(ind.r[1] == 0){ind.r = ind.r + 1}
          riego[j, ind.r] = Run.riego[[j]][,'HWAH']
        }
      }
      
      # place results in array
      yld_secano[,,m,v] = secano
      yld_riego[,,m,v] = riego
    }
  }
  
  # Descartar los years sin datos climaticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones tecnicas!
  yld_secano = yld_secano[,anos,,] # Incluir los indices de los years "buenos" aqui con datos por todos los pixeles
  yld_riego = yld_riego[,anos,,]
  yld_secano[yld_secano == -99] = 0 # Re-emplazar -99 con 0 for legitimate crop failures
  yld_riego[yld_riego == -99] = 0
  
  # #Check median durations by variety
  # quantile(c(yld_secano[,,10,1,2],yld_riego[,,10,1,2]),na.rm=T)
  
  # Identify best variety in historical baseline (higher mean yields & less crop failures)
  # across all 3 varieties
  wfd.r = apply(yld_riego[,,(p-1),], c(1, 3), mean, na.rm = T) # multi-annual means
  wfd.s = apply(yld_secano[,,(p-1),], c(1, 3), mean, na.rm = T)
  thresh = mean(yld_secano[,,(p-1),], na.rm = T) * 0.2 # define crop failure as 20% of mean rainfed yield
  wfd.fail.r = apply(yld_riego[,,(p-1),], c(1, 3), function(x) sum(x < thresh, na.rm = T)) # multi-annual means
  wfd.fail.s = apply(yld_secano[,,(p-1),], c(1, 3), function(x) sum(x < thresh,na.rm = T))
  
  # Highest-yielding variety
  wfd.r.high = apply(wfd.r, 1, which.max)
  wfd.s.high = apply(wfd.s, 1, which.max)
  
  # Least crop failures
  wfd.r.lowFail = apply(wfd.fail.r, 1, which.min)
  wfd.s.lowFail = apply(wfd.fail.s, 1, which.min)
  
  # High-yielding and no more than 4 crop failures (15%)
  if(crops[i] %in% c("Maize", "Rice")){
    
    wfd.s.best = mat.or.vec(dim(wfd.s)[1], 1)
    for (j in 1:dim(wfd.s)[1])  {
      sortYield = sort.list(wfd.s[j,], dec = T)
      if (wfd.fail.s[j,sortYield[1]] <= 4) {wfd.s.best[j] = sortYield[1]}  else{
        if (wfd.fail.s[j,sortYield[2]] <= 4) {wfd.s.best[j] = sortYield[2]}  else{
          if (wfd.fail.s[j,sortYield[3]] <= 4) {wfd.s.best[j] = sortYield[3]}  else{
            wfd.s.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
          }
        }
      }
    }
    
    wfd.r.best = mat.or.vec(dim(wfd.r)[1], 1)
    for (j in 1:dim(wfd.r)[1])  {
      sortYield = sort.list(wfd.r[j,], dec = T) 
      if (wfd.fail.r[j,sortYield[1]] <= 4) {wfd.r.best[j] = sortYield[1]}  else{
        if (wfd.fail.r[j,sortYield[2]] <= 4) {wfd.r.best[j] = sortYield[2]}  else{
          if (wfd.fail.r[j,sortYield[3]] <= 4) {wfd.r.best[j] = sortYield[3]}  else{
            wfd.r.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
          }
        }
      }
    }
    
  } else {
    if(crops[i] == "Soybean"){
      
      wfd.s.best = mat.or.vec(dim(wfd.s)[1], 1)
      for (j in 1:dim(wfd.s)[1])  {
        sortYield = sort.list(wfd.s[j,], dec = T)
        if (wfd.fail.s[j,sortYield[1]] <= 4) {wfd.s.best[j] = sortYield[1]}  else{
          if (wfd.fail.s[j,sortYield[2]] <= 4) {wfd.s.best[j] = sortYield[2]}  else{
            wfd.s.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
          }
        }
      }
      
      wfd.r.best = mat.or.vec(dim(wfd.r)[1], 1)
      for (j in 1:dim(wfd.r)[1])  {
        sortYield = sort.list(wfd.r[j,], dec = T) 
        if (wfd.fail.r[j,sortYield[1]] <= 4) {wfd.r.best[j] = sortYield[1]}  else{
          if (wfd.fail.r[j,sortYield[2]] <= 4) {wfd.r.best[j] = sortYield[2]}  else{
            wfd.r.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
          }
        }
      }
      
    } else {
      if(crops[i] == "Bean"){
        
        wfd.s.best = mat.or.vec(dim(wfd.s)[1], 1)
        for (j in 1:dim(wfd.s)[1])  {
          sortYield = sort.list(wfd.s[j,], dec = T)
          if (wfd.fail.s[j,sortYield[1]] <= 4) {wfd.s.best[j] = sortYield[1]}  else{
            if (wfd.fail.s[j,sortYield[2]] <= 4) {wfd.s.best[j] = sortYield[2]}  else{
              if (wfd.fail.s[j,sortYield[3]] <= 4) {wfd.s.best[j] = sortYield[3]}  else{
                if (wfd.fail.s[j,sortYield[4]] <= 4) {wfd.s.best[j] = sortYield[4]} else{
                  if (wfd.fail.s[j,sortYield[5]] <= 4) {wfd.s.best[j] = sortYield[5]} else{
                    if (wfd.fail.s[j,sortYield[6]] <= 4) {wfd.s.best[j] = sortYield[6]} else{
                      if (wfd.fail.s[j,sortYield[7]] <= 4) {wfd.s.best[j] = sortYield[7]} else{
                        wfd.s.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
                      }
                    }
                  }
                }
              }
            }
          }
        }
        
        wfd.r.best = mat.or.vec(dim(wfd.r)[1], 1)
        for (j in 1:dim(wfd.r)[1])  {
          sortYield = sort.list(wfd.r[j,], dec = T) 
          if (wfd.fail.r[j,sortYield[1]] <= 4) {wfd.r.best[j] = sortYield[1]}  else{
            if (wfd.fail.r[j,sortYield[2]] <= 4) {wfd.r.best[j] = sortYield[2]}  else{
              if (wfd.fail.r[j,sortYield[3]] <= 4) {wfd.r.best[j] = sortYield[3]}  else{
                if (wfd.fail.r[j,sortYield[4]] <= 4) {wfd.r.best[j] = sortYield[4]} else{
                  if (wfd.fail.r[j,sortYield[5]] <= 4) {wfd.r.best[j] = sortYield[5]} else{
                    if (wfd.fail.r[j,sortYield[6]] <= 4) {wfd.r.best[j] = sortYield[6]} else{
                      if (wfd.fail.r[j,sortYield[7]] <= 4) {wfd.r.best[j] = sortYield[7]} else{
                        wfd.r.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
                      }
                    }
                  }
                }
              }
            }
          }
        }
        
      } else {
        if(crops[i] == "Wheat"){
          
          wfd.s.best = mat.or.vec(dim(wfd.s)[1], 1)
          for (j in 1:dim(wfd.s)[1])  {
            sortYield = sort.list(wfd.s[j,], dec = T)
            if (wfd.fail.s[j,sortYield[1]] <= 4) {wfd.s.best[j] = sortYield[1]}  else{
              if (wfd.fail.s[j,sortYield[2]] <= 4) {wfd.s.best[j] = sortYield[2]}  else{
                if (wfd.fail.s[j,sortYield[3]] <= 4) {wfd.s.best[j] = sortYield[3]}  else{
                  if (wfd.fail.s[j,sortYield[4]] <= 4) {wfd.s.best[j] = sortYield[4]} else{
                    if (wfd.fail.s[j,sortYield[5]] <= 4) {wfd.s.best[j] = sortYield[5]} else{
                      if (wfd.fail.s[j,sortYield[6]] <= 4) {wfd.s.best[j] = sortYield[6]} else{
                        wfd.s.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
                      }
                    }
                  }
                }
              }
            }
          }
          
          wfd.r.best = mat.or.vec(dim(wfd.r)[1], 1)
          for (j in 1:dim(wfd.r)[1])  {
            sortYield = sort.list(wfd.r[j,], dec = T) 
            if (wfd.fail.r[j,sortYield[1]] <= 4) {wfd.r.best[j] = sortYield[1]}  else{
              if (wfd.fail.r[j,sortYield[2]] <= 4) {wfd.r.best[j] = sortYield[2]}  else{
                if (wfd.fail.r[j,sortYield[3]] <= 4) {wfd.r.best[j] = sortYield[3]}  else{
                  if (wfd.fail.r[j,sortYield[4]] <= 4) {wfd.r.best[j] = sortYield[4]} else{
                    if (wfd.fail.r[j,sortYield[5]] <= 4) {wfd.r.best[j] = sortYield[5]} else{
                      if (wfd.fail.r[j,sortYield[6]] <= 4) {wfd.r.best[j] = sortYield[6]} else{
                        wfd.r.best[j] = sortYield[1]  #if all varieties have more than 4 crop failures, use highest-yielding
                      }
                    }
                  }
                }
              }
            }
          }
          
        }
      }
    }
  }
  
  # Just pick highest-yielding by growth habit for dry bean
  if(crops[i] == 'Bean'){ # check highest yielding by country according to growth habit
    habit = read.table(paste0(path.root, '/_documentos/Bean_growthHabit.csv'), header = T, sep = ',')
    for(t in 1:2){
      eval(parse(text = paste("meso = match(crop_", treat[t], "$country, habit$X[habit$Mesoamerican == 1])", sep = "")))
      ind.meso = which(is.na(meso) == F)
      ind.andean = which(is.na(meso))
      eval(parse(text = paste("meso.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.meso, 1:2], 1, which.max)", sep = "")))
      eval(parse(text = paste("andean.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.andean, 3:4], 1, which.max)+2", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1), ".best[ind.meso] = meso.high", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1), ".best[ind.andean] = andean.high", sep = "")))
    }
  }
  
  # Select variety with highest yields for every pixel, all params 
  yld_secano.2 <- yld_secano # make backup with varieties
  yld_riego.2 <- yld_riego
  yld_secano <- array(NA, dim = c(dim(crop_secano)[1], length(anos), (p-1), length(params))) # pixels x years x models x params
  yld_riego <- array(NA, dim = c(dim(crop_riego)[1], length(anos), (p-1), length(params)))
  for(k in 1:length(variedades)){
    yld_secano[wfd.s.best==k,,,] = yld_secano.2[wfd.s.best==k,,k,] # yld_secano[wfd.s.best==k,,,] = yld_secano.2[wfd.s.best==k,,,k,]
    yld_riego[wfd.r.best==k,,,] = yld_riego.2[wfd.r.best==k,,k,] # yld_riego[wfd.r.best==k,,,] = yld_riego.2[wfd.r.best==k,,,k,]
  }
  
  # #check average seasonal temperatures for WFD  (assumes tmax & tmin are params 3 & 4)
  # wfd.s.tmax = apply(yld_secano[,,10,5],1,mean,na.rm=T)
  # wfd.s.tmin = apply(yld_secano[,,10,6],1,mean,na.rm=T)
  # wfd.s.tavg = (wfd.s.tmax + wfd.s.tmin)/2
  # wfd.r.tmax = apply(yld_riego[,,10,5],1,mean,na.rm=T)
  # wfd.r.tmin = apply(yld_riego[,,10,6],1,mean,na.rm=T)
  # wfd.r.tavg = (wfd.r.tmax + wfd.r.tmin)/2
  # wfd.tavg = c(wfd.s.tavg,wfd.r.tavg)
  # areas = c(crop_secano$secano.area,crop_riego$riego.area)
  # ind.na = which(is.na(wfd.tavg))
  # if (length(ind.na)>0) {
  #   wfd.tavg = wfd.tavg[-ind.na]
  #   areas = areas[-ind.na]
  # }
  # sum(wfd.tavg * areas)/sum(areas)
  
  # Filter out bad pixels with excess crop failures
  # Contar # de NA y 0 en rendimiento
  riego.fallas <- apply(yld_riego[,,,1], c(1, 3), function(x) sum(is.na(x) | x == 0))
  secano.fallas <- apply(yld_secano[,,,1], c(1, 3), function(x) sum(is.na(x) | x == 0))
  
  # Filtrar pixeles con muchas fallas en WFD
  ind.bad.r <- which(riego.fallas[,10] >= 14) # half of total years
  ind.bad.s <- which(secano.fallas[,10] >= 14)
  if(length(ind.bad.r) > 0){
    yld_riego   <- yld_riego[-ind.bad.r,,,]
    crop_riego2 <- crop_riego # make backup of original
    crop_riego  <- crop_riego[-ind.bad.r,]
  }
  if(length(ind.bad.s) > 0){
    yld_secano <- yld_secano[-ind.bad.s,,,]
    crop_secano2 <- crop_secano # make backup of original
    crop_secano <- crop_secano[-ind.bad.s,]
  }
  
  # Filter out irrigated pixels with no irrigation at all in historical baseline
  test <- apply(yld_riego[,,10,3], 1, mean, na.rm = T)  #check multi-annual mean in baseline period of irrigation applied
  ind.rain <- which(test == 0)
  length(ind.rain)/dim(yld_riego)[1]*100  #check % of pixels, shouldn't be too high
  if(length(ind.rain) > 0){
    yld_riego <- yld_riego[-ind.rain,,,]
    crop_riego <- crop_riego[-ind.rain,]
  }
  
  # Identify locations of crop failures and reset ancillary variables to NA
  for(m in 1:10){
    
    ind.fail.s = which(yld_secano[,,m,1] == 0 | is.na(yld_secano[,,m,1]), arr.ind = T) # Indices of crop failures for a given model
    ind.fail.r = which(yld_riego[,,m,1] == 0 | is.na(yld_riego[,,m,1]), arr.ind = T)
    
    if(length(ind.fail.s) > 0){
      for(v in 2:length(params)){
        test <- yld_secano[,,m,v] # pull out matrix
        test[ind.fail.s] <- NA # reset values
        yld_secano[,,m,v] <- test # put it back
      }
    }
    
    if(length(ind.fail.r) > 0){
      for(v in 2:length(params)){
        test <- yld_riego[,,m,v] # pull out matrix
        test[ind.fail.r] <- NA # reset values
        yld_riego[,,m,v] <- test # put it back
      }
    }
  }
  
  # Set variable to graph here!!!
  lapply(1:length(params), function(z){
    
    var.g <- z  #var.g = 4 is yield/IRCM for irrigation productivity
    cat(paste("Processing parameter: ", params[var.g], "\n", sep = ""))
    # params = c(params,'YPIM')
    # params = c(params,'SRADT') # total accumulated radiation, SRADA*NDCH
    # params = c(params,'PRCP.ET') # precip to ET ratio (PRCP/ETCP)
    # params = c(params,'GRWT') # grain weight, i.e. HWAH/H.AM/10 (g/grain)
    # params = c(params,'PRCPA') # precip divided by duration
    
    # Switch to look only at non-crop failures for yields (Tables 5 & 6 in paper)
    yld.na <- F  #should leave at F by default
    
    # Calculate multi-annual means for var.g
    if(params[var.g] == 'YPIM'){
      wfd.r1 = yld_riego[,,(p-1),1]
      wfd.r1[wfd.r1 == 0] = NA # reset 0's to NA
      wfd.r1 = apply(wfd.r1, 1, mean, na.rm = T) # yield
      wfd.s1 = yld_secano[,,(p-1),1]
      wfd.s1[wfd.s1 == 0] = NA
      wfd.s1 = apply(wfd.s1, 1, mean, na.rm = T) # yield
      models.r1 = apply(yld_riego[,,1:(p-2),1], c(1, 3), mean, na.rm = T) # multi-annual means
      models.s1 = apply(yld_secano[,,1:(p-2),1], c(1, 3), mean, na.rm = T)
      
      wfd.r3 = apply(yld_riego[,,(p-1),3], 1, mean, na.rm = T) # irrigation
      wfd.s3 = apply(yld_secano[,,(p-1),3], 1, mean, na.rm = T)
      models.r3 = apply(yld_riego[,,1:(p-2),3], c(1, 3), mean, na.rm = T) # multi-annual means
      models.s3 = apply(yld_secano[,,1:(p-2),3], c(1, 3), mean, na.rm = T)
      
      models.r3[models.r3 == 0] <- 0.1 # reset 0 irrigation to 0.1 to avoid dividing by zero
      wfd.r = wfd.r1/wfd.r3  #calculate yield to irrigation ratio
      models.r = models.r1/models.r3
      wfd.s = wfd.s1/wfd.s3  #secano, doesn't matter, zero irrigation
      models.s = models.s1/models.s3
      
    } else {
      if(params[var.g] == 'SRADT') { # total intercepted radiation
        
        sradt.s = yld_secano[,,,4] * yld_secano[,,,5] # double-check these positions!
        sradt.r = yld_riego[,,,4] * yld_riego[,,,5]
        
        wfd.r = apply(sradt.r[,,10], 1, mean, na.rm = T) # multi-annual means
        wfd.s = apply(sradt.s[,,10], 1, mean, na.rm = T)
        models.r = apply(sradt.r[,,1:(p-2)], c(1, 3), mean, na.rm = T) # multi-annual means
        models.s = apply(sradt.s[,,1:(p-2)], c(1, 3), mean, na.rm = T)
        
      } else {
        if(params[var.g] == 'PRCP.ET'){
          pret.s = yld_secano[,,,4] / yld_secano[,,,2] * 100 # assumes correct position!
          pret.r = yld_riego[,,,4] / yld_riego[,,,2] * 100
          
          wfd.r = apply(pret.r[,,10], 1, mean, na.rm = T) # multi-annual means
          wfd.s = apply(pret.s[,,10], 1, mean, na.rm = T)
          models.r = apply(pret.r[,,1:(p-2)], c(1, 3), mean, na.rm = T) # multi-annual means
          models.s = apply(pret.s[,,1:(p-2)], c(1, 3), mean, na.rm = T)
          
        } else {
          if(params[var.g] == 'GRWT'){ # assumes correct position for HWAH & H.AM
            yld.s = yld_secano[,,,1]
            yld.s[yld.s == 0] = NA # put NA's for crop failures
            yld.r = yld_riego[,,,1]
            yld.r[yld.r == 0] = NA
            grwt.s = yld.s / yld_secano[,,,2]
            grwt.r = yld.r / yld_riego[,,,2]
            
            grwt.s[grwt.s == Inf] = NA # get rid of divide by zero errors (only happens with yield non-zero, but grain # zero)
            grwt.r[grwt.r == Inf] = NA
            
            wfd.r = apply(grwt.r[,,10], 1, mean, na.rm = T) # multi-annual means
            wfd.s = apply(grwt.s[,,10], 1, mean, na.rm = T)
            models.r = apply(grwt.r[,,1:(p-2)], c(1, 3), mean, na.rm = T) # multi-annual means
            models.s = apply(grwt.s[,,1:(p-2)], c(1, 3), mean, na.rm = T)
            
          } else {
            if(params[var.g] == 'PRCPA'){
              prcp.s = yld_secano[,,,4] / yld_secano[,,,2] # assumes correct position! (seasonal prcp/ duration)
              prcp.r = yld_riego[,,,4] / yld_riego[,,,2]  
              
              wfd.r = apply(prcp.r[,,10], 1, mean, na.rm = T) # multi-annual means
              wfd.s = apply(prcp.s[,,10], 1, mean, na.rm = T)
              models.r = apply(prcp.r[,,1:(p-2)], c(1, 3), mean, na.rm = T) # multi-annual means
              models.s = apply(prcp.s[,,1:(p-2)], c(1, 3), mean, na.rm = T)
              
            } else {
              if(params[var.g] == 'HWAH' & yld.na == T){
                wfd.r = yld_riego[,,(p-1),var.g] # extract data
                wfd.r[wfd.r == 0] = NA # reset 0's to NA (without touching original data)
                wfd.r = apply(wfd.r, 1, mean, na.rm = T)
                
                wfd.s = yld_secano[,,(p-1),var.g] # extract data
                wfd.s[wfd.s == 0] = NA # reset 0's to NA (without touching original data)
                wfd.s = apply(wfd.s, 1, mean, na.rm = T)
                
                models.r = yld_riego[,,1:(p-2),var.g]
                models.r[models.r == 0] = NA
                models.r = apply(models.r, c(1, 3), mean, na.rm = T)
                
                models.s = yld_secano[,,1:(p-2),var.g]
                models.s[models.s == 0] = NA
                models.s = apply(models.s, c(1, 3), mean, na.rm = T)
                
              } else {
                wfd.r = apply(yld_riego[,,(p-1),var.g], 1, mean, na.rm = T) # multi-annual means
                wfd.s = apply(yld_secano[,,(p-1),var.g], 1, mean, na.rm = T)
                models.r = apply(yld_riego[,,1:(p-2),var.g], c(1, 3), mean, na.rm = T) # multi-annual means
                models.s = apply(yld_secano[,,1:(p-2),var.g], c(1, 3), mean, na.rm = T)
              }
            }
          }
        }
      }
    }
    
    quantile(wfd.s, c(.05, .95), na.rm = T)
    quantile(wfd.r, c(.05, .95), na.rm = T)
    
    # Save out WFD values (modify name by yield type)
    if(yld.na == T & params[var.g] == 'HWAH'){
      save(wfd.r, wfd.s, file = paste("./resultados_graficas/", crops[i], "_WFD_", params[var.g], "_NA.Rdat", sep = ""))
    } else {
      if(yld.na == F & params[var.g] == 'HWAH'){
        save(wfd.r, wfd.s, file = paste("./resultados_graficas/", crops[i], "_WFD_", params[var.g], "_zeros.Rdat", sep = ""))
      } else {
        save(wfd.r, wfd.s, file = paste("./resultados_graficas/", crops[i], "_WFD_", params[var.g], ".Rdat", sep = ""))
      }
    }
    
    # Repeat wfd for 9 models
    wfd.r2 = replicate(9, wfd.r)
    wfd.s2 = replicate(9, wfd.s)
    
    # Calculate % change from each model to WFD
    if(params[var.g] == 'TMINA' | params[var.g] == 'TMAXA' | params[var.g] == 'Stress_water_all' | params[var.g] == 'Stress_water1' | params[var.g] == 'Stress_nitrogen_all' | params[var.g] == 'HIAM' | params[var.g] == 'PRCP.ET' | params[var.g] == 'IRCM'){
      
      pct.ch.r = (models.r-wfd.r2) # solo calcular diferencia
      pct.ch.s = (models.s-wfd.s2)
      
    } else {
      
      pct.ch.r = (models.r-wfd.r2) / (wfd.r2) * 100 # calcular % de cambio
      pct.ch.s = (models.s-wfd.s2) / (wfd.s2) * 100  
      pct.ch.r[pct.ch.r == Inf] = NA # reset divide by zero errors to NA (shouldn't be many!)
      pct.ch.s[pct.ch.s == Inf] = NA
      
    }
    
    quantile(c(pct.ch.r, pct.ch.s), c(.05, .95), na.rm = T)
    quantile(c(wfd.s, models.s), c(.05, .95), na.rm = T)
    quantile(c(wfd.r, models.r, wfd.s, models.s), c(.05, .95), na.rm = T)
    
    # Bootstrapping on percent change to get confidence intervals
    # have to bootstrap for every pixel?
    pct.ch.s2 = array(NA, dim = c(dim(crop_secano)[1], 3)) # mean plus CI
    for(j in 1:dim(crop_secano)[1]){
      boot = mat.or.vec(500, 1)
      for(b in 1:500){
        boot.ind = sample(1:(p-2), (p-2), replace = T)
        boot[b] = mean(pct.ch.s[j, boot.ind])
      }
      pct.ch.s2[j, 1:2] = quantile(boot, c(.05, .95), na.rm = T)
    }
    
    pct.ch.r2 = array(NA, dim = c(dim(crop_riego)[1], 3))
    for(j in 1:dim(crop_riego)[1]){
      boot = mat.or.vec(500, 1)
      for(b in 1:500){
        boot.ind = sample(1:(p-2), (p-2), replace = T)
        boot[b] = mean(pct.ch.r[j, boot.ind])
      }
      pct.ch.r2[j, 1:2] = quantile(boot, c(.05, .95), na.rm = T)
    }
    
    # Calculate multi-model means for % change and yields
    pct.ch.s2[,3] = apply(pct.ch.s, 1, mean, na.rm = T)
    pct.ch.r2[,3] = apply(pct.ch.r, 1, mean, na.rm = T)
    mmm.s = apply(models.s, 1, mean, na.rm = T)
    mmm.r = apply(models.r, 1, mean, na.rm = T)
    
    # Add another column to pct.ch putting NA for non-significant changes
    pct.ch.r2 = cbind(pct.ch.r2, pct.ch.r2[,3])
    ind.na.r = which(pct.ch.r2[,1] < 0 & pct.ch.r2[,2] > 0)
    pct.ch.r2[ind.na.r, 4] = NA
    
    pct.ch.s2 = cbind(pct.ch.s2, pct.ch.s2[,3])
    ind.na.s = which(pct.ch.s2[,1] < 0 & pct.ch.s2[,2] > 0)
    pct.ch.s2[ind.na.s, 4] = NA
    
    # Add column for categorical changes
    pct.ch.r2 = cbind(pct.ch.r2, NA)
    pct.ch.r2[pct.ch.r2[,4] > 0, 5] = 1 # sig increases
    pct.ch.r2[pct.ch.r2[,4] < 0, 5] = -1 # sig decreases
    pct.ch.r2[is.na(pct.ch.r2[,4]), 5] = NA
    
    pct.ch.s2 = cbind(pct.ch.s2, NA)
    pct.ch.s2[pct.ch.s2[,4] > 0, 5] = 1 # sig increases
    pct.ch.s2[pct.ch.s2[,4] < 0, 5] = -1 # sig decreases
    pct.ch.s2[is.na(pct.ch.s2[,4]), 5] = NA
    
    # Look at range of sig.ch values
    quantile(c(pct.ch.s2[,4], pct.ch.r2[,4]), c(.05, .95), na.rm = T)
    quantile(c(pct.ch.s2[,4]), c(.05, .95), na.rm = T)
    quantile(c(pct.ch.r2[,4]), c(.05, .95), na.rm = T)
    
    # Look at mean weighted by harvested area
    sum(pct.ch.s2[,4] * crop_secano$secano.area, na.rm = T) / sum(crop_secano$secano.area[is.na(pct.ch.s2[,4]) == F])
    sum(pct.ch.r2[,4] * crop_riego$riego.area, na.rm = T) / sum(crop_riego$riego.area[is.na(pct.ch.r2[,4]) == F])
    
    # Add column names & reorder columns
    pct.ch.r2 = cbind(pct.ch.r2[,3:4], pct.ch.r2[,1:2], pct.ch.r2[,5]) # re-order columns
    pct.ch.s2 = cbind(pct.ch.s2[,3:4], pct.ch.s2[,1:2], pct.ch.s2[,5])
    pct.ch.r2 = cbind(pct.ch.r2, crop_riego$riego.area, crop_riego$x, crop_riego$y) # add pixel-wise areas
    pct.ch.s2 = cbind(pct.ch.s2, crop_secano$secano.area, crop_secano$x, crop_secano$y)
    
    pct.ch_cols = c('PctCh.MMM', 'SigCh.MMM', 'PctCh.lower', 'PctCh.upper', 'Categorical', 'Area', 'Lon', 'Lat')
    colnames(pct.ch.s2) = pct.ch_cols
    colnames(pct.ch.r2) = pct.ch_cols
    models = rbind(models, 'PctCh.MMM', 'SigCh.MMM', 'PctCh.lower', 'PctCh.upper', 'Categorical') # concatenate for plotting purposes
    
    # Save vectors to compare across crops (modify name by yield type)
    if(yld.na == T & params[var.g] == 'HWAH'){
      
      save(pct.ch.r2, file = paste("./resultados_graficas/", crops[i], "_riego_pctCh_", params[var.g], "_NA.Rdat", sep = ""))
      save(pct.ch.s2, file = paste("./resultados_graficas/", crops[i], "_secano_pctCh_", params[var.g], "_NA.Rdat", sep = ""))
      
    } else {
      
      if(yld.na == F & params[var.g] == 'HWAH'){
        
        save(pct.ch.r2, file = paste("./resultados_graficas/", crops[i], "_riego_pctCh_", params[var.g], "_zeros.Rdat", sep = ""))
        save(pct.ch.s2, file = paste("./resultados_graficas/", crops[i], "_secano_pctCh_", params[var.g], "_zeros.Rdat", sep = ""))
        
      } else {
        
        save(pct.ch.r2, file = paste("./resultados_graficas/", crops[i], "_riego_pctCh_", params[var.g], ".Rdat", sep = ""))
        save(pct.ch.s2, file = paste("./resultados_graficas/", crops[i], "_secano_pctCh_", params[var.g], ".Rdat", sep = ""))
        
      }
      
    }
    
    for(t in 1:2){ # loop through riego (t=1)/ secano (t=2)
      
      print(t)
      
      for(m in c(p+1)){ # (p-1):(p+1):  WFD, multi-model mean y % cambio (4 tipos)
        
        # multi-annual means per model
        if(m <= (p-1)){
          
          eval(parse(text = paste('promedio = apply(yld_', treat[t], '[,,m,var.g], 1, mean, na.rm = T)', sep = ''))) # average in zeros??
          
        } else {
          
          if(m == p){ # multi-model mean
            
            eval(parse(text = paste('promedio = mmm.', substr(treat[t], 1, 1), sep = '')))
            
          } else {
            
            if(m >= (p+1)){ # % change
              
              eval(parse(text = paste('promedio = pct.ch.', substr(treat[t], 1, 1), '2[,m-p]', sep = ''))) # change column with loop
            }
          }
        }
        
        # set limits, color scale and labels
        if(m <= p){ # for models + WFD
          # promedio[promedio==0] = NA  #set 0 values to NA
          # default values
          limits2 = quantile(c(yld_riego[,,,var.g], yld_secano[,,,var.g]), c(.05, .95), na.rm = T) # limites flexibles segun el cultivo
          labs2 = ''
          color_scale = brewer.pal(9, 'YlGn')
          
          if(params[var.g] == 'HWAH'){
            
            promedio = promedio/1000 # convert from kg/ha to t/ha
            labs2 = '      t/ha'
            limits2 = quantile(c(yld_riego[,,,var.g]/1000, yld_secano[,,,var.g]/1000), c(.05, .95), na.rm = T) # reset limits
            
            if(c == 1 | c == 2 | c == 5){
              
              limits2 = c(0.9, 10.1)
              
            } else {
              
              limits2 = c(0.7, 5)
              
            } # limites fijados
          }
          if(params[var.g] == 'Stress_nitrogen_all'){
            
            limits2 = c(0, 0.45) # stress indices
            
          } 
          if(params[var.g] == 'Stress_water_all'){
            
            limits2 = c(0, 0.25) # stress indices, 0.305
            labs2 = ''
            
          }
          if(params[var.g] == 'NDCH'){
            
            limits2 = c(75, 175) # duration
            labs2 = '    Days'
            
          }
          if(params[var.g] == 'IRCM'){
            
            limits2 = quantile(c(yld_riego[,,,var.g]), c(.05, .95), na.rm = T)
            limits2 = c(0, 300)
            labs2 = '    Mm'
            
          }
          if(params[var.g] == 'Stress_water_all' | params[var.g] == 'Stress_nitrogen_all'){
            # color_scale = colorRampPalette(c('forestgreen','gold2','red'), space="rgb")(25)
            color_scale = brewer.pal(9, 'YlOrBr')
          }
          if(params[var.g] == 'IRCM'){
            # color_scale = colorRampPalette(c('yellow','green','forestgreen','blue'), space="rgb")(25)
            color_scale =  brewer.pal(9, 'GnBu')
          }
          
        } else { # categorical plots
          if(m == 16){
            
            limits2 = c(-1, 1)
            color_scale = brewer.pal(11, 'RdYlGn') # fix this!
            labs2 = ''
            
          } else { # percent change plots
            dat = c(pct.ch.r, pct.ch.s)
            #limits2 = quantile(dat[dat<0],c(.025),na.rm=T)  
            limits2 = max(abs(quantile(dat,c(.05, .95), na.rm = T)))
            limits2 = sort(c(limits2, -1*limits2)) # set positive equal to opposite of negative
            #color_scale = colorRampPalette(c('red','orangered','orange','lightyellow','lightgreen','green','forestgreen'), space="rgb")(15)
            color_scale = brewer.pal(11, 'RdYlGn')
            labs2 = '      %'
            
            if(params[var.g] == 'TMAXA' | params[var.g] == 'TMINA'){
              
              limits2 = c(0, 3.5) # temperatura
              color_scale = colorRampPalette(c('white', 'orange', 'red'), space = "rgb")(15)
              
            }
            if(params[var.g] == 'HWAH' | params[var.g] == 'YPEM' | params[var.g] == 'SRADT'){
              
              limits2 = c(-50, 50) # rend y WUE
              
            }
            if(params[var.g] == 'YPIM'){
              
              limits2 = c(-100, 100) # rend y WUE
              
            }
            if(params[var.g] == 'NDCH'){
              
              limits2 = c(-12, 12) # duracion
              
            }
            if(params[var.g] == 'Stress_water_all' | params[var.g] == 'Stress_nitrogen_all' || params[var.g] == 'Stress_water1'){
              
              limits2 = c(-0.08, 0.08) # stress indices
              # color_scale = colorRampPalette(c('forestgreen','green','lightgreen','lightyellow','orange','orangered','red'), space="rgb")(15)
              color_scale = rev(brewer.pal(11, 'BrBG'))
              labs2 = ''
            }
            if(params[var.g] == 'SRADA'){
              
              limits2 = c(-30, 30)
              
            }
            if(params[var.g] == 'EPCM'){
              
              limits2 = c(-25, 25)
              
            } 
            if(params[var.g] == 'H.AM'){
              
              limits2 = c(-30, 30)
              
            }
            if(params[var.g] == 'GRWT'){
              
              limits2 = c(-30, 30)
              
            }
            if(params[var.g] == 'IRCM'){
              dat = pct.ch.r
              limits2 = max(abs(quantile(dat, c(.1, .9), na.rm = T)))
              print(round(quantile(dat, c(.1, .9), na.rm = T), 2))
              limits2 = sort(c(limits2, -1 * limits2))
              limits2 = c(-100, 100)
              labs2 = '    Mm'
              # color_scale = colorRampPalette(c('red','orangered','orange','lightyellow','green','forestgreen','blue'), space="rgb")(25)
              color_scale = brewer.pal(9, 'RdYlBu')
            }
            #limits2 = c(-25,25)  #hard-code limits instead
          }
        }
        
        # get data ready for plotting
        promedio[promedio < limits2[1]] = limits2[1] # reset end points of data to plotting limits
        promedio[promedio > limits2[2]] = limits2[2]
        eval(parse(text = paste("df = data.frame(Long = crop_", treat[t], "[,1], Lat = crop_", treat[t], "[,2], yield = promedio)", sep = '')))
        
        #create ggplot & save
        y = ggplot() +
          geom_polygon(data = Map_LatinAmerica1, aes(x = long, y = lat, group = group), colour = "gray80", fill = "white" ) + # fill = "gray70"
          geom_path(data = Map_LatinAmerica1, aes(x = long, y = lat, group = group), colour = "black", size = 0.25) +
          coord_equal() +
          geom_raster(data = df, aes(x = Long, y = Lat, fill = yield)) +
          ggtitle(paste(capitalize(cultivos.en[i]), ' (', treat.en[t], '): \n', models[m,], sep = '')) +
          scale_fill_gradientn(colours = color_scale, limits = limits2, na.value = "gray85") + # guide='none' turns off legend,lightskyblue, limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
          theme_bw() +
          labs(fill = labs2) +
          theme(panel.grid.major.x = element_blank(),
                panel.grid.minor.x = element_blank(),
                panel.grid.major.y = element_blank(),
                panel.grid.minor.y = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                legend.text = element_text(size = 16),
                legend.title = element_text(face = "bold", size = 18),
                legend.background = element_blank(),
                legend.key = element_blank(),
                legend.key.size = unit(1, 'cm'),
                legend.justification = c(0, 0),
                legend.position = c(0.2, 0.2), # baseline yields only!
                plot.title = element_blank(), # element_text(face="bold", size=18)  #this turns on or off plot title
                panel.border = element_blank(),
                axis.ticks = element_blank(),
                plot.margin = unit(c(0, 0, 0, 0), "mm")
          )
        #plot(y)
        ggsave(filename = paste("./resultados_graficas/", crops[i], "_", treat[t], "_bestVariety_", models[m,], "_", params[var.g], ".png", sep = ""), plot = y, width = 5, height = 5, dpi = 400, scale = 1.5)
      }
    }
    
  })
  
  return("Done!\n")
  
})
