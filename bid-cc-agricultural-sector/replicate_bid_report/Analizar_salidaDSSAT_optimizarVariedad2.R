# Visualize DSSAT runs
# limpiar workspace
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

crops <- c('Maize', 'Rice', 'Soybean', 'Bean', 'Wheat')

lapply(1:length(crops), function(i){
  
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
  
  # set graphing limits
  xlim.c = c(-116, -35)
  ylim.c = c(-54, 32)
  cex.c = 0.5
  
  # get list of climate models
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
      
      print(m)
      
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
      
      # unlist everything into matrices
      secano = array(NA, dim = c(length(Run.secano), 30)) # initialize arrays
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
  
  # check data points in each year
  # apply(yld_riego[,,10,3], 2, function(x) sum(is.na(x) == F))
  # apply(yld_riego.2[,,5,1],2,function(x) sum(is.na(x)|x==0))
  # apply(yld_secano[,,10,1], 2, function(x) sum(is.na(x) == F))
  
  # Descartar los a�os sin datos clim�ticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones t�cnicas!
  yld_secano = yld_secano[,anos,,]  #incluir los indices de los a�os "buenos" aqu� con datos por todos los pixeles
  yld_riego = yld_riego[,anos,,]
  yld_secano[yld_secano == -99] = 0  #re-emplazar -99 con 0 for legitimate crop failures
  yld_riego[yld_riego == -99] = 0
  
  # Identify best variety in historical baseline (higher mean yields & less crop failures)
  # across all 3 varieties
  wfd.r = apply(yld_riego[,,(p-1),], c(1, 3), mean, na.rm = T)  #multi-annual means
  wfd.s = apply(yld_secano[,,(p-1),], c(1, 3), mean, na.rm = T)
  thresh = mean(yld_secano[,,(p-1),], na.rm = T) * 0.2  #define crop failure as 20% of mean rainfed yield
  wfd.fail.r = apply(yld_riego[,,(p-1),], c(1, 3), function(x) sum(x < thresh, na.rm = T))  #multi-annual means
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
            wfd.s.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
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
            wfd.r.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
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
            wfd.s.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
          }
        }
      }
      
      wfd.r.best = mat.or.vec(dim(wfd.r)[1], 1)
      for (j in 1:dim(wfd.r)[1])  {
        sortYield = sort.list(wfd.r[j,], dec = T) 
        if (wfd.fail.r[j,sortYield[1]] <= 4) {wfd.r.best[j] = sortYield[1]}  else{
          if (wfd.fail.r[j,sortYield[2]] <= 4) {wfd.r.best[j] = sortYield[2]}  else{
            wfd.r.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
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
                        wfd.s.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
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
                        wfd.r.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
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
                        wfd.s.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
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
                        wfd.r.best[j] = sortYield[1] # if all varieties have more than 4 crop failures, use highest-yielding
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
      eval(parse(text = paste("meso = match(crop_", treat[t], "$country,habit$X[habit$Mesoamerican == 1])", sep = "")))
      ind.meso = which(is.na(meso) == F)
      ind.andean = which(is.na(meso))
      eval(parse(text = paste("meso.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.meso, 1:2], 1, which.max)", sep = "")))
      eval(parse(text = paste("andean.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.andean, 3:4], 1, which.max)+2", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1), ".best[ind.meso] = meso.high", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1), ".best[ind.andean] = andean.high", sep = "")))
    }
  }
  
  par(mai = c(0.5, 0.5, 0.5, 0.5))
  par(ask = F)
  plot(Map_LatinAmerica)
  points(crop_riego$x[wfd.r.best == 1], crop_riego$y[wfd.r.best == 1], pch = 20, cex = 0.5, col = 'blue')
  points(crop_riego$x[wfd.r.best == 2], crop_riego$y[wfd.r.best == 2], pch = 20, cex = 0.5, col = 'green')
  points(crop_riego$x[wfd.r.best == 3], crop_riego$y[wfd.r.best == 3], pch = 20, cex = 0.5, col = 'red')
  points(crop_riego$x[wfd.r.best == 4], crop_riego$y[wfd.r.best == 4], pch = 20, cex = 0.5, col = 'orange')
  #legend(-115,-20,variedades,col=c('blue','green','red','orange'),pch=20,cex=0.8)
  #title('Riego')
  
  plot(Map_LatinAmerica)
  points(crop_secano$x[wfd.s.best == 1], crop_secano$y[wfd.s.best == 1], pch = 20, cex = 0.5, col = 'blue')
  points(crop_secano$x[wfd.s.best == 2], crop_secano$y[wfd.s.best == 2], pch = 20, cex = 0.5, col = 'green')
  points(crop_secano$x[wfd.s.best == 3], crop_secano$y[wfd.s.best == 3], pch = 20, cex = 0.5, col = 'red')
  points(crop_secano$x[wfd.s.best == 4], crop_secano$y[wfd.s.best == 4], pch = 20, cex = 0.5, col = 'orange')
  #legend(-105,-20,variedades,col=c('blue','green','red'),pch=20,cex=0.75)
  legend(-115, -20, variedades, col = c('blue', 'green', 'red', 'orange'), pch = 20, cex = 0.9)
  #title('Secano')
  
  # Select yield for best variety for every pixel 
  yld_secano.2 = yld_secano  #make backup with varieties
  yld_riego.2 = yld_riego
  yld_secano = array(NA, dim = c(dim(crop_secano)[1], length(anos), (p-1)))  #pixels x years x models 
  yld_riego = array(NA, dim = c(dim(crop_riego)[1], length(anos), (p-1)))
  for(k in 1:length(variedades)){
    yld_secano[wfd.s.best == k,,] = yld_secano.2[wfd.s.best == k,,,k]
    yld_riego[wfd.r.best == k,,] = yld_riego.2[wfd.r.best == k,,,k]
  }
  
  # Filter out bad pixels with excess crop failures
  # contar # de NA y 0 en rendimiento
  riego.fallas = apply(yld_riego, c(1, 3), function(x) sum(is.na(x) | x == 0))
  secano.fallas = apply(yld_secano, c(1, 3), function(x) sum(is.na(x) | x == 0))
  
  # filtrar pixeles con muchas fallas en WFD
  ind.bad.r = which(riego.fallas[,10] >= 14)  #half of total years
  ind.bad.s = which(secano.fallas[,10] >= 14)
  if(length(ind.bad.r) > 0){yld_riego = yld_riego[-ind.bad.r,,]
  crop_riego2 = crop_riego  #make backup of original
  crop_riego = crop_riego[-ind.bad.r,]}
  if(length(ind.bad.s) > 0){yld_secano = yld_secano[-ind.bad.s,,]
  crop_secano2 = crop_secano  #make backup of original
  crop_secano = crop_secano[-ind.bad.s,]}
  
  ## Algorithm to get confidence intervals on pixel-level % changes:
  # Calculate multi-annual means for each model
  # Calculate % change betwen each GCM and WFD for multi-annual means
  # Bootstrapping on these 10 values per pixel to get 2.5 & 97.5 confidence intervals
  # Calculate multi-model mean on % changes for comparison to CI
  
  # Calculate multi-model mean, % changes, and changes relative to sd
  wfd.r = apply(yld_riego[,,(p-1)], 1, mean, na.rm = T) # multi-annual means
  wfd.s = apply(yld_secano[,,(p-1)], 1, mean, na.rm = T)
  models.r = apply(yld_riego[,,1:(p-2)], c(1, 3), mean, na.rm = T) # multi-annual means
  models.s = apply(yld_secano[,,1:(p-2)], c(1, 3), mean, na.rm = T)
  
  # repeat wfd for 9 models
  wfd.r2 = replicate(9, wfd.r)
  wfd.s2 = replicate(9, wfd.s)
  
  # calculate % change from each model to WFD
  pct.ch.r = (models.r - wfd.r2)/wfd.r2 * 100
  pct.ch.r[which(wfd.r2 == 0)] = NA # set pixels with 0 yield in WFD to NA
  pct.ch.s = (models.s - wfd.s2)/wfd.s2 * 100
  pct.ch.s[which(wfd.s2 == 0)] = NA # set pixels with 0 yield in WFD to NA
  
  # Bootstrapping on percent change to get confidence intervals
  # have to bootstrap for every pixel?
  pct.ch.s2 = array(NA, dim = c(dim(crop_secano)[1], 5))  #two sets of CI plus mean
  for(j in 1:dim(crop_secano)[1]){
    boot = mat.or.vec(500, 1)
    for(b in 1:500){
      boot.ind = sample(1:(p-2), (p-2), replace = T)
      boot[b] = mean(pct.ch.s[j, boot.ind])
    }
    pct.ch.s2[j, 1:2] = quantile(boot, c(.1, .9), na.rm = T)
    pct.ch.s2[j, 3:4] = quantile(boot, c(.025, .975), na.rm = T)
  }
  
  pct.ch.r2 = array(NA, dim = c(dim(crop_riego)[1], 5))
  for(j in 1:dim(crop_riego)[1]){
    boot = mat.or.vec(500, 1)
    for(b in 1:500){
      boot.ind = sample(1:(p-2), (p-2), replace = T)
      boot[b] = mean(pct.ch.r[j, boot.ind])
    }
    pct.ch.r2[j, 1:2] = quantile(boot, c(.1, .9), na.rm = T)
    pct.ch.r2[j, 3:4] = quantile(boot, c(.025, .975), na.rm = T)
  }
  
  # calculate multi-model means for % change and yields
  pct.ch.s2[,5] = apply(pct.ch.s, 1, mean, na.rm = T)
  pct.ch.r2[,5] = apply(pct.ch.r, 1, mean, na.rm = T)
  mmm.s = apply(models.s, 1, mean, na.rm = T)
  mmm.r = apply(models.r, 1, mean, na.rm = T)
  
  # add another column to pct.ch putting -99999 for non-significant changes
  pct.ch.r2 = cbind(pct.ch.r2, pct.ch.r2[,5])
  ind.na.r = which(pct.ch.r2[,3] < 0 & pct.ch.r2[,4] > 0)
  pct.ch.r2[ind.na.r, 6] = 0
  
  pct.ch.s2 = cbind(pct.ch.s2, pct.ch.s2[,5])
  ind.na.s = which(pct.ch.s2[,3] < 0 & pct.ch.s2[,4] > 0)
  pct.ch.s2[ind.na.s, 6] = 0
  
  # Add column names & reorder columns
  pct.ch.r2 = cbind(pct.ch.r2[,5:6], pct.ch.r2[,1:4])  #re-order columns
  pct.ch.s2 = cbind(pct.ch.s2[,5:6], pct.ch.s2[,1:4])
  pct.ch_cols = c('PctCh.MMM', 'SigCh.MMM', 'PctCh.10.lower', 'PctCh.90.upper', 'PctCh.2.5.lower', 'PctCh.97.5.upper')
  colnames(pct.ch.s2) = pct.ch_cols
  colnames(pct.ch.r2) = pct.ch_cols
  models = rbind(models, 'PctCh.MMM', 'SigCh.MMM', 'PctCh.10.lower', 'PctCh.90.upper', 'PctCh.2.5.lower', 'PctCh.97.5.upper')  #concatenate for plotting purposes
  
  for(t in 1:2){ # loop through riego (t=1)/ secano (t=2)
    
    print(t)
    
    for(m in c((p+1))){ # (p-1):(p+1),(p+3):(p+4)  WFD, multi-model mean y % cambio (4 tipos)
      
      # multi-annual means per model
      if(m <= (p-1)){
        eval(parse(text = paste('promedio = apply(yld_', treat[t], '[,,m], 1, mean, na.rm = T)', sep = ''))) # average in zeros??
      } else {
        if(m == p){ # multi-model mean
          eval(parse(text = paste('promedio = mmm.', substr(treat[t], 1, 1), sep = '')))
        } else {
          if(m >= (p+1)){ #% change
            eval(parse(text = paste('promedio = pct.ch.', substr(treat[t], 1, 1), '2[,m-p]', sep = '')))  #change column with loop
          }
        }
      }
      
      if(m <= p){ # for models + WFD
        promedio[promedio == 0] = NA # set 0 values to NA
        # These limits should be calculated on wfd.s/r & mmm.s/r
        # limits2 = quantile(c(wfd.s,wfd.r,mmm.s,mmm.r),c(.025,.975),na.rm=T)  #limites flexibles segun el cultivo
        limits2 = quantile(c(wfd.s, wfd.r, mmm.s, mmm.r), c(.05, .95), na.rm = T)
        limits2 = c(0, 12000)  #limites fijados
        promedio[which(promedio < limits2[1] & promedio > 0)] = limits2[1]  #reset end points of data
        promedio[which(promedio > limits2[2])] = limits2[2]
        
        eval(parse(text = paste("df = data.frame(Long = crop_", treat[t], "[,1], Lat = crop_", treat[t], "[,2], yield = promedio)", sep = '')))
        color_scale = colorRampPalette(c('red', 'gold2', 'forestgreen'), space = "rgb")(25)
        labs2 = 'Yield \n(Kg/ha)'
      } else { # for % change plots
        if(m >= (p+1)){
          dat = c(pct.ch.r2[,1], pct.ch.s2[,1])
        }
        limits2 = quantile(dat[dat < 0], c(.025), na.rm = T)  
        limits2 = c(limits2, -1*limits2) # set positive equal to opposite of negative
        limits2 = c(-50, 50) # hard-code limits instead
        promedio[promedio == 'NaN'] = NA
        promedio[promedio < limits2[1]] = limits2[1] # reset end points
        promedio[promedio > limits2[2]] = limits2[2]
        eval(parse(text = paste("df = data.frame(Long = crop_", treat[t], "[,1], Lat = crop_", treat[t], "[,2], yield = promedio)", sep = '')))
        color_scale = colorRampPalette(c('red', 'orangered', 'orange', 'white', 'lightgreen', 'green', 'forestgreen'), space = "rgb")(20)
        labs2 = ''
      }
      
      y = ggplot() +
        geom_polygon(data=Map_LatinAmerica1, aes(x = long, y = lat, group = group), colour = "gray90", fill = "gray70") +
        geom_path(data = Map_LatinAmerica1, aes(x = long, y = lat, group = group), colour = "black", size = 0.25) +
        coord_equal() +
        geom_raster(data = df, aes(x = Long, y = Lat, fill = yield)) +
        ggtitle(paste(capitalize(cultivos.en[i]), ' (', treat.en[t], '): \n', models[m,], sep = '')) +
        scale_fill_gradientn(colours = color_scale, limits = limits2, na.value = "blue") + # limits ,breaks=as.vector(limits),labels=as.vector(limits),limits=as.vector(limits)
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
              legend.text = element_text(size = 14),
              legend.title = element_text(face = "bold", size = 14),
              legend.background = element_blank(),
              legend.key = element_blank(),
              plot.title = element_text(face = "bold", size = 18),
              panel.border = element_blank(),
              axis.ticks = element_blank())
      #plot(y)
      if(!dir.exists("./resultados_graficas")){
        dir.create(path = "./resultados_graficas", recursive = T)
      }
      ggsave(filename = paste("./resultados_graficas/", crops[i], "_", treat[t], "_bestVariety_", models[m,], ".png", sep = ""), plot = y, width = 5, height = 5, dpi = 400, scale = 1.5)  #_documentos/graficas
    }
  }
  
  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% %
  
  # check for 0's (or low yields)
  riego_zero = array(NA, dim = c(length(anos), (p-1)))
  secano_zero = array(NA, dim = c(length(anos), (p-1)))
  # riego_na = array(NA,dim=c(30,(p-1)))
  # secano_na = array(NA,dim=c(30,(p-1)))
  thresh = 0 # median(yld_secano,na.rm=T)*.01  #1% of median yield
  for(m in 1:(p-1)){
    for(j in anos){
      riego_zero[j, m] = sum(yld_riego[,j,m] <= thresh | is.na(yld_riego[,j,m]))  #number of pixels
      secano_zero[j, m] = sum(yld_secano[,j,m] <= thresh | is.na(yld_secano[,j,m]))  
      #     riego_na[j,m] = sum(is.na(yld_riego[,j,m])==F)
      #     secano_na[j,m] = sum(is.na(yld_secano[,j,m])==F)
    }
  }
  riego_zero2 = apply(riego_zero, 2, mean, na.rm = T)/dim(yld_riego)[1] * 100
  secano_zero2 = apply(secano_zero, 2, mean, na.rm = T)/dim(yld_secano)[1] * 100
  # riego_zero2 = apply(riego_zero/riego_na*100,2,mean,na.rm=T)   #dim(yld_riego)[1]*100
  # secano_zero2 = apply(secano_zero/secano_na*100,2,mean,na.rm=T)
  names(riego_zero2) = models$Models[1:(p-1)]
  names(secano_zero2) = models$Models[1:(p-1)]
  
  # mirar tasa de fallas
  round(riego_zero2, 2)
  round(secano_zero2, 2)
  
  # barplot of % failures in future vs. WFD
  par(mai = c(1.25, 1.25, 0.5, 0.5))  #fijar los margenes de la gr�fica
  par(ask = F)  #preguntar antes de seguir a la pr�xima gr�fica en un loop
  #par(ask=F)
  for(t in 1:2){ # riego, secano
    eval(parse(text = paste("barplot(", treat[t], "_zero2[1:(p-2)], xaxt = 'n', ylim = c(0, 35), ylab = '% Crop Failures')", sep = '')))
    eval(parse(text = paste("abline(h = ", treat[t], "_zero2[(p-1)], col = 'red', lty = 2, lwd = 2)", sep = '')))
    eval(parse(text = paste("abline(h = mean(", treat[t], "_zero2[1:(p-2)], na.rm = T), lty = 2, lwd = 2.5)", sep = '')))
    title(paste(cultivos.en[i], ' (', treat.en[t], ')', sep = ''))
    text(x = seq(1, p, 1.2), y = -1, cex = 1, labels = models$Models[1:(p-2)], xpd = TRUE, srt = 40, pos = 2)
    legend('topright', c('GCMs - future', 'Baseline period', 'Multi-model mean'), bty = 'n', col = c('grey', 'red', 'black'), pch = c(15, -1, -1), lty = c(0, 2, 2), lwd = c(0, 2, 2.5))
  }
  
  # country-scale aggregation (combining riego & secano)
  # get list of countries
  countries = as.character(na.omit(unique(c(as.character(crop_riego$country), as.character(crop_secano$country)))))
  
  # Calculate weighted averages by model & compare to WFD
  area.riego = replicate(length(anos), crop_riego$riego.area)
  area.secano = replicate(length(anos), crop_secano$secano.area)
  
  # find areas where WFD consistently failing and discard these from aggregation??
  zeros.wfd.r = apply(yld_riego[,,(p-1)], 1, function(x) sum(x == 0, na.rm = T))
  zeros.wfd.s = apply(yld_secano[,,(p-1)], 1, function(x) sum(x == 0, na.rm = T))
  ## MIGHT WANT TO IMPLEMENT THIS, OR TRY WITH & WITHOUT?
  
  # Aggregate to country scale for WFD in each model & year (across riego & secano)
  yld.country = array(NA, dim = c(length(anos), length(countries))) # initialize matrices
  colnames(yld.country) = countries
  rownames(yld.country) = 1971:1998    
  for(r in 1:length(countries)){ # loop by country
    
    ind.reg.r = which(crop_riego$country == countries[r])
    ind.reg.s = which(crop_secano$country == countries[r])
    
    if(length(c(ind.reg.r, ind.reg.s)) > 1){
      for(y in 1:length(anos)){ # loop through years
        ylds.y = c(yld_riego[ind.reg.r,y,10], yld_secano[ind.reg.s,y,10])
        #ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0 for legitimate crop failures
        areas.y = c(area.riego[ind.reg.r,y], area.secano[ind.reg.s,y])
        yld.country[y, r] = sum(ylds.y * areas.y)/sum(areas.y)    
      }
    }
  }
  
  # Save results to csv
  eval(parse(text = paste("write.csv(yld.country, file = './FAOSTAT_validation/country.ylds_", crops[i], "_optVariety.csv')", sep = "")))
  
  # AGGREGATE RESULTS TO FPU SCALE
  # get list of FPU's
  FPUs = unique(c(as.character(crop_riego$New_FPU), as.character(crop_secano$New_FPU)))
  
  # Calculate weighted averages by model & compare to WFD
  area.riego = replicate(length(anos), crop_riego$riego.area)
  area.secano = replicate(length(anos), crop_secano$secano.area)
  
  # find areas where WFD consistently failing and discard these from aggregation
  zeros.wfd.r = apply(yld_riego[,,(p-1)], 1, function(x) sum(x == 0, na.rm = T))
  zeros.wfd.s = apply(yld_secano[,,(p-1)], 1, function(x) sum(x == 0, na.rm = T))
  
  # Aggregate to FPU scale for each model & year
  yld.FPU.r = array(NA, dim = c(p-1, length(anos), length(FPUs))) # initialize matrices
  yld.FPU.s = array(NA, dim = c(p-1, length(anos), length(FPUs)))
  
  # aggregation
  for(t in 1:2){ # loop by treatment
    
    for(r in 1:length(FPUs)){ # then by FPU
      
      eval(parse(text = paste("ind.reg = which(crop_", treat[t], "$New_FPU == FPUs[r] & zeros.wfd.", substr(treat[t], 1, 1), " <= 14)", sep = '')))
      
      if(length(ind.reg) > 1){
        for(j in 1:(p-1)){ # loop through models
          for(y in (anos-1)){ # loop through years (fix anos - 1!)
            eval(parse(text = paste("ylds.y = yld_", treat[t], "[ind.reg,y,j]", sep = ''))) 
            # ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0 for legitimate crop failures
            eval(parse(text = paste("areas.y = area.", treat[t], "[ind.reg,y]", sep = ''))) 
            eval(parse(text = paste("yld.FPU.", substr(treat[t], 1, 1), "[j,y,r] = sum(ylds.y * areas.y)/sum(areas.y)", sep = '')))    
          }
        }
      }
    }
  }
  
  # Calculate interannual means & CV's
  yld.FPU.mean.r = apply(yld.FPU.r, c(1, 3), function(x) mean(x, na.rm = T))
  yld.FPU.mean.s = apply(yld.FPU.s, c(1, 3), function(x) mean(x, na.rm = T))
  yld.FPU.CV.r = apply(yld.FPU.r, c(1, 3), function(x) sd(x, na.rm = T)/mean(x, na.rm = T) * 100)
  yld.FPU.CV.s = apply(yld.FPU.s, c(1, 3), function(x) sd(x, na.rm = T)/mean(x, na.rm = T) * 100)
  
  # calculate changes
  WFD.FPU.mean.r = t(replicate(9, yld.FPU.mean.r[10,])) # repeat WFD for 10 models
  WFD.FPU.mean.s = t(replicate(9, yld.FPU.mean.s[10,]))
  WFD.FPU.CV.r = t(replicate(9, yld.FPU.CV.r[10,]))
  WFD.FPU.CV.s = t(replicate(9, yld.FPU.CV.s[10,]))
  FPU.pctCh.r = (yld.FPU.mean.r[1:9,] - WFD.FPU.mean.r)/WFD.FPU.mean.r * 100
  FPU.pctCh.s = (yld.FPU.mean.s[1:9,] - WFD.FPU.mean.s)/WFD.FPU.mean.s * 100
  FPU.CVCh.r = (yld.FPU.CV.r[1:9,] - WFD.FPU.CV.r)
  FPU.CVCh.s = (yld.FPU.CV.s[1:9,] - WFD.FPU.CV.s)
  
  # multi-model means
  FPU.pctCh.mmm.r = apply(FPU.pctCh.r, 2, mean, na.rm = T) # remove missing models
  FPU.pctCh.mmm.s = apply(FPU.pctCh.s, 2, mean, na.rm = T)
  FPU.CVCh.mmm.r = apply(FPU.CVCh.r, 2, mean, na.rm = T)
  FPU.CVCh.mmm.s = apply(FPU.CVCh.s, 2, mean, na.rm = T)
  
  # Plot mean yield changes vs. baseline CV
  plot(yld.FPU.CV.r[10,], FPU.pctCh.mmm.r, xlab = 'Baseline IAV', ylab = 'Mean yield change')
  lines(0:60, seq(0, -60, -1), col = 'green', lwd = 2)
  title('Riego')
  plot(yld.FPU.CV.s[10,], FPU.pctCh.mmm.s, xlab = 'Baseline IAV', ylab = 'Mean yield change')
  lines(0:60, seq(0, -60, -1), col = 'green', lwd = 2)
  title('Secano')
  
  # Plot mean yield vs. IAV changes
  # plot(FPU.CVCh.mmm.r,FPU.pctCh.mmm.r,xlab='Change in IAV',ylab='Change in mean yields',xlim=c(0,max(FPU.CVCh.mmm.r,na.rm=T)+2),ylim=c(min(FPU.pctCh.mmm.r,na.rm=T)-2,0))
  plot(FPU.CVCh.mmm.r, FPU.pctCh.mmm.r, xlab = 'Change in IAV (%)', ylab = 'Change in mean yields (%)', xlim = c(0, 50), ylim = c(-60, 0))
  abline(h = -25, lty = 3)
  abline(v = 10, lty = 3)
  title('Riego')
  hotspots.r = which(FPU.CVCh.mmm.r > 10 & FPU.pctCh.mmm.r < (-25))
  if(length(hotspots.r) > 0){
    points(FPU.CVCh.mmm.r[hotspots.r], FPU.pctCh.mmm.r[hotspots.r], pch = 15, col = 'purple')
    text(FPU.CVCh.mmm.r[hotspots.r], FPU.pctCh.mmm.r[hotspots.r], FPUs[hotspots.r], pos = 1, cex = 0.9)
  }
  
  # plot(FPU.CVCh.mmm.s,FPU.pctCh.mmm.s,xlab='Change in IAV',ylab='Change in mean yields',xlim=c(0,max(FPU.CVCh.mmm.s,na.rm=T)+2),ylim=c(min(FPU.pctCh.mmm.s,na.rm=T)-2,0))
  plot(FPU.CVCh.mmm.s, FPU.pctCh.mmm.s, xlab = 'Change in IAV', ylab = 'Change in mean yields', xlim = c(0, 50), ylim = c(-60, 0))
  abline(h = -25, lty = 3)
  abline(v = 10, lty = 3)
  title('Secano')
  hotspots.s = which(FPU.CVCh.mmm.s > 10 & FPU.pctCh.mmm.s < (-25))
  if(length(hotspots.s) > 0){
    points(FPU.CVCh.mmm.s[hotspots.s], FPU.pctCh.mmm.s[hotspots.s], pch = 16, col = 'purple')
    text(FPU.CVCh.mmm.s[hotspots.s], FPU.pctCh.mmm.s[hotspots.s], FPUs[hotspots.s], cex = 0.9, pos = 1, offset = 0.2)
  }
  
  # POINT-SCALE PLOTS
  # Plot mean yield changes vs. initial interannual CV for WFD
  # (identify areas where projected yield declines are higher than interannual variability)
  par(ask = F)
  wfd.s.sd = apply(yld_secano[,,(p-1)], 1, sd, na.rm = T)
  wfd.s.cv = wfd.s.sd/wfd.s * 100
  plot(wfd.s.cv, pct.ch.s2[,1], xlim = c(0, 100), ylim = c(-65, 50), xlab = 'WFD CV', ylab = '% yield change (MMM)')
  abline(h = 0)
  lines(0:60, seq(0, -60, -1), col = 'green', lwd = 2)
  ind.hotspot = which(pct.ch.s2[,1] < wfd.s.cv * -1)
  if(length(ind.hotspot) > 0){
    text(5, -50, paste(round(sum(crop_secano[ind.hotspot,'secano.area'])/sum(crop_secano[,'secano.area']) * 100, 2), '%'))
    aggregate(crop_secano[ind.hotspot, 'secano.area'], by = list(crop_secano[ind.hotspot, 'country']), FUN = sum)
  }
  title(paste(crops[i], '- secano'))
  
  wfd.r.sd = apply(yld_riego[,,(p-1)], 1, sd, na.rm = T)
  wfd.r.cv = wfd.r.sd/wfd.r * 100
  plot(wfd.r.cv, pct.ch.r2[,1], xlim = c(0, 100), ylim = c(-65, 50), xlab = 'WFD CV', ylab = '% yield change (MMM)')
  abline(h = 0)
  lines(0:60, seq(0, -60, -1), col = 'green', lwd = 2)
  ind.hotspot = which(pct.ch.r2[,1] < wfd.r.cv * -1)
  if(length(ind.hotspot) > 0){
    text(5, -50, paste(round(sum(crop_riego[ind.hotspot, 'riego.area'])/sum(crop_riego[,'riego.area']) * 100, 2),'%'))
    aggregate(crop_riego[ind.hotspot, 'riego.area'], by = list(crop_riego[ind.hotspot, 'country']), FUN = sum)
  }
  title(paste(crops[i], '- riego'))
  
  # Plot yield changes vs. change in CV
  models.r.cv = apply(yld_riego[,,1:(p-2)], c(1, 3), function(x) sd(x, na.rm = T)/mean(x, na.rm = T) * 100) # multi-annual means
  models.s.cv = apply(yld_secano[,,1:(p-2)],c(1, 3), function(x) sd(x, na.rm = T)/mean(x, na.rm = T) * 100)
  wfd.r.cv2 = replicate(9, wfd.r.cv)
  wfd.s.cv2 = replicate(9, wfd.s.cv)
  CV.ch.r = models.r.cv - wfd.r.cv2
  CV.ch.s = models.s.cv - wfd.s.cv2
  CV.ch.r.mmm = apply(CV.ch.r, 1, mean, na.rm = T)
  CV.ch.s.mmm = apply(CV.ch.s, 1, mean, na.rm = T)
  
  par(ask = F)
  plot(CV.ch.s.mmm, pct.ch.s2[,3], xlim = c(-50, 100), ylim = c(-65, 50), xlab = 'Change in CV', ylab = '% yield change (MMM)')
  ind.good.s = which(CV.ch.s.mmm > (-50) & CV.ch.s.mmm < 100 & pct.ch.s2[,3] > (-65) & pct.ch.s2[,3] < 50)
  mod.s = lm(pct.ch.s2[ind.good.s, 3] ~ CV.ch.s.mmm[ind.good.s] + eval(CV.ch.s.mmm[ind.good.s]^2), x = T)
  abline(h = 0)
  points(mod.s$x[,2], mod.s$fitted, pch = 20, col = 'red')
  title(paste(crops[i],'- secano'))
  
  plot(CV.ch.r.mmm, pct.ch.r2[,3], xlim = c(-50, 100), ylim = c(-65, 50), xlab = 'Change in CV', ylab = '% yield change (MMM)')
  ind.good.r = which(CV.ch.r.mmm > (-50) & CV.ch.r.mmm < 100 & pct.ch.r2[,3] > (-65) & pct.ch.r2[,3] < 50)
  mod.r = lm(pct.ch.r2[ind.good.r, 3] ~ CV.ch.r.mmm[ind.good.r] + eval(CV.ch.r.mmm[ind.good.r]^2), x = T)
  abline(h = 0)
  points(mod.r$x[,2], mod.r$fitted, pch = 20, col = 'red')
  title(paste(crops[i],'- riego'))
  
  # AGGREGATION TO 6 REGIONS
  # Calculate overall mean yield changes for entire region (then sub-regions)
  # load physical area from SPAM to weight yield changes
  
  # Calculate weighted averages by model & compare to WFD
  area.riego = replicate(length(anos), crop_riego$riego.area)
  area.secano = replicate(length(anos), crop_secano$secano.area)
  
  # find areas where WFD consistently failing and discard these from aggregation
  zeros.wfd.r = apply(yld_riego[,,(p-1)], 1, function(x) sum(x == 0, na.rm = T))
  zeros.wfd.s = apply(yld_secano[,,(p-1)], 1, function(x) sum(x == 0, na.rm = T))
  
  # initialize arrays
  yld.ag.r = array(NA, dim = c(8, 6)) # 8 indicators x 6 regions
  yld.ag.s = array(NA, dim = c(8, 6))
  
  for(j in 1:6){ # 5 regions + all
    
    for(t in 1:2){ # loop through riego/ secano
      
      # subset regions
      if(j <= 5){
        eval(parse(text = paste("ind.reg = which(crop_", treat[t], "$Region == regions[j] & zeros.wfd.", substr(treat[t], 1, 1), " <= 14)", sep = '')))
      } else {
        eval(parse(text = paste("ind.reg = which(zeros.wfd.", substr(treat[t], 1, 1), " <= 14)", sep = '')))
      }
      
      if(length(ind.reg) > 1){ # at least 2 pixels
        
        # hacer el promedio ponderado por �rea de los rendimientos
        mean.ylds = array(NA, dim = c((p-1), length(anos))) # models x years
        for(m in 1:(p-1)){
          for(y in (anos-1)){ # fix anos-1!
            eval(parse(text = paste("ylds.y = yld_", treat[t], "[ind.reg,y, m]", sep = ''))) 
            # ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0
            eval(parse(text = paste("areas.y = area.", treat[t], "[ind.reg, y]", sep = ''))) 
            mean.ylds[m, y] = sum(ylds.y * areas.y)/sum(areas.y) # promedio ponderado
          }
        }
        mean.ylds = apply(mean.ylds, 1, mean, na.rm = T) # take inter-annual mean
        
        wfd = mean.ylds[p-1] # WFD
        mmm = mean(mean.ylds[1:(p-2)], na.rm = T) # multi-model mean
        eval(parse(text = paste("yld.ag.", substr(treat[t], 1, 1), "[1, j] = wfd", sep = '')))
        eval(parse(text = paste("yld.ag.", substr(treat[t], 1, 1), "[2, j] = mmm", sep = '')))
        eval(parse(text = paste("yld.ag.", substr(treat[t], 1, 1), "[3, j] = (mmm - wfd)/wfd * 100", sep = ''))) # % diff: riego
        
        # calculate across-model range of % changes
        ch = mat.or.vec((p-2), 1)
        for(m in 1:(p-2)){
          ch[m] = (mean.ylds[m] - mean.ylds[p-1] )/mean.ylds[p-1] * 100
        }
        eval(parse(text = paste("yld.ag.", substr(treat[t], 1, 1), "[4:5, j] = range(ch, na.rm = T)", sep = '')))
        
        # try bootstrapping across models to get CI
        boot = mat.or.vec(500, 1)
        for(b in 1:500){
          boot.ind = sample(1:(p-2), (p-2), replace = T)
          boot[b] = mean(mean.ylds[boot.ind])
        }
        boot.ch = (boot - mean.ylds[p-1])/mean.ylds[p-1] * 100
        eval(parse(text = paste("yld.ag.", substr(treat[t], 1, 1), "[6:7, j] = quantile(boot.ch, c(.025, .975), na.rm = T)", sep = '')))
        eval(parse(text = paste("yld.ag.", substr(treat[t], 1, 1), "[8, j] = sum(crop_", treat[t], "[ind.reg,'", treat[t], ".area'])", sep = '')))
        
      }
    }
  }
  
  # label results and save
  rownames(yld.ag.r) = c('WFD', 'MMM', '%Ch', 'Range.ch.l', 'Range.ch.u', 'CI.l', 'CI.u', 'Area')
  rownames(yld.ag.s) = rownames(yld.ag.r)
  colnames(yld.ag.r) = c(regions, 'LAC')
  colnames(yld.ag.s) = colnames(yld.ag.r)
  
  eval(parse(text = paste(crops[i], ".ch.r = yld.ag.r ", sep = '')))
  eval(parse(text = paste(crops[i], ".ch.s = yld.ag.s ", sep = '')))
  eval(parse(text = paste("save(", crops[i], ".ch.r,file = '", "./summaries/", crops[i], ".ch.r.Rdat')", sep = ""))) # guardar matrices
  eval(parse(text = paste("save(", crops[i], ".ch.s,file = '", "./summaries/", crops[i], ".ch.s.Rdat')", sep = "")))
  
  # Mirar resultados regionales
  round(yld.ag.r, 2)
  round(yld.ag.s, 2)
  
  # CALCULATE REGIONAL IAV
  # try plotting boxplots with interannual variability by model
  # have to aggregate to LAM or regions first
  # look at physical area from SPAM to weight yield changes
  # Calculate weighted averages by model & compare to WFD
  boxplot.r = array(NA, dim = c(p-1, length(anos), 6)) # initialize matrices
  boxplot.s = array(NA, dim = c(p-1, length(anos), 6))
  
  # loop by region
  for(t in 1:2){ # then by treatment
    for(r in 1:6){
      if(r <= 5){
        eval(parse(text = paste("ind.reg = which(crop_", treat[t], "$Region == regions[r] & zeros.wfd.", substr(treat[t], 1, 1), " <= 14)", sep = '')))
      } else {
        eval(parse(text = paste("ind.reg = which(zeros.wfd.", substr(treat[t], 1, 1), " <= 12)", sep = '')))          
      }
      if(length(ind.reg) > 1){
        for(j in 1:(p-1)){ # loop through models
          for(y in (anos-1)){ # loop through years, fix anos-1!
            eval(parse(text = paste("ylds.y = yld_", treat[t], "[ind.reg,y,j]", sep = ''))) 
            ylds.y[is.na(ylds.y)] = 0 # reemplazar NA con 0 for legitimate crop failures
            eval(parse(text = paste("areas.y = area.", treat[t], "[ind.reg, y]", sep = ''))) 
            eval(parse(text = paste("boxplot.", substr(treat[t], 1, 1), "[j,y,r] = sum(ylds.y * areas.y)/sum(areas.y)", sep = '')))    
          }
        }
      }
    }
  }
  
  dimnames(boxplot.r)[1] = list(models$Models[1:(p-1)])
  dimnames(boxplot.r)[3] = list(c(regions, 'LAC'))
  dimnames(boxplot.s)[1] = list(models$Models[1:(p-1)])
  dimnames(boxplot.s)[3] = list(c(regions, 'LAC'))
  
  par(ask = F)
  if(crops[i] == 'Maize'){ylim.c = c(2000, 9000)} # hard-code ranges of plots
  if(crops[i] == 'Rice'){ylim.c = c(0, 6600)}
  if(crops[i] == 'Soybean'){ylim.c = c(500, 1700)}
  if(crops[i] == 'Wheat'){ylim.c = c(0, 8000)}
  if(crops[i] == 'Bean'){ylim.c = c(0, 2500)}
  
  # Create plots of interannual variability for Latin American region
  # riego
  boxplot(t(boxplot.r[c(10, 1:9),,6]), xaxt = 'n', ylim = ylim.c)
  text(x = seq(1, p, 1.1), y = ylim.c[1] + 50, cex = 1, labels = dimnames(boxplot.r)[[1]][c(10, 1:9)], xpd = TRUE, srt = 40, pos = 2)
  title(paste('Inter-annual variability for region: ', crops[i], ' (riego)', sep = ''))
  
  # secano
  boxplot(t(boxplot.s[c(10, 1:9),,6]), xaxt = 'n', ylim = ylim.c)
  text(x = seq(1, p, 1.1), y = ylim.c[1] + 50, cex = 1, labels = dimnames(boxplot.s)[[1]][c(10, 1:9)], xpd = TRUE, srt = 40, pos = 2)
  title(paste('Inter-annual variability for region: ', crops[i], ' (secano)', sep = ''))
  
  # Calculate inter-annual variability (standard dev) for regions
  yld.CV.r = array(NA, dim = c(6, 6)) # initialize matrices
  yld.CV.s = array(NA, dim = c(6, 6))
  for(t in 1:2){
    
    for(r in 1:6){
      
      eval(parse(text = paste("test = sum(is.na(boxplot.", substr(treat[t], 1, 1), "[,1,r]))", sep = '')))
      
      if(test == 0){ # check for NA's in 1st year
        
        eval(parse(text = paste("yld.cv = apply(boxplot.", substr(treat[t], 1, 1), "[,,r], 1, function(x) sd(x, na.rm = T)/mean(x, na.rm = T)) * 100", sep = '')))
        
        eval(parse(text = paste("yld.CV.", substr(treat[t], 1, 1), "[1,r] = yld.cv[(p-1)]", sep = ''))) # WFD
        eval(parse(text = paste("yld.CV.", substr(treat[t], 1, 1), "[2,r] = mean(yld.cv[1:(p-2)], na.rm = T)", sep = ''))) # Multi-model mean
        
        eval(parse(text = paste("yld.CV.", substr(treat[t], 1, 1), "[3:4, r] = range(yld.cv[1:(p-2)], na.rm = T)", sep = ''))) # range of models
        
        boot = mat.or.vec(500, 1) # bootstrap results
        for(j in 1:500){
          boot.ind = sample(1:(p-2), (p-2), replace = T)
          boot[j] = mean(yld.cv[boot.ind])
        }
        eval(parse(text = paste("yld.CV.", substr(treat[t], 1, 1), "[5:6, r] = quantile(boot, c(.025,.975), na.rm = T)", sep = '')))
      }
    }
  }
  
  dimnames(yld.CV.r)[1] = list(c('WFD', 'MMM', 'Range.ch.l', 'Range.ch.u', 'CI.l', 'CI.u'))
  dimnames(yld.CV.r)[2] = list(c(regions, 'LAC'))
  dimnames(yld.CV.s)[1] = list(c('WFD', 'MMM', 'Range.ch.l', 'Range.ch.u', 'CI.l', 'CI.u'))
  dimnames(yld.CV.s)[2] = list(c(regions, 'LAC'))
  
  # put results in data frame and save
  eval(parse(text = paste(crops[i], ".cv.r = yld.CV.r ", sep = '')))
  eval(parse(text = paste(crops[i], ".cv.s = yld.CV.s ", sep = '')))
  eval(parse(text = paste("save(", crops[i], ".cv.r,file = '", "./summaries/", crops[i], ".cv.r.Rdat')", sep = "")))
  eval(parse(text = paste("save(", crops[i], ".cv.s,file = '", "./summaries/", crops[i], ".cv.s.Rdat')", sep = "")))
  
  # mirar resultados
  round(yld.CV.r,2)
  round(yld.CV.s,2)
  round(yld.CV.r[2,] - yld.CV.r[1,],1)
  round(yld.CV.s[2,] - yld.CV.s[1,],1)
  
  # Calculate average area under crop failures for regions
  # first identify crop failures by pixel & year
  thresh = mean(yld_secano[,,10]) * 0.05 # 5% of mean rainfed yield is considered "failure"
  riego.fail = (yld_riego <= thresh | is.na(yld_riego)) * 1
  secano.fail = (yld_secano <= thresh | is.na(yld_secano)) * 1
  
  # Calculate % area in region with crop failure (by year, then average across years)
  area.fail.r = array(NA, dim = c(6, 6)) # initialize matrices
  area.fail.s = array(NA, dim = c(6, 6))
  for(t in 1:2){
    for(r in 1:6){
      # subset regions
      if(r <= 5){
        eval(parse(text = paste("ind.reg = which(crop_", treat[t], "$Region == regions[r])", sep = '')))
      } else {
        eval(parse(text = paste("ind.reg = 1:dim(crop_", treat[t], ")[1]", sep = '')))
      }
      
      if(length(ind.reg) > 1){ # at least 2 pixels
        eval(parse(text = paste("test = ", treat[t], ".fail[ind.reg,,] * crop_", treat[t], "$", treat[t], ".area[ind.reg]", sep = ""))) # area of pixels with crop failures
        fail.reg = apply(test, c(2, 3), sum) # areas with failure in region r (by year & model)
        eval(parse(text = paste("fail.reg = fail.reg/sum(crop_", treat[t], "$", treat[t], ".area[ind.reg]) * 100", sep = ""))) # % area with crop failure in region r
        fail.avg.reg = apply(fail.reg, 2, mean) # take interannual average
        
        eval(parse(text = paste("area.fail.", substr(treat[t], 1, 1), "[1:4, r] = c(fail.avg.reg[p-1], mean(fail.avg.reg[1:(p-2)]), range(fail.avg.reg[1:(p-2)], na.rm = T))", sep = "")))
        
        boot = mat.or.vec(500, 1) # bootstrap results
        for(j in 1:500){
          boot.ind = sample(1:(p-2),(p-2), replace = T)
          boot[j] = mean(fail.avg.reg[boot.ind])
        }
        eval(parse(text = paste("area.fail.", substr(treat[t], 1, 1), "[5:6, r] = quantile(boot, c(.025, .975), na.rm = T)", sep = "")))
      }
    }
  }
  
  dimnames(area.fail.r)[1] = list(c('WFD', 'MMM', 'Range.ch.l', 'Range.ch.u', 'CI.l', 'CI.u'))
  dimnames(area.fail.r)[2] = list(c(regions, 'LAC'))
  dimnames(area.fail.s)[1] = list(c('WFD', 'MMM', 'Range.ch.l', 'Range.ch.u', 'CI.l', 'CI.u'))
  dimnames(area.fail.s)[2] = list(c(regions, 'LAC'))
  
  # put results in data frame and save
  eval(parse(text = paste(crops[i], ".fail.r = area.fail.r ", sep = '')))
  eval(parse(text = paste(crops[i], ".fail.s = area.fail.s ", sep = '')))
  eval(parse(text = paste("save(", crops[i], ".fail.r,file = '", "./summaries/", crops[i], ".fail.r.Rdat')", sep = "")))
  eval(parse(text = paste("save(", crops[i], ".fail.s,file = '", "./summaries/", crops[i], ".fail.s.Rdat')", sep = "")))
  
  # mirar resultados
  round(area.fail.r, 2)
  round(area.fail.s, 2)
  suppressMessages(library(gplots))
  barplot2(area.fail.r[1:2,], beside = T, plot.ci = T, ci.l = rbind(NA, area.fail.r[5,]), ci.u = rbind(NA, area.fail.r[6,]))
  barplot2(area.fail.s[1:2,], beside = T, plot.ci = T, ci.l = rbind(NA, area.fail.s[5,]), ci.u = rbind(NA, area.fail.s[6,]))
  
  return(cat(paste0("Process done for: ", crops[i], "!\n")))
  
})
