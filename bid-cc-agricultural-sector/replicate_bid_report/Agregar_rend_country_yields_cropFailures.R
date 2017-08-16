# Aggregate yields to country level to calculate % changes in yield and area at risk of crop failure

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
suppressMessages(library(gplots))

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
  cultivos.en = c('Maize', 'Rice', 'Soybean', 'bean', 'wheat')
  anos = 1:28  # Fijar years para analizar aqui
  anos2 = 1971:1998
  
  # #classification 1
  # NOR = c('Mexico','Cuba')
  # CEN = c('Belize','Guatemala','Honduras','El Salvador','Nicaragua','Costa Rica','Panama','Dominican Republic','Jamaica','Haiti','Colombia','Venezuela','Trinidad and Tobago')
  # BRA = c('Guyana','French Guiana','Brazil','Suriname')
  # AND = c('Ecuador','Peru','Bolivia','Chile')
  # SUR = c('Argentina','Uruguay','Paraguay')
  # 
  # #classification 2
  # NOR = c('Mexico','Cuba','Belize','Guatemala','Honduras','El Salvador','Nicaragua','Costa Rica','Panama','Dominican Republic','Jamaica','Haiti')
  # AND = c('Colombia','Venezuela','Trinidad and Tobago','Ecuador','Peru','Bolivia')
  # BRA = c('Guyana','French Guiana','Brazil','Suriname')
  # SUR = c('Chile','Argentina','Uruguay','Paraguay')
  
  # Classification 3
  NOR = c('Mexico','Cuba','Belize','Guatemala','Honduras','El Salvador','Nicaragua','Dominican Republic','Jamaica','Haiti')
  CEN = c('Costa Rica','Panama','Colombia','Venezuela','Trinidad and Tobago','Guyana','French Guiana','Suriname')
  AND = c('Ecuador','Peru','Bolivia')
  BRA = c('Brazil','Paraguay')
  SUR = c('Argentina','Uruguay','Chile')
  
  # Load pixel id's
  eval(parse(text=paste('load("', path.root, '/08-Cells_toRun/matrices_cultivo/version2017/', cultivos.en[i], '_riego.Rdat")', sep = '')))  #08-Cells_toRun/
  eval(parse(text=paste('load("', path.root, '/08-Cells_toRun/matrices_cultivo/version2017/', cultivos.en[i], '_secano.Rdat")', sep = '')))
  
  # get list of climate models
  models = read.table(paste(path.root, '/_documentos/ModelosGCM.csv', sep = ''), header = T, sep = ',', stringsAsFactors = F)
  models = rbind(models,'Historical baseline','Future multi-GCM average')
  p = dim(models)[1]
  colnames(models) = 'Models'  #hack for now (machetazo)
  
  # load, unlist and extract data to arrays (gridcells x years x models)
  # initialize arrays
  yld_secano = array(NA, dim = c(dim(crop_secano)[1], 30, (p-1), length(variedades)))  #10 GCM's + baseline
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
      secano = array(NA, dim = c(length(Run.secano), 30))  #initialize arrays
      for(j in 1:length(Run.secano)){
        if(is.null(dim(Run.secano[[j]]))){
          secano[j,,] = NA
        } else {
          secano[j, 1:dim(Run.secano[[j]])[1]] = as.numeric(as.character(Run.secano[[j]][,'HWAH']))
        }
      }
      riego = array(NA, dim = c(length(Run.riego), 30))
      for(j in 1:length(Run.riego)){
        if(is.null(dim(Run.riego[[j]]))){
          riego[j,,] = NA
        }  else{
          riego[j, 1:dim(Run.riego[[j]])[1]] = as.numeric(as.character(Run.riego[[j]][,'HWAH']))
        }
      }
      
      # place results in array
      yld_secano[,,m,v] = secano
      yld_riego[,,m,v] = riego
      
    }
  }
  
  # Descartar los years sin datos climaticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones tecnicas!
  yld_secano = yld_secano[,anos,,] # incluir los indices de los a�os "buenos" aqu� con datos por todos los pixeles
  yld_riego = yld_riego[,anos,,]
  yld_secano[yld_secano == -99 | yld_secano == -99.9 | yld_secano == 9999999] = 0  #set crop failures to 0 yield
  yld_riego[yld_riego == -99 | yld_riego == -99.9 | yld_riego == 9999999] = 0
  
  #Identify best variety in historical baseline (higher mean yields & less crop failures)
  #across all 3 varieties
  wfd.r = apply(yld_riego[,,(p-1),], c(1, 3), mean, na.rm = T) # multi-annual means
  wfd.s = apply(yld_secano[,,(p-1),], c(1, 3), mean, na.rm = T)
  thresh = mean(yld_secano[,,(p-1),], na.rm = T) * 0.2 # define crop failure as 20% of mean rainfed yield
  wfd.fail.r = apply(yld_riego[,,(p-1),], c(1, 3), function(x) sum(x < thresh, na.rm = T)) # multi-annual means
  wfd.fail.s = apply(yld_secano[,,(p-1),], c(1, 3), function(x) sum(x < thresh, na.rm = T))
  
  #highest-yielding variety
  wfd.r.high = apply(wfd.r, 1, which.max)
  wfd.s.high = apply(wfd.s, 1, which.max)
  
  #least crop failures
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
      eval(parse(text = paste("meso = match(crop_", treat[t], "$country,habit$X[habit$Mesoamerican == 1])", sep = "")))
      ind.meso = which(is.na(meso) == F)
      ind.andean = which(is.na(meso))
      eval(parse(text = paste("meso.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.meso, 1:2], 1, which.max)", sep = "")))
      eval(parse(text = paste("andean.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.andean, 3:4], 1, which.max)+2", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1), ".best[ind.meso] = meso.high", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1), ".best[ind.andean] = andean.high", sep = "")))
    }
  }
  
  # Select variety with highest yields for every pixel, all params 
  yld_secano.2 = yld_secano # make backup with varieties
  yld_riego.2 = yld_riego
  yld_secano = array(NA, dim = c(dim(crop_secano)[1], length(anos), (p-1)))  #pixels x years x models x params
  yld_riego = array(NA, dim = c(dim(crop_riego)[1], length(anos), (p-1)))
  for(k in 1:length(variedades)){
    yld_secano[wfd.s.best == k,,] = yld_secano.2[wfd.s.best == k,,,k]
    yld_riego[wfd.r.best == k,,] = yld_riego.2[wfd.r.best == k,,,k]
  }
  
  # check some crop characteristics for table
  (sum(crop_secano$secano.area) + sum(crop_riego$riego.area)) # total harvested area
  sum(c(crop_secano$elev*crop_secano$secano.area,crop_riego$elev*crop_riego$riego.area))/sum(c(crop_secano$secano.area,crop_riego$riego.area))  #area-weighted elevation
  sum(c(abs(crop_secano$y)*crop_secano$secano.area,abs(crop_riego$y)*crop_riego$riego.area))/sum(c(crop_secano$secano.area,crop_riego$riego.area))  #area-weighted latitude
  
  sum(crop_secano$secano.area)/1000000  #rainfed
  sum(crop_secano$elev*crop_secano$secano.area)/sum(crop_secano$secano.area)  #area-weighted elevation
  sum(abs(crop_secano$y)*crop_secano$secano.area)/sum(crop_secano$secano.area)  #area-weighted latitude
  
  sum(crop_riego$riego.area)/1000000  #irrigated
  sum(crop_riego$elev*crop_riego$riego.area)/sum(crop_riego$riego.area)  #area-weighted elevation
  sum(abs(crop_riego$y)*crop_riego$riego.area)/sum(crop_riego$riego.area)  #area-weighted latitude
  
  # Look at harvested area by country and FPU
  areas = c(crop_secano$secano.area, crop_riego$riego.area)
  countries = c(crop_secano$country, crop_riego$country)
  areas.country = aggregate(areas, by = list(countries), FUN = sum)
  sum(areas.country$x)
  areas.country[order(areas.country$x, decreasing = T),]
  
  fpus = c(as.character(crop_secano$New_FPU), as.character(crop_riego$New_FPU))
  areas.fpu = aggregate(areas, by = list(fpus), FUN = sum)
  sum(areas.fpu$x)
  areas.fpu[order(areas.fpu$x, decreasing = T),]
  
  # Filter out bad pixels with excess crop failures
  # contar # de NA y 0 en rendimiento
  riego.fallas = apply(yld_riego, c(1, 3), function(x) sum(is.na(x) | x == 0))
  secano.fallas = apply(yld_secano, c(1, 3), function(x) sum(is.na(x) | x == 0))
  
  # filtrar pixeles con muchas fallas en WFD
  ind.bad.r = which(riego.fallas[,10] >= 14)  #half of total years
  ind.bad.s = which(secano.fallas[,10] >= 14)
  if (length(ind.bad.r) > 0) {yld_riego = yld_riego[-ind.bad.r,,]
  crop_riego2 = crop_riego  # make backup of original
  crop_riego = crop_riego[-ind.bad.r,]}
  if (length(ind.bad.s) > 0) {yld_secano = yld_secano[-ind.bad.s,,]
  crop_secano2 = crop_secano  #make backup of original
  crop_secano = crop_secano[-ind.bad.s,]}
  
  # #Set NA to 0 for crop failure
  # yld_secano[is.na(yld_secano)] = 0
  # yld_riego[is.na(yld_riego)] = 0
  
  # Aggregate yields to COUNTRY-SCALE
  # get list of countries
  countries = as.character(na.omit(unique(c(as.character(crop_riego$country), as.character(crop_secano$country)))))
  
  # Calculate weighted averages by model & compare to WFD
  area.riego = replicate(length(anos), crop_riego$riego.area)  #keep same dimension as yield matrix
  area.secano = replicate(length(anos), crop_secano$secano.area)
  
  # Aggregate to country scale for all models & years (across riego & secano)
  yld.country = array(NA, dim = c(length(countries), length(anos), 10))  #initialize matrices (years x country x model )
  dimnames(yld.country)[1] = list(countries)
  dimnames(yld.country)[2] = list(anos2) 
  dimnames(yld.country)[3] = list(models[1:10,])
  for(r in 1:length(countries)){ # loop by country
    
    ind.reg.r = which(crop_riego$country == countries[r])
    ind.reg.s = which(crop_secano$country == countries[r])
    
    if(length(c(ind.reg.r, ind.reg.s)) >= 1){
      for(m in 1:10){
        prod_riego.m = yld_riego[ind.reg.r,,m] * area.riego[ind.reg.r,] # calculate production by pixel
        prod_secano.m = yld_secano[ind.reg.s,,m] * area.secano[ind.reg.s,]
        prod_all = rbind(prod_riego.m, prod_secano.m) # stack riego & secano
        area_all = rbind(area.riego[ind.reg.r,], area.secano[ind.reg.s,]) # same for area
        yld.country[r,,m] = apply(prod_all,2,sum)/apply(area_all, 2, sum) # sum production at country-scale & divide out by total area
      }
    }
  }
  
  # calculate interannual means in each period
  yld.country.m = apply(yld.country, c(1, 3), mean, na.rm = T)
  
  # Calculate percent change in yield between each model & WFD with bootstrapping for CI
  pct.ch.country = (apply(yld.country.m[,1:9], 1, mean) - yld.country.m[,10])/yld.country.m[,10] * 100
  boot = mat.or.vec(500, length(countries)) # bootstrap results
  for(j in 1:500){boot.ind = sample(1:(p-2), (p-2), replace = T)
  boot[j,] = (apply(yld.country.m[,boot.ind], 1, mean) - yld.country.m[,10])/yld.country.m[,10] * 100}
  pct.ch.country.CI = apply(boot, 2, function(x) quantile(x, c(.05, .95), na.rm = T))
  colnames(pct.ch.country.CI) = countries
  
  # Save results 
  eval(parse(text = paste("save(yld.country.m, file = './FAOSTAT_validation/ylds_", crops[i], "_optVariety.Rdat')", sep = "")))
  eval(parse(text = paste("save(pct.ch.country, pct.ch.country.CI, file = './FAOSTAT_validation/country.pctCh_", crops[i], "_optVariety.Rdat')", sep = "")))
  
  ## GRAPH YIELD CHANGES ##
  # Color-code country labels by region
  regions = character(length(countries))
  ind.NOR = match(countries, NOR)
  regions[which(is.na(ind.NOR) == F)] = 'darkorange' # yellow
  ind.CEN = match(countries, CEN)
  regions[which(is.na(ind.CEN) == F)] = 'yellow' # orange
  ind.BRA = match(countries, BRA)
  regions[which(is.na(ind.BRA) == F)] = 'green2'
  ind.AND = match(countries, AND)
  regions[which(is.na(ind.AND) == F)] = 'royalblue3'
  ind.SUR = match(countries, SUR)
  regions[which(is.na(ind.SUR) == F)] = 'red3'
  
  ind.sort = order(pct.ch.country)  #sort by yield change values
  
  # Create barplot
  par(mar=c(6.5, 4.1, 2.1, 2.1))
  barplot2(pct.ch.country[ind.sort], width = 0.8, xlim = c(0, 38), col = regions[ind.sort], yaxt = 'n', xaxt = 'n',
           ylim = c(-55, 55), xlab = '', space = 1, plot.ci = T, ci.l = pct.ch.country.CI[1, ind.sort], ci.u = pct.ch.country.CI[2, ind.sort])
  axis(side = 2, at = seq(-55, 55, 10))
  abline(h = 0)
  # text(seq(1.5,length(countries)*2,by=2), par("usr")[3]-0.25, 
  #      srt = 60, adj= 1, xpd = TRUE,
  #      labels = paste(countries[ind.sort]), cex=0.85)
  text(seq(0.8, length(countries) * 0.8 * 2, by = 0.8 * 2), par("usr")[3]-7, 
       srt = 55, adj = 1, xpd = TRUE,
       labels = paste(countries[ind.sort]), cex = 0.8)
  
  ## CROP FAILURES ##
  ## Aggregate area with crop failures to country-scale
  # first identify crop failures by pixel & year
  thresh = mean(yld_secano[,,10]) * 0.05  #5% of mean rainfed yield is considered "failure"
  # thresh = 50
  riego.fail = (yld_riego <= thresh | is.na(yld_riego)) * 1
  secano.fail = (yld_secano <= thresh | is.na(yld_secano)) * 1
  
  fail.country = array(NA, dim = c(length(countries), length(anos), 10)) # initialize matrices (years x country x model)
  dimnames(fail.country)[1] = list(countries)
  dimnames(fail.country)[2] = list(anos2) 
  dimnames(fail.country)[3] = list(models[1:10,])
  for(r in 1:length(countries)){ # loop by country
    
    ind.reg.r = which(crop_riego$country == countries[r])
    ind.reg.s = which(crop_secano$country == countries[r])
    
    if(length(c(ind.reg.r, ind.reg.s)) >= 1){
      
      for(m in 1:10){
        
        fail_riego.m = riego.fail[ind.reg.r,,m] * area.riego[ind.reg.r,] # calculate area with crop failure
        fail_secano.m = secano.fail[ind.reg.s,,m] * area.secano[ind.reg.s,]
        fail_all = rbind(fail_riego.m, fail_secano.m) # stack riego & secano
        area_all = rbind(area.riego[ind.reg.r,], area.secano[ind.reg.s,])
        fail.country[r,,m] = apply(fail_all, 2, sum)/apply(area_all, 2, sum) * 100 # sum area with crop failure at country-scale, divide by total area to get %
      }
    }
  }
  
  # Calculate interannual means in each period
  fail.country.m = apply(fail.country, c(1, 3), mean)
  
  # Calculate multi-model mean in future period, put with baseline
  fail.compare = cbind(fail.country.m[,10], apply(fail.country.m[,1:9], 1, mean))
  colnames(fail.compare) = c('Historical', 'Future')
  
  # Calculate confidence intervals
  boot = mat.or.vec(500, length(countries)) # bootstrap results
  for(j in 1:500){boot.ind = sample(1:(p-2), (p-2), replace = T)
  boot[j,] = apply(fail.country.m[,boot.ind], 1, mean)}
  fail.future.CI = apply(boot, 2, function(x) quantile(x, c(.05, .95), na.rm = T))
  colnames(fail.future.CI) = countries
  
  # order results by crop failures in baseline
  ind.sort = order(fail.compare[,1], decreasing = T)
  
  # Save results 
  eval(parse(text = paste("save(fail.compare, fail.future.CI, file = './FAOSTAT_validation/country.areaFail_", crops[i], "_optVariety.Rdat')", sep = "")))
  
  # Try color-coding country labels by region
  regions = character(length(countries))
  ind.NOR = match(countries, NOR)
  regions[which(is.na(ind.NOR) == F)] = 'yellow'
  ind.CEN = match(countries, CEN)
  regions[which(is.na(ind.CEN) == F)] = 'darkorange'
  ind.BRA = match(countries, BRA)
  regions[which(is.na(ind.BRA) == F)] = 'green2'
  ind.AND = match(countries, AND)
  regions[which(is.na(ind.AND) == F)] = 'royalblue3'
  ind.SUR = match(countries, SUR)
  regions[which(is.na(ind.SUR) == F)] = 'red3'
  
  # Create barplot
  barplot2(t(fail.compare[ind.sort,]), beside = T, ylim = c(0, 30), density = c(NA, 50),
           col = rbind(regions[ind.sort], regions[ind.sort]), yaxt = 'n', xaxt = 'n', xlab = '',
           plot.ci = T, ci.l = rbind(NA, fail.future.CI[1, ind.sort]), ci.u = rbind(NA, fail.future.CI[2, ind.sort]))
  axis(side = 2, at = seq(0, 30, 5))
  abline(h = 0)
  text(seq(1.5, length(countries) * 3, by = 3), par("usr")[3]-0.25, 
       srt = 60, adj = 1, xpd = TRUE,
       labels = paste(countries[ind.sort]), cex = 0.85)
  
  ## Try a plot only showing countries with significant changes in crop failures 
  # sig.ch = which((fail.future.CI[1,]>fail.compare[,1]|fail.future.CI[2,]<fail.compare[,1])&((fail.compare[,2]>5)|(fail.compare[,1]>5)))  #(and past or future >5%)
  # sig.ch = which((fail.future.CI[1,]>fail.compare[,1]|fail.future.CI[2,]<fail.compare[,1]))
  # sig.ch = which((fail.future.CI[1,]>fail.compare[,1]|fail.future.CI[2,]<fail.compare[,1]))  #(no 5% rule)
  change = fail.compare[,2] - fail.compare[,1]
  sig.ch = which((fail.future.CI[1,] > fail.compare[,1] | fail.future.CI[2,] < fail.compare[,1]) & abs(change) > 2) # (significant & change > 2%)
  
  fail.compare.2 = fail.compare[sig.ch,]
  fail.future.CI.2 = fail.future.CI[,sig.ch]
  countries.2 = countries[sig.ch]
  
  regions = character(length(countries.2))
  ind.NOR = match(countries.2, NOR)
  regions[which(is.na(ind.NOR) == F)] = 'darkorange'
  ind.CEN = match(countries.2, CEN)
  regions[which(is.na(ind.CEN) == F)] = 'yellow'
  ind.BRA = match(countries.2, BRA)
  regions[which(is.na(ind.BRA) == F)] = 'green2'
  ind.AND = match(countries.2, AND)
  regions[which(is.na(ind.AND) == F)] = 'royalblue3'
  ind.SUR = match(countries.2, SUR)
  regions[which(is.na(ind.SUR) == F)] = 'red3'
  
  # Handle one country only
  if(length(countries.2) == 1){
    change = fail.compare.2[2] - fail.compare.2[1]
    if(change < 0){ind.sort3 = c(NA, 1)} else {ind.sort3 = c(1, NA)}
  } else {
    change = fail.compare.2[,2] - fail.compare.2[,1]
    pos = names(which(change > 0))
    pos = pos[order(fail.compare.2[which(change > 0), 2], decreasing = T)] # order by future values
    neg = names(which(change < 0))
    neg = neg[order(fail.compare.2[which(change < 0), 2], decreasing = T)]
    ind.sort3 = match(c(pos, NA, neg), names(change)) # sort by future, first positive, then negative changes
  }
  
  # ind.sort = order(fail.compare.2[,1],decreasing=T)
  # change.CI = cbind(fail.future.CI.2[1,]-fail.compare.2[,1],fail.future.CI.2[2,]-fail.compare.2[,1])
  # ind.sort = order(change,decreasing=T)
  # ind.sort2 = order(fail.compare.2[,2],decreasing=T)  #sort by future values
  
  #adjust colors for NA's
  cols = regions[ind.sort3]
  cols[is.na(cols)] = 'black'
  
  # #look at changes
  # barplot2(rbind(fail.compare.2[ind.sort,1],change[ind.sort]),width=0.8,xlim=c(0,48),beside=T,ylim=c(-10,25),density=c(NA,50),col=rbind(regions[ind.sort],regions[ind.sort]),yaxt='n',xaxt='n',xlab='',plot.ci=T,ci.l=rbind(NA,change.CI[ind.sort,1]),ci.u=rbind(NA,change.CI[ind.sort,2]))
  # axis(side = 2, at = seq(-10,25,5))
  # abline(h=0)
  # text(seq(0.8,length(countries.2)*0.8*3,by=0.8*3), par("usr")[3]-0.25, 
  #      srt = 60, adj= 1, xpd = TRUE,
  #      labels = paste(countries.2[ind.sort]), cex=0.85)
  
  # look at baseline vs. future values 
  if(length(countries.2) == 1){
    barplot2(cbind(NA, fail.compare.2), width = 0.8, xlim = c(0, 48), beside = T, ylim = c(0, 40),
             density = c(NA, 40), col = rbind(cols, cols), yaxt = 'n', xaxt = 'n', xlab = '', plot.ci = T,
             ci.l = cbind(NA, c(NA, fail.future.CI.2[1])), ci.u = cbind(NA, c(NA, fail.future.CI.2[2])))
  } else{
    barplot2(t(fail.compare.2[ind.sort3,]), width = 0.8, xlim = c(0, 48), beside = T, ylim = c(0,40),
             density = c(NA, 40), col = rbind(cols, cols), yaxt = 'n', xaxt = 'n', xlab = '', plot.ci = T,
             ci.l = rbind(NA, fail.future.CI.2[1, ind.sort3]), ci.u = rbind(NA, fail.future.CI.2[2, ind.sort3]))
  }
  axis(side = 2, at = seq(0, 40, 5))
  abline(h = 0)
  abline(v = which(is.na(ind.sort3)) * (0.8 * 3) - 1.2, lty = 3)
  test = paste(countries.2[ind.sort3])
  test[test == 'NA'] = '' # fix NA in labels
  text(seq(0.8, length(countries.2[ind.sort3]) * 0.8 * 3, by = 0.8 * 3), par("usr")[3]-0.5, 
       srt = 55, adj = 1, xpd = TRUE,
       labels = paste(test), cex = 0.9)
  #legend('topright',c('Baseline','Future'),fill=c('black','black'),density=c(NA,40))
  
  ## LOOK AT YIELDS & YIELD CHANGES AT FPU SCALE
  # get list of countries
  FPUs = unique(c(as.character(crop_riego$New_FPU), as.character(crop_secano$New_FPU)))
  
  # Calculate weighted averages by model & compare to WFD
  area.riego = replicate(length(anos), crop_riego$riego.area)  #keep same dimension as yield matrix
  area.secano = replicate(length(anos), crop_secano$secano.area)
  
  # Aggregate to FPU scale for all models & years (across riego & secano)
  yld.FPU = array(NA, dim = c(length(FPUs), length(anos), 10))  #initialize matrices (years x country x model )
  dimnames(yld.FPU)[1] = list(FPUs)
  dimnames(yld.FPU)[2] = list(anos2) 
  dimnames(yld.FPU)[3] = list(models[1:10,])
  for(r in 1:length(FPUs)){ # loop by FPU
    
    ind.reg.r = which(crop_riego$New_FPU == FPUs[r])
    ind.reg.s = which(crop_secano$New_FPU == FPUs[r])
    print(FPUs[r])
    
    if(length(c(ind.reg.r, ind.reg.s)) >= 1){
      for(m in 1:10){
        prod_riego.m = yld_riego[ind.reg.r,,m] * area.riego[ind.reg.r,] # calculate production by pixel
        prod_secano.m = yld_secano[ind.reg.s,,m] * area.secano[ind.reg.s,]
        prod_all = rbind(prod_riego.m, prod_secano.m) # stack riego & secano
        area_all = rbind(area.riego[ind.reg.r,], area.secano[ind.reg.s,]) # same for area
        yld.FPU[r,,m] = apply(prod_all, 2, sum)/apply(area_all, 2, sum)  #sum production at country-scale & divide out by total area
      }
    }
  }
  
  # calculate interannual means in each period
  yld.FPU.m = apply(yld.FPU, c(1, 3), mean, na.rm = T)
  
  # Calculate percent change in yield between each model & WFD with bootstrapping for CI
  pct.ch.FPU = (apply(yld.FPU.m[,1:9], 1, mean) - yld.FPU.m[,10])/yld.FPU.m[,10] * 100
  boot = mat.or.vec(500, length(FPUs)) # bootstrap results
  for(j in 1:500){boot.ind = sample(1:(p-2), (p-2), replace = T)
  boot[j,] = (apply(yld.FPU.m[,boot.ind], 1, mean) - yld.FPU.m[,10])/yld.FPU.m[,10] * 100}
  pct.ch.FPU.CI = apply(boot, 2, function(x) quantile(x, c(.05, .95), na.rm = T))
  colnames(pct.ch.FPU.CI) = FPUs
  
  # Save results 
  eval(parse(text = paste("save(pct.ch.FPU, pct.ch.FPU.CI, file = './FAOSTAT_validation/country.pctCh_", crops[i], "_optVariety_FPU.Rdat')", sep = "")))
  
  # Aggregate area with crop failures to REGIONAL scale
  # first identify crop failures by pixel & year
  thresh = mean(yld_secano[,,10]) * 0.05 # 5% of mean rainfed yield is considered "failure"
  riego.fail = (yld_riego <= thresh | is.na(yld_riego)) * 1
  secano.fail = (yld_secano <= thresh | is.na(yld_secano)) * 1
  
  # Add region2 classification to crop matrices
  regions2 = c('NOR', 'CEN', 'BRA', 'AND', 'SUR')
  for(j in 1:5){
    eval(parse(text = paste('ind.', regions2[j], ' = match(crop_riego$country,', regions2[j], ')', sep = '')))
    eval(parse(text = paste('crop_riego$region2[is.na(ind.', regions2[j], ') == F] = "', regions2[j], '"', sep = '')))
    
    eval(parse(text = paste('ind.', regions2[j], ' = match(crop_secano$country,', regions2[j], ')', sep = '')))
    eval(parse(text = paste('crop_secano$region2[is.na(ind.', regions2[j], ') == F] = "', regions2[j], '"', sep = '')))
  }
  
  fail.region = array(NA, dim = c(5, length(anos), 10)) # Initialize matrices (regions x years x model)
  dimnames(fail.region)[1] = list(regions2)
  dimnames(fail.region)[2] = list(anos2) 
  dimnames(fail.region)[3] = list(models[1:10,])
  for(r in 1:5){ # loop by region
    
    ind.reg.r = which(crop_riego$region2 == regions2[r])  
    ind.reg.s = which(crop_secano$region2 == regions2[r])
    
    if(length(c(ind.reg.r, ind.reg.s)) >= 1){
      for(m in 1:10){
        
        fail_riego.m = riego.fail[ind.reg.r,,m] * area.riego[ind.reg.r,] # calculate area with crop failure
        fail_secano.m = secano.fail[ind.reg.s,,m] * area.secano[ind.reg.s,]
        fail_all = rbind(fail_riego.m, fail_secano.m) # stack riego & secano
        area_all = rbind(area.riego[ind.reg.r,], area.secano[ind.reg.s,])
        fail.region[r,,m] = apply(fail_all, 2, sum)/apply(area_all, 2, sum) * 100 # Sum area with crop failure at country-scale, divide by total area to get %
      }
    }
  }
  
  # Calculate interannual means in each period
  fail.region.m = apply(fail.region, c(1, 3), mean)
  
  # Calculate multi-model mean in future period, put with baseline
  fail.compare = cbind(fail.region.m[,10], apply(fail.region.m[,1:9], 1, mean))
  colnames(fail.compare) = c('Historical', 'Future')
  
  # calculate confidence intervals
  boot = mat.or.vec(500, length(regions2)) # bootstrap results
  for(j in 1:500){boot.ind = sample(1:(p-2), (p-2), replace = T)
  boot[j,] = apply(fail.region.m[,boot.ind], 1, mean)}
  fail.future.CI = apply(boot, 2, function(x) quantile(x, c(.05, .95), na.rm = T))
  colnames(fail.future.CI) = regions2
  
  # #order results by crop failures in baseline
  # ind.sort = order(fail.compare[,1],decreasing=T)
  
  # Save results 
  eval(parse(text = paste("save(fail.compare, fail.future.CI, file = './FAOSTAT_validation/regions.areaFail_", crops[i], "_optVariety.Rdat')", sep = "")))
  
  # Color-code regions
  colors = c('darkorange', 'yellow', 'green2', 'royalblue3', 'red3')
  
  # create barplot
  barplot2(t(fail.compare), beside = T, ylim = c(0, 20), density = c(NA, 50), col = rbind(colors, colors),
           yaxt = 'n', xaxt = 'n', xlab = '', plot.ci = T, ci.l = rbind(NA, fail.future.CI[1,]), ci.u = rbind(NA, fail.future.CI[2,]))
  axis(side = 2, at = seq(0, 20, 5))
  abline(h = 0)
  text(seq(1.5, length(regions2) * 3, by = 3), par("usr")[3]-0.25,
       srt = 60, adj = 1, xpd = TRUE,
       labels = paste(regions2), cex = 0.85)
  
  return(cat(paste0("Process done for: ", crops[i], "!\n")))
  
})
