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

# Escoger parametro de interes
params <- c('HWAH', 'MDAT', 'PDAT', 'NDCH') #,'PDAT','EPCM') # Duracion, rendimiento y otro variables

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
  
  treat = c('riego', 'secano')  #riego o secano (which to plot)
  treat.en = c('Irrigated', 'Rainfed')
  cultivos = c('maiz', 'arroz', 'soya', 'frijol', 'trigo')
  cultivos.en = c('Maize', 'Rice', 'Soybeans', 'Bean', 'Wheat')
  anos = 1:29 # Fijar years para analizar aqui (leave extra year at end)
  anos2 = 1971:1999
  
  # Get list of climate models
  models = read.table(paste(path.root, '/_documentos/ModelosGCM.csv', sep = ''), header = T, sep = ',', stringsAsFactors = F)
  models = rbind(models, 'Historical baseline', 'Future multi-GCM average')
  p = dim(models)[1]
  colnames(models) = 'Models' #hack for now (machetazo)
  
  # Load pixel id's
  eval(parse(text = paste('load("', path.root, '/08-Cells_toRun/matrices_cultivo/version2017/', cultivos.en[i], '_riego.Rdat")', sep = '')))
  eval(parse(text = paste('load("', path.root, '/08-Cells_toRun/matrices_cultivo/version2017/', cultivos.en[i], '_secano.Rdat")', sep = '')))
  
  # Initialize data matrices
  dat_secano = array(NA, dim = c(dim(crop_secano)[1], 30, length(params), length(variedades)))  #initialize arrays
  dat_riego = array(NA, dim = c(dim(crop_riego)[1], 30, length(params), length(variedades)))
  
  # Loop through varieties
  for (v in 1:length(variedades)){
    
    # load, unlist and extract WFD yield data to arrays (gridcells x years x params)
    if(crops[i] == "Soybean"){
      load(paste(path.res, '/', crops[i], '/', carp.res.secano, '/SOY_rainfed_', variedades[v], '_WFD.Rdat', sep = ''))
    } else {
      load(paste(path.res, '/', crops[i], '/', carp.res.secano, '/', toupper(crops[i]), '_rainfed_', variedades[v], '_WFD.Rdat', sep = ''))
    }
    Run.secano = Run
    if(crops[i] == "Soybean"){
      load(paste(path.res, '/', crops[i], '/', carp.res.riego, '/SOY_irrigation_', variedades[v], '_WFD.Rdat', sep = ''))
    } else {
      load(paste(path.res, '/', crops[i], '/', carp.res.riego, '/', toupper(crops[i]), '_irrigation_', variedades[v], '_WFD.Rdat', sep = ''))
    }
    Run.riego = Run
    
    # unlist everything into matrices
    for (j in 1:length(Run.secano))  {
      if (is.null(dim(Run.secano[[j]])))  {
        dat_secano[j,,,] = NA
      }  else{
        for (p2 in 1:length(params)) {
          ind.s = as.numeric(substr(Run.secano[[j]][,1],1,4))-1970  #index based on simulation start date
          if (ind.s[1]==0)  {ind.s = ind.s +1}
          dat_secano[j,ind.s,p2,v] = as.numeric(as.character(Run.secano[[j]][,params[p2]]))
        }
      }
    }
    
    for (j in 1:length(Run.riego))  {
      if (is.null(dim(Run.riego[[j]])))  {
        dat_riego[j,,,] = NA
      }  else{
        for (p2 in 1:length(params)) {
          ind.r = as.numeric(substr(Run.riego[[j]][,1],1,4))-1970
          if (ind.r[1]==0)  {ind.r = ind.r +1}
          dat_riego[j,ind.r,p2,v] = as.numeric(as.character(Run.riego[[j]][,params[p2]]))
        }
      }
    }
  }
  
  # Check data by year
  # apply(dat_riego[,,1,3],2,function(x) sum(is.na(x)==F)) # yield for 3rd variety
  # apply(dat_secano[,,1,2],2,function(x) sum(is.na(x)==F))
  
  # Descartar los years sin datos climaticos, luego reemplaza -99 con 0 (para fallas); no debe existir fallas en DSSAT por razones tecnicas
  yld_secano = dat_secano[,anos,1,] # Incluir los indices de los years "buenos" aqui con datos por todos los pixeles
  yld_riego = dat_riego[,anos,1,]
  yld_secano[yld_secano==-99] = 0 # Reemplazar -99 con 0 para rendimiento
  yld_riego[yld_riego==-99] = 0
  fechas_secano = dat_secano[,anos,2:4,]
  fechas_riego = dat_riego[,anos,2:4,]
  fechas_secano[fechas_secano==-99] = NA # Reemplazar -99 con NA para fechas de siembra y cosecha
  fechas_riego[fechas_riego==-99] = NA
  
  # Identify best variety in historical baseline (higher mean yields & less crop failures)
  # across all varieties
  wfd.r = apply(yld_riego,c(1,3),mean,na.rm=T)  #multi-annual means
  wfd.s = apply(yld_secano,c(1,3),mean,na.rm=T)
  thresh = mean(yld_secano,na.rm=T)*0.2  #define crop failure as 20% of mean rainfed yield
  wfd.fail.r = apply(yld_riego,c(1,3),function(x) sum(x<thresh,na.rm=T))  #multi-annual means
  wfd.fail.s = apply(yld_secano,c(1,3),function(x) sum(x<thresh,na.rm=T))
  
  # Highest-yielding variety
  wfd.r.high = apply(wfd.r,1,which.max)
  wfd.s.high = apply(wfd.s,1,which.max)
  
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
  if(crops[i] == 'Bean'){ # Check highest yielding by country according to growth habit
    habit = read.table(paste0(path.root, '/_documentos/Bean_growthHabit.csv'), header = T, sep = ',')
    for(t in 1:2){
      eval(parse(text = paste("meso = match(crop_", treat[t], "$country, habit$X[habit$Mesoamerican == 1])", sep="")))
      ind.meso = which(is.na(meso) == F)
      ind.andean = which(is.na(meso))
      eval(parse(text = paste("meso.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.meso, 1:2], 1, which.max)", sep = "")))
      eval(parse(text = paste("andean.high = apply(wfd.", substr(treat[t], 1, 1), "[ind.andean, 3:4], 1, which.max)+2", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1), ".best[ind.meso] = meso.high", sep = "")))
      eval(parse(text = paste("wfd.", substr(treat[t], 1, 1),".best[ind.andean] = andean.high", sep = "")))
    }
  }
  
  # Select yield (& dates) for best variety for every pixel 
  yld_secano.2 = yld_secano # make backup with varieties
  yld_riego.2 = yld_riego
  fechas_secano.2 = fechas_secano # make backup with varieties
  fechas_riego.2 = fechas_riego
  yld_secano = array(NA, dim = c(dim(crop_secano)[1], length(anos))) # pixels x years
  yld_riego = array(NA, dim = c(dim(crop_riego)[1], length(anos)))
  fechas_secano = array(NA, dim = c(dim(crop_secano)[1], length(anos),3)) # pixels x years
  fechas_riego = array(NA, dim = c(dim(crop_riego)[1], length(anos),3)) # pixels x years x params
  
  # #to pick just one variety
  # yld_secano = yld_secano.2[,,2]
  # yld_riego = yld_riego.2[,,2]
  # fechas_secano = fechas_secano.2[,,,2]
  # fechas_riego = fechas_riego.2[,,,2]
  
  # Optimal variety
  for(k in 1:length(variedades)){
    yld_secano[wfd.s.best==k,] = yld_secano.2[wfd.s.best==k,,k]
    yld_riego[wfd.r.best==k,] = yld_riego.2[wfd.r.best==k,,k]
    fechas_secano[wfd.s.best==k,,] = fechas_secano.2[wfd.s.best==k,,,k]
    fechas_riego[wfd.r.best==k,,] = fechas_riego.2[wfd.r.best==k,,,k]
  }
  
  # Contar # de 0 en rendimiento
  riego.fallas = apply(yld_riego,1,function(x) sum(is.na(x)|x==0))
  secano.fallas = apply(yld_secano,1,function(x) sum(is.na(x)|x==0))
  
  # Filtrar pixeles con muchas fallas en WFD
  ind.bad.r = which(riego.fallas>14)
  ind.bad.s = which(secano.fallas>14)
  if (length(ind.bad.r)>0) {yld_riego = yld_riego[-ind.bad.r,]
  fechas_riego = fechas_riego[-ind.bad.r,,]
  yld_riego.2 = yld_riego.2[-ind.bad.r,,]
  crop_riego2 = crop_riego # make backup of original
  crop_riego = crop_riego[-ind.bad.r,]}
  if (length(ind.bad.s)>0) {yld_secano = yld_secano[-ind.bad.s,]
  fechas_secano = fechas_secano[-ind.bad.s,,]
  yld_secano.2 = yld_secano.2[-ind.bad.s,,]
  crop_secano2 = crop_secano # make backup of original
  crop_secano = crop_secano[-ind.bad.s,]}
  
  # Extract mode of planting & harvest months (across years)
  siembra.s = apply(fechas_secano[,,2],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
  siembra.s[sapply(siembra.s, is.null)] <- NA
  siembra.s = as.numeric(unlist(siembra.s))
  siembra.r = apply(fechas_riego[,,2],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
  siembra.r[sapply(siembra.r, is.null)] <- NA
  siembra.r = as.numeric(unlist(siembra.r))
  
  cosecha.s = apply(fechas_secano[,,1],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
  cosecha.s[sapply(cosecha.s, is.null)] <- NA
  cosecha.s = as.numeric(unlist(cosecha.s))
  cosecha.r = apply(fechas_riego[,,1],1,function(x) names(sort(table(format(strptime(x,'%Y %j'),'%m')),dec=T))[1])  
  cosecha.r[sapply(cosecha.r, is.null)] <- NA
  cosecha.r = as.numeric(unlist(cosecha.r))
  
  # shift years for harvest in next year (Feb or after)
  colnames(yld_riego) = anos2
  colnames(yld_secano) = anos2
  
  ind.harv.s = which(cosecha.s < siembra.s) # & cosecha.s>=2 
  yld_secano[ind.harv.s,2:length(anos)] = yld_secano[ind.harv.s,1:(length(anos)-1)] # put in following year when harvest month earlier than sowing
  yld_secano[ind.harv.s,1] = NA # reset first year to NA
  
  ind.harv.r = which(cosecha.r < siembra.r) # & cosecha.r>=2
  yld_riego[ind.harv.r,2:length(anos)] = yld_riego[ind.harv.r,1:(length(anos)-1)] # put in following year when harvest month earlier than sowing
  yld_riego[ind.harv.r,1] = NA # reset first year to NA
  
  # # I'M NOT SURE WITH THIS
  # # Exception for Chilean wheat!
  # ind.chile.s = which(crop_secano$country=='Chile' & cosecha.s>=11) 
  # yld_secano[ind.chile.s,2:length(anos)] = yld_secano[ind.chile.s,1:(length(anos)-1)] # put in following year when harvest in Nov/Dec
  # yld_secano[ind.chile.s,1] = NA # reset first year to NA
  
  # ind.chile.r = which(crop_riego$country=='Chile' & cosecha.r>=11) 
  # yld_riego[ind.chile.r,2:length(anos)] = yld_riego[ind.chile.r,1:(length(anos)-1)]  #put in following year when harvest in Nov/Dec 
  # yld_riego[ind.chile.r,1] = NA  #reset first year to NA
  
  # Check NA's by year again
  # apply(yld_secano,2,function(x) sum(is.na(x)==T))
  # apply(yld_riego,2,function(x) sum(is.na(x)==T))
  
  # Save out yields for comparison with Iizumi
  save(yld_riego,crop_riego,file=paste('./ylds_evaluacion/',crops[i],'_yld_pixel_riego.Rdat',sep=''))
  save(yld_secano,crop_secano,file=paste('./ylds_evaluacion/',crops[i],'_yld_pixel_secano.Rdat',sep=''))
  
  # Get list of countries
  countries = unique(c(as.character(crop_riego$country),as.character(crop_secano$country)))
  
  # Calculate weighted averages by model & compare to WFD
  area.riego = replicate(length(anos),crop_riego$riego.area)  #keep same dimension as yield matrix
  area.secano = replicate(length(anos),crop_secano$secano.area)
  
  # Aggregate to country scale for WFD in each model & year (across riego & secano)
  yld.country = array(NA,dim=c(length(anos),length(countries)))  #initialize matrices
  colnames(yld.country) = countries
  rownames(yld.country) = anos2    
  for (r in 1:length(countries))  {  #loop by country
    ind.reg.r = which(crop_riego$country==countries[r])
    ind.reg.s = which(crop_secano$country==countries[r])
    
    if (length(c(ind.reg.r,ind.reg.s))>=1)  {
      for (y in 1:dim(yld.country)[1])  { #loop through years
        ylds.y = c(yld_riego[ind.reg.r,y],yld_secano[ind.reg.s,y])
        #ylds.y[is.na(ylds.y)] = 0  #reemplazar NA con 0 for legitimate crop failures
        areas.y = c(area.riego[ind.reg.r,y],area.secano[ind.reg.s,y])
        yld.country[y,r] = sum(ylds.y*areas.y)/sum(areas.y)    
      }
    }
  }
  
  # Aggregate area by country
  secano.area = aggregate(crop_secano$secano.area,by=list(crop_secano$country),FUN=sum)
  riego.area = aggregate(crop_riego$riego.area,by=list(crop_riego$country),FUN=sum)
  area.merge = merge(secano.area, riego.area, by = "Group.1", all = TRUE)
  area.merge[is.na(area.merge)]=0
  area.merge$total = area.merge[,2]+area.merge[,3]
  colnames(area.merge) = c('country','secano','riego','total')
  
  # Save results to csv
  eval(parse(text=paste("write.csv(yld.country,file='./FAOSTAT_validation/country.ylds_",crops[i],"_optVariety_cosecha.csv')",sep="")))
  eval(parse(text=paste("write.csv(area.merge,file='./FAOSTAT_validation/country.areas_",crops[i],".csv')",sep="")))
  
  return(cat(paste0("Process done for: ", crops[i], "!\n")))
  
})
