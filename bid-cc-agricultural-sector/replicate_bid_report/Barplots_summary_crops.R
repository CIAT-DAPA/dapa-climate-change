# Across-crop comparison with error bars
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
suppressMessages(library(gplots))

# static data
crops <- c('Maize', 'Rice', 'Soybean', 'Bean', 'Wheat')
cultivos = c('maiz','arroz','trigo','soya','frijol')
cultivos.en = c('maize','rice','wheat','soybean','dry bean')

# Load input data
for(i in 1:length(crops)){
  eval(parse(text = paste("load('./summaries/", crops[i], ".ch.r.Rdat')", sep = "")))
  eval(parse(text = paste("load('./summaries/", crops[i], ".ch.s.Rdat')", sep = "")))
  eval(parse(text = paste("load('./summaries/", crops[i], ".cv.r.Rdat')", sep = "")))
  eval(parse(text = paste("load('./summaries/", crops[i], ".cv.s.Rdat')", sep = "")))
  eval(parse(text = paste("load('./summaries/", crops[i], ".fail.r.Rdat')", sep = "")))
  eval(parse(text = paste("load('./summaries/", crops[i], ".fail.s.Rdat')", sep = "")))
}
regions = dimnames(Rice.ch.r)[[2]]

# Merge data frames by region & make plots
# Plot mean
par(ask = F)
for(r in 1:5){
  
  crop.all = data.frame(arroz.r = Rice.ch.r[,r], arroz.s = Rice.ch.s[,r], frijol.r = Bean.ch.r[,r], frijol.s = Bean.ch.s[,r], trigo.r = Wheat.ch.r[,r], trigo.s = Wheat.ch.s[,r], maiz.r = Maize.ch.r[,r], maiz.s = Maize.ch.s[,r], soya.r = Soybean.ch.r[,r], soya.s = Soybean.ch.s[,r])
  crop.all = as.matrix(crop.all)
  
  # Create barplot with errorbars
  barplot2(crop.all[3,], plot.ci = T, ci.l = crop.all[6,], ci.u = crop.all[7,])
  title(paste('Area-weighted yield changes for ', regions[r], sep = ''))
}

# CV plots
for(r in 1:5){
  
  crop.cv = data.frame(arroz.r = Rice.cv.r[,r], arroz.s = Rice.cv.s[,r], frijol.r = Bean.cv.r[,r], frijol.s = Bean.cv.s[,r], trigo.r = Wheat.cv.r[,r], trigo.s = Wheat.cv.s[,r], maiz.r = Maize.cv.r[,r], maiz.s = Maize.cv.s[,r], soya.r = Soybean.cv.r[,r], soya.s = Soybean.cv.s[,r])
  crop.cv = as.matrix(crop.cv)
  
  # Create barplot with errorbars
  barplot2(crop.cv[1:2,], beside = T, plot.ci = T, ci.l = rbind(NA, crop.cv[5,]), ci.u = rbind(NA, crop.cv[6,]))
  title(paste('Inter-annual CV changes for ', regions[r], sep = ''))
  
}

# Now merge data frames by crop system & make plots
# Plot mean
par(ask = F)
for(i in 1:5){
  
  eval(parse(text = paste("crop.ch.c = rbind(", crops[i], ".ch.r[c(3, 6:8),],", crops[i], ".ch.s[c(3,6:8),])", sep = "")))
  rownames(crop.ch.c)[c(1, 5)] = c('Riego', 'Secano')
  ind.low.r = which(crop.ch.c[4,] < 5000)
  ind.low.s = which(crop.ch.c[8,] < 5000)
  if(length(ind.low.r) > 0){crop.ch.c[1:3, ind.low.r] = NA}
  if(length(ind.low.s) > 0){crop.ch.c[5:7, ind.low.s] = NA}
  
  # Create barplot with errorbars
  barplot2(crop.ch.c[c(1,5),], beside = T, ylim = c(-30, 30), plot.ci = T, ci.l = crop.ch.c[c(2, 6),], ci.u = crop.ch.c[c(3, 7),], col = c('blue', 'green'), ylab = '% yield change')
  abline(h = 0)
  title(paste(crops[i], sep = ''))
  if(i == 5){legend('topright', c('Irrigated', 'Rainfed'), fill = c('blue', 'green'))}
  
}

# CV plots
par(ask = F)
for(i in 1:5){
  
  eval(parse(text = paste("crop.cv.c = rbind(", crops[i], ".cv.r[c(1:2, 5:6),],", crops[i],".cv.s[c(1:2, 5:6),])", sep = "")))
  eval(parse(text = paste("crop.ch.c = rbind(", crops[i], ".ch.r[c(3, 6:8),],", crops[i], ".ch.s[c(3, 6:8),])", sep = "")))
  ind.low.r = which(crop.ch.c[4,] < 5000)
  ind.low.s = which(crop.ch.c[8,] < 5000)
  if(length(ind.low.r) > 0){crop.cv.c[1:4, ind.low.r] = NA}
  if(length(ind.low.s) > 0){crop.cv.c[5:8, ind.low.s] = NA}
  
  # Create barplot with errorbars
  barplot2(crop.cv.c[c(1:2, 5:6),], beside = T, ylim = c(0, 45), plot.ci = T, ci.l = rbind(NA, crop.cv.c[3,], NA, crop.cv.c[7,]), ci.u = rbind(NA, crop.cv.c[4,], NA, crop.cv.c[8,]), col = c('blue','blue','green','green'), density = c(-1, 30, -1, 30), ylab = 'Interannual CV (%)')
  title(paste(crops[i], sep = ''))
  
  legend('top', c('Irrigated - Baseline', 'Irrigated - Multi-model mean', 'Rainfed - Baseline', 'Rainfed - Multi-model mean'), fill = c('blue', 'blue', 'green', 'green'), density = c(-1, 50, -1,50))
  
}

# Crop failures plots
for(i in 1:5){
  
  eval(parse(text = paste("crop.fail.c = rbind(", crops[i], ".fail.r[c(1:2, 5:6),],", crops[i], ".fail.s[c(1:2, 5:6),])", sep = "")))
  eval(parse(text = paste("crop.ch.c = rbind(", crops[i], ".ch.r[c(3, 6:8),],", crops[i], ".ch.s[c(3, 6:8),])", sep = "")))
  ind.low.r = which(crop.ch.c[4,] < 5000)
  ind.low.s = which(crop.ch.c[8,] < 5000)
  if(length(ind.low.r) > 0){crop.cv.c[1:4, ind.low.r] = NA}
  if(length(ind.low.s) > 0){crop.cv.c[5:8, ind.low.s] = NA}
  
  # Create barplot with errorbars
  barplot2(crop.fail.c[c(1:2, 5:6),], beside = T, ylim = c(0, 20), plot.ci = T, ci.l = rbind(NA, crop.fail.c[3,], NA, crop.fail.c[7,]), ci.u = rbind(NA, crop.fail.c[4,], NA, crop.fail.c[8,]), col = c('blue', 'blue', 'green', 'green'), density = c(-1, 30, -1, 30), ylab = 'Crop failures (% area)')
  title(paste(crops[i], sep = ''))
  if(i == 5){
    legend('topright', c('Irrigated - Baseline', 'Irrigated - Multi-model mean', 'Rainfed - Baseline', 'Rainfed - Multi-model mean'), fill = c('blue', 'blue', 'green', 'green'), density = c(-1, 50, -1, 50))
  }
  
}

# Create plot comparing crops across Latin America
crop.all.r = data.frame(maiz.r = Maize.ch.r[,6], arroz.r = Rice.ch.r[,6], trigo.r = Wheat.ch.r[,6], frijol.r = Bean.ch.r[,6], soya.r = Soybean.ch.r[,6])
crop.all.s = data.frame(maiz.s = Maize.ch.s[,6], arroz.s = Rice.ch.s[,6], trigo.s = Wheat.ch.s[,6], frijol.s = Bean.ch.s[,6], soya.s = Soybean.ch.s[,6])
crop.all = rbind(as.matrix(crop.all.r), as.matrix(crop.all.s))
colnames(crop.all) = c('Maize', 'Rice', 'Wheat', 'Dry Bean', 'Soybean')
barplot2(crop.all[c(3, 11),], beside = T, plot.ci = T, ci.l = crop.all[c(6, 14),], ci.u = crop.all[c(7, 15),], col = c('blue', 'green'), ylab = '% yield change')
title('Area-weighted yield changes for Latin America')
legend('topleft', c('Irrigated', 'Rainfed'), fill = c('blue', 'green'))

# Generate results for table
round(Maize.cv.r, 2)
Maize.cv.r[2, 6] - Maize.cv.r[1, 6] # MMM - WFD
#c(maiz.cv.r[3,6] - maiz.cv.r[1,6],maiz.cv.r[4,6] - maiz.cv.r[1,6])  #range of models relative to WFD
c(Maize.cv.r[5, 6] - Maize.cv.r[1, 6], Maize.cv.r[6, 6] - Maize.cv.r[1, 6])  #bootstrapped changes

round(Maize.cv.s,2)
Maize.cv.s[2,6] - Maize.cv.s[1,6]
#c(maiz.cv.s[3,6] - maiz.cv.s[1,6],maiz.cv.s[4,6] - maiz.cv.s[1,6])  #range
c(Maize.cv.s[5,6] - Maize.cv.s[1,6], Maize.cv.s[6,6] - Maize.cv.s[1,6])  #intervalos de confianza

# Check to see if mean change > CV & significant
abs(Maize.ch.r[3,]) > Maize.cv.r[1,] & Maize.ch.r[8,] > 5000 & (sign(Maize.ch.r[6,]) == sign(Maize.ch.r[7,]))
abs(Rice.ch.r[3,]) > Rice.cv.r[1,] & Rice.ch.r[8,] > 5000 & (sign(Rice.ch.r[6,]) == sign(Rice.ch.r[7,]))
abs(Soybean.ch.r[3,]) > Soybean.cv.r[1,] & Soybean.ch.r[8,] > 5000 & (sign(Soybean.ch.r[6,]) == sign(Soybean.ch.r[7,]))

abs(Maize.ch.s[3,]) > Maize.cv.s[1,] & Maize.ch.s[8,] > 5000 & (sign(Maize.ch.s[6,]) == sign(Maize.ch.s[7,]))
abs(Rice.ch.s[3,]) > Rice.cv.s[1,] & Rice.ch.s[8,] > 5000 & (sign(Rice.ch.s[6,]) == sign(Rice.ch.s[7,]))
abs(Soybean.ch.s[3,]) > Soybean.cv.s[1,] & Soybean.ch.s[8,] > 5000 & (sign(Soybean.ch.s[6,]) == sign(Soybean.ch.s[7,]))

# look for sig changes in CV
((Maize.cv.r[2,] - Maize.cv.r[1,]) > 5 & Maize.cv.r[5,] > Maize.cv.r[1,] & Maize.ch.r[8,] > 5000) 
((Rice.cv.r[2,] - Rice.cv.r[1,]) > 5 & Rice.cv.r[5,] > Rice.cv.r[1,] & Rice.ch.r[8,] > 5000)
((Soybean.cv.r[2,] - Soybean.cv.r[1,]) > 5 & Soybean.cv.r[5,] > Soybean.cv.r[1,] & Soybean.ch.r[8,] > 5000)

((Maize.cv.s[2,] - Maize.cv.s[1,]) > 5 & Maize.cv.s[5,] > Maize.cv.s[1,] & Maize.ch.s[8,] > 5000)
((Rice.cv.s[2,] - Rice.cv.s[1,]) > 5 & Rice.cv.s[5,] > Rice.cv.s[1,] & Rice.ch.s[8,] > 5000)
((Soybean.cv.s[2,] - Soybean.cv.s[1,]) > 5 & Soybean.cv.s[5,] > Soybean.cv.s[1,] & Soybean.ch.s[8,] > 5000)
