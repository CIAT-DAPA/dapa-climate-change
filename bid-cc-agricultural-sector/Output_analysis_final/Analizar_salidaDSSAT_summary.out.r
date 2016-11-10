#Results directory
path.res = "Z:/12-Resultados/"; #'D:/BID/Resultados_DSSAT/'  #results directory
path.root = "Z:/"; #'D:/BID/'  #root path

#cultivos
cultivos = c('maiz','arroz','soya')
cultivos.en = c('Maize','Rice','Soybeans')
# c=1  #maize
# material = 'IB0011_XL45'
c=2  #rice
material = 'IR72'
# c=3  #soy
# material = 'BRS-Tracaja'

#Load pixel id's & maps
eval(parse(text=paste('load("',path.root,'08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#get list of climate models
models = read.table(paste(path.root,'/_documentos/ModelosGCM.csv',sep=''),header=T,sep=',',stringsAsFactors=F)
models = rbind(models,'WFD')
colnames(models) = 'Models'  #hack for now

#load, unlist and extract yield data to arrays (gridcells x years x models)
#initialize arrays
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,13,dim(models)[1]))  #pixels x years x variables x GCM's
yld_riego = array(NA,dim=c(dim(crop_riego)[1],30,13,dim(models)[1]))

for (m in c(1:3,11))  {  #loop por modelos
  
  print(m)
  if (m==dim(models)[1])  {
    #Load & extract baseline data from list
    load(paste(path.res,cultivos[c],'/1971-1998/_',material,'_Secano.Rdat',sep=''))
    Run.secano = Run
    load(paste(path.res,cultivos[c],'/1971-1998/_',material,'_Riego.Rdat',sep=''))
    Run.riego = Run
  } else{    
    #Load future yields from models
    load(paste(path.res,cultivos[c],'/2021-2048/_',material,'_Secano',models[m,],'.Rdat',sep=''))
    Run.secano = Run
    load(paste(path.res,cultivos[c],'/2021-2048/_',material,'_Riego',models[m,],'.Rdat',sep=''))
    Run.riego = Run
  }
  
  #unlist everything into matrices
  secano = array(NA,dim=c(length(Run.secano),30,dim(Run.secano[[1]])[2]))  #initialize arrays
  for (j in 1:length(Run.secano))  {
    secano[j,1:dim(Run.secano[[j]])[1],] = as.matrix(Run.secano[[j]])  #asumimos que toda la tabla es númerico
  }
  riego = array(NA,dim=c(length(Run.riego),30,dim(Run.secano[[1]])[2]))  #initialize arrays
  for (j in 1:length(Run.riego))  {  #se necesita pasar tablas con solo NA
    riego[j,1:dim(Run.riego[[j]])[1],] = as.matrix(Run.riego[[j]])
  }
  
  #place results in array
  yld_secano[,,,m] = secano
  yld_riego[,,,m] = riego
  
}

#agregar nombres de columnas
dimnames(yld_secano)[[2]] = 2020:2049
dimnames(yld_secano)[[3]] = colnames(Run.secano[[1]])
dimnames(yld_secano)[[4]] = models$Models

#substituir -99 con NA
yld_secano[which(yld_secano==-99)]=NA
yld_riego[which(yld_riego==-99)]=NA

#mirar tendencias en duracion
anos = c(1971:2000,2020:2049)
m=2  #fijar el modelo
var = 'NDCH'  #duracion
#var = 'HWAH'  #rendimiento
n.s = dim(yld_secano)[1]  #numero de pixels - secano
n.r = dim(yld_riego)[1]   #numero de pixels - riego

betas.s = array(NA,dim=c(n.s,1))
betas.r = array(NA,dim=c(n.r,1))
pvals.s = array(NA,dim=c(n.s,1))
pvals.r = array(NA,dim=c(n.r,1))

par(ask=T)
for (j in 1:n.s)  {  #loop through pixels
  datos = c(yld_secano[j,,var,11],yld_secano[j,,var,m])  #WFD, luego modelo
  if (sum(is.na(datos))<15)  {
    mod.rl = lm(datos~anos,x=T)
    betas.s[j] =  summary(mod.rl)$coef[2,1]  
    pvals.s[j] = summary(mod.rl)$coef[2,4]
  }
#   plot(anos,datos)  
#   if (pval<0.05) {lwd_ = 2} else{lwd_ = 1}
#   lines(mod.rl$x[,2],mod.rl$fitted,col='red',lwd=lwd_)
#   title(paste('pixel',j))
}

#analizar cambios
mean(yld_secano[,,var,11],na.rm=T)  #promedio del WFD
mean(yld_secano[,,var,m],na.rm=T)  #promedio del modelo
t.test(yld_secano[,,var,11],yld_secano[,,var,m])
t.test(betas.s)
sum(pvals.s<0.05,na.rm=T)  #numero de pixeles con cambios significativos
sum(pvals.s>=0.05,na.rm=T)  #cambios no significativos
