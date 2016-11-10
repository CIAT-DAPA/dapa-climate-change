#Analizar tendencias en fecha de siembra, fecha de anthesis, duración, índice de cosecha

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

#Load pixel id's 
eval(parse(text=paste('load("',path.root,'08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_riego.Rdat")',sep='')))
eval(parse(text=paste('load("',path.root,'08-Cells_toRun/matrices_cultivo/',cultivos.en[c],'_secano.Rdat")',sep='')))

#get list of climate models
models = read.table(paste(path.root,'_documentos/ModelosGCM.csv',sep=''),header=T,sep=',',stringsAsFactors=F)
colnames(models) = 'Models'  #hack for now

#load, unlist and extract yield data to arrays (gridcells x years x models)
#initialize arrays
yld_secano = array(NA,dim=c(dim(crop_secano)[1],30,9,dim(models)[1]))  #30 years x 9 variables 

for (m in 1:(dim(models)[1]))  {  #loop through GCM's
  
  print(m)
    
  #Load future yields from models
  load(paste(path.res,cultivos[c],'/2021-2048/_',material,'_Secano',models[m,],'.Rdat',sep=''))
  Run.secano = Run
  
  #unlist everything into matrices
  secano = array(NA,dim=c(length(Run.secano),30,9))  #initialize arrays
  for (j in 1:length(Run.secano))  {
    if (is.null(dim(Run.secano[[j]])))  {
      secano[j,,] = 0
    }  else{
      secano[j,1:dim(Run.secano[[j]])[1],6] = Run.secano[[j]][,8]  #look at harvest index (6) for now
    }
  }
  secano[secano==-99] = NA
  secano[secano==9999999] = NA
  
  #place results in array
  yld_secano[,,,m] = secano
  
}

#Calculate time trends per model and pixel
pix = dim(yld_secano)[1]
HI = yld_secano[,,6,]
yrs = 1:30
betas = array(NA,dim=c(pix,10))
pval = array(NA,dim=c(pix,10))
for (p in 1:dim(HI)[1]) {
  for (m in 1:10) {
    dat = HI[p,,m]
    if (sum(is.na(dat)==F)>3) {
      mod = lm(dat~yrs)
      betas[p,m] = summary(mod)$coef[2,1]
      pval[p,m] = summary(mod)$coef[2,4]
    }
  }
}

#Calculate mean % changes by model (over all pixels)
pct.ch = apply(betas*30,2,function(x) mean(x,na.rm=T))/apply(HI,3,function(x) mean(x,na.rm=T))*100
barplot(pct.ch)

#Calculate time trends by pixel for multi-model mean
pix = dim(yld_secano)[1]
HI = apply(yld_secano[,,6,],c(1,2),mean)
yrs = 1:30
betas.m = array(NA,dim=c(pix))
pval.m = array(NA,dim=c(pix))
for (p in 1:dim(HI)[1]) {
    dat = HI[p,]
    if (sum(is.na(dat)==F)>3) {
      mod = lm(dat~yrs)
      betas.m[p] = summary(mod)$coef[2,1]
      pval.m[p] = summary(mod)$coef[2,4]
    }
}

sum(betas.m>0,na.rm=T)
sum(betas.m<0,na.rm=T)
sum(betas.m>0 & pval.m<.05,na.rm=T)/pix*100
sum(betas.m<0 & pval.m<.05,na.rm=T)/pix*100
