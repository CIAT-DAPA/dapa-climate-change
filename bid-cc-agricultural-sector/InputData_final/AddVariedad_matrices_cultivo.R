#Agregar columnas a las matrices de cultivo de variedades

#load libraries
library('xlsx')

#Static data
cultivos = c('Maize','Potatoes','Rice','Soybeans','Wheat')
treat = c('riego','secano')
regions = c('AND','BRA','CEN','MEX','SUR')
path <- "Z:/08-Cells_toRun/matrices_cultivo/"

#cargar variedades
#variedades.riego = read.xlsx('Z:/_documentos/Cultivares_porRegion2_R.xlsx',sheetName='riego',header=T,stringsAsFactors=F)
#variedades.secano = read.xlsx('Z:/_documentos/Cultivares_porRegion2_R.xlsx',sheetName='secano.myles',header=T,stringsAsFactors=F)
variedades.riego = read.csv('Z:/_documentos/Cultivares_porRegion2_R_riego.csv',header=T,stringsAsFactors=F)
variedades.secano = read.csv('Z:/_documentos/Cultivares_porRegion2_R_secano.csv',header=T,stringsAsFactors=F)

# Load crop pixel data
for (c in 1:5)  {  #loop through crops
  for (t in 1:2)  {  #loop through treatments
    
    # Organizing data to identify FPU categories for all coordinates
    load(paste0(path,cultivos[c],"_",treat[t],".Rdat"))
    ind.var = grep(cultivos[c],variedades.riego$cultivares,ignore.case=T)  #filas para cultivo c
    
    #Crear columnas con variedad
    eval(parse(text=paste("crop_",treat[t],"$variedad.1 = NA",sep="")))
    eval(parse(text=paste("crop_",treat[t],"$variedad.2 = NA",sep="")))
    eval(parse(text=paste("crop_",treat[t],"$variedad.3 = NA",sep="")))
    
    for (r in 1:5)  {  #loop through regions
      eval(parse(text=paste("crop_",treat[t],"$variedad.1[which(crop_",treat[t],"$Region==regions[r])] = variedades.",treat[t],"[ind.var[1],regions[r]]",sep="")))
      eval(parse(text=paste("crop_",treat[t],"$variedad.2[which(crop_",treat[t],"$Region==regions[r])] = variedades.",treat[t],"[ind.var[2],regions[r]]",sep="")))
      eval(parse(text=paste("crop_",treat[t],"$variedad.3[which(crop_",treat[t],"$Region==regions[r])] = variedades.",treat[t],"[ind.var[3],regions[r]]",sep="")))
    }
    
    #Guardar resultados
    eval(parse(text=paste("save(crop_",treat[t],", file='",path,cultivos[c],"_",treat[t],".RDat')",sep="")))
    
  }
}
