##################################################################################
#                          Embrapa Arroz e Feij?o                                #
#                          Autor: Danillo Santana                                #
#                              Ano  : 2013                                       #
#                                  Oryza                                         #
#                                                                                #
#OBS: As datas devem estar no formato ano-mes-dia. O arquivo de entrada deve     #
#ser CSV,separado por ';',  e conter as colunas dispostas na seguinte ordem:     #
#                                                                                #
#id_estacoes,data,radiacao,temp_max,temp_min,precipitacao,radiacao_pot,latitude, #                                                                        #
#longitude,uf,municipio,temp_max_f,temp_min_f,precipitacao_f,radiacao_f,altitude #
#                                                                                #
#                                                                                #
##################################################################################

#------------------------Cria Arquivo---------------------------------------------------

criarArquivo<-function(nome,ano){
  nome<-gsub(" ","_",nome)
  return<-file(paste(nome,"1.",ano,sep=""),"w")
}  
#-------------------------Escreve o cabecalho no arquivo--------------------------------
cabecalho<-function(arquivo,ano,estacao,municipio,latitude,longitude,altitude){
  cat("*--------------------------------------------------------\n",file=arquivo)  
  writeLines(paste("*Station Name:",estacao,sep=""),arquivo)
  cat("*Author: ...\n*Source: Embrapa   Arroz e Feijao/year ",ano,"\n*\n",file=arquivo)
  cat("*Longitude: ",longitude,"(DD) Latitude: ",latitude," (DECIMAL DEGREES) Altitude: ",altitude,"\n",file=arquivo)
  cat("*Column\tDaily Value","\n",file=arquivo)
  cat("*\t1\tStation number\n*\t2\tYear\n*\t3\tDay\n*\t4\tirradiance\t\tKJ m-2 d-1\n*\t5\ttmin temperature\toC\n*\t6\ttmax temperature\toC",ano,"\n",file=arquivo)
  cat("*\t7\tvapor pressure\t\tkPa\n*\t8\tmean wind speed\t\tm s-1\n*\t9\tprecipitation\t\tmm d-1\n",file=arquivo)
  cat("*--------------------------------------------------------\n",file=arquivo)
  cat(longitude,latitude,altitude,"0.00,0.00\n",file=arquivo,sep=",")
}

#-------------------------------
consultaDados<-function(arquivao){
  dados<-read.table(arquivao, header=T, sep=';',dec='.')
  return<-dados
}
#------------------------Consulta para pegar dados da estacao-------------------------------
gerarArquivos<-function(){
  
  dados<-consultaDados(arquivao)
  estacoes<-unique(dados$id_estacoes)
  dataInicial<-min(as.Date(dados$data,"%Y-%m-%d"))
  dataFinal<-max(as.Date(dados$data,"%Y-%m-%d"))
  
  for(i in 1:length(estacoes)){
      linha<-subset(dados,dados$id_estacoes == estacoes[i])
      municipio<-unique(linha$municipio)
      tmpDir<- paste(getwd(),"/",municipio[1],"/",sep="")
      tmpDir<-gsub("/","\\\\",tmpDir)
      tmpDir<-gsub(" ","_",tmpDir)
      dir.create(tmpDir)#criando diretorio
      estacao<-estacoes[i]
      print(paste(municipio[1]) )
      p=as.integer(format(as.Date(dataInicial,"%Y-%m-%d"),"%Y"))
      while(p<=as.integer(format(as.Date(dataFinal,"%Y-%m-%d"),"%Y"))){
        arquivo<-criarArquivo(paste(tmpDir,substring(municipio[1],1,5),sep=""),substring(p,2,4))
        cabecalho(arquivo,p,estacao,municipio[1],linha$latitude[1],linha$longitude[1],linha$altitude[1])
        s=1
        while(s<=length(linha$data)){
          if( format(as.Date(linha$data[s], "%Y-%m-%d"),"%Y")== p){
            writeLines(paste("1",format(as.Date(linha$data[s], "%Y-%m-%d"),"%Y"),as.integer(format(as.Date(linha$data[s], "%Y-%m-%d"),"%j")),linha$radiacao[s]*1000,linha$temp_min[s],linha$temp_max[s],"-99","-99",linha$precipitacao[s],sep=','),arquivo)
          }    
          s<-s+1
        }
        close(arquivo)
        p<-p+1 
    }
    cat("\n")
  }
}  


#-------------------------Listando estacoes do Estado Informado---------------------------

#start<-function(){
#  gerarArquivos()
#}

#-------------------------Informe as Configuracao-------------------------------------
#arquivao="I:\\area_publica\\Danillo\\Reruns_2\\ArquivaoTO.csv" #arquivo de entrada
#setwd('I:\\area_publica\\Danillo\\Reruns_2\\')  #pasta de trabalho



#-------------------------Execucao---------------------------------------------
#start()





