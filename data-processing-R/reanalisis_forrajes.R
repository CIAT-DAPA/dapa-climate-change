##########################################################################################
## Purpose: Reanalisis de encuestas ganaderas
## Author: Eliana Vallejo
##dpto Cauca
##########################################################################################
##load package
library(plyr)
library(dplyr);library(ggplot2);library(reshape2)

library(FactoMineR);library(NbClust);library(dendextend)
##end load package

##parameters
p_na<-10 #Tolerancia de NA
 
#################FUNCTIOMS########################
count_na=function(x){
  na_count <-sapply(x, function(y) round(sum(length(which(is.na(y))))/length(y)*100,2))
  na_count= data.frame(na_count)
  colnames(na_count)="P.NA"
  na_count
}
labelColors = labelColors = c('red', 'blue', 'darkgreen', 'darkgrey', 'purple')
# cut dendrogram in 4 clusters
# function to get color labels
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[datos1$groups[which(names(datos1$groups) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}
##load d

datos<-read.csv("D:/evallejo/reanalisis_forrajes/datos/datos.csv",na.strings =c("NR","NA"," ","N.R"),header=T)
na_count<-count_na(datos)
selec_var<-rownames(na_count)[na_count$P.NA<10]
datos<-datos[,selec_var]
#####
summary(datos)
##seleccion de variables por modulos; se excluyen las variables sociodemograficas, casi todos son propietarios (ej)  
#no se incluye genero y las edades son muy similares )46-6.
#la 2_2 no se incluye porque dice lo mismo que la 2_1
#######se calcula el porcentaje del area en ganaderia y p_area cultivada
datos$p_ganaderia<-datos$X2_12/datos$X2_1
datos$p_cultivada<-datos$X2_13/datos$X2_1
mod_2<-c("X2_1", "X2_3","X2_9","Plano", "Ondulado","Pendiente","p_ganaderia","p_cultivada") #todas son numericas
##el proposito se debe incluir en otro grupo
##modulo 5
datos$area_natura<-datos$Area.NRturalizada/datos$X2_1
datos$area_mejor<-datos$Area.Mejorada/datos$X2_1
mod_5<-c("adopcion","area_natura","area_mejor","X9_13", ##se incluye la x9_13 por ser numerica es el numero de animales muertos
         "X14_2_b", ##trabajadores en actividad 2 al año
         ##pasturas
         "B..decumbens","B..Brizantha","Mombaza","Estrella","Angleton","Paspalum","Caña","Puntero","Kin.grass")

###se incluye todo el modulo 7 y 8
mod_7<-colnames(datos)[grep("X7_",colnames(datos))]
mod_8<-colnames(datos)[grep("X8_",colnames(datos))]
mod_9<-c("X9_1a",  "X9_1c",  "X9_1e","X9_1k", "X9_3", "X9_4",  "X9_6",  "X9_10b", "X9_10d","X9_15a")
###modulos 15 y 16
mod_15_16<-c("colaboracion_otros","asociacion")
#modulo 17: asistencia tecnica
mod_17_18<-c("Extensionista.del.Gobierno","Técnico","No.recibe.asistencia.tècnica", "Casa.comercial","model_product","X18_1") ##asistencia tecnica

mod_19<-c("X19_1_1_a", "X19_1_1_b", "X19_1_1_d","X19_1_1_f","celu_emergen")  ##accesibilidad
mod_20<-c("X20_1_1","Ha.solicitado.alguna.vez.el.credito", "Necesita.De.Credito.informal.Para.Produccion.De.GaNRdo.Y.Forajes")
          #"Ha.solicitado.alguna.vez.el.credito.1" )  ##creditos

mod_21<-c("lluevias.fuertes.e.Inundaciones","Vientos.fuertes") ##eventos extremos
mod_22<-c( "Bomba.de.agua..Motobomba.", "Tanque.de.agua..Domestico.o.agropecuario.","Fumigadora","Establo", "Cerca.electrica" ) ##activos
selec_var<-c(mod_2,mod_5,mod_9,mod_17_18,mod_20,mod_21,mod_22)
  #c(mod_2,mod_5,mod_7, mod_8,mod_9,mod_15_16,mod_17_18,mod_19,mod_20,mod_21,mod_22)
names_group<-c("caracteristicas_finca","estructura_finca", #incluye areas, adopcion de pasturas, trabajadores, etc en escencia variables cuantitativas y generales
             "pasturas","manejo_animal","asistencia_tecnica","creditos","eventos_extremos","activos")
  
  #c("caracteristicas_finca","estructura_finca", #incluye areas, adopcion de pasturas, trabajadores, etc en escencia variables cuantitativas y generales
   #              "pasturas","practicas_culturales","estructura_hato","manejo_animal","colaboracion_asociacion","asistencia_tecnica",
    #             "disponibilidad_acceso_region","creditos","eventos_extremos","activos")
datos1=datos  
datos<-datos[,selec_var]
datos$X2_1<-log1p(datos$X2_1)
datos$X2_3<-log1p(datos$X2_3)
datos$X2_9<-log1p(datos$X2_9)
datos$X14_2_b<-log1p(datos$X14_2_b)
#datos$X2_14<-NULL
###reemplaza NA por 0 solo para las variables de respuesta binaria es decir es no hay o no responde
datos[is.na(datos)] <- 0
datos[, 14:length(datos)] <- lapply(datos[, 14:length(datos)], as.factor)
#datos<-cbind(datos[,1:13],as.factor(datos[, 14:86]))
#############################
###ANALISIS FACTORIAL
#names_group<-c("caracteristicas_finca","estructura_finca", #incluye areas, adopcion de pasturas, trabajadores, etc en escencia variables cuantitativas y generales
 #              "pasturas","practicas_culturales","estructura_hato","manejo_animal","colaboracion_asociacion","asistencia_tecnica",
  #             "disponibilidad_acceso_region","creditos","eventos_extremos","activos")

g=c(8,5,9,length(mod_9),length(mod_17_18),length(mod_20),length(mod_21),length(mod_22))
  #c(8,5,9,length(mod_7),length(mod_8),length(mod_9),length(mod_15_16),length(mod_17_18),length(mod_19),length(mod_20),length(mod_21),length(mod_22))

typ=c("s","s",rep("n",length(g)-2))
afm=MFA(datos,g,typ, name.group = names_group, ncp =1)
correlacion<-afm$group$RV
afm$eig
##caracteristicas de la finca
afm$separate.analyses$caracteristicas_finca$eig
##estructura de la finca
afm$separate.analyses$estructura_finca$eig

coordenadas=afm$ind$coord
NCluster=NbClust(coordenadas, diss=NULL, distance="euclidean", min.nc=2, max.nc=5, method="kmeans", index="ch", alphaBeale=0.1)
Num.ClusterBest=data.frame(Best=NCluster$Best.nc)
NumCluster=Num.ClusterBest[1,1]
d=dist(coordenadas)
hc <- hclust(d, method="ave")
groups<-cutree(hc, k=NumCluster)
table(groups)
##variables principales
afm$group$contrib
datos1<-cbind(datos1,groups)
####################################################################################
##############DENDOGRAMA
# load code of A2R function
clusDendro = dendrapply(as.dendrogram(hc), colLab)
colors_to_use <- datos1$groups
colors_to_use <- colors_to_use[order.dendrogram(clusDendro)]
labels_colors(clusDendro) <- colors_to_use
# Now each state has a color
labels_colors(clusDendro,col=c(3,1,1)) 
par(cex=0.8,cex.lab=1)
#pdf(paste0(path.out,z.a,"_dendograma.pdf"),width=7,height=5)
plot(clusDendro, main = "") 
#dev.off()
####################################################################################
#DESCRIPCION DE VARIABLES
###modulo 2: Caracteristicas de la finca

mod_2g<-datos1[,c(mod_2,"groups")]
mod_2g$X2_1<-log1p(mod_2g$X2_1)

mod_2g%>%
  melt(.,id.vars="groups")%>%data.frame()%>%
  ggplot(aes(x = factor(groups), y = as.numeric(value), fill = factor(groups))) +
  geom_boxplot()+
  guides(fill =FALSE)+
  facet_wrap(~ variable, scales = "free")
rm(mod_2g)
##mod5.1: Estructura de la finca
mod5.1<-datos1[,c("adopcion","area_natura","area_mejor","X9_13", ##se incluye la x9_13 por ser numerica es el numero de animales muertos
  "X14_2_b","groups")]

mod5.1%>%
  melt(.,id.vars="groups")%>%data.frame()%>%
  ggplot(aes(x = factor(groups), y = as.numeric(value), fill = factor(groups))) +
  geom_boxplot()+
  guides(fill =FALSE)+
  facet_wrap(~ variable, scales = "free")
rm(mod5.1)
#########pasturas
mod5.2<-datos1[,c( "Angleton","B..decumbens","B..Brizantha","Mombaza","Estrella","Paspalum","Caña","Puntero","Kin.grass","groups")]

  mod5.2%>%
  melt(.,id.vars="groups")%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
    facet_wrap(~ variable)
rm(mod5.2)

#################Practicas culturales
mod7<-datos1[,c(mod_7,"groups")]
mod7%>%
  melt(.,id.vars="groups")%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod7)

###############ESTRUCTURA DEL HATO
mod8<-datos1[,c(mod_8,"groups")]
mod8%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod8)

###############MANEJO ANIMAL
mod9<-datos1[,c(mod_9,"groups")]
mod9%>%
  melt(.,id.vars="groups")%>%data.frame()%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod9)

####COLABORACION Y ASOCIACION
mod_15.16<-datos1[,c(mod_15_16,"groups")]
mod_15.16%>%
  melt(.,id.vars="groups")%>%data.frame()%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod_15.16)


####COLABORACION Y ASOCIACION
mod_15.16<-datos1[,c(mod_15_16,"groups")]
mod_15.16%>%
  melt(.,id.vars="groups")%>%data.frame()%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod_15.16)
###ASISTENCIA TECNICA
mod_17.18<-datos1[,c(mod_17_18,"groups")]
mod_17.18%>%
  melt(.,id.vars="groups")%>%data.frame()%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod_17.18)

####Accesibilidad
mod19<-datos1[,c(mod_19,"groups")]
mod19%>%
  melt(.,id.vars="groups")%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod19)
####CREDITOS
mod20<-datos1[,c(mod_20,"groups")]
mod20%>%
  melt(.,id.vars="groups")%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=groups,y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod20)
###EVENTOS EXTREMOS
mod21<-datos1[,c(mod_21,"groups")]
mod21%>%
  melt(.,id.vars="groups")%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=as.factor(groups),y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod21)
####ACTIVOS
mod22<-datos1[,c(mod_22,"groups")]
mod22%>%
  melt(.,id.vars="groups")%>%
  group_by(groups,variable)%>%
  summarise_each(funs((sum(.)/length(.))*100))%>%
  data.frame()%>%
  ggplot(aes( x=as.factor(groups),y=value,fill=groups))+geom_bar(stat="identity")+
  guides(fill =FALSE)+
  facet_wrap(~ variable)

rm(mod22)

