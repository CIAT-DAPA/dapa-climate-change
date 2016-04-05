require(raster)

##validacion llanos 

##datos observados
# data.obs<-"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/monthly-interpolations/stations-averages"
# 
# prec<-read.csv(paste0(data.obs,"/rain_lla.csv"))
# tmax<-read.csv(paste0(data.obs,"/tmax_lla.csv"))
# tmin<-read.csv(paste0(data.obs,"/tmin_lla.csv"))
# 
# #tmax.sample<-sample(tmax$ID,87,replace=FALSE)
# prec.sample<-which(prec$pos==1)
# tmax.sample<-which(tmax$pos==1)
# tmin.sample<-which(tmin$pos==1)
# 
# prec.obs<-write.csv(prec[prec.sample,],"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs.csv",row.names=F)
# tmax.obs<-write.csv(tmax[tmax.sample,],"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmax_obs.csv",row.names=F)
# tmin.obs<-write.csv(tmin[tmax.sample,],"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmin_obs.csv",row.names=F)

prec.obs<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs.csv")
tmax.obs<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmax_obs.csv")
tmin.obs<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmin_obs.csv")


##datos ajustados
# data.ajus<-"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline"
# require(raster)
# prec<-stack(lapply(paste0(data.ajus,"/prec_",1:12,".asc"),raster))
# tmax<-stack(lapply(paste0(data.ajus,"/tmax_",1:12,".asc"),raster))
# tmin<-stack(lapply(paste0(data.ajus,"/tmin_",1:12,".asc"),raster))
# 
# coor.prec<-prec.obs[7:6]
# coor.tmax<-tmax.obs[7:6]
# coor.tmin<-tmin.obs[7:6]
# 
# ext.prec<-extract(prec,coor.prec)
# ext.tmax<-extract(tmax,coor.tmax)
# ext.tmin<-extract(tmin,coor.tmin)
# 
# 
# prec.aju<-write.csv(ext.prec,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus.csv",row.names=F)
# tmax.aju<-write.csv(ext.tmax,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus.csv",row.names=F)
# tmin.aju<-write.csv(ext.tmin,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus.csv",row.names=F)

prec.aju<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus.csv")
tmax.aju<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus.csv")
tmin.aju<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus.csv")

###datos worldclim
# data.ajus.w<-"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/linea_base_worldClim"
# require(raster)
# prec.w<-stack(lapply(paste0(data.ajus.w,"/prec_",1:12,".tif"),raster))
# tmax.w<-stack(lapply(paste0(data.ajus.w,"/tmax_",1:12,".tif"),raster))
# tmin.w<-stack(lapply(paste0(data.ajus.w,"/tmin_",1:12,".tif"),raster))
# 
# coor.prec<-prec.obs[7:6]
# coor.tmax<-tmax.obs[7:6]
# coor.tmin<-tmin.obs[7:6]
# 
# ext.prec.w<-extract(prec.w,coor.prec)
# ext.tmax.w<-extract(tmax.w,coor.tmax)
# ext.tmin.w<-extract(tmin.w,coor.tmin)
# 
# prec.aju.w<-write.csv(ext.prec.w,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus_wl.csv",row.names=F)
# tmax.aju.w<-write.csv(ext.tmax.w,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus_wl.csv",row.names=F)
# tmin.aju.w<-write.csv(ext.tmin.w,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus_wl.csv",row.names=F)

prec.aju.w<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus_wl.csv")
tmax.aju.w<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus_wl.csv")
tmin.aju.w<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus_wl.csv")

##########################################################################################################



### Ciclo Anual

mth = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

for(i in 1:length(prec.obs$OLD_ID)){
  
  val_prec<-(rbind(unlist(prec.aju[i,]),unlist(prec.obs[i,9:20]),unlist(prec.aju.w[i,])))
  val_prec1<-data.frame(val_prec)
  names(val_prec1)<-mth
  prec<-rbind(unlist(val_prec1[1,]),unlist(val_prec1[2,]),unlist(val_prec1[3,]))
  
  png(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/ciclo_anual/llanos/",prec.obs$OLD_ID[i],"_plot_anual.png",sep=""),width = 900, height = 700)
  
  # windows()
  par(mar=c(8,5,5,5)) 
  barplot(prec,ylim=c(0,800),bes=T,axes=F,col=c("blue","cyan","olivedrab1"))
  
  
  axis(c(1,800)  			 #primero el intervalo que tiene que cubrir
       ,at=c(seq(0,800,100))			#donde queremos las marcas
       ,labels=c(seq(0,800,100))			#que numeros pone R en las marcas
       ,side=2					#en que lado
       ,las=2					#orientación del texto  ver help(par) para todos estos
  )
  
  par(new=T)   
  plot.window(xlim=c(1,12),ylim=c(1,38))
  
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmax.obs[i,9:20],col="red",type="l",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="")
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmax.aju[i,1:12]/10,col="red",type="l",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="",lty=2)
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmax.aju.w[i,1:12]/10,col="red",type="l",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="",lty=3)
  
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmin.obs[i,9:20],col="orange",type="l",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="")
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmin.aju[i,1:12]/10,col="orange",type="l",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="",lty=2)
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmin.aju.w[i,1:12]/10,col="orange",type="l",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="",lty=3)
  
  axis(c(1,38),at=c(seq(1,38,6)),labels=c(seq(1,38,6)),side=4,outer=F,las=2)
  mtext(c("Precipitación (mm)"),side=2,line=3,at=c(19))
  
  par(xpd=T)
  text(13.1,19,c("Temperatura (°C)"),srt=270)
  
  legend("bottom",inset=-0.22,cex=0.9,ncol=3,legend=c("Temp_max estimada","Temp_max observada","Temp_max Worldclim","Temp_min estimada","Temp_min observada","Temp_min Worldclim","Prec. estimada","Prec. observada","Prec. Worldclim"),col=c("red","red","red","orange","orange","orange","blue","cyan","olivedrab1"),lwd=2,lty=c(2,1,3,2,1,3,1,1,1))
  
  
  dev.off()
}



##################
## Plot dispersion
##################
require(raster)

mth = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
bDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/monthly-interpolations/promedio"
oDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_llanos"
stDir <- "X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_llanos"
varList <- c("prec", "tmax", "tmin")

for (var in varList){

  tiff(paste(oDir, "/skill_interpolation_dispersion_chart_llanos", var, ".tiff",sep=""),width = 900, height = 1200, pointsize = 20, compression="lzw")
  
  # c(bottom, left, top, right)
  par(mfrow=c(4,3), mar=c(3, 2, 2, 2), oma=c(3, 3, 0, 0))
  
  if (var == "prec"){
    limits = c(0, 800)
    color = "blue"
    xlabel = "Observados (mm/mes)"
    ylabel = "Estimados (mm/mes)"
  } else if (var == "tmax") {
    limits = c(0, 35)
    color = "red"
    xlabel = "Observados (°C)"
    ylabel = "Estimados (°C)"
  } else if (var == "tmin") {
    limits = c(0, 35)
    color = "orange"
    xlabel = "Observados (°C)"
    ylabel = "Estimados (°C)"
  }
  
  for(i in 1:12){
    
    rs <-raster(paste0(bDir, "/", var ,"_",i,".asc"))
    st <-read.csv(paste0(stDir, "/evalue_", var, "_obs.csv"))
    obs <- unlist(st[i+8])
    val <- extract(rs,st[,7:6])
    
    m1 <- lm(val ~ obs)
    r <- summary(m1)$r.squared
    
    plot(obs, val, col=color, xlab="", ylab="", xlim=limits, ylim=limits, cex=1.5, main=mth[i])
    grid()
    abline(0,1,lty=1)
    legend("topleft",paste(expression(R2),round(r,4),sep="="), bty = "n")
    
  }
  
  mtext(c(xlabel, ylabel), c(SOUTH<-1, WEST<-2), line=1, col="black", outer=TRUE)
  
  dev.off()
  
}


##########################################################FDP trimestres

# 
# 
#   require(raster)
# 
# ###trimestres de worldclim
# p1<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(12,2,1),".tif"),raster))
# p2<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(3,4,5),".tif"),raster))
# p3<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(6,7,8),".tif"),raster))
# p4<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(9,10,11),".tif"),raster))
# 
# tma1<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(12,2,1),".tif"),raster))
# tma2<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(3,4,5),".tif"),raster))
# tma3<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(6,7,8),".tif"),raster))
# tma4<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(9,10,11),".tif"),raster))
# 
# tmi1<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(12,2,1),".tif"),raster))
# tmi2<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(3,4,5),".tif"),raster))
# tmi3<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(6,7,8),".tif"),raster))
# tmi4<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(9,10,11),".tif"),raster))
# 
#   datos1<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs.csv")
#   datos2<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmax_obs.csv")
#   datos3<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmin_obs.csv")
# #   
# #   coor1=datos1[,7:6]
# #   
# # values11<-extract(p1,coor1)
# # values12<-extract(p2,coor1)
# # values13<-extract(p3,coor1)
# # values14<-extract(p4,coor1)
# # 
# # values21<-extract(tma1,coor1)
# # values22<-extract(tma2,coor1)
# # values23<-extract(tma3,coor1)
# # values24<-extract(tma4,coor1)
# # 
# # values31<-extract(tmi1,coor1)
# # values32<-extract(tmi2,coor1)
# # values33<-extract(tmi3,coor1)
# # values34<-extract(tmi4,coor1)
# #   
# #   
# # write.csv(values11,"D:/datos11.csv",row.name=T)
# # write.csv(values12,"D:/datos12.csv",row.name=T)
# # write.csv(values13,"D:/datos13.csv",row.name=T)
# # write.csv(values14,"D:/datos14.csv",row.name=T)
# # 
# # write.csv(values21,"D:/datos21.csv",row.name=T)
# # write.csv(values22,"D:/datos22.csv",row.name=T)
# # write.csv(values23,"D:/datos23.csv",row.name=T)
# # write.csv(values24,"D:/datos24.csv",row.name=T)
# # 
# # write.csv(values31,"D:/datos31.csv",row.name=T)
# # write.csv(values32,"D:/datos32.csv",row.name=T)
# # write.csv(values33,"D:/datos33.csv",row.name=T)
# # write.csv(values34,"D:/datos34.csv",row.name=T)
#   
# data11<-read.csv("D:/datos11.csv")
# data12<-read.csv("D:/datos12.csv")
# data13<-read.csv("D:/datos13.csv")
# data14<-read.csv("D:/datos14.csv")
# 
# data21<-read.csv("D:/datos21.csv")
# data22<-read.csv("D:/datos22.csv")
# data23<-read.csv("D:/datos23.csv")
# data24<-read.csv("D:/datos24.csv")
# 
# data31<-read.csv("D:/datos31.csv")
# data32<-read.csv("D:/datos32.csv")
# data33<-read.csv("D:/datos33.csv")
# data34<-read.csv("D:/datos34.csv")
# 
# ##trimestres observados
# 
# dat11<-datos1[,c(20,9,10)]
# dat12<-datos1[,c(11,12,13)]
# dat13<-datos1[,c(14,15,16)]
# dat14<-datos1[,c(17,18,19)]
# 
# dat21<-datos2[,c(20,9,10)]
# dat22<-datos2[,c(11,12,13)]
# dat23<-datos2[,c(14,15,16)]
# dat24<-datos2[,c(17,18,19)]
# 
# dat31<-datos3[,c(20,9,10)]
# dat32<-datos3[,c(11,12,13)]
# dat33<-datos3[,c(14,15,16)]
# dat34<-datos3[,c(17,18,19)]
# 
# ################################calculo detrimestres
# #####observados
# trim11<-apply(dat11,1,sum)
# trim12<-apply(dat12,1,sum)
# trim13<-apply(dat13,1,sum)
# trim14<-apply(dat14,1,sum)
# 
# trim21<-apply(dat21,1,mean)
# trim22<-apply(dat22,1,mean)
# trim23<-apply(dat23,1,mean)
# trim24<-apply(dat24,1,mean)
# 
# trim31<-apply(dat31,1,mean)
# trim32<-apply(dat32,1,mean)
# trim33<-apply(dat33,1,mean)
# trim34<-apply(dat34,1,mean)
# 
# ######worldclim
# atrim11<-apply(data11,1,sum)
# atrim12<-apply(data12,1,sum)
# atrim13<-apply(data13,1,sum)
# atrim14<-apply(data14,1,sum)
# 
# atrim21<-apply(data21,1,mean)
# atrim22<-apply(data22,1,mean)
# atrim23<-apply(data23,1,mean)
# atrim24<-apply(data24,1,mean)
# 
# atrim31<-apply(data31,1,mean)
# atrim32<-apply(data32,1,mean)
# atrim33<-apply(data33,1,mean)
# atrim34<-apply(data34,1,mean)
#   
#   
# observados<-data.frame(trim11,trim12,trim13,trim14,trim21,trim22,trim23,trim24,trim31,trim32,trim33,trim34)
# estimados<-data.frame(atrim11,atrim12,atrim13,atrim14,atrim21,atrim22,atrim23,atrim24,atrim31,atrim32,atrim33,atrim34)
# names<-c("DJF","MAM","JJA","SON")
# 
# png(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/FDP/llanos/plot_density.png",sep=""),width = 2000, height = 1000)
# 
# #windows()
# nf <- layout(mat = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12),3,4, byrow=TRUE),  height = c(0.95,0.95))
# par(mar=c(4, 4, 3,3))
# 
# for(i in 1:4){
#   
#  
# plot(density(observados[,i],na.rm = T),col="blue",ylim=c(0,0.005),lwd=2,xlim=c(0,2000),main="",xlab="Prec")
# title(names[i])
# lines(density(estimados[,i],na.rm = T),col="blue",lty=2,lwd=2)
# grid()
# legend("topleft",legend=c("Estaciones","WorldClim"),col=c("blue","blue"),lwd=2,lty=c(1,2))
# 
# 
# plot(density(observados[,i+4],na.rm = T),col="red",lwd=2,xlim=c(0,45),ylim=c(0,0.11),main="",xlab="tmax")
# lines(density(estimados[,i+4]/10,na.rm = T),col="red",lty=2,lwd=2)
# grid()
# legend("topleft",legend=c("Estaciones","WorldClim"),col=c("red","red"),lwd=2,lty=c(1,2))
# 
# plot(density(observados[,i+8],na.rm = T),col="orange",lwd=2,xlim=c(0,45),ylim=c(0,0.11),main="",xlab="tmin")
# lines(density(estimados[,i+8]/10,na.rm = T),col="orange",lty=2,lwd=2)
# grid()
# legend("topleft",legend=c("Estaciones","WorldClim"),col=c("orange","orange"),lwd=2,lty=c(1,2))
# 
# }
# dev.off()


require(raster)

###trimestres de worldclim
p1<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(12,2,1),".tif"),raster))
p2<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(3,4,5),".tif"),raster))
p3<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(6,7,8),".tif"),raster))
p4<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/prec_",c(9,10,11),".tif"),raster))

tma1<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(12,2,1),".tif"),raster))
tma2<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(3,4,5),".tif"),raster))
tma3<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(6,7,8),".tif"),raster))
tma4<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmax_",c(9,10,11),".tif"),raster))

tmi1<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(12,2,1),".tif"),raster))
tmi2<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(3,4,5),".tif"),raster))
tmi3<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(6,7,8),".tif"),raster))
tmi4<-stack(lapply(paste0("D:/col-cormacarena/linea_base_worldClim/tmin_",c(9,10,11),".tif"),raster))


atrim11<-sum(p1)
atrim12<-sum(p2)
atrim13<-sum(p3)
atrim14<-sum(p4)

atrim21<-mean(tma1)
atrim22<-mean(tma2)
atrim23<-mean(tma3)
atrim24<-mean(tma4)

atrim31<-mean(tmi1)
atrim32<-mean(tmi2)
atrim33<-mean(tmi3)
atrim34<-mean(tmi4)

rut="X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline"

###trimestres de worldclim ajustado
op1<-stack(lapply(paste0(rut,"/prec_",c(12,2,1),".asc"),raster))
op2<-stack(lapply(paste0(rut,"/prec_",c(3,4,5),".asc"),raster))
op3<-stack(lapply(paste0(rut,"/prec_",c(6,7,8),".asc"),raster))
op4<-stack(lapply(paste0(rut,"/prec_",c(9,10,11),".asc"),raster))

otma1<-stack(lapply(paste0(rut,"/tmax_",c(12,2,1),".asc"),raster))
otma2<-stack(lapply(paste0(rut,"/tmax_",c(3,4,5),".asc"),raster))
otma3<-stack(lapply(paste0(rut,"/tmax_",c(6,7,8),".asc"),raster))
otma4<-stack(lapply(paste0(rut,"/tmax_",c(9,10,11),".asc"),raster))

otmi1<-stack(lapply(paste0(rut,"/tmin_",c(12,2,1),".asc"),raster))
otmi2<-stack(lapply(paste0(rut,"/tmin_",c(3,4,5),".asc"),raster))
otmi3<-stack(lapply(paste0(rut,"/tmin_",c(6,7,8),".asc"),raster))
otmi4<-stack(lapply(paste0(rut,"/tmin_",c(9,10,11),".asc"),raster))


oatrim11<-sum(op1)
oatrim12<-sum(op2)
oatrim13<-sum(op3)
oatrim14<-sum(op4)

oatrim21<-mean(otma1)
oatrim22<-mean(otma2)
oatrim23<-mean(otma3)
oatrim24<-mean(otma4)

oatrim31<-mean(otmi1)
oatrim32<-mean(otmi2)
oatrim33<-mean(otmi3)
oatrim34<-mean(otmi4)

ajustado<-stack(oatrim11,oatrim12,oatrim13,oatrim14,oatrim21,oatrim22,oatrim23,oatrim24,oatrim31,oatrim32,oatrim33,oatrim34)
original<-stack(atrim11,atrim12,atrim13,atrim14,atrim21,atrim22,atrim23,atrim24,atrim31,atrim32,atrim33,atrim34) 
names<-c("DEF","MAM","JJA","SON")

png(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_llanos/plot_density.png",sep=""),width = 1200, height = 1200)


nf <- layout(mat = matrix(c(1,4,7,10,2,5,8,11,3,6,9,12),3,4, byrow=TRUE),  height = c(0.95,0.95))
par(mar=c(4.6, 4, 3,3))

for(i in 1:4){
  #   a=density(ajustado[[i]],plot=FALSE)
  #   
  #   min=min(a$x)
  #   max=max(a$x)
  #   
  #   miny=min(a$y)
  #   maxy=max(a$y)
  
  #   b=density(ajustado[[i+4]],plot=FALSE)
  #   
  #   bmin=min(b$x)
  #   bmax=max(b$x)
  #   
  #   bminy=min(b$y)
  #   bmaxy=max(b$y)
  #   
  #   c=density(ajustado[[i+8]],plot=FALSE)
  #   
  #   cmin=min(c$x)
  #   cmax=max(c$x)
  #   
  #   cminy=min(c$y)
  #   cmaxy=max(c$y)
  #windows()
  density(ajustado[[i]],lty=2,lwd=2,yaxt="n",xaxt="n",col="blue",ylim=c(0,0.006),plot=T,xlim=c(0,2500),main=paste(names[i]),xlab="",ylab="",cex=2) 
   par(new=T)  
  density(original[[i]],col="blue",lwd=2,main="",ylim=c(0,0.006),xlim=c(0,2500),cex.lab=1.3,xlab="Prec (mm)",ylab="FDP",cex.axis=1.2)
  grid()
  legend("topleft",legend=c("WorldClim Ajustado","WorldClim Original"),col=c("blue","blue"),lwd=2,lty=c(2,1),bty="n",cex=1.5)
  
  
  
  density(ajustado[[i+4]]/10,lty=2,lwd=2,yaxt="n",xaxt="n",col="red",ylim=c(0,1),plot=T,xlim=c(25,35),main="",xlab="",ylab="",cex=2) 
  #title("FDP Variables bioclimaticas")
  par(new=T)  
  density(original[[i+4]]/10,col="red",lwd=2,main="",ylim=c(0,1),xlim=c(25,35),xlab="Tmax (°C)",ylab="FDP",cex.lab=1.3,cex.axis=1.2)
  grid()
  legend("topleft",legend=c("WorldClim Ajustado","WorldClim Original"),col=c("red","red"),lwd=2,lty=c(2,1),bty="n",cex=1.5)
  
  density(ajustado[[i+8]]/10,lty=2,lwd=2,yaxt="n",xaxt="n",col="orange",ylim=c(0,1),plot=T,xlim=c(15,28),main="",xlab="",ylab="",cex=2) 
  #title("FDP Variables bioclimaticas")
  par(new=T)  
  density(original[[i+8]]/10,col="orange",lwd=2,main="",ylim=c(0,1),xlim=c(15,28),xlab="Tmin (°C)",ylab="FDP",cex.lab=1.3,cex.axis=1.2)
  grid()
  legend("topleft",legend=c("WorldClim Ajustado","WorldClim Original"),col=c("orange","orange"),lwd=2,lty=c(2,1),bty="n",cex=1.5)
}
# par(xpd=T)
# legend("bottom",inset=-0.9,cex=0.9,ncol=3,legend=c("Temp_max estimada","Temp_max observada","Temp_min estimada","Temp_min observada","Prec. estimada","Prec. observada"),col=c("red","red","orange","orange","blue","lightblue"),lwd=2,pch=19,lty=c(2,1,2,1,1,1))

dev.off()



####################################################################################################
###############################################################################################
#################graficos tropico#############################
###################### ###########################
################## #######################################

##datos observados
# data.obs<-"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline_tropico/interpolation_america/monthly-interpolations/stations-averages"
# # 
# prec<-read.csv(paste0(data.obs,"/rain_ame.csv"))
# tmax<-read.csv(paste0(data.obs,"/tmax_ame.csv"))
# tmin<-read.csv(paste0(data.obs,"/tmin_ame.csv"))
# 
# #tmax.sample<-sample(tmax$ID,100,replace=FALSE)
# prec.sample<-which(prec$pos==1)
# tmax.sample<-which(tmax$pos==1)
# tmin.sample<-which(tmin$pos==1)
# 
# prec.obs<-write.csv(prec[prec.sample,],"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs1.csv",row.names=F)
# tmax.obs<-write.csv(tmax[tmax.sample,],"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmax_obs1.csv",row.names=F)
# tmin.obs<-write.csv(tmin[tmax.sample,],"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmin_obs1.csv",row.names=F)

prec.obs<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs1.csv")
tmax.obs<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmax_obs1.csv")
tmin.obs<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmin_obs1.csv")


##datos ajustados
data.ajus<-"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline_tropico/interpolation_america/monthly-interpolations/promedios_interpolation"

prec<-stack(lapply(paste0(data.ajus,"/prec_",1:12,".asc"),raster))
tmax<-stack(lapply(paste0(data.ajus,"/tmax_",1:12,".asc"),raster))
tmin<-stack(lapply(paste0(data.ajus,"/tmin_",1:12,".asc"),raster))

coor.prec<-prec.obs[6:7]
coor.tmax<-tmax.obs[6:7]
coor.tmin<-tmin.obs[6:7]

ext.prec<-extract(prec,coor.prec)
ext.tmax<-extract(tmax,coor.tmax)
ext.tmin<-extract(tmin,coor.tmin)


prec.aju<-write.csv(ext.prec,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus1.csv",row.names=F)
tmax.aju<-write.csv(ext.tmax,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus1.csv",row.names=F)
tmin.aju<-write.csv(ext.tmin,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus1.csv",row.names=F)

prec.aju<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus1.csv")
tmax.aju<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus1.csv")
tmin.aju<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus1.csv")

###datos worldclim
data.ajus.w1<-"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/linea_base_worldClim_tropico"
require(raster)
prec.w1<-stack(lapply(paste0(data.ajus.w1,"/prec_",1:12,".tif"),raster))
tmax.w1<-stack(lapply(paste0(data.ajus.w1,"/tmax_",1:12,".tif"),raster))
tmin.w1<-stack(lapply(paste0(data.ajus.w1,"/tmin_",1:12,".tif"),raster))

coor.prec<-prec.obs[6:7]
coor.tmax<-tmax.obs[6:7]
coor.tmin<-tmin.obs[6:7]

ext.prec.w1<-extract(prec.w1,coor.prec)
ext.tmax.w1<-extract(tmax.w1,coor.tmax)
ext.tmin.w1<-extract(tmin.w1,coor.tmin)

prec.aju.w1<-write.csv(ext.prec.w1,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus_wt.csv",row.names=F)
tmax.aju.w1<-write.csv(ext.tmax.w1,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus_wt.csv",row.names=F)
tmin.aju.w1<-write.csv(ext.tmin.w1,"X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus_wt.csv",row.names=F)

prec.aju.w1<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/rain_ajus_wt.csv")
tmax.aju.w1<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmax_ajus_wt.csv")
tmin.aju.w1<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/tmin_ajus_wt.csv")
##########################################################################################################



#hacemos el grafico

mth = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")


for(i in 1:length(prec.obs$OLD_ID)){
  
  val_prec<-(rbind(unlist(prec.aju[i,]),unlist(prec.obs[i,9:20]),unlist(prec.aju.w1[i,])))
  val_prec1<-data.frame(val_prec)
  names(val_prec1)<-mth
  prec<-rbind(unlist(val_prec1[1,]),unlist(val_prec1[2,]),unlist(val_prec1[3,]))
  
  png(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/ciclo_anual/tropico/",prec.obs$OLD_ID[i],"_plot_anual.png",sep=""),width = 900, height = 700)
  
  #windows()
  par(mar=c(8,5,5,5)) 
  barplot(prec,ylim=c(0,500),bes=T,axes=F,col=c("blue","cyan","olivedrab1"))
  
  
  axis(c(1,500)    		 #primero el intervalo que tiene que cubrir
       ,at=c(seq(0,500,100))			#donde queremos las marcas
       ,labels=c(seq(0,500,100))			#que numeros pone R en las marcas
       ,side=2					#en que lado
       ,las=2					#orientación del texto  ver help(par) para todos estos
  )
  
  par(new=T)   
  plot.window(xlim=c(1,12),ylim=c(1,38))
  
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmax.obs[i,9:20],col="red",type="o",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="")
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmax.aju[i,1:12],col="red",type="o",lwd=2,lty=2,yaxt="n",ylab="",xaxt="n",xlab="")
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmax.aju.w1[i,1:12]/10,col="red",type="o",lwd=3,lty=2,yaxt="n",ylab="",xaxt="n",xlab="")
  
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmin.obs[i,9:20],col="orange",type="o",lwd=2,yaxt="n",ylab="",xaxt="n",xlab="")
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmin.aju[i,1:12],col="orange",type="o",lwd=2,lty=2,yaxt="n",ylab="",xaxt="n",xlab="")
  par(new=T)
  plot(xlim=c(1,12),ylim=c(0,38),1:12,tmin.aju.w1[i,1:12]/10,col="orange",type="o",lwd=2,lty=3,yaxt="n",ylab="",xaxt="n",xlab="")
  
  
  
  axis(c(1,38),at=c(seq(1,38,6)),labels=c(seq(1,38,6)),side=4,outer=F,las=2)
  mtext(c("Precipitación (mm)"),side=2,line=3,at=c(19))
  
  par(xpd=T)
  text(13.1,19,c("Temperatura (°C)"),srt=270)
  
  legend("bottom",inset=-0.22,cex=0.9,ncol=3,legend=c("Temp_max estimada","Temp_max observada","Temp_max Worldclim","Temp_min estimada","Temp_min observada","Temp_min Worldclim","Prec. estimada","Prec. observada","Prec. Worldclim"),col=c("red","red","red","orange","orange","orange","blue","cyan","olivedrab1"),lwd=2,lty=c(2,1,3,2,1,3,1,1,1))
  #title("Comparación clima observado vs. estimado")
  dev.off()
}

##################################################diagramas de dispersion
require(raster)
mth = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

if (var == "prec"){
  limits = c(0, 800)
  color = "blue"
  xlabel = "Observados (mm/mes)"
  ylabel = "Estimados (mm/mes)"
} else if (var == "tmax") {
  limits = c(0, 35)
  color = "red"
  xlabel = "Observados (°C)"
  ylabel = "Estimados (°C)"
} else if (var == "tmin") {
  limits = c(0, 35)
  color = "orange"
  xlabel = "Observados (°C)"
  ylabel = "Estimados (°C)"
}


tiff(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_tropico/interpolation_skill_disp_tropico.tif",sep=""),width = 900, height = 1200, pointsize = 20, compression="lzw")
par(mfrow=c(4,3), mar=c(3, 2, 2, 2), oma=c(3, 3, 0, 0))

for(i in 1:12){
  
  p<-raster(paste0("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline_tropico/interpolation_america/monthly-interpolations/promedios_interpolation/prec_",i,".asc"))
  datos1<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs1.csv")
  
  coor1=datos1[,6:7]
  values1<-extract(p,coor1)
  
  write.csv(values1,"D:/datos1.csv",row.name=T)
  data1<-read.csv("D:/datos1.csv")
  
  obs1<-unlist(datos1[i+8])
  
  m1<-lm((data1$x)~obs1)
  r<-summary(m1)$r.squared
  
  plot(obs1,data1$x,col="blue",xlab="",ylab="",main=mth[i],xlim=limits, ylim=limits,cex=1.5)
  grid()
  abline(0,1,lty=1)
  legend("topleft",paste(expression(R^2),round(r,4),sep="="), bty = "n")
  
  
}
mtext(c(xlabel, ylabel), c(SOUTH<-1, WEST<-2), line=1, col="black", outer=TRUE)
dev.off()
#######################

tiff(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolation_dispersion_chart_tropicotmax.tif",sep=""),width = 900, height = 1200, pointsize = 20, compression="lzw")
par(mfrow=c(4,3), mar=c(3, 2, 2, 2), oma=c(3, 3, 0, 0))

for(i in 1:12){
  
  tma<-raster(paste0("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline_tropico/interpolation_america/monthly-interpolations/promedios_interpolation/tmax_",i,".asc"))
  datos2<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmax_obs1.csv")
  datos1<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs1.csv")
  
  coor1=datos1[,6:7]
  values2<-extract(tma,coor1)
  write.csv(values2,"D:/datos2.csv",row.name=T)
  data2<-read.csv("D:/datos2.csv")
  
  obs2<-unlist(datos2[i+8])
  
  m2<-lm((data2$x)~obs2)
  r<-summary(m2)$r.squared
  
  plot(obs2,data2$x,col="red",xlab="",ylab="",main=mth[i],xlim=limits, ylim=limits,cex=1.5)
  grid()
  abline(0,1,lty=1)
  legend("topleft",paste(expression(R^2),round(r,4),sep="="), bty = "n")
}
mtext(c(xlabel, ylabel), c(SOUTH<-1, WEST<-2), line=1, col="black", outer=TRUE)
dev.off()
#########################
tiff(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/skill_interpolacion_tropico/skill_interpolation_dispersion_chart_tropicotmin.tif",sep=""),width = 900, height = 1200, pointsize = 20, compression="lzw")
par(mfrow=c(4,3), mar=c(3, 2, 2, 2), oma=c(3, 3, 0, 0))

for(i in 1:12){
  tmi<-raster(paste0("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline_tropico/interpolation_america/monthly-interpolations/promedios_interpolation/tmin_",i,".asc"))
  datos3<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_tmin_obs1.csv")
  datos1<-read.csv("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/evalue_rain_obs1.csv")
  
  
  coor1=datos1[,6:7]
  values3<-extract(tmi,coor1)
  write.csv(values3,"D:/datos3.csv",row.name=T)
  data3<-read.csv("D:/datos3.csv")
  
  
  obs3<-unlist(datos3[i+8])
  
  m3<-lm((data3$x)~obs3)
  r<-summary(m3)$r.squared
  
  plot(obs3,data3$x,col="orange",xlab="",ylab="",main=mth[i],xlim=limits, ylim=limits,cex=1.5)
  grid()
  abline(0,1,lty=1)
  legend("topleft",paste(expression(R^2),round(r,4),sep="="), bty = "n")
}
mtext(c(xlabel, ylabel), c(SOUTH<-1, WEST<-2), line=1, col="black", outer=TRUE)
dev.off()
  

##########################################################################################################
##############comprarar bioclimaticas de worldclim con bioclimaticas ajustadas mediante las estaciones
#########################################################################################################


require(raster)
bio_ajus<-stack(lapply(paste0("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/baseline_tropico/interpolation_america/monthly-interpolations/promedios_interpolation/bio_",1:19),raster))                
bio_wclim<-stack(lapply(paste0("S:/observed/gridded_products/worldclim/Global_30s/bio_",1:19),raster))

e<-raster("C:/Users/jardila/Desktop/interpolacion precipitacion/CHIRPS/chirps-v2.0.1981.01.01.tif")
extent(e)

bio_ajus_col<-crop(bio_ajus,extent(e))
bio_wclim_col<-crop(bio_wclim,extent(e))

escala=c("(°C)","(°C)","(°C/°C)","(°C)","(°C)","(°C)","(°C)","(°C)","(°C)","(°C)","(°C)","(mm)","(mm)","(mm)","","(mm)","(mm)","(mm)","(mm)")

png(paste("X:/ALPACAS/Plan_Regional_de_Cambio_Climatico_Orinoquia/01-datos_clima/evaluaciones/FDP/bioclimaticas/_plot_density_bioclim.png",sep=""),width = 1200, height = 1600)

nf <- layout(mat = matrix(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,21,21,21),6,4, byrow=TRUE),  height = c(0.95,0.95))
par(mar=c(5,5,2,2))


par(xpd=F)

for(i in 1:19){  
  a=density(bio_wclim_col[[i]],plot=FALSE)
  
  min=min(a$x)
  max=max(a$x)
  
  miny=min(a$y)
  maxy=max(a$y)
  
  b=density(bio_ajus_col[[i]],plot=FALSE)
  
  bmin=min(b$x)
  bmax=max(b$x)
  
  bminy=min(b$y)
  bmaxy=max(b$y)
  
    
  density(bio_wclim_col[[i]],lty=2,lwd=2,yaxt="n",cex.lab=1.6,cex.axis=1.6,xaxt="n",col="blue",ylim=c(miny,max(maxy,bmaxy)),plot=T,xlim=c(min,max),main=paste("bio_", i),xlab=paste("bio_", i, escala[i],sep=" "),ylab="FDP") 
  #title("FDP Variables bioclimaticas")
  par(new=T)  
  density(bio_ajus_col[[i]],col="blue",ylab="",lwd=2,main="",xlab="",cex.lab=1.6,cex.axis=1.6,ylim=c(miny,max(maxy,bmaxy)),xlim=c(min,max))
  grid()
  
}

plot(1,axes=FALSE, xlab="", ylab="",type = "n")
#plot(1,axes=FALSE, xlab="", ylab="",type = "n")

# windows()
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
legend(x = "top",inset = 0,cex=2,legend= c("WorldClim Ajustado","WorldClim Original"),col=c("blue","blue"),lwd=2,lty=c(1,2),ncol=1,horiz=TRUE)

dev.off()



