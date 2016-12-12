library(raster)
#chirps = raster(x = "D:/Tobackup/CIAT/Projects/USAID/Data_clima/data_filling/stack_chirps.grd")
chirps_all = stack("D:/Tobackup/CIAT/Projects/USAID/Data_clima/data_filling/stack_chirps.grd")

setwd("D:\\Tobackup\\CIAT\\Projects\\USAID\\Data_clima\\data_filling\\")
dpto="tolima"
dir.create(dpto)
    
station_data = read.csv(file = paste0(dpto,"_precip_filter.csv"),header=T)
station_coord = read.csv(file = paste0(dpto,"_catalog.csv"),header=T)

station_chirps.b = raster::extract(x=chirps_all, y=station_coord[-1], method = 'bilinear')
station_chirps.b = as.data.frame(t(station_chirps.b))[1:nrow(station_data),]
names(station_chirps.b)=names(station_data)

dates=seq(as.Date("1981/01/01"),as.Date("2013/12/31"),"month")
months=months.Date(dates)
names_st=names(station_chirps.b)

dir.create(paste0(dpto,"/all"))
dir.create(paste0(dpto,"/monthly"))
setwd(paste0("D:\\Tobackup\\CIAT\\Projects\\USAID\\Data_clima\\data_filling\\",dpto,"/all/"))

add_legend <- function(...) {
       opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
               mar=c(0, 0, 0, 0), new=TRUE)
              on.exit(par(opar))
              plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
              legend(...)
   }

for (i in 1:ncol(station_chirps.b)){
  data.model = as.data.frame(cbind("y"=station_data[,i],"x"=station_chirps.b[,i]))
  model = lm(data=data.model,formula = y~x)
  rmse <- round(sqrt(mean(resid(model)^2)), 2)
  coefs <- coef(model)
  b0 <- round(coefs[1], 2)
  b1 <- round(coefs[2],2)
  r2 <- round(summary(model)$r.squared, 2)
  
  eqn <- bquote(italic(y) == .(b0) + .(b1)*italic(x) * "," ~~ 
                  R^2 == .(r2) * "," ~~ RMSE == .(rmse))
  
   
  data_model = predict(model,as.data.frame(station_chirps.b[,i]))
  data_model[data_model<0] = 0
  tiff(paste0(names_st[i],".tiff"),compression = 'lzw',height = 10,width = 10,units="in", res=200)
  par(mfrow=c(2,1))
  
  plot(dates,station_data[,i],lwd=1.5,type="l",xlab="",ylab="Precipitation (mm)",main=names_st[i])
  lines(dates,station_chirps.b[,i],col="red",lty=2,lwd=1)
  lines(dates,data_model,col="blue",lty=2)
  
    
  plot(station_data[,i],station_chirps.b[,i],xlab="Observed_stations",ylab="CHIRPS")
  abline(model,col="red")
  legend('bottomright', legend = eqn, bty = 'n')
  
  add_legend("topright",c("Observed","CHIRPS","Model"),
             horiz=T, bty='n', cex=0.9,lty=c(1,2,2),lwd=c(1.5,1,1),col=c("black","blue","red")) 
  
  dev.off()
  
  pos.na = which(is.na(station_data[,i]))
  station_data[pos.na,i] = as.numeric(data_model[pos.na])
  
}

write.csv(cbind("date" = format(dates,"%Y-%m"),station_data),paste0(dpto,"_fill.csv"),row.names = F)



######Validación Cruzada

# setwd(paste0("D:\\Tobackup\\CIAT\\Projects\\USAID\\Data_clima\\data_filling\\",dpto,"/all/"))
# dir.create("cross_validation")
# 
# cor.cv.all = matrix(NA,21,length(names_st))
# rmse.cv.all = matrix(NA,21,length(names_st))
# #for (i in 1:ncol(station_chirps.b)){
# 
#   cross_v = function(i){
#     tiff(paste0("cross_validation/",names_st[i],"_cv.tiff"),compression = 'lzw',height = 10,width = 12,units="in", res=150)
#     
#     for(j in 1:20){
#       data.model = as.data.frame(cbind("y"=station_data[,i],"x"=station_chirps.b[,i]))
#       na.sample = nrow(data.model)*sample(seq(0.01,0.2,0.01),1,replace = T)
#       na.sample = round(na.sample,0)
#       
#       pos.na = sample(1:nrow(data.model),na.sample,replace=F)
#       
#       data.model$y[pos.na] = NA
#       model = lm(data=data.model,formula = y~x)
#       
#       x = station_chirps.b[pos.na,i]
#       data_predict = predict(model,as.data.frame(x))
#       data_predict[data_predict<0] = 0
#       
#       
#       if(j==1){
#         
#         plot(station_data[pos.na,i],data_predict,xlab="Observed_stations",ylab="Fitted",main=names_st[i])
#         cor.cv = cor(station_data[pos.na,i],data_predict,use="complete.obs")
#         rmse.cv <- round(sqrt(mean((station_data[pos.na,i]-data_predict)^2,na.rm=T)), 2)
#         
#       }
#       
#       points(station_data[pos.na,i],data_predict,pch=j)
#       cor.cv = rbind(cor.cv,cor(station_data[pos.na,i],data_predict,use="complete.obs"))
#       rmse.cv <- rbind(rmse.cv, round(sqrt(mean((station_data[pos.na,i]-data_predict)^2,na.rm=T)), 2))
#       
#       
#     }
#     cor.cv.all[,i] = cor.cv
#     rmse.cv.all[,i] = rmse.cv
#     eqn <- bquote(r == .(mean(cor.cv.all[,i])) * "," ~~ RMSE == .(mean(rmse.cv.all[,i])))
#     legend('bottomright', legend = eqn, bty = 'n',cex=2)
#     dev.off()
#     
#     assign("cor.cv.all",cor.cv.all,.GlobalEnv)
#     assign("rmse.cv.all",rmse.cv.all,.GlobalEnv)
#     
#        
#       }  
#     
#   lapply(1:ncol(station_chirps.b),cross_v)
#   
#   tiff(paste0("cross_validation/rmse_cv.tiff"),compression = 'lzw',height = 5,width = 16,units="in", res=150)
#   par(mfrow=c(1,2))
#   boxplot(cor.cv.all,names=names_st,las=2,main="Correlation")
#   boxplot(rmse.cv.all,names=names_st,las=2,main="RMSE")
#   dev.off()
  
#}
  
