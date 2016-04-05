require(ggplot2)

wtgDir <- "D:/CIAT/Projects/col-cormacarena/01-datos-clima/datos_diarios/baseline/0001"
# setwd(wtgDir)
wtgLs <- list.files(wtgDir, full.names = T, pattern="\\.WTG$")


mat <- c()
wtgDt <- lapply(paste(wtgLs,sep=""), function(x){read.table(x, header=T, sep="", skip=5)})

for (i in 1:99){
  mat <- rbind(mat, wtgDt[[i]][1:365,2:5])
}

moy <- rep(c(rep(1, 31), rep(2, 28), rep(3, 31), rep(4, 30), rep(5, 31), rep(6, 30), rep(7, 31), rep(8, 31), rep(9, 30), rep(10, 31), rep(11, 30), rep(12, 31)), 99)
todo_final <- cbind(rep(1:365,99),moy, mat)


observado=data_all[which(data_all$year==año_p & data_all$month %in% 1:6),]
#desvest=aggregate(final[,4],list(final$day,final$month),sd,na.rm=T)
cuantil_up=aggregate(final[,4],list(final$day,final$month),quantile,0.8,na.rm=T)
cuantil_lo=aggregate(final[,4],list(final$day,final$month),quantile,0.1,na.rm=T)
#media=aggregate(final[,4],list(final$day,final$month),mean,na.rm=T)
#[-which(cuantil_up$Group.1==29 & cuantil_up$Group.2==2)]
todo_final=cbind(cuantil_lo[-60,],cuantil_up[-60,3],observado[,4])
# todo_final=cbind(cuantil_lo,cuantil_up[,3],observado[,4])

names(todo_final)=c("Dia","Mes","RadS","Tmax","Tmin", "Prec")

bp <- ggplot(todo_final) + 
  geom_boxplot(aes(x=Dia, y=Tmax, group=Dia), outlier.shape=NA, colour = "red") +
  geom_boxplot(aes(x=Dia, y=Tmin, group=Dia), outlier.shape=NA, colour = "orange") +
  geom_boxplot(aes(x=Dia, y=Prec, group=Dia), outlier.shape=NA, colour = "blue") +
  scale_y_continuous(limits = c(0, 50)) + 
  scale_x_continuous(limits = c(0, 366))
print(bp)
ggsave(paste0(wtgDir, "/all.png"), width = 10, height = 2, dpi = 150)



bp <- ggplot(todo_final, aes(x=Dia, y=Tmax)) + 
  geom_boxplot(aes(group=Dia), outlier.shape=NA, colour = "red") +
  scale_y_continuous(limits = c(-10, 40))
print(bp)
ggsave(paste0(wtgDir, "/tmax.png"), width = 10, height = 2, dpi = 150)




bp <- ggplot(todo_final, aes(x=Dia, y=Tmin)) + 
  geom_boxplot(aes(group=Dia), outlier.shape=NA, colour = "orange") +
  scale_y_continuous(limits = c(-10, 40))
print(bp)
ggsave(paste0(wtgDir, "/tmin.png"), width = 10, height = 2, dpi = 150)

