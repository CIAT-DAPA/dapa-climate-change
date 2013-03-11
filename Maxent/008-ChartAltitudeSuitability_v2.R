require(splines)

altitude <- read.csv("D:/CIAT/Articles/Maxent_Nicaragua/occurrence_files/alt.csv")
baseline <- read.csv("D:/CIAT/Articles/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/crossval/coffea_arabica_bsl_avg.csv")
list_gcm <- list.files("D:/CIAT/Articles/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/projections/extracts/",full.names=T,pattern=".csv")
area <- read.csv("D:/CIAT/Articles/Maxent_Nicaragua/occurrence_files/altitude_vs_area.csv")

################
##Data GCM EMN##
################

gcm_data <- lapply(list_gcm,FUN=read.csv)
table_gcm <- gcm_data[[1]]

for(i in 2:length(gcm_data)){
  table_gcm <- cbind(table_gcm,gcm_data[[i]][,3])
  names(table_gcm)[dim(table_gcm)[2]] <- names(gcm_data[[i]])[3]
}

table_gcm <- cbind(table_gcm, baseline[,3])
names(table_gcm)[length(names(table_gcm))] <- "baseline"


for (i in 0:24){
  
  bsl_i <- read.csv(paste("D:/CIAT/Articles/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/crossval/", spID, "_", i, "_Baseline.csv", sep=""))
  table_gcm <- cbind(table_gcm, bsl_i[,3])
  names(table_gcm)[length(names(table_gcm))] <- paste("bsl_", i, sep="")

}


table_gcm <- cbind(table_gcm, altitude[,4])
names(table_gcm)[length(names(table_gcm))] <- "alt"


breaks <- seq(0,1400,50)
gcm_EMN <- matrix(0,dim(table_gcm)[2],length(breaks)-1)
iter <- length(breaks) - 1

for(i in 1:iter){
  # gcm_EMN[,i]=apply(subset(table_gcm_EMN,table_gcm_EMN$alt>breaks[i]&table_gcm_EMN$alt<=breaks[i+1]),2,mean)}
  gcm_EMN[,i] <- apply(subset(table_gcm, table_gcm$alt > breaks[i] & table_gcm$alt <= breaks[i+1]), 2, mean, na.rm=T)
}

gcm_EMN = t(gcm_EMN)
gcm_EMN = data.frame(gcm_EMN)
names(gcm_EMN) = names(table_gcm)
 

# ################
# ##Data GCM ESD##
# ################
# 
# list_gcm_ESD <- list.files("D:/CIAT/Articles/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/projections/extracts/ESD/",full.names=T,pattern="ESD.csv")
# gcm_data_ESD <- lapply(list_gcm_ESD,FUN=read.csv)
# 
# table_gcm_ESD <- gcm_data_ESD[[1]]
# 
# for(i in 2:length(gcm_data_ESD)){
#   table_gcm_ESD <- cbind(table_gcm_ESD,gcm_data_ESD[[i]][,3])
#   names(table_gcm_ESD)[dim(table_gcm_ESD)[2]] <- names(gcm_data_ESD[[i]])[3]
# }
# 
# table_gcm_ESD <- cbind(table_gcm_ESD,altitude[,4])
# names(table_gcm_ESD)[length(names(table_gcm_ESD))] <- "alt"
# 
# breaks <- seq(0,1400,50)
# gcm_ESD <- matrix(0,dim(table_gcm_ESD)[2],length(breaks)-1)
# 
# for(i in 1:iter){
#   gcm_ESD[,i]=apply(subset(table_gcm_ESD,table_gcm_ESD$alt>breaks[i]&table_gcm_ESD$alt<=breaks[i+1]),2,mean)
# }
# 
# gcm_ESD=t(gcm_ESD)
# gcm_ESD=data.frame(gcm_ESD)
# names(gcm_ESD)=names(table_gcm_ESD)

#########################
##Adjustinh EMN and ESD##
#########################

gcm_EMN[gcm_EMN=="NaN"] <- 0
# gcm_ESD[gcm_ESD=="NaN"] <- 0

gcm <- cbind(gcm_EMN,apply(gcm_EMN[,3:477],1,sd))
names(gcm)[length(names(gcm))] <- "std"
gcm <- cbind(gcm,apply(gcm_EMN[,3:477],1,mean))
names(gcm)[length(names(gcm))] <- "avg"
gcm <- rbind(gcm,0)


###########
##Graphic##
###########

gcm[gcm=="NaN"]=0
breaks_mod <- seq(25,1425,50)
gcm$alt <- breaks_mod

tiff(paste("D:/CIAT/Articles/Maxent_Nicaragua/others/fig-4-suit-alt-mod.tif", sep=""),width=1024, height=768,pointsize=8,compression='lzw',res=150)

par(mar=c(4,4,1,4)+ 0.1)

plot(gcm$alt,gcm$bccr_bcm2_0,type="l",xaxt="n", yaxt="n",xlim=c(0,1400),ylim=c(0,0.6),col="white",xlab="Altitude (m)", ylab="Suitability (dec)")

axis(1, cex.axis=0.9)
axis(2, cex.axis=0.9)

pos_models <- which(names(gcm) == c("alt"))-1

for(i in 3:pos_models){
  
  smooth <- predict(interpSpline(gcm$alt, gcm[,i]))
  smooth$y[smooth$y<0]=0
  lines(smooth,col="gray")
}


for(n in 479:503){
  
  smooth <- predict(interpSpline(gcm$alt, gcm[,n]))
  smooth$y[smooth$y<0]=0
  lines(smooth,col="lightpink3")
}


smooth=predict(interpSpline(gcm$alt,gcm$avg))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=2)

smooth=predict(interpSpline(gcm$alt,gcm$avg+gcm$std))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=2,lty=3)

smooth=predict(interpSpline(gcm$alt,gcm$avg-gcm$std))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=2,lty=3)


smooth=predict(interpSpline(gcm$alt,gcm$baseline))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=2,col="red")
grid()


par(new=TRUE)

plot(area$alt, area$area / 1000, col="green4", lwd=3, axes=F, ylim=c(0,4000), ann=FALSE, type="l")
axis(4, cex.axis=1, ylim=c(0,4000), las=0, cex.axis=0.9)
par(las=0)
mtext("Area (Km^2)", side = 4, line = 3)

legend(150, 4050, c("Current", "Avg Current", "GCMs SRES A2", "Avg GCMs SRES A2", "Avg +/- Stdv GCMs SRES A2", "Altitude Area"),
       lty=c(1,1,1,1,3,1),col=c("lightpink3","red","gray","black","black","green4"),cex=1,
       lwd=c(1,2,1,2,2,2),box.lty=0,bg="transparent") 



dev.off()
