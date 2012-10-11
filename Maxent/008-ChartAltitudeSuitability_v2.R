################
##Data GCM EMN##
################

list_gcm_EMN=list.files("D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_1/projections/averages/extracts1/gcm/",full.names=T,pattern="EMN.csv")
gcm_data_EMN=lapply(list_gcm_EMN,FUN=read.csv)

table_gcm_EMN=gcm_data_EMN[[1]]
for(i in 2:length(gcm_data_EMN)){
table_gcm_EMN=cbind(table_gcm_EMN,gcm_data_EMN[[i]][,3])
names(table_gcm_EMN)[dim(table_gcm_EMN)[2]]=names(gcm_data_EMN[[i]])[3]}

altitude=read.csv("D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_1/projections/averages/extracts1/alt.csv")

table_gcm_EMN=cbind(table_gcm_EMN,altitude[,3])
names(table_gcm_EMN)[length(names(table_gcm_EMN))]="alt"

breaks=seq(0,2000,100)
gcm_EMN=matrix(0,dim(table_gcm_EMN)[2],length(breaks)-1)
iter=length(breaks)-1
for(i in 1:iter){
# gcm_EMN[,i]=apply(subset(table_gcm_EMN,table_gcm_EMN$alt>breaks[i]&table_gcm_EMN$alt<=breaks[i+1]),2,mean)}
gcm_EMN[,i]=apply(subset(table_gcm_EMN,table_gcm_EMN$alt>breaks[i]&table_gcm_EMN$alt<=breaks[i+1]),2,mean,na.rm=T)}


gcm_EMN=t(gcm_EMN)
gcm_EMN=data.frame(gcm_EMN)
names(gcm_EMN)=names(table_gcm_EMN)


################
##Data GCM ESD##
################

list_gcm_ESD=list.files("D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_1/projections/averages/extracts1/gcm/",full.names=T,pattern="ESD.csv")
gcm_data_ESD=lapply(list_gcm_ESD,FUN=read.csv)

table_gcm_ESD=gcm_data_ESD[[1]]
for(i in 2:length(gcm_data_ESD)){
table_gcm_ESD=cbind(table_gcm_ESD,gcm_data_ESD[[i]][,3])
names(table_gcm_ESD)[dim(table_gcm_ESD)[2]]=names(gcm_data_ESD[[i]])[3]}


table_gcm_ESD=cbind(table_gcm_ESD,altitude[,3])
names(table_gcm_ESD)[length(names(table_gcm_ESD))]="alt"
head(table_gcm_ESD)

breaks=seq(0,2000,100)
gcm_ESD=matrix(0,dim(table_gcm_ESD)[2],length(breaks)-1)
for(i in 1:iter){
gcm_ESD[,i]=apply(subset(table_gcm_ESD,table_gcm_ESD$alt>breaks[i]&table_gcm_ESD$alt<=breaks[i+1]),2,mean)}

gcm_ESD=t(gcm_ESD)
gcm_ESD=data.frame(gcm_ESD)
names(gcm_ESD)=names(table_gcm_ESD)

####################
##Paste ESD in ESD##
####################

gcm_EMN[gcm_EMN=="NaN"]=0
gcm=cbind(gcm_EMN,apply(gcm_EMN[,3:21],1,sd))
names(gcm)[length(names(gcm))]="s_deviation"

#############
##Area Data##
#############

area=read.csv("D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_1/projections/averages/extracts1/altitude_vs_area.csv")
area$area=area$area/1000

###########
##Graphic##
###########

gcm[gcm=="NaN"]=0
gcm$alt=breaks[1:iter]

require(splines)


tiff(paste("D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_1/projections/averages/extracts1/graph.tif", sep=""),width=1024, height=768,pointsize=8,compression='lzw',res=150)
par(mar=c(4,4,4,5))
plot(gcm$alt,gcm$bccr_bcm2_0,type="l",xaxt="n", yaxt="n",xlim=c(0,1500),ylim=c(0,0.8),col="white",xlab="Altitude (m)",ylab="Suitability",main="Suitability Variantions With Altitude")
axis(1, las=1)
axis(2, las=2)

pos_models=which(names(gcm)==c("alt"))-1
for(i in 3:pos_models){
smooth=predict(interpSpline(gcm$alt,gcm[,i]))
smooth$y[smooth$y<0]=0
lines(smooth,col="gray")}
smooth=predict(interpSpline(gcm$alt,gcm$average))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=3)
smooth=predict(interpSpline(gcm$alt,gcm$average+gcm$s_deviation))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=3,lty=3)
smooth=predict(interpSpline(gcm$alt,gcm$average-gcm$s_deviation))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=3,lty=3)
smooth=predict(interpSpline(gcm$alt,gcm$baseline))
smooth$y[smooth$y<0]=0
lines(smooth,lwd=3,col="red")

par(new=TRUE)

plot(area$alt,area$area,col="green4",lwd=3,axes=FALSE,ann=FALSE,type="l")
axis(4,cex.axis=1,las=2)
par(las=0)
mtext("Area (Km)", side = 4, line = 3,padj=1)


legend(100,6000, c("Current","Average GCM 2050","Stdv","19 GCM SRES A2","Area"),
       lty=c(1,1,3,1,1),col=c("red","black","black","gray","green4"),cex=1.2,
       lwd=c(3,3,3,1,3),box.lty=0,bg="transparent") 
dev.off()
