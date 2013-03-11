###################################
###Cross Validation Performance####
###################################

bDir="D:/CIAT/workspace/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/projections/summarize"
oDir="D:/CIAT/workspace/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica_swd/projections/summarize"
id <- read.csv(paste(bDir, "/mxe-auc-maxkappa.csv", sep=""))
	
tiff(paste(oDir, "/hist_train_auc.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Training_AUC,breaks=10,xlim=c(0.8,0.95),xlab="AUC (dec)",ylab="Frecuency",main=NA)
dev.off()

tiff(paste(oDir, "/hist_test_auc.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Test_AUC,breaks=10,xlim=c(0.8,0.95),xlab="AUC (dec)",ylab="Frecuency",main=NA)
dev.off()

	
tiff(paste(oDir, "/hist_train_maxkappa.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Training_maxKappa,breaks=10,xlim=c(0.8,0.95),xlab="Max Kappa (dec)",ylab="Frecuency",main=NA)
dev.off()

tiff(paste(oDir, "/hist_test_maxkappa.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Test_maxKappa,breaks=10,xlim=c(0.8,0.95),xlab="Max Kappa (dec)",ylab="Frecuency",main=NA)
dev.off()


cans <- read.csv(paste(bDir, "/canasta-auc-maxkappa-calcs.csv", sep=""))
mxnt <- read.csv(paste(bDir, "/mxe-auc-maxkappa.csv", sep=""))


tiff(paste(oDir, "/boxplot_auc_canasta.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(5,5,1,1))
boxplot(cans$AUC~cans$Sample*cans$Treatment,pch=20,ylim=c(0,1),ylab="AUC (dec)",col="grey 80",names=c("AC: Test","AC: Train", "FL: Test", "FL: Train"))
grid()
dev.off()


tiff(paste(oDir, "/boxplot_maxKappa_canasta.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(5,5,1,1))
boxplot(cans$MaxKappa~cans$Sample*cans$Treatment,pch=20,ylim=c(0,1),ylab="Max Kappa (dec)",col="grey 80",names=c("AC: Test","AC: Train", "FL: Test", "FL: Train"))
grid()
dev.off()


tiff(paste(oDir, "/boxplot_auc_maxent.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(5,5,1,1))
boxplot(mxnt$AUC~mxnt$Sample,pch=20,ylim=c(0.7,1),col="grey 80",ylab="AUC (dec)",names=c("Test","Train"))
grid()
dev.off()


tiff(paste(oDir, "/boxplot_maxKappa_maxent.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
par(mar=c(5,5,1,1))
boxplot(mxnt$MaxKappa~mxnt$Sample,pch=20,ylim=c(0.7,1),col="grey 80",ylab="Max Kappa (dec)",names=c("Test","Train"))
grid()
dev.off()

windows()
plot(density(mxnt$MaxKappa),col="red",xlim=c(0,1))
lines(density(cans$MaxKappa),col="blue")

