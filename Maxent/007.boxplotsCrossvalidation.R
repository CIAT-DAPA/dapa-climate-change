###################################
###Cross Validation Performance####
###################################

bDir="D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica/crossval"
oDir="D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica/metrics"
id <- read.csv(paste(bDir, "/stack_presence_absence_kappa_evaluation.csv", sep=""))
	
tiff(paste(oDir, "/Hist_Kappa.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Kappa,breaks=10,xlim=c(0.898,0.902),xlab="Kappa (dec)",ylab="Frecuency",main=NA)
dev.off()

tiff(paste(oDir, "/Hist_Kappa_threshold.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Kappa_threshold,breaks=10,xlim=c(0.28,0.32),xlab="Kappa_threshold (dec)",ylab="Frecuency",main=NA)
dev.off()

tiff(paste(oDir, "/Hist_AUC.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$AUC,breaks=10,xlim=c(0.981,0.983),xlab="AUC (dec)",ylab="Frecuency",main=NA)
dev.off()



###################################
###Cross Validation Performance####
###################################

bDir="D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica/metrics"
oDir="D:/Maxent_Nicaragua/mxe_outputs/sp-coffea_arabica/metrics"
id <- read.csv(paste(bDir, "/maxentResults.csv", sep=""))
	
tiff(paste(oDir, "/Hist_test_AUC_maxent.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Test_AUC,breaks=10,xlim=c(0.8,0.9),xlab="Kappa (dec)",ylab="Frecuency",main=NA)
dev.off()

tiff(paste(oDir, "/Hist_training_AUC_maxent.tif", sep=""),width=600, height=600,pointsize=8,compression='lzw',res=150)
hist(id$Training_AUC,breaks=10,xlim=c(0.8,0.9),xlab="Kappa (dec)",ylab="Frecuency",main=NA)
dev.off()
