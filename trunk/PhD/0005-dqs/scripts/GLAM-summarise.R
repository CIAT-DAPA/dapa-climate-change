#Julian Ramirez-Villegas
#Uol / CCAFS / CIAT
#May 2012

wd <- "F:/PhD-work/data-quality-study"
glamDir <- paste(wd,"/GLAM-GNUT",sep="")

spDir <- paste(glamDir,"/rmse_ccoef",sep="")

sDir <- paste(spDir,"/dqs_shuffled",sep="")
pDir <- paste(spDir,"/dqs",sep="")

gConfig <- "A"

#Shuffled experiments
if (!file.exists(paste(spDir,"/dqs_shuffled.csv",sep=""))) {
  xpList <- list.files(sDir,pattern=paste("config-",gConfig,sep=""))
  for (xp in xpList) {
    xpm <- gsub("\\.csv","",xp)
    vn <- strsplit(xpm,"_",fixed=T)[[1]][2]
    vn <- substr(vn,2,nchar(vn))
    sc <- strsplit(xpm,"_",fixed=T)[[1]][3]
    
    #read file
    xpData <- read.csv(paste(sDir,"/",xp,sep=""),header=F,skip=1)
    xpData$V4 <- NULL
    names(xpData) <- c("SEED","RMSE","CORR")
    xpData <- cbind(VARIABLE=toupper(vn),SCALE=toupper(sc),xpData)
    
    if (xp == xpList[1]) {
      xp_all <- xpData
    } else {
      xp_all <- rbind(xp_all,xpData)
    }  
  }
  write.csv(xp_all,paste(spDir,"/dqs_shuffled.csv",sep=""),quote=T,row.names=F)
} else {
  xp_all <- read.csv(paste(spDir,"/dqs_shuffled.csv",sep=""))
}

xp_all$LEGEND <- paste(toupper(xp_all$VARIABLE)," (",toupper(xp_all$SCALE),")",sep="") #add legend column

#plot tif of RMSE
tifName <- paste(spDir,"/s-performance_rmse.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,8,1,1),cex=0.6,las=2,lwd=0.65)
boxplot(xp_all$RMSE~xp_all$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,
        xlab="RMSE (kg/ha)",
        horizontal=T,boxwex=0.7)
abline(v=281,lwd=0.7,lty=1,col="red")
abline(v=seq(0,700,by=50),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of CORR
tifName <- paste(spDir,"/s-performance_corr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,8,1,1),cex=0.6,las=2,lwd=0.65)
boxplot(xp_all$CORR~xp_all$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,
        xlab="Correlation coefficient",
        horizontal=T,boxwex=0.7)
abline(v=0.74,lwd=0.7,lty=1,col="red")
abline(v=seq(-1,1,by=0.2),lwd=0.6,lty=2,col="grey50")
dev.off()


#Biased experiments
if (!file.exists(paste(spDir,"/dqs_bias.csv",sep=""))) {
  xpList <- list.files(pDir,pattern=paste("config-",gConfig,sep=""))
  for (xp in xpList) {
    xpm <- gsub("\\.csv","",xp)
    vn <- strsplit(xpm,"_",fixed=T)[[1]][2]
    sc <- strsplit(xpm,"_",fixed=T)[[1]][3]
    
    #read file
    xpData <- read.csv(paste(pDir,"/",xp,sep=""),header=F,skip=1)
    xpData$V5 <- NULL
    names(xpData) <- c("SEED","P","RMSE","CORR")
    xpData <- cbind(VARIABLE=toupper(vn),SCALE=toupper(sc),xpData)
    
    if (xp == xpList[1]) {
      xp_all <- xpData
    } else {
      xp_all <- rbind(xp_all,xpData)
    }  
  }
  write.csv(xp_all,paste(spDir,"/dqs_bias.csv",sep=""),quote=T,row.names=F)
} else {
  xp_all <- read.csv(paste(spDir,"/dqs_bias.csv",sep=""))
}

xp_all$LEGEND <- paste(toupper(xp_all$VARIABLE)," (",toupper(xp_all$SCALE),")",sep="") #add legend column

#plot tif of RMSE
tifName <- paste(spDir,"/p-performance_rmse.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,9,1,1),cex=0.6,las=2,lwd=0.65)
boxplot(xp_all$RMSE~xp_all$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,
        xlab="RMSE (kg/ha)",
        horizontal=T,boxwex=0.7)
abline(v=281,lwd=0.7,lty=1,col="red")
abline(v=seq(0,700,by=50),lwd=0.6,lty=2,col="grey50")
dev.off()

#plot tif of CORR
tifName <- paste(spDir,"/p-performance_corr.tif",sep="")
tiff(tifName,res=300,pointsize=8,width=800,height=800,units="px",compression="lzw")
par(mar=c(5,9,1,1),cex=0.6,las=2,lwd=0.65)
boxplot(xp_all$CORR~xp_all$LEGEND,
        col="grey",lty=1,
        pch=20,outwex=0.3,
        xlab="Correlation coefficient",
        horizontal=T,boxwex=0.7)
abline(v=0.74,lwd=0.7,lty=1,col="red")
abline(v=seq(-1,1,by=0.2),lwd=0.6,lty=2,col="grey50")
dev.off()


