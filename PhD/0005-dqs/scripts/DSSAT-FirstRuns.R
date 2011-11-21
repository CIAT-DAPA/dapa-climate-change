#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL

#Plot the results of the first runs
source("DSSAT-functions.R")

firstOpt()
setwd("D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT")

rs <- read.csv("FirstRuns.csv")
mn <- min(rs$Obs_Yield,rs$CHINESE.TMV2.TA,rs$TMV2.mod.tamnu,rs$TMV.2.uf.v.24)
mx <- max(rs$Obs_Yield,rs$CHINESE.TMV2.TA,rs$TMV2.mod.tamnu,rs$TMV.2.uf.v.24)

#Plot the best prediction
tiff("D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/FirstRuns.tiff",compression='lzw',res=300,
     pointsize=8,width=1500,height=1000)
par(mar=c(4.5,4.5,1,1),cex=0.8)
plot(rs$Year,rs$Obs_Yield,type="p",pch=20,
     xlab="Year",ylab="Yield (kg/ha)",xlim=c(1966,1989),ylim=c(mn,mx))
lines(rs$Year,rs$Obs_Yield)
points(rs$Year,rs$CHINESE.TMV2.TA,type="p",pch=20,col='red')
lines(rs$Year,rs$CHINESE.TMV2.TA,col='red',lty=1)
points(rs$Year,rs$TMV2.mod.tamnu,type="p",pch=20,col='red')
lines(rs$Year,rs$TMV2.mod.tamnu,col='red',lty=2)
points(rs$Year,rs$TMV.2.uf.v.24,type="p",pch=20,col='red')
lines(rs$Year,rs$TMV.2.uf.v.24,col='red',lty=3)
grid()
legend(1980,3500,legend=c("Observed","CHINESE TMV2 TA","TMV2, mod tamnu","TMV-2,uf v 24"),
       col=c("black","red","red","red","red"),lty=c(1,1,2,3),pch=c(20,20,20,20),cex=1)
dev.off()

