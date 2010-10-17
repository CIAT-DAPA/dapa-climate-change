#Debugging script

#Getting unique coordinates
source("./src/getUniqueCoord.R")
rs <- read.csv("./data/sorghum.csv")
rs <- getUniqueCoord(rs, c(18,17), resol=10/60)

#Dummy part of unloading and re-loading the data
write.csv(rs, "./data/dataset.csv", row.names=F, quote=T)
rs <- rs[which(rs$IS_UNIQUE == TRUE),]
write.csv(rs, "./data/unique.csv", row.names=F, quote=T)
rs <- read.csv("./data/unique.csv")

#Splitting in test/train datasets
source("./src/randomSplit.R")
rs <- randomSplit(rs, 20)

#Plot & write test and train datasets separately
jpeg("./img/test_train.jpg", quality=100, height=600, width=600)
plot(cbind(rs$Longitude[which(rs$TEST_TRAIN == "TEST")], rs$Latitude[which(rs$TEST_TRAIN == "TEST")]), pch="+", col="red", xlim=c(-20,90), ylim=c(-30,30), xlab="", ylab="")
points(cbind(rs$Longitude[which(rs$TEST_TRAIN == "TRAIN")], rs$Latitude[which(rs$TEST_TRAIN == "TRAIN")]), pch=20, col="blue", cex=0.5)
dev.off()

write.csv(rs, "./data/unique.csv", row.names=F, quote=T)
write.csv(rs[which(rs$TEST_TRAIN == "TEST"),], "./data/test.csv", row.names=F, quote=T)
write.csv(rs[which(rs$TEST_TRAIN == "TRAIN"),], "./data/train.csv", row.names=F, quote=T)

#Extracting climate data
source("./src/extractClimateData.R")
rs <- read.csv("./data/unique.csv")
for (v in c("prec", "tmin", "tmean", "tmax")) {
	rs <- extractMonthlyData(wd="./data/climate", variable=v, ext=".asc", rs, fields=c(18,17), verbose=T)
}
write.csv(rs, "./data/climates.csv", row.names=F, quote=T)

#Calculating gs parameters for calibration
source("./src/calibrationParameters.R")
rs <- read.csv("./data/climates.csv")
rs <- calibrationParameters(rs, gs=6, verbose=T)
write.csv(rs, "./data/calibration.csv", row.names=F, quote=T)

#Plotting histograms
source("./src/histPlot.R")
rs <- read.csv("./data/calibration.csv")
pd <- histPlot(rs, gs=1, plotdir="./img/", nb=20)
pd <- histPlot(rs, gs="mode", plotdir="./img/", nb=20)

#Get calibration parameters
source("./src/getParameters.R")
dataset <- read.csv("./data/calibration.csv")
#for (gs in 1:12) {
	gs <- 1
	plotdata <- dataset[,grep(paste("GS", gs, "_", sep=""), names(dataset))]
	
	varList <- c("prec", "tmean", "tmin", "tmax")
	v <- 1
	for (varn in varList) {
		calPar <- getParameters(plotdata[,v], nb=200, plotit=T, plotdir="./img", gs=gs, varname=varn)
		if (v == 1 & gs == 1) {finalTable <- calPar} else {finalTable <- rbind(finalTable, calPar)}
		v <- v+1
	}
#}
for (gs in c("MEAN", "MODE", "MAX", "MIN")) {
	plotdata <- dataset[,grep(paste(gs, "_", sep=""), names(dataset))]
	varList <- c("prec", "tmean", "tmin", "tmax")
	v <- 1
	for (varn in varList) {
		calPar <- getParameters(plotdata[,v], nb=200, plotit=T, plotdir="./img", gs=gs, varname=varn)
		finalTable <- rbind(finalTable, calPar)
		v <- v+1
	}
}
write.csv(finalTable, "./data/calibration-parameters.csv", row.names=F)

#Cut climate data
library(rgdal)
library(raster)
cd <- "./data/climate"
od <- "./data/climate-cut"

lg <- list.files(cd, pattern=".asc")
x <- extent(c(-20,90,-30,30))
for (g in lg) {
	cat(g, "\n")
	rs <- raster(paste(cd,"/",g,sep=""))
	rs <- crop(rs, x)
	rs <- writeRaster(rs, paste(od, "/", g, sep=""), format='ascii')
}

#Running the model
source("./src/EcoCrop.R")
p <- read.csv("./data/calibration-parameters.csv")
eco <- suitCalc(climPath='./data/climate', Gmin=180,Gmax=180,Tkmp=p$KILL[2],Tmin=p$MIN[2],Topmin=p$OPMIN[2],Topmax=p$OPMAX[2],Tmax=p$MAX[2],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],Ropmax=p$OPMAX[1],Rmax=p$MAX[1], outfolder='./data/runs', cropname='sorghum-tmean')
eco <- suitCalc(climPath='./data/climate', Gmin=180,Gmax=180,Tkmp=p$KILL[3],Tmin=p$MIN[3],Topmin=p$OPMIN[3],Topmax=p$OPMAX[3],Tmax=p$MAX[3],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],Ropmax=p$OPMAX[1],Rmax=p$MAX[1], outfolder='./data/runs', cropname='sorghum-tmin')
eco <- suitCalc(climPath='./data/climate', Gmin=180,Gmax=180,Tkmp=p$KILL[4],Tmin=p$MIN[4],Topmin=p$OPMIN[4],Topmax=p$OPMAX[4],Tmax=p$MAX[4],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],Ropmax=p$OPMAX[1],Rmax=p$MAX[1], outfolder='./data/runs', cropname='sorghum-tmax')

