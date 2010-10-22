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
for (gs in 1:12) {
	plotdata <- dataset[,grep(paste("GS", gs, "_", sep=""), names(dataset))]
	varList <- c("prec", "tmean", "tmin", "tmax")
	v <- 1
	for (varn in varList) {
		calPar <- getParameters(plotdata[,v], nb=200, plotit=T, plotdir="./img", gs=gs, varname=varn)
		if (v == 1 & gs == 1) {finalTable <- calPar} else {finalTable <- rbind(finalTable, calPar)}
		v <- v+1
	}
}
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
for (gs in c(1:12,"MEAN","MODE","MAX","MIN")) {
	p <- read.csv("./data/calibration-parameters.csv")
	p <- p[which(p$GS==gs),]
	vl <- c("tmean","tmin","tmax")
	for (rw in 2:4) {
		if (!file.exists(paste("./data/runs/", gs, "-sorghum-",vl[rw-1],"-suitability.jpg",sep=""))) {
			eco <- suitCalc(climPath='./data/climate', Gmin=180,Gmax=180,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],Ropmax=p$OPMAX[1],Rmax=p$MAX[1], outfolder='./data/runs', cropname=paste(gs,'-sorghum-',vl[rw-1],sep=""))
			jpeg(paste("./data/runs/", gs, "-sorghum-",vl[rw-1],"-suitability.jpg",sep=""), quality=100)
			plot(eco)
			dev.off()
		}
	}
}

#Assess accuracy of each growing season and each parameter tuning
source("./src/accuracy.R")
test <- read.csv("./data/test.csv"); test <- cbind(test[,18], test[,17])
train <- read.csv("./data/train.csv"); train <- cbind(train[,18], train[,17])
parList <- read.csv("./data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
	pList <- parList[which(parList$GS==gs),][2:4,]
	for (vr in pList$VARIABLE) {
		for (suf in c("_p","_t","_")) {
			cat("GS:", gs, "- VAR:", vr, "- SUF:", suf, "\n")
			rs <- raster(paste("./data/runs/", gs, "-sorghum-", vr, suf, "suitability.asc", sep=""))
			tem <- accMetrics(rs, test) #doing with test data
			trm <- accMetrics(rs, train) #doing with training data
			resrow <- data.frame(GS=gs, VARIABLE=vr, TYPE=paste(suf, "suitability", sep=""), TEST.AV.SUIT=tem$METRICS$SUIT, TEST.SD.SUIT=tem$METRICS$SUITSD, TEST.MAX.SUIT=tem$METRICS$SUITX, TEST.MIN.SUIT=tem$METRICS$SUITN, TEST.OMISSION.RATE=tem$METRICS$OMISSION_RATE, TEST.ERROR=tem$METRICS$RMSQE, TEST.ERR.DIST=tem$METRICS$ERR_DIST, TEST.MXE=tem$METRICS$MAX_ENT, TEST.SLOPE=tem$METRICS$SLOPE, TRAIN.AV.SUIT=trm$METRICS$SUIT, TRAIN.SD.SUIT=trm$METRICS$SUITSD, TRAIN.MAX.SUIT=trm$METRICS$SUITX, TRAIN.MIN.SUIT=trm$METRICS$SUITN, TRAIN.OMISSION.RATE=trm$METRICS$OMISSION_RATE, TRAIN.ERROR=trm$METRICS$RMSQE, TRAIN.ERR.DIST=trm$METRICS$ERR_DIST, TRAIN.MXE=trm$METRICS$MAX_ENT, TRAIN.SLOPE=trm$METRICS$SLOPE)
			rescol <- data.frame(tem$MXE_CURVE, trm$MXE_CURVE)
			names(rescol) <- c(paste("TEST.GS.",gs,sep=""), paste("TRAIN.GS.",gs,sep=""))
			if (gs == gsList[1] & vr == pList$VARIABLE[1] & suf == "_p") {
				rres <- resrow
				cres <- cbind(SUIT=c(1:100), rescol)
			} else {
				rres <- rbind(rres, resrow)
				cres <- cbind(cres, rescol)
			}
		}
	}
}
write.csv(rres, "./data/accuracy-metrics.csv", row.names=F)
write.csv(cres, "./data/entropy-curves.csv", row.names=F)

#Buffer all points at 25km, 50km 100km, 200km, 300km, 400km, 500km, 750km, 1000km, 1500km, 2000km
source("./src/bufferPoints.R")
ds <- read.csv("./data/unique.csv"); ds <- cbind(ds[,18], ds[,17])
rs <- "./data/climate/prec_1.asc"
for (bd in c(25, 50, 100, 200, 300, 400, 500, 750, 1000, 1500, 2000)) {
	if (!file.exists(paste("./data/buffered/bf-", bd, "km.asc", sep=""))) {
		bf <- createBuffers(ds, rs, buffDist=bd*1000, method=1, verbose=T)
		bf <- writeRaster(bf, paste("./data/buffered/bf-", bd, "km.asc", sep=""), format='ascii', overwrite=T)
	}
}

#Method 1 started on "Thu Oct 21 08:34:26 2010", finished on "Thu Oct 21 09:48:18 2010", totalling 1 hour, 13 min, 52 sec (4432 sec)
#Method 2 started on "Thu Oct 21 10:25:21 2010", finished on "Thu Oct 21 11:52:54 2010", totalling 1 hour, 27 min, 33 sec (5253 sec)
#Difference was 0 hour, 13 min, 41 sec, favoring method 1
