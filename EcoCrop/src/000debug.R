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

#Merge tmax & tmin runs
source("./src/suitMerge.R")
library(maptools)
data(wrld_simpl)
d <- read.csv("./data/unique.csv")
parList <- read.csv("./data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
	cat("GS:", gs, "\n")
	n <- raster(paste("./data/runs/", gs, "-sorghum-tmin_suitability.asc", sep=""))
	x <- raster(paste("./data/runs/", gs, "-sorghum-tmax_suitability.asc", sep=""))
	r <- suitMerge(n,x)
	
	rs <- raster(r,1); rs <- writeRaster(rs, paste("./data/runs/", gs, "-sorghum-merged_suitability.asc", sep=""), overwrite=T, format='ascii')
	pd <- raster(r,2); rs <- writeRaster(pd, paste("./data/runs/", gs, "-sorghum-mergedwhich_suitability.asc", sep=""), overwrite=T, format='ascii')
	
	jpeg(paste("./data/runs/", gs, "-sorghum-merged_suitability.jpg", sep=""), quality=100, height=600, width=1800)
	par(mfrow=c(1,2))
	plot(rs, col=colorRampPalette(c("yellow","red"))(100)); plot(wrld_simpl, add=T)
	points(d$Longitude, d$Latitude, pch=20, cex=0.7, col="blue")
	plot(pd); plot(wrld_simpl, add=T)
	points(d$Longitude, d$Latitude, pch=20, cex=0.7, col="blue")
	dev.off()
}

#Accuracy metrics for the merged grids
source("./src/accuracy.R")
test <- read.csv("./data/test.csv"); test <- cbind(test[,18], test[,17])
train <- read.csv("./data/train.csv"); train <- cbind(train[,18], train[,17])
parList <- read.csv("./data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
	cat("GS:", gs, "\n")
	rs <- raster(paste("./data/runs/", gs, "-sorghum-merged_suitability.asc", sep=""))
	tem <- accMetrics(rs, test) #doing with test data
	trm <- accMetrics(rs, train) #doing with training data
	resrow <- data.frame(GS=gs, TEST.AV.SUIT=tem$METRICS$SUIT, TEST.SD.SUIT=tem$METRICS$SUITSD, TEST.MAX.SUIT=tem$METRICS$SUITX, TEST.MIN.SUIT=tem$METRICS$SUITN, TEST.OMISSION.RATE=tem$METRICS$OMISSION_RATE, TEST.ERROR=tem$METRICS$RMSQE, TEST.ERR.DIST=tem$METRICS$ERR_DIST, TEST.MXE=tem$METRICS$MAX_ENT, TEST.SLOPE=tem$METRICS$SLOPE, TRAIN.AV.SUIT=trm$METRICS$SUIT, TRAIN.SD.SUIT=trm$METRICS$SUITSD, TRAIN.MAX.SUIT=trm$METRICS$SUITX, TRAIN.MIN.SUIT=trm$METRICS$SUITN, TRAIN.OMISSION.RATE=trm$METRICS$OMISSION_RATE, TRAIN.ERROR=trm$METRICS$RMSQE, TRAIN.ERR.DIST=trm$METRICS$ERR_DIST, TRAIN.MXE=trm$METRICS$MAX_ENT, TRAIN.SLOPE=trm$METRICS$SLOPE)
	rescol <- data.frame(tem$MXE_CURVE, trm$MXE_CURVE)
	names(rescol) <- c(paste("TEST.GS.",gs,sep=""), paste("TRAIN.GS.",gs,sep=""))
	if (gs == gsList[1]) {
		rres <- resrow
		cres <- cbind(SUIT=c(1:100), rescol)
	} else {
		rres <- rbind(rres, resrow)
		cres <- cbind(cres, rescol)
	}
}
write.csv(rres, "./data/accuracy-metrics-merged.csv", row.names=F)
write.csv(cres, "./data/entropy-curves-merged.csv", row.names=F)

#Area outside and inside different buffer sizes for merged suitability rasters
source("./src/areaFalse.R")
parList <- read.csv("./data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
	for (bd in c(25, 50, 100, 200, 300, 400, 500, 750, 1000, 1500, 2000)) {
		cat("Process started for BD", bd, "\n")
		a <- raster("./data/area/cell-area.asc")
		s <- raster(paste("./data/runs/", gs, "-sorghum-merged_suitability.asc", sep=""))
		b <- raster(paste("./data/buffered/bf-", bd, "km.asc", sep=""))
		
		m <- areaFalse(a,s,b)
		m <- data.frame(DIST.KM=bd, SUI.IN.AREA=m[2,2], SUI.IN.FRAC=m[2,3], SUI.OT.AREA=m[1,2], SUI.OT.FRAC=m[1,3], UNS.IN.AREA=m[4,2], UNS.IN.FRAC=m[4,3], UNS.OT.AREA=m[3,2], UNS.OT.FRAC=m[3,3])
		
		if (bd == 25) {mo <- m} else {mo <- rbind(mo, m)}
	}
	mo$TOTAL.IN <- mo$SUI.IN.AREA + mo$UNS.IN.AREA
	mo$SUI.FRAC <- mo$SUI.IN.AREA / mo$TOTAL.IN
	mo$UNS.FRAC <- mo$UNS.IN.AREA / mo$TOTAL.IN
	mo$TOTAL.SUI <- mo$SUI.IN.AREA + mo$SUI.OT.AREA
	mo$IN.FRAC <- mo$SUI.IN.AREA / mo$TOTAL.SUI
	mo$OT.FRAC <- mo$SUI.OT.AREA / mo$TOTAL.SUI
	mo$GS <- rep(gs, times=nrow(mo))
	
	if (gs == gsList[1]) {
		mf <- mo
	} else {
		mf <- rbind(mf, mo)
	}
	
}
write.csv(mf, "./data/areaFalse.csv", row.names=F)

#Now... the best predictions are MAX and MEAN, the best distributed is MEAN (according to the entropy curve)

#Validation stuff
source("./src/validation.R")
source("./src/createMask.R")
rsl <- raster("D:/_tools/dapa-climate-change/trunk/EcoCrop/data/runs/selected/MEAN-sorghum-merged_suitability.asc")
shp.faostat <- "F:/EcoCrop-development/agricultural-data/FAOSTAT/world-adm0-sorghum.shp"
shp.agromaps1 <- "F:/EcoCrop-development/agricultural-data/agroMAPS/_shapefiles/sorghum-adm1.shp"
shp.agromaps2 <- "F:/EcoCrop-development/agricultural-data/agroMAPS/_shapefiles/sorghum-adm2.shp"
shp.countrystat1 <- "F:/EcoCrop-development/agricultural-data/countrySTAT/shapefiles/sorghum-adm1.shp"
shp.countrystat2 <- "F:/EcoCrop-development/agricultural-data/countrySTAT/shapefiles/sorghum-adm2.shp"
shp.icrisat <- "F:/EcoCrop-development/agricultural-data/Leeds/IND2-sorg-to.shp"
oblist <- ls(pattern="shp")
for (ob in oblist) {
	cat("Processing", ob, "\n")
	shp <- readShapePoly(get(ob))
	if (ob == "shp.faostat") {field <- "ISPRES"} else {field <- "Is_present"}
	res <- extractFromShape(shp, field, naValue=-9999, rsl)
	write.csv(res, paste("./data/validation/", ob, ".csv", sep=""))
	mets <- valMetrics(res, pres.field=field)
	if (ob == oblist[1]) {
		rr <- cbind(SHP=ob, mets)
	} else {
		rr <- rbind(rr, cbind(SHP=ob, mets))
	}
}
write.csv(rr, "./data/validation/rates.csv", quote=F, row.names=F)

#Validation for all adm1 and all adm2 (sorghum specific)
source("./src/validation.R")
dataf1 <- rbind(read.csv("./data/validation/shp.agromaps1.csv")[,1:12],read.csv("./data/validation/shp.countrystat1.csv")[,1:12])
mets <- cbind(TYPE="adm1", valMetrics(dataf1, pres.field="Is_present"))
dataf2 <- rbind(read.csv("./data/validation/shp.agromaps2.csv")[,1:12],read.csv("./data/validation/shp.countrystat2.csv")[,1:12],read.csv("./data/validation/shp.icrisat.csv")[,1:12])
mets <- rbind(mets, cbind(TYPE="adm2", valMetrics(dataf2, pres.field="Is_present")))
write.csv(mets, "./data/validation/rates-merged.csv", quote=F, row.names=F)

#Now the charts of the relationships
source("./src/relationships.R")
#FAOSTAT
relationships("./data/validation/shp.faostat.csv", area.field="AHARV_AV", fac=0.01, yield.field="YIELD_AV", naValue=-9999, outfile="./data/validation/figures/faostat-av.png")
relationships("./data/validation/shp.faostat.csv", area.field="AHARV_MX", fac=0.01, yield.field="YIELD_MX", naValue=-9999, outfile="./data/validation/figures/faostat-mx.png")
relationships("./data/validation/shp.faostat.csv", area.field="AHARV_MN", fac=0.01, yield.field="YIELD_MN", naValue=-9999, outfile="./data/validation/figures/faostat-mn.png")
#AGROMAPS1
relationships("./data/validation/shp.agromaps1.csv", area.field="Aharv_aver", fac=0.01, yield.field="Yield_aver", naValue=-9999, outfile="./data/validation/figures/agromaps1-av.png")
relationships("./data/validation/shp.agromaps1.csv", area.field="Aharv_max_", fac=0.01, yield.field="Yield_max_", naValue=-9999, outfile="./data/validation/figures/agromaps1-mx.png")
relationships("./data/validation/shp.agromaps1.csv", area.field="Aharv_min_", fac=0.01, yield.field="Yield_min_", naValue=-9999, outfile="./data/validation/figures/agromaps1-mn.png")
#AGROMAPS2
relationships("./data/validation/shp.agromaps2.csv", area.field="Aharv_aver", fac=0.01, yield.field="Yield_aver", naValue=-9999, outfile="./data/validation/figures/agromaps2-av.png")
relationships("./data/validation/shp.agromaps2.csv", area.field="Aharv_max_", fac=0.01, yield.field="Yield_max_", naValue=-9999, outfile="./data/validation/figures/agromaps2-mx.png")
relationships("./data/validation/shp.agromaps2.csv", area.field="Aharv_min_", fac=0.01, yield.field="Yield_min_", naValue=-9999, outfile="./data/validation/figures/agromaps2-mn.png")
#COUNTRYSTAT1
relationships("./data/validation/shp.countrystat1.csv", area.field="Aharv_aver", fac=0.01, yield.field="Yield_aver", naValue=-9999, outfile="./data/validation/figures/countrystat1-av.png")
relationships("./data/validation/shp.countrystat1.csv", area.field="Aharv_max_", fac=0.01, yield.field="Yield_max_", naValue=-9999, outfile="./data/validation/figures/countrystat1-mx.png")
relationships("./data/validation/shp.countrystat1.csv", area.field="Aharv_min_", fac=0.01, yield.field="Yield_min_", naValue=-9999, outfile="./data/validation/figures/countrystat1-mn.png")
#COUNTRYSTAT2
relationships("./data/validation/shp.countrystat2.csv", area.field="Aharv_aver", fac=0.01, yield.field="Yield_aver", naValue=-9999, outfile="./data/validation/figures/countrystat2-av.png")
relationships("./data/validation/shp.countrystat2.csv", area.field="Aharv_max_", fac=0.01, yield.field="Yield_max_", naValue=-9999, outfile="./data/validation/figures/countrystat2-mx.png")
relationships("./data/validation/shp.countrystat2.csv", area.field="Aharv_min_", fac=0.01, yield.field="Yield_min_", naValue=-9999, outfile="./data/validation/figures/countrystat2-mn.png")
#ICRISAT
relationships("./data/validation/shp.icrisat.csv", area.field="Aharv_aver", fac=10, yield.field="Yield_aver", naValue=-9999, outfile="./data/validation/figures/icrisat-av.png")
relationships("./data/validation/shp.icrisat.csv", area.field="Aharv_max_", fac=10, yield.field="Yield_max_", naValue=-9999, outfile="./data/validation/figures/icrisat-mx.png")
relationships("./data/validation/shp.icrisat.csv", area.field="Aharv_min_", fac=10, yield.field="Yield_min_", naValue=-9999, outfile="./data/validation/figures/icrisat-mn.png")
