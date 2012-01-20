#### LIBRARIES: raster, maptools, rgdal, sp
####

#Getting unique coordinates
source("./src/getUniqueCoord.R") #loading the function
rs <- read.csv("F:/EcoCrop-development/analyses/data/sorghum.csv") #load the data
rs <- getUniqueCoord(rs, c(18,17), resol=2.5/60) #running the function

#Dummy part of unloading and re-loading the data
write.csv(rs, "F:/EcoCrop-development/analyses/data/dataset2_5m.csv", row.names=F, quote=T) #write outcome into new file
rs <- rs[which(rs$IS_UNIQUE == TRUE),] #selecting unique records
write.csv(rs, "F:/EcoCrop-development/analyses/data/unique2_5m.csv", row.names=F, quote=T) #write new dataset only containing unique records
rs <- read.csv("F:/EcoCrop-development/analyses/data/unique2_5m.csv") #re-loading the data
nrow(rs)

#Splitting in test/train datasets
source("./src/randomSplit.R")
rs <- randomSplit(rs, 20) #20 is percentage of data to be taken out

#Plot & write test and train datasets separately
jpeg("F:/EcoCrop-development/analyses/img/test_train.jpg", quality=100, height=600, width=600)
plot(cbind(rs$Longitude[which(rs$TEST_TRAIN == "TEST")], rs$Latitude[which(rs$TEST_TRAIN == "TEST")]), pch="+", col="red", xlim=c(-20,90), ylim=c(-30,30), xlab="", ylab="")
library(maptools); data(wrld_simpl); plot(wrld_simpl, add=T)
points(cbind(rs$Longitude[which(rs$TEST_TRAIN == "TRAIN")], rs$Latitude[which(rs$TEST_TRAIN == "TRAIN")]), pch=20, col="blue", cex=0.5)
dev.off()

write.csv(rs, "F:/EcoCrop-development/analyses/data/unique.csv", row.names=F, quote=T) #write unique records with new field TRAIN/TEST
write.csv(rs[which(rs$TEST_TRAIN == "TEST"),], "F:/EcoCrop-development/analyses/data/test.csv", row.names=F, quote=T) #reselect and store test data
write.csv(rs[which(rs$TEST_TRAIN == "TRAIN"),], "F:/EcoCrop-development/analyses/data/train.csv", row.names=F, quote=T) #reselect and store train data

#Extracting climate data
source("./src/extractClimateData.R")
rs <- read.csv("F:/EcoCrop-development/analyses/data/unique.csv") #load unique records
for (v in c("prec", "tmin", "tmean", "tmax")) {
	rs <- extractMonthlyData(wd="F:/EcoCrop-development/climate/global_2_5min", variable=v, ext=".asc", rs, fields=c(18,17), verbose=T)
}
write.csv(rs, "F:/EcoCrop-development/analyses/data/climates.csv", row.names=F, quote=T)

#Calculating gs parameters for calibration
source("./src/calibrationParameters.R")
rs <- read.csv("F:/EcoCrop-development/analyses/data/climates.csv")
rs <- calibrationParameters(rs, gs=6, verbose=T)
write.csv(rs, "F:/EcoCrop-development/analyses/data/calibration.csv", row.names=F, quote=T)

#Get calibration parameters
source("./src/getParameters.R")
dataset <- read.csv("F:/EcoCrop-development/analyses/data/calibration.csv")
for (gs in 1:12) {
	plotdata <- dataset[,grep(paste("GS", gs, "_", sep=""), names(dataset))]
	varList <- c("prec", "tmean", "tmin", "tmax")
	v <- 1
	for (varn in varList) {
		calPar <- getParameters(plotdata[,v], nb=200, plotit=T, plotdir="F:/EcoCrop-development/analyses/img", gs=gs, varname=varn)
		if (v == 1 & gs == 1) {finalTable <- calPar} else {finalTable <- rbind(finalTable, calPar)}
		v <- v+1
	}
}
for (gs in c("MEAN", "MODE", "MAX", "MIN")) {
	plotdata <- dataset[,grep(paste(gs, "_", sep=""), names(dataset))]
	varList <- c("prec", "tmean", "tmin", "tmax")
	v <- 1
	for (varn in varList) {
		calPar <- getParameters(plotdata[,v], nb=200, plotit=T, plotdir="F:/EcoCrop-development/analyses/img", gs=gs, varname=varn)
		finalTable <- rbind(finalTable, calPar)
		v <- v+1
	}
}
write.csv(finalTable, "F:/EcoCrop-development/analyses/data/calibration-parameters.csv", row.names=F)

#Running the model
source("./src/EcoCrop.R")
for (gs in c(1:12,"MEAN","MODE","MAX","MIN")) {
	cat("GS", gs, "\n")
	p <- read.csv("F:/EcoCrop-development/analyses/data/calibration-parameters.csv")
	p <- p[which(p$GS==gs),]
	vl <- c("tmean","tmin","tmax")
	for (rw in 2:4) {
		if (!file.exists(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-",vl[rw-1],"-suitability.jpg",sep=""))) {
			eco <- suitCalc(climPath='F:/EcoCrop-development/climate/afasia_2_5min', Gmin=180,Gmax=180,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],Ropmax=p$OPMAX[1],Rmax=p$MAX[1], outfolder='F:/EcoCrop-development/analyses/runs', cropname=paste(gs,'-sorghum-',vl[rw-1],sep=""))
			jpeg(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-",vl[rw-1],"-suitability.jpg",sep=""), quality=100)
			plot(eco)
			dev.off()
		}
	}
}

#Merge tmax & tmin runs
source("./src/suitMerge.R")
library(maptools)
data(wrld_simpl)
d <- read.csv("F:/EcoCrop-development/analyses/data/unique.csv")
parList <- read.csv("F:/EcoCrop-development/analyses/data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
	cat("GS:", gs, "\n")
	n <- raster(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-tmin_suitability.asc", sep=""))
	x <- raster(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-tmax_suitability.asc", sep=""))
	r <- suitMerge(n,x)
	
	rs <- raster(r,1); rs <- writeRaster(rs, paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-merged_suitability.asc", sep=""), overwrite=T, format='ascii')
	pd <- raster(r,2); pd <- writeRaster(pd, paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-mergedwhich_suitability.asc", sep=""), overwrite=T, format='ascii')
	
	jpeg(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-merged_suitability.jpg", sep=""), quality=100, height=600, width=1800)
	par(mfrow=c(1,2))
	plot(rs, col=colorRampPalette(c("yellow","red"))(100)); plot(wrld_simpl, add=T)
	points(d$Longitude, d$Latitude, pch=20, cex=0.7, col="blue")
	plot(pd); plot(wrld_simpl, add=T)
	points(d$Longitude, d$Latitude, pch=20, cex=0.7, col="blue")
	dev.off()
}

#Buffer all points at 25km, 50km 100km, 200km, 300km, 400km, 500km, 750km, 1000km, 1500km, 2000km
source("./src/bufferPoints.R")
ds <- read.csv("F:/EcoCrop-development/analyses/data/unique.csv"); ds <- cbind(ds[,18], ds[,17])
rs <- "F:/EcoCrop-development/climate/afasia_2_5min/prec_1.asc"
for (bd in c(25, 50, 100, 200, 300, 400, 500, 750, 1000, 1500, 2000)) {
	if (!file.exists(paste("F:/EcoCrop-development/analyses/buffered/bf-", bd, "km.asc", sep=""))) {
		bf <- createBuffers(ds, rs, buffDist=bd*1000, method=1, verbose=T)
		bf <- writeRaster(bf, paste("F:/EcoCrop-development/analyses/buffered/bf-", bd, "km.asc", sep=""), format='ascii', overwrite=T)
	}
}

#Assess accuracy of each growing season and each parameter tuning
source("./src/accuracy.R")
test <- read.csv("F:/EcoCrop-development/analyses/data/test.csv"); test <- cbind(test[,18], test[,17])
train <- read.csv("F:/EcoCrop-development/analyses/data/train.csv"); train <- cbind(train[,18], train[,17])
parList <- read.csv("F:/EcoCrop-development/analyses/data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
	pList <- parList[which(parList$GS==gs),][2:4,]
	for (vr in pList$VARIABLE) {
		for (suf in c("_p","_t","_")) {
			cat("GS:", gs, "- VAR:", vr, "- SUF:", suf, "\n")
			rs <- raster(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-", vr, suf, "suitability.asc", sep=""))
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
write.csv(rres, "F:/EcoCrop-development/analyses/data/accuracy-metrics.csv", row.names=F)
write.csv(cres, "F:/EcoCrop-development/analyses/data/entropy-curves.csv", row.names=F)

#Accuracy metrics for the merged grids
source("./src/accuracy.R")
test <- read.csv("F:/EcoCrop-development/analyses/data/test.csv"); test <- cbind(test[,18], test[,17])
train <- read.csv("F:/EcoCrop-development/analyses/data/train.csv"); train <- cbind(train[,18], train[,17])
parList <- read.csv("F:/EcoCrop-development/analyses/data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
	cat("GS:", gs, "\n")
	rs <- raster(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-merged_suitability.asc", sep=""))
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
write.csv(rres, "F:/EcoCrop-development/analyses/data/accuracy-metrics-merged.csv", row.names=F)
write.csv(cres, "F:/EcoCrop-development/analyses/data/entropy-curves-merged.csv", row.names=F)

#Area outside and inside different buffer sizes for merged suitability rasters
source("./src/areaFalse.R")
parList <- read.csv("F:/EcoCrop-development/analyses/data/calibration-parameters.csv")
gsList <- unique(parList$GS)
bd <- 50
for (gs in gsList) {
	cat("Process started for BD", bd, "\n")
	a <- raster("F:/EcoCrop-development/analyses/area/area.asc")
	s <- raster(paste("F:/EcoCrop-development/analyses/runs/", gs, "-sorghum-merged_suitability.asc", sep=""))
	b <- raster(paste("F:/EcoCrop-development/analyses/buffered/bf-", bd, "km.asc", sep=""))
	
	m <- areaFalse(a,s,b)
	m <- data.frame(DIST.KM=bd, SUI.IN.AREA=m[2,2], SUI.IN.FRAC=m[2,3], SUI.OT.AREA=m[1,2], SUI.OT.FRAC=m[1,3], UNS.IN.AREA=m[4,2], UNS.IN.FRAC=m[4,3], UNS.OT.AREA=m[3,2], UNS.OT.FRAC=m[3,3])
	
	mo <- m
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
write.csv(mf, "F:/EcoCrop-development/analyses/data/areaFalse.csv", row.names=F)



#Validation stuff... got to validate 1(!),2(!),3(!),4(!),5(!),6(!),7(!),ME(!),MO,MX
setwd("D:/_tools/dapa-climate-change/trunk/EcoCrop")
source("./src/validation.R")
source("./src/createMask.R")
rsl <- raster("F:/EcoCrop-development/analyses/runs/MAX-sorghum-merged_suitability.asc")
shp.faostat <- "F:/EcoCrop-development/agricultural-data/FAOSTAT/afasia-adm0-sorghum.shp"
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
	write.csv(res, paste("F:/EcoCrop-development/analyses/validation/gs-MODE/", ob, ".csv", sep=""))
	#res <- read.csv(paste("F:/EcoCrop-development/analyses/validation/", ob, ".csv", sep=""))
	mets <- valMetrics(res, pres.field=field)
	if (ob == oblist[1]) {
		rr <- cbind(SHP=ob, mets)
	} else {
		rr <- rbind(rr, cbind(SHP=ob, mets))
	}
	rm(res); rm(shp); rm(mets); gc()
}
write.csv(rr, "F:/EcoCrop-development/analyses/validation/gs-MODE/rates.csv", quote=F, row.names=F)


#Validation for all adm1 and all adm2 (sorghum specific)
source("./src/validation.R")
dataf1 <- rbind(read.csv("./data/validation/shp.agromaps1.csv")[,1:12],read.csv("./data/validation/shp.countrystat1.csv")[,1:12])
mets <- cbind(TYPE="adm1", valMetrics(dataf1, pres.field="Is_present"))
dataf2 <- rbind(read.csv("./data/validation/shp.agromaps2.csv")[,1:12],read.csv("./data/validation/shp.countrystat2.csv")[,1:12],read.csv("./data/validation/shp.icrisat.csv")[,1:12])
mets <- rbind(mets, cbind(TYPE="adm2", valMetrics(dataf2, pres.field="Is_present")))
write.csv(mets, "./data/validation/rates-merged.csv", quote=F, row.names=F)


#Projection onto future
source("./src/futureRuns.tmp.R")
gls <- c("bccr_bcm2_0","cccma_cgcm3_1_t47","cccma_cgcm3_1_t63","cnrm_cm3","csiro_mk3_0",
"csiro_mk3_5","gfdl_cm2_0","gfdl_cm2_1","giss_aom","giss_model_eh","giss_model_er","iap_fgoals1_0_g",
"ingv_echam4","inm_cm3_0","ipsl_cm4","miroc3_2_hires","miroc3_2_medres","miub_echo_g",
"mpi_echam5","mri_cgcm2_3_2a","ncar_ccsm3_0","ncar_pcm1","ukmo_hadcm3","ukmo_hadgem1")
bDir="F:/EcoCrop-development"
for (gcm in gls) {
	cat(gcm, "\n")
	fDir <- paste(bDir, "/climate/afasia_2_5min_future", sep="")
	rDir <- paste(bDir, "/analyses/runs-future/", gcm, sep="")
	pDir <- paste(bDir, "/analyses/runs", sep="")
	#Uncompressing the ascii files
	zDir <- paste(fDir, "/", gcm, "/2020_2049/_asciis", sep="")
	aDir <- paste(fDir, "/", gcm, "/2020_2049", sep="")
	vList <- c("prec", "tmin", "tmax", "tmean")
	for (v in vList) {
		f <- paste(zDir, "/", v, "_asc.zip", sep="")
		fd <- paste(aDir, "/", v, "_asc.zip", sep="")
		cat("Copy..."); file.copy(f, fd)
		cat("Unzip..."); unzip(fd, files=NULL, exdir=aDir)
		cat("Remove dup... \n"); file.remove(fd)
	}
	#Run the function
	fut <- futruns(climdir=aDir, oDir=rDir, cDir=pDir, gs=3, gsl=180, 
		parlist="F:/EcoCrop-development/analyses/data/calibration-parameters.csv", cropname="sorghum", 
		ow.runs=F, ow.merge=T)
	#Delete ascii files
	vList <- c("prec", "tmin", "tmax", "tmean")
	for (v in vList) {
		fList <- list.files(aDir, pattern=v)
		for (f in fList) {
			if (file.exists(paste(aDir, "/", f, sep=""))) {file.remove(paste(aDir, "/", f, sep=""))}
		}
	}
}


#Calculate impact metrics per countries and for the study area
source("./src/impacts.R")
bd <- "F:/EcoCrop-development/analyses"
cd <- paste(bd, "/runs", sep="")
fd <- paste(bd, "/runs-future/", sep="")
shname <- "starea-countries.shp" #starea-countries selcountries
sh <- readShapePoly(paste("F:/EcoCrop-development/analysis-mask/", shname, sep=""))
gls <- list.files(fd)
for (gcm in gls) {
	cat("Model", gcm, "\n")
	od <- paste(bd, "/impacts/", gcm, sep="")
	if (!file.exists(od)) {dir.create(od)}
	id <- paste(fd, gcm, sep="")
	r1 <- raster(paste(cd, "/3-sorghum-merged_suitability.asc", sep="")) #current
	r2 <- raster(paste(id, "/3-sorghum-merged_suitability.asc", sep="")) #future
	pp <- iMetrix(r1,r2,sh,od, chggrid=F, impact=T, classes=T)
	
	im <- cbind(GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
	cl <- cbind(GCM=rep(gcm,times=nrow(pp$CLASSES)), pp$CLASSES)
	
	if (gcm == gls[1]) {
		res.im <- im
		res.cl <- cl
	} else {
		res.im <- rbind(res.im, im)
		res.cl <- rbind(res.cl, cl)
	}
	rm(im); rm(cl)
}
write.csv(res.im, paste(bd, "/impacts/impacts-", shname, ".csv", sep=""), quote=T, row.names=F)
write.csv(res.cl, paste(bd, "/impacts/classes-", shname, ".csv", sep=""), quote=T, row.names=F)


#Average and uncertainties
#Creating the stack
source("./src/uncertainty.R")
gcmDir <- "F:/EcoCrop-development/analyses/impacts"
gcmList <- list.files(gcmDir); gcmList <- gcmList[-grep(".shp", gcmList)]
rsn <- "1-Study area-suitability-change"
for (gcm in gcmList) {
	cat(gcm, "\n")
	rDir <- paste(gcmDir, "/", gcm, sep="")
	rs <- raster(paste(rDir, "/", rsn, ".asc", sep=""))
	#v <- extract(rs, xy)
	assign(gcm, rs); rm(rs)

	if (gcm == gcmList[1]) {
		gcmstack <- c(get(gcm))
	} else {
		gcmstack <- c(gcmstack, get(gcm))
	}
}
gcmstack <- stack(gcmstack)
uc <- uncertainties(gcmstack, outFolder="F:/EcoCrop-development/analyses/uncertainties")

#Calculate currently suitable and future suitable area
setwd("D:/_tools/dapa-climate-change/trunk/EcoCrop")
source("./src/impacts.R")
bd <- "F:/EcoCrop-development/analyses"
#cd <- paste(bd, "/runs", sep="")
fd <- paste(bd, "/runs-future/", sep="")
shname <- "starea-countries.shp" #starea-countries selcountries
sh <- readShapePoly(paste("F:/EcoCrop-development/analysis-mask/", shname, sep=""))
#r1 <- raster(paste(cd, "/3-sorghum-merged_suitability.asc", sep="")) #current
#cp <- suitArea(r1, sh)
#write.csv(cp, paste(bd, "/impacts/current-area-", shname,".csv", sep=""), row.names=F, quote=T)
gls <- list.files(fd)
for (gcm in gls) {
	cat("Model", gcm, "\n")
	id <- paste(fd, gcm, sep="")
	r2 <- raster(paste(id, "/3-sorghum-merged_suitability.asc", sep="")) #future
	pp <- suitArea(r2,sh)
	im <- cbind(GCM=rep(gcm,times=nrow(pp)), pp)
	
	if (gcm == gls[1]) {
		res.im <- im
	} else {
		res.im <- rbind(res.im, im)
	}
	rm(im)
}
write.csv(res.im, paste(bd, "/impacts/future-area-", shname, ".csv", sep=""), quote=T, row.names=F)
