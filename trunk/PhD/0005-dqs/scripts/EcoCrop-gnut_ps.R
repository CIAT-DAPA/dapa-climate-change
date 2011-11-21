#### LIBRARIES: raster, maptools, rgdal, sp
####
stop("no")

#sources dir
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop"
source(paste(src.dir,"/src/EcoCrop.R",sep=""))
source(paste(src.dir,"/src/accuracy.R",sep=""))
vaList <- c("prec","tmin","tmean")

bDir <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT"
ty <- "p"; va <- "prec"; sc <- "seasonal"
s <- 1056; p <- 99

#check if model was run, if not, create lock file
psDir <- paste(bDir,"/shuffle-perturb/climate/",ty,"-",sc,sep="")
if (is.na(p)) {
  psDataDir <- paste(psDir,"/",va,"_s-",s,sep="")
} else {
  psDataDir <- paste(psDir,"/",va,"_p-",p,"_s-",s,sep="")
}
checkFile <- paste(psDataDir,"/proc.done",sep="")
if (!file.exists(checkFile)) {
  zz <- file(paste(psDataDir,"/proc.lock",sep=""),open="w");close(zz)
  #add remaining chunk of code below here
} else {
  #do nothing
}

#create a run dir in the ./bin/ folder
bin_dir <- paste(bDir,"/bin",sep="")
runNumber <- round(runif(1,0,9999),0)
run_dir <- paste(bin_dir,"/run-",runNumber,sep="")
while (file.exists(run_dir)) {
  runNumber <- round(runif(1,0,9999),0)
  run_dir <- paste(bin_dir,"/run-",runNumber,sep="")
}
dir.create(run_dir)

#copy data from perturbed folder to run folder
ascList <- list.files(psDataDir,pattern=".asc")
ascList <- lapply(ascList,function(x,idir,odir) {k<-file.copy(paste(idir,"/",x,sep=""),paste(odir,"/",x,sep=""))},psDataDir,run_dir)

#copy the rest of data required for model runs
cat("copying the remaining climate files \n")
notPert <- vaList[which(vaList!=va)]
iDir <- paste(bDir,"/climate/ind_coarse",sep="")
for (vnp in notPert) {
  ascList <- paste(vnp,"_",1:12,".asc",sep="")
  ascList <- lapply(ascList,function(x,idir,odir) {k<-file.copy(paste(idir,"/",x,sep=""),paste(odir,"/",x,sep=""))},iDir,run_dir)
}

#Running the model
gs <- 6
parFile <- paste(bDir,"/analyses/data/calibration-parameters.csv",sep="")
params <- read.csv(parFile); params <- params[which(params$GS==gs),] #select growing parameters
vl <- "tmean"; rw=2
if (!file.exists(paste(run_dir, "/gnut_suitability.asc",sep=""))) {
	eco <- suitCalc(climPath=run_dir, 
                  Gmin=120,Gmax=120,Tkmp=params$KILL[rw],Tmin=params$MIN[rw],Topmin=params$OPMIN[rw],
                  Topmax=params$OPMAX[rw],Tmax=params$MAX[rw],Rmin=params$MIN[1],Ropmin=params$OPMIN[1],
                  Ropmax=params$OPMAX[1],Rmax=params$MAX[1], 
                  outfolder=run_dir, 
                  cropname="gnut")
}
rm(eco); g=gc(); rm(g)
#move the output to required folder and remove unnecessary files, unlink run_dir
status <- file.copy(paste(run_dir,"/gnut_suitability.asc",sep=""),paste(psDataDir,"/gnut_suitability.asc",sep=""))
ascList <- list.files(run_dir)
ascList <- lapply(ascList,function(x,idir) {k<-file.remove(paste(idir,"/",x,sep=""))},run_dir)
unlink(run_dir,recursive=T)

#Assess accuracy of each growing season and each parameter tuning
test <- read.csv(paste(bDir,"/analyses/data/test.csv",sep="")); test <- cbind(test[,"longitude"], test[,"latitude"])
train <- read.csv(paste(bDir,"/analyses/data/train.csv",sep="")); train <- cbind(train[,"longitude"], train[,"latitude"])
parList <- read.csv(paste(bDir,"/analyses/data/calibration-parameters.csv",sep=""))
pList <- parList[which(parList$GS==gs),][2,]
vr <- pList$VARIABLE

rs <- raster(paste(psDataDir,"/gnut_suitability.asc",sep=""))
tem <- accMetrics(rs, test) #doing with test data
trm <- accMetrics(rs, train) #doing with training data
res_acc <- data.frame(GS=gs, VARIABLE=vr, TYPE="suitability", TEST.AV.SUIT=tem$METRICS$SUIT, 
                      TEST.SD.SUIT=tem$METRICS$SUITSD, TEST.MAX.SUIT=tem$METRICS$SUITX, 
                      TEST.MIN.SUIT=tem$METRICS$SUITN, TEST.OMISSION.RATE=tem$METRICS$OMISSION_RATE, 
                      TEST.ERROR=tem$METRICS$RMSQE, TEST.ERR.DIST=tem$METRICS$ERR_DIST, 
                      TEST.MXE=tem$METRICS$MAX_ENT, TEST.SLOPE=tem$METRICS$SLOPE, 
                      TRAIN.AV.SUIT=trm$METRICS$SUIT, TRAIN.SD.SUIT=trm$METRICS$SUITSD, 
                      TRAIN.MAX.SUIT=trm$METRICS$SUITX, TRAIN.MIN.SUIT=trm$METRICS$SUITN, 
                      TRAIN.OMISSION.RATE=trm$METRICS$OMISSION_RATE, TRAIN.ERROR=trm$METRICS$RMSQE, 
                      TRAIN.ERR.DIST=trm$METRICS$ERR_DIST, TRAIN.MXE=trm$METRICS$MAX_ENT, 
                      TRAIN.SLOPE=trm$METRICS$SLOPE)
write.csv(res_acc, paste(psDataDir,"/accuracy-metrics.csv",sep=""), row.names=F)

## here!

#plot accuracy metrics here
plot.acc <- rres[which(rres$TYPE=="_suitability"),]
tiff("./analyses/img/accuracy.tiff",
     res=300,pointsize=10,width=1500,height=1300,units="px",compression="lzw")
par(mar=c(4.5,4,1,1),cex=1)
plot(plot.acc$TEST.OMISSION.RATE[which(plot.acc$VARIABLE=="tmean")],
     plot.acc$TEST.ERROR[which(plot.acc$VARIABLE=="tmean")],
     xlim=c(0,1),ylim=c(0,1),pch=21,
     xlab="OR",ylab="RMSE")
points(plot.acc$TEST.OMISSION.RATE[which(plot.acc$VARIABLE=="tmin")],
       plot.acc$TEST.ERROR[which(plot.acc$VARIABLE=="tmin")],
       pch=22)
points(plot.acc$TEST.OMISSION.RATE[which(plot.acc$VARIABLE=="tmax")],
       plot.acc$TEST.ERROR[which(plot.acc$VARIABLE=="tmax")],
       pch=24)
abline(h=0.5,lwd=1.2,lty=2)
abline(v=0.1,lwd=1.2,lty=2)
grid()
legend(0.6,1,cex=0.8,pch=c(21,22,24),legend=c("Tmean-based","Tmin-based","Tmax-based"))
dev.off()

#Validation stuff
#THIS MUST BE CHANGED!!!
#!!!
setwd("D:/_tools/dapa-climate-change/trunk/EcoCrop")
source(paste(src.dir,"/src/validation.v2.R",sep=""))
rsl <- raster("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/runs/6-gnut-tmean_suitability.asc")
shp.icrisat <- "D:/CIAT_work/GLAM/PNAS-paper/India-yield-data/shp/IND2-gnut.shp"
oblist <- ls(pattern="shp")
for (ob in oblist) {
	cat("Processing", ob, "\n")
	shp <- readShapePoly(get(ob))
	field <- "H_ISPRES"
	res <- extractFromShape(shp, field, naValue=-9999, rsl)
	write.csv(res, paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/evaluation/", ob, ".csv", sep=""),row.names=F)
	res <- read.csv(paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/evaluation/", ob, ".csv", sep=""))
	mets <- valMetrics(res, pres.field=field,thresh=0)
	if (ob == oblist[1]) {
		rr <- cbind(SHP=ob, mets)
	} else {
		rr <- rbind(rr, cbind(SHP=ob, mets))
	}
	rm(res); rm(shp); rm(mets); gc()
}
write.csv(rr, "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/evaluation/rates.csv", quote=F, row.names=F)

#plot relation between yield/aharv/pdn and PS/ME
res2 <- res[which(!is.na(res$H_ISPRES)),]
res2 <- res2[which(res2$Y_AVG!=0),]; res2 <- res2[which(res2$PS!=1),]

tiff("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/img/yield-relations.tiff",
     res=600,pointsize=5,width=2000,height=1300,units="px",compression="lzw")
par(mar=c(4.5,4,1,1),cex=0.5,mfrow=c(2,3),lwd=0.7)
plot(res2$Y_AVG,res2$PS,pch=20,xlab="Mean yield (kg/ha)",ylab="Fraction suitable",lwd=0.4); grid()
plot(res2$H_AVG,res2$PS,pch=20,xlab="Mean area harvested (ha x 10^3)",ylab="Fraction suitable",lwd=0.4); grid()
plot(res2$P_AVG,res2$PS,pch=20,xlab="Mean production (ton x 10^3)",ylab="Fraction suitable",lwd=0.4); grid()
plot(res2$Y_AVG,res2$MZ,pch=20,xlab="Mean yield (kg/ha)",ylab="Mean suitability (%)",lwd=0.4); grid()
plot(res2$H_AVG,res2$MZ,pch=20,xlab="Mean area harvested (ha x 10^3)",ylab="Mean suitability (%)",lwd=0.4); grid()
plot(res2$P_AVG,res2$MZ,pch=20,xlab="Mean production (ton x 10^3)",ylab="Mean suitability (%)",lwd=0.4); grid()
dev.off()
