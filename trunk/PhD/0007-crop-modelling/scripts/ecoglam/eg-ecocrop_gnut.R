#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL

#### LIBRARIES: raster, maptools, rgdal, sp
library(raster); library(rgdal); library(maptools)
data(wrld_simpl)
stop("not to run yet")

#sources dir
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop" #local
src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"

#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/EcoCrop" #eljefe
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"

#basic information
crop_name <- "gnut"
b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")
ec_dir <- paste(crop_dir,"/ecg_analyses/ecocrop-",tolower(crop_name),sep="")


#Getting unique coordinates
source(paste(src.dir,"/src/getUniqueCoord.R",sep="")) #loading the function
rs <- read.csv(paste(ec_dir,"/analyses/data/gnut-india.csv",sep="")) #load the data
rs <- getUniqueCoord(rs, fields=c(24,23), resol=2.5/60) #running the function


#Dummy part of unloading and re-loading the data
write.csv(rs, paste(ec_dir,"/analyses/data/dataset2_5m.csv",sep=""), row.names=F, quote=T) #write outcome into new file
rs <- rs[which(rs$IS_UNIQUE == TRUE),] #selecting unique records
write.csv(rs, paste(ec_dir,"/analyses/data/unique2_5m.csv",sep=""), row.names=F, quote=T) #write new dataset only containing unique records
rs <- read.csv(paste(ec_dir,"/analyses/data/unique2_5m.csv",sep="")) #re-loading the data


#Splitting in test/train datasets
source(paste(src.dir,"./src/randomSplit.R",sep=""))
rs <- randomSplit(rs, 20) #20 is percentage of data to be taken out

#Plot & write test and train datasets separately
jpeg(paste(ec_dir,"/analyses/img/test_train.jpg",sep=""), quality=100, height=800, width=600)
plot(cbind(rs$longitude[which(rs$TEST_TRAIN == "TEST")], rs$latitude[which(rs$TEST_TRAIN == "TEST")]), 
     pch="+", col="red", xlim=c(min(rs$longitude),max(rs$longitude)), cex=1.5,
     ylim=c(min(rs$latitude),max(rs$latitude)), xlab="", ylab="")
plot(wrld_simpl, add=T)
points(cbind(rs$longitude[which(rs$TEST_TRAIN == "TRAIN")], 
             rs$latitude[which(rs$TEST_TRAIN == "TRAIN")]), pch=20, col="blue", cex=1.5)
dev.off()

#writing the test and training data
write.csv(rs, paste(ec_dir,"/analyses/data/unique.csv",sep=""), row.names=F, quote=T) #write unique records with new field TRAIN/TEST
write.csv(rs[which(rs$TEST_TRAIN == "TEST"),], paste(ec_dir,"/analyses/data/test.csv",sep=""), row.names=F, quote=T) #reselect and store test data
write.csv(rs[which(rs$TEST_TRAIN == "TRAIN"),], paste(ec_dir,"/analyses/data/train.csv",sep=""), row.names=F, quote=T) #reselect and store train data


#Extracting climate data
source(paste(src.dir,"/src/extractClimateData.R",sep=""))
rs <- read.csv(paste(ec_dir,"/analyses/data/unique.csv",sep="")) #load unique records
for (v in c("prec", "tmin", "tmean", "tmax")) {
  rs <- extractMonthlyData(wd=paste(ec_dir,"/climate/ind_2_5min",sep=""), 
                           variable=v, ext=".tif", rs, fields=c(24,23), verbose=T)
}
write.csv(rs, paste(ec_dir,"/analyses/data/climates.csv",sep=""), row.names=F, quote=T)


#Calculating gs parameters for calibration
source(paste(src.dir,"/src/calibrationParameters.R",sep=""))
rs <- read.csv(paste(ec_dir,"/analyses/data/climates.csv",sep=""))

#here use Sacks et al. (2010) planting dates to get an approximation of when each
#of these points is sown. Use the average sowing and harvest date
pdate_dir <- paste(b_dir,"/climate-signals-yield/",toupper(crop_name),"/calendar/",tolower(crop_name),sep="")
pdate <- raster(paste(pdate_dir,"/plant.tif",sep=""))
xt <- extent(raster(paste(pdate_dir,"/plant_lr.tif",sep="")))
pdate <- crop(pdate,xt)

hdate <- raster(paste(pdate_dir,"/harvest.tif",sep=""))
hdate <- crop(hdate,xt)

#using these data select the growing season for each of the points
rs$SOW_DATE <- round(extract(pdate,cbind(x=rs$longitude,y=rs$latitude)),0)
rs$HAR_DATE <- round(extract(hdate,cbind(x=rs$longitude,y=rs$latitude)),0)

source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))
dg <- createDateGrid(2005)
dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
dg$DAY <- as.numeric(substr(dg$MTH.DAY,5,6))
dg$MTH.DAY <- NULL

#function to find corresponding fraction of month for a given Julian day
find_month <- function(jd,dg) {
  mth <- dg$MTH[which(dg$JD==jd)]
  day <- dg$DAY[which(dg$JD==jd)] / length(dg$DAY[which(dg$MTH==mth)])
  mth <- mth+day
  return(mth)
}

rs$SOW_MTH <- round(sapply(rs$SOW_DATE,FUN=find_month,dg),0)
rs$HAR_MTH <- round(sapply(rs$HAR_DATE,FUN=find_month,dg),0)
rs$DUR <- NA
rs$DUR[which(rs$HAR_MTH < rs$SOW_MTH)] <- (365-rs$HAR_MTH[which(rs$HAR_MTH < rs$SOW_MTH)]) + rs$SOW_MTH[which(rs$HAR_MTH < rs$SOW_MTH)]
rs$DUR[which(rs$HAR_MTH > rs$SOW_MTH)] <- rs$HAR_MTH[which(rs$HAR_MTH > rs$SOW_MTH)] - rs$SOW_MTH[which(rs$HAR_MTH > rs$SOW_MTH)]


#produce growing season parameters based on sowing date and duration
#for (i in 1:nrow(rs)) {
get_gs_data <- function(i,rs) {
  #i=1
  x <- rs[i,] #get that row
  x2 <- calibrationParameters(x, gs=x$DUR, verbose=F) #get growing season data for that duration
  this_gs <- x$SOW_MTH #which month the crop was planted
  fields <- c(paste("GS",this_gs,"_P",sep=""),paste("GS",this_gs,"_T",sep=""),
              paste("GS",this_gs,"_N",sep=""),paste("GS",this_gs,"_X",sep=""))
  gs_data <- x2[,fields] #get only data for the growing season when crop was planted
  names(gs_data) <- c("GS1_P","GS1_T","GS1_N","GS1_X") #assign new names to fields for final merging
  x <- cbind(x,gs_data) #merge with original input data
  return(x)
}

rs_out <- lapply(1:nrow(rs),FUN=get_gs_data,rs)
rs_out <- do.call("rbind",rs_out)

write.csv(rs_out, paste(ec_dir,"/analyses/data/calibration.csv",sep=""), row.names=F, quote=T)



#Get calibration parameters
source(paste(src.dir,"/src/getParameters.R",sep=""))
dataset <- read.csv(paste(ec_dir,"/analyses/data/calibration.csv",sep=""))
gs <- 1
plotdata <- dataset[,grep(paste("GS", gs, "_", sep=""), names(dataset))]
varList <- c("prec", "tmean", "tmin", "tmax")
v <- 1
for (varn in varList) {
  calPar <- getParameters(plotdata[,v], nb=200, plotit=T, 
                          plotdir=paste(ec_dir,"/analyses/img",sep=""), 
                          gs=gs, varname=varn)
  if (v == 1) {finalTable <- calPar} else {finalTable <- rbind(finalTable, calPar)}
  v <- v+1
}
write.csv(finalTable, paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""), row.names=F)



#Running the model
source(paste(src.dir,"/src/EcoCrop.R",sep=""))
gs <- 1
cat("GS", gs, "\n")
p <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
p <- p[which(p$GS==gs),]
vl <- c("tmean","tmin","tmax")
for (rw in 2:4) {
  if (!file.exists(paste(ec_dir,"/analyses/runs/", gs, "-",crop_name,"-",vl[rw-1],"-suitability.jpg",sep=""))) {
    eco <- suitCalc(climPath=paste(ec_dir,"/climate/ind_2_5min",sep=""), 
                    Gmin=120,Gmax=120,Tkmp=p$KILL[rw],Tmin=p$MIN[rw],Topmin=p$OPMIN[rw],
                    Topmax=p$OPMAX[rw],Tmax=p$MAX[rw],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                    Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                    outfolder=paste(ec_dir,"/analyses/runs",sep=""), 
                    cropname=paste(gs,"-",crop_name,"-",vl[rw-1],sep=""),ext=".tif")
    jpeg(paste(ec_dir,"/analyses/runs/", gs, "-",crop_name,"-",vl[rw-1],"-suitability.jpg",sep=""), quality=100)
    plot(eco)
    dev.off()
  }
}


#Assess accuracy of each growing season and each parameter tuning
source(paste(src.dir,"/src/accuracy.R",sep=""))
test <- read.csv("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/data/test.csv"); test <- cbind(test[,"longitude"], test[,"latitude"])
train <- read.csv("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/data/train.csv"); train <- cbind(train[,"longitude"], train[,"latitude"])
parList <- read.csv("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/data/calibration-parameters.csv")
gsList <- unique(parList$GS)
for (gs in gsList) {
  pList <- parList[which(parList$GS==gs),][2:4,]
  for (vr in pList$VARIABLE) {
    for (suf in c("_p","_t","_")) {
      cat("GS:", gs, "- VAR:", vr, "- SUF:", suf, "\n")
      rs <- raster(paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/runs/", gs, "-gnut-", vr, suf, "suitability.asc", sep=""))
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
write.csv(rres, "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/data/accuracy-metrics.csv", row.names=F)
write.csv(cres, "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/data/entropy-curves.csv", row.names=F)


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


#Validation stuff... got to validate 1(!),2(!),3(!),4(!),5(!),6(!),7(!),ME(!),MO,MX
setwd("D:/_tools/dapa-climate-change/trunk/EcoCrop")
source(paste(src.dir,"/src/validation.R",sep=""))
source(paste(src.dir,"/src/createMask.R",sep=""))
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


#Calculate area for aggregating to average district size
sh <- readShapePoly("D:/CIAT_work/GLAM/PNAS-paper/India-yield-data/shp/IND2-gnut.shp")
npol <- length(sh@polygons)

for (i in 1:npol) {
  if (i==1) {
    a <- sh@polygons[[i]]@area
  } else {
    a <- c(a,sh@polygons[[i]]@area)
  }
}

fct <- round(mean(a)/2.5*60,0) #aggregating factor

rsl <- raster("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/analyses/runs/6-gnut-tmean_suitability.asc")
shp <- readShapePoly("D:/CIAT_work/GLAM/PNAS-paper/India-yield-data/shp/IND2-gnut.shp")
naValue <- -9999

#Extract data from shapefile
shpData <- shp@data
pafield <- "H_ISPRES"

#create raster
rs <- raster(rsl)
pars <- rasterize(shp,rs,field=pafield); pars[which(pars[]==naValue)] <- NA

#aggregate raster
pars.agg <- aggregate(pars,fact=fct,fun=mean) #aggregate
pars.agg[which(pars.agg[]<0.75)] <- 0; pars.agg[which(pars.agg[]>=0.75)] <- 1 #set anything below 25%

prec1 <- raster("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_2_5min/prec_1.asc")
prec1.agg <- aggregate(prec1,fact=fct,fun=mean)

brks <- quantile(c(prec1[],prec1.agg[]),probs=seq(0,1,by=0.05),na.rm=T)
brks.lab <- round(brks,0)
nb <- length(brks)-1

tiff("analyses/img/prec-orig-res.tiff",
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(prec1,useRaster=F,
     col=colorRampPalette(c("light blue","blue","purple"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=nb*100)
plot(wrld_simpl,add=T,lwd=0.6)
grid()
dev.off()

tiff("analyses/img/prec-aggr-res.tiff",
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(prec1.agg,useRaster=F,
     col=colorRampPalette(c("light blue","blue","purple"))(nb),
     breaks=brks,
     lab.breaks=brks.lab,
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=nb*100)
plot(wrld_simpl,add=T,lwd=0.6)
grid()
dev.off()

tiff("analyses/img/crop-presence-aggr.tiff",
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(pars.agg,useRaster=F,
     col=c("red","green"),
     breaks=c(0,0.5,1),
     lab.breaks=c("Absent",NA,"Present"),
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=nb*100)
#plot(wrld_simpl,add=T,lwd=0.6)
plot(sh,add=T,border="black")
grid()
dev.off()

#Quantify the effects of alteration (on Jan rain)
d.prec1 <- density(prec1[],na.rm=T); d.prec1$y <- d.prec1$y / max(d.prec1$y)
d.prec1.agg <- density(prec1.agg[],na.rm=T); d.prec1.agg$y <- d.prec1.agg$y / max(d.prec1.agg$y)

tiff("./analyses/img/aggregation-effects.tiff",
     res=300,pointsize=10,width=1500,height=1300,units="px",compression="lzw")
par(mar=c(4.5,4,1,1),cex=1)
plot(d.prec1,xlim=c(0,100),col='red',main=NA,xlab="January rainfall (mm)",ylab="Normalised PDF",lwd=1.2)
lines(d.prec1.agg,col='blue',lwd=1.2)
abline(v=mean(prec1[],na.rm=T),lwd=1.2,lty=2,col='red')
abline(v=mean(prec1.agg[],na.rm=T),lwd=1.2,lty=2,col='blue')
grid()
legend(60,1,cex=0.8,lty=c(1,1),legend=c("Original","Aggregated"),col=c("red","blue"))
dev.off()

#store the evaluation dataset
dir.create("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/shuffle-perturb/evaluation")
pars <- writeRaster(pars,"D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/shuffle-perturb/evaluation/pa_fine.asc",format='ascii')
pars.agg <- writeRaster(pars.agg,"D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/shuffle-perturb/evaluation/pa_coarse.asc",format='ascii')

#aggregate the climate data
dir.create("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_coarse")
for (i in 1:12) {
  cat("Aggregating month",i,"\n")
  prec <- raster(paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_2_5min/prec_",i,".asc",sep=""))
  tmin <- raster(paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_2_5min/tmin_",i,".asc",sep=""))
  tmean <- raster(paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_2_5min/tmean_",i,".asc",sep=""))
  
  prec <- aggregate(prec,fact=fct,fun=mean)
  tmin <- aggregate(tmin,fact=fct,fun=mean)
  tmean <- aggregate(tmean,fact=fct,fun=mean)
  
  prec <- writeRaster(prec,paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_coarse/prec_",i,".asc",sep=""),format='ascii')
  tmin <- writeRaster(tmin,paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_coarse/tmin_",i,".asc",sep=""),format='ascii')
  tmean <- writeRaster(tmean,paste("D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT/climate/ind_coarse/tmean_",i,".asc",sep=""),format='ascii')
}






