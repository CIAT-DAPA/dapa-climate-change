#Julian Ramirez-Villegas
#July 2012
#CIAT / CCAFS / UoL

#### LIBRARIES: raster, maptools, rgdal, sp
library(raster); library(rgdal); library(maptools); library(MASS)
data(wrld_simpl)
stop("not to run yet")

#sources dir
src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop" #local
src.dir2 <- "D:/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
src.dir3 <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/EcoCrop" #eljefe
#src.dir2 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0006-weather-data"
#src.dir3 <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"

source(paste(src.dir3,"/scripts/ecoglam/eg-ecocrop_gnut-functions.R",sep=""))
source(paste(src.dir,"/src/getUniqueCoord.R",sep=""))
source(paste(src.dir,"/src/randomSplit.R",sep=""))
source(paste(src.dir,"/src/extractClimateData.R",sep=""))
source(paste(src.dir,"/src/calibrationParameters.R",sep=""))
source(paste(src.dir2,"/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir2,"/scripts/watbal.R",sep=""))
source(paste(src.dir,"/src/getParameters.R",sep=""))
source(paste(src.dir,"/src/EcoCrop.R",sep=""))
source(paste(src.dir,"/src/validation.R",sep=""))
source(paste(src.dir,"/src/createMask.R",sep=""))
source(paste(src.dir,"/src/accuracy.R",sep=""))


#basic information
crop_name <- "gnut"
vnames <- read.table(paste(src.dir3,"/data/GLAM-varnames.tab",sep=""),sep="\t",header=T)
r_dir <- "W:/eejarv/PhD-work/crop-modelling"
#r_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
b_dir <- paste(r_dir,"/GLAM",sep="")
crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")
ec_dir <- paste(crop_dir,"/ecg_analyses/ecocrop-",tolower(crop_name),sep="")
glam_dir <- paste(crop_dir,"/ecg_analyses/glam_output",sep="")

######################################################################
#Getting unique coordinates
rs <- read.csv(paste(ec_dir,"/analyses/data/gnut-india.csv",sep="")) #load the data
rs <- getUniqueCoord(rs, fields=c(24,23), resol=2.5/60) #running the function


######################################################################
#Dummy part of unloading and re-loading the data
write.csv(rs, paste(ec_dir,"/analyses/data/dataset2_5m.csv",sep=""), row.names=F, quote=T) #write outcome into new file
rs <- rs[which(rs$IS_UNIQUE == TRUE),] #selecting unique records
write.csv(rs, paste(ec_dir,"/analyses/data/unique2_5m.csv",sep=""), row.names=F, quote=T) #write new dataset only containing unique records
rs <- read.csv(paste(ec_dir,"/analyses/data/unique2_5m.csv",sep="")) #re-loading the data


######################################################################
#Splitting in test/train datasets
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


######################################################################
#Extracting climate data
rs <- read.csv(paste(ec_dir,"/analyses/data/unique.csv",sep="")) #load unique records
for (v in c("prec", "tmin", "tmean", "tmax")) {
  rs <- extractMonthlyData(wd=paste(ec_dir,"/climate/ind_2_5min",sep=""), 
                           variable=v, ext=".tif", rs, fields=c(24,23), verbose=T)
}
write.csv(rs, paste(ec_dir,"/analyses/data/climates.csv",sep=""), row.names=F, quote=T)


######################################################################
#Calculating gs parameters for calibration
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

dg <- createDateGrid(2005)
dg$MTH <- as.numeric(substr(dg$MTH.DAY,2,3))
dg$DAY <- as.numeric(substr(dg$MTH.DAY,5,6))
dg$MTH.DAY <- NULL

rs$SOW_MTH <- round(sapply(rs$SOW_DATE,FUN=find_month,dg),0)
rs$HAR_MTH <- round(sapply(rs$HAR_DATE,FUN=find_month,dg),0)
rs$DUR <- NA
rs$DUR[which(rs$HAR_MTH < rs$SOW_MTH)] <- (365-rs$HAR_MTH[which(rs$HAR_MTH < rs$SOW_MTH)]) + rs$SOW_MTH[which(rs$HAR_MTH < rs$SOW_MTH)]
rs$DUR[which(rs$HAR_MTH > rs$SOW_MTH)] <- rs$HAR_MTH[which(rs$HAR_MTH > rs$SOW_MTH)] - rs$SOW_MTH[which(rs$HAR_MTH > rs$SOW_MTH)]

#produce growing season parameters based on sowing date and duration
rs_out <- lapply(1:nrow(rs),FUN=get_gs_data,rs)
rs_out <- do.call("rbind",rs_out)
write.csv(rs_out, paste(ec_dir,"/analyses/data/calibration.csv",sep=""), row.names=F, quote=T)


######################################################################
#Get calibration parameters
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


######################################################################
#Running the model
gs <- 1
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


######################################################################
#Assess accuracy of each growing season and each parameter tuning
test <- read.csv(paste(ec_dir,"/analyses/data/test.csv",sep=""))
test <- cbind(test[,"longitude"], test[,"latitude"])

train <- read.csv(paste(ec_dir,"/analyses/data/train.csv",sep=""))
train <- cbind(train[,"longitude"], train[,"latitude"])

parList <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
gsList <- unique(parList$GS)
for (gs in gsList) {
  pList <- parList[which(parList$GS==gs),][2:4,]
  for (vr in pList$VARIABLE) {
    for (suf in c("_p","_t","_")) {
      cat("GS:", gs, "- VAR:", vr, "- SUF:", suf, "\n")
      rs <- raster(paste(ec_dir,"/analyses/runs/", gs, "-",crop_name,"-", vr, suf, "suitability.asc", sep=""))
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
write.csv(rres, paste(ec_dir,"/analyses/data/accuracy-metrics.csv",sep=""), row.names=F)
write.csv(cres, paste(ec_dir,"/analyses/data/entropy-curves.csv",sep=""), row.names=F)

#plot accuracy metrics here
plot.acc <- rres[which(rres$TYPE=="_suitability"),]
tiff(paste(ec_dir,"/analyses/img/accuracy.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
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


######################################################################
#Validation stuff... got to validate 1(!),2(!),3(!),4(!),5(!),6(!),7(!),ME(!),MO,MX
rsl <- raster(paste(ec_dir,"/analyses/runs/1-",crop_name,"-tmean_suitability.asc",sep=""))
shp.icrisat <- paste(ec_dir,"/analyses/evaluation/IND2-gnut.shp",sep="")
oblist <- ls(pattern="shp")
for (ob in oblist) {
  cat("Processing", ob, "\n")
  shp <- readShapePoly(get(ob))
  field <- "H_ISPRES"
  res <- extractFromShape(shp, field, naValue=-9999, rsl)
  write.csv(res, paste(ec_dir,"/analyses/evaluation/", ob, ".csv", sep=""),row.names=F)
  res <- read.csv(paste(ec_dir,"/analyses/evaluation/", ob, ".csv", sep=""))
  mets <- valMetrics(res, pres.field=field,thresh=0)
  if (ob == oblist[1]) {
    rr <- cbind(SHP=ob, mets)
  } else {
    rr <- rbind(rr, cbind(SHP=ob, mets))
  }
  rm(res); rm(shp); rm(mets); gc()
}
write.csv(rr, paste(ec_dir,"/analyses/evaluation/rates.csv", sep=""),quote=F, row.names=F)


######################################################################
#aggregate the original evaluation data to desired resolution
dres <- 1 #desired resolution, in degree
fct <- round(dres/2.5*60,0) #aggregating factor

rsl <- raster(paste(ec_dir,"/analyses/runs/1-",crop_name,"-tmean_suitability.asc",sep=""))
shp <- readShapePoly(paste(ec_dir,"/analyses/evaluation/IND2-gnut.shp",sep=""))
naValue <- -9999

#Extract data from shapefile
shpData <- shp@data
pafield <- "H_ISPRES"

#create raster
rs <- raster(rsl)
pars <- raster:::.polygonsToRaster(shp,rs,field=pafield)
pars[which(pars[]==naValue)] <- NA

#aggregate raster
pars.agg <- aggregate(pars,fact=fct,fun=mean) #aggregate
pars.agg[which(pars.agg[]<0.75)] <- 0 #set anything below 25%
pars.agg[which(pars.agg[]>=0.75)] <- 1 #set anything below 25%

#plot the presence-absence surface
aspect <- (pars@extent@xmax-pars@extent@xmin)/(pars@extent@ymax-pars@extent@ymin)+0.15
tiff(paste(ec_dir,"/analyses/img/crop-presence-aggr.tiff",sep=""),
     res=300,pointsize=10,width=1500,height=1500*aspect,units="px",compression="lzw")
par(mar=c(2.5,2.5,1,1),cex=0.8)
plot(pars.agg,useRaster=F,
     col=c("red","green"),
     breaks=c(0,0.5,1),
     lab.breaks=c("Absent",NA,"Present"),
     horizontal=T,
     legend.width=1.5,
     legend.shrink=0.8,
     nlevel=100)
#plot(wrld_simpl,add=T,lwd=0.6)
plot(shp,add=T,border="black")
grid()
dev.off()

#store the evaluation datasets
pars.agg <- writeRaster(pars.agg,paste(ec_dir,"/analyses/evaluation/pa_coarse.asc",sep=""),format="ascii")
pars <- writeRaster(pars,paste(ec_dir,"/analyses/evaluation/pa_fine.asc",sep=""),format="ascii")


######################################################################
#1. run & assess EcoCrop with IITM/CRU data, climatological means

#a. copy the data to a folder for further runs
iitm_dir <- paste(r_dir,"/climate-data/IND-TropMet_clm",sep="")
cru_dir <- paste(r_dir,"/climate-data/CRU_TS_v3-1_data",sep="")
clm_dir <- paste(ec_dir,"/climate/climatology",sep="")

clm_type <- "1966_1993"
oclm_dir <- copy_clim_data(clm_type=clm_type,
                           iitm_dir=paste(iitm_dir,"/climatology_",clm_type,sep=""),
                           cru_dir=paste(cru_dir,"/climatology_",clm_type,sep=""),
                           oclm_dir=clm_dir,cru_prefix=NA)

clm_type <- "1960_2000"
oclm_dir <- copy_clim_data(clm_type=clm_type,
                           iitm_dir=paste(iitm_dir,"/climatology_",clm_type,sep=""),
                           cru_dir=paste(cru_dir,"/climatology_",clm_type,sep=""),
                           oclm_dir=clm_dir,cru_prefix=NA)

#b. run EcoCrop with these two datasets
#run details (parameters)
gs <- 1
p <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
p <- p[which(p$GS==gs),]
gs_type <- "tmean"
gs_loc <- which(p$VARIABLE == gs_type)

#model run
period <- "1966_1993"
eco <- suitCalc(climPath=paste(clm_dir,"/",period,sep=""), 
                Gmin=120,Gmax=120,Tkmp=p$KILL[gs_loc],Tmin=p$MIN[gs_loc],Topmin=p$OPMIN[gs_loc],
                Topmax=p$OPMAX[gs_loc],Tmax=p$MAX[gs_loc],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                outfolder=paste(ec_dir,"/analyses/runs_eg/clm_",period,sep=""), 
                cropname=paste(gs,"-",crop_name,"-",gs_type,sep=""),ext=".tif")

#model run, second period
period <- "1960_2000"
eco <- suitCalc(climPath=paste(clm_dir,"/",period,sep=""), 
                Gmin=120,Gmax=120,Tkmp=p$KILL[gs_loc],Tmin=p$MIN[gs_loc],Topmin=p$OPMIN[gs_loc],
                Topmax=p$OPMAX[gs_loc],Tmax=p$MAX[gs_loc],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                outfolder=paste(ec_dir,"/analyses/runs_eg/clm_",period,sep=""), 
                cropname=paste(gs,"-",crop_name,"-",gs_type,sep=""),ext=".tif")

#c. assess EcoCrop using the presence-absence surface (low resolution)
pa_rs <- raster(paste(ec_dir,"/analyses/evaluation/pa_coarse.asc",sep=""))
pred_p1 <- raster(paste(ec_dir,"/analyses/runs_eg/clm_1966_1993/",gs,"-",crop_name,"-",gs_type,"_suitability.asc",sep=""))
pred_p2 <- raster(paste(ec_dir,"/analyses/runs_eg/clm_1960_2000/",gs,"-",crop_name,"-",gs_type,"_suitability.asc",sep=""))

met_p1 <- eval_ecocrop(rsl=pred_p1,eval_rs=pa_rs)
met_p1 <- cbind(PERIOD="1966_1993",met_p1)

met_p2 <- eval_ecocrop(rsl=pred_p2,eval_rs=pa_rs)
met_p2 <- cbind(PERIOD="1960_2000",met_p2)

met <- rbind(met_p1,met_p2)

#write evaluation metrics
omet_dir <- paste(ec_dir,"/analyses/runs_eg/evaluation",sep="")
if (!file.exists(omet_dir)) {dir.create(omet_dir,recursive=T)}
write.csv(met,paste(omet_dir,"/clm_metrics.csv",sep=""),quote=T,row.names=F)


######################################################################
######################################################################
#2. run historical simulations of EcoCrop (1966-1993)
iitm_dir <- paste(r_dir,"/climate-data/IND-TropMet_mon",sep="")
cru_dir <- paste(r_dir,"/climate-data/CRU_TS_v3-1_data",sep="")
clm_dir <- paste(ec_dir,"/climate/yearly",sep="")

#loop years to produce the datasets for running the model
for (yr in 1966:1993) {
  cat("processing year",yr,"\n")
  oclim_dir <- copy_clim_data(clm_type=yr,
                              iitm_dir=paste(iitm_dir,"/",yr,sep=""),
                              cru_dir=paste(cru_dir,"/monthly_grids",sep=""),
                              oclm_dir=clm_dir,cru_prefix=yr)
}


#run the model using each year's climate data
#run details (1966-1993 climatology)
gs <- 1
p <- read.csv(paste(ec_dir,"/analyses/data/calibration-parameters.csv",sep=""))
p <- p[which(p$GS==gs),]
gs_type <- "tmean"
gs_loc <- which(p$VARIABLE == gs_type)

#loop through years and calculate suitability
met_all <- data.frame()
for (yr in 1966:1993) {
  cat("processing year",yr,"\n")
  eco <- suitCalc(climPath=paste(clm_dir,"/",yr,sep=""), 
                  Gmin=120,Gmax=120,Tkmp=p$KILL[gs_loc],Tmin=p$MIN[gs_loc],Topmin=p$OPMIN[gs_loc],
                  Topmax=p$OPMAX[gs_loc],Tmax=p$MAX[gs_loc],Rmin=p$MIN[1],Ropmin=p$OPMIN[1],
                  Ropmax=p$OPMAX[1],Rmax=p$MAX[1], 
                  outfolder=paste(ec_dir,"/analyses/runs_eg/yearly/",yr,sep=""), 
                  cropname=paste(gs,"-",crop_name,"-",gs_type,sep=""),ext=".tif")
  
  #use yearly harv. area data to assess EcoCrop
  aha_dir <- paste(crop_dir,"/harvested_area/raster/gridded",sep="")
  aha_rs <- raster(paste(aha_dir,"/raw-",yr,".asc",sep=""))
  aha_rs[which(aha_rs[] > 0)] <- 1
  
  met_yr <- eval_ecocrop(rsl=eco[[5]],eval_rs=aha_rs)
  met_yr <- cbind(YEAR=yr,met_yr)
  met_all <- rbind(met_all,met_yr)
}
write.csv(met_all,paste(omet_dir,"/yearly_metrics.csv",sep=""),quote=T,row.names=F)

#plot accuracy metrics here
#true positive rate
tiff(paste(ec_dir,"/analyses/img/historical_TPR.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=1)
plot(met_all$YEAR,met_all$TPR,ylim=c(0.75,1),pch=20,ty="l",xlab=NA,ylab="TPR")
abline(h=met$TPR[1],lwd=1.2,lty=2,col="red")
grid()
dev.off()

#false positive rate
tiff(paste(ec_dir,"/analyses/img/historical_FPR.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=1)
plot(met_all$YEAR,met_all$FPR,ylim=c(0,0.3),pch=20,ty="l",xlab=NA,ylab="FPR")
abline(h=met$FPR[1],lwd=1.2,lty=2,col="red")
grid()
dev.off()

#true negative rate
tiff(paste(ec_dir,"/analyses/img/historical_TNR.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(3,5,1,1),cex=1)
plot(met_all$YEAR,met_all$TNR,ylim=c(0.5,0.90),pch=20,ty="l",xlab=NA,ylab="TNR")
abline(h=met$TNR[1],lwd=1.2,lty=2,col="red")
grid()
dev.off()



#####################################################################
#####################################################################

#####################################################################
#3. coupling of both models
exp <- 10 #experiment to focus in
if (exp < 10) {exp <- paste("0",exp,sep="")} else {exp <- paste(exp)}

#####################################################################
##### data loading
#load glam potential yields
glam_pot_rfd <- stack(paste(glam_dir,"/exp-",exp,"/gridded/pot_",1966:1993,"_yield_rfd.tif",sep=""))
glam_pot_irr <- stack(paste(glam_dir,"/exp-",exp,"/gridded/pot_",1966:1993,"_yield_irr.tif",sep=""))
glam_pot_bth <- stack(paste(glam_dir,"/exp-",exp,"/gridded/pot_",1966:1993,"_yield_bth.tif",sep=""))

#load glam farmers (ygp-limited) yields
glam_frm_rfd <- stack(paste(glam_dir,"/exp-",exp,"/gridded/frm_",1966:1993,"_yield_rfd.tif",sep=""))
glam_frm_irr <- stack(paste(glam_dir,"/exp-",exp,"/gridded/frm_",1966:1993,"_yield_irr.tif",sep=""))
glam_frm_bth <- stack(paste(glam_dir,"/exp-",exp,"/gridded/frm_",1966:1993,"_yield_bth.tif",sep=""))


#calculate mean glam potential yields
glam_pot_rfd_m <- calc(glam_pot_rfd,fun=function(x) {mean(x,na.rm=T)})
glam_pot_irr_m <- calc(glam_pot_irr,fun=function(x) {mean(x,na.rm=T)})
glam_pot_bth_m <- calc(glam_pot_bth,fun=function(x) {mean(x,na.rm=T)})

#calculate mean glam farmers yields
glam_frm_rfd_m <- calc(glam_frm_rfd,fun=function(x) {mean(x,na.rm=T)})
glam_frm_irr_m <- calc(glam_frm_irr,fun=function(x) {mean(x,na.rm=T)})
glam_frm_bth_m <- calc(glam_frm_bth,fun=function(x) {mean(x,na.rm=T)})


#load ecocrop yearly predictions
ecrp_yr <- stack(paste(ec_dir,"/analyses/runs_eg/yearly/",1966:1993,"/1-",crop_name,"-tmean_suitability.asc",sep=""))

#load ecocrop climatological mean prediction
ecrp_cl <- raster(paste(ec_dir,"/analyses/runs_eg/clm_1966_1993/1-",crop_name,"-tmean_suitability.asc",sep=""))

#load zones grid
zones <- raster(paste(crop_dir,"/gnut-zones/zones_lr.asc",sep=""))


#####################################################################
###extract data for gridcells of interest

#extract coordinates in GLAM, and then extract values of years and other stuff
xy <- as.data.frame(xyFromCell(glam_pot_rfd[[1]],which(!is.na(glam_pot_rfd[[1]][]))))
xy$CELL <- cellFromXY(glam_pot_rfd[[1]],cbind(x=xy$x,y=xy$y))
xy$ZONE <- extract(zones,cbind(x=xy$x,y=xy$y))
xy$GLAM.POT.RFD <- extract(glam_pot_rfd_m,cbind(x=xy$x,y=xy$y))
xy$GLAM.POT.IRR <- extract(glam_pot_irr_m,cbind(x=xy$x,y=xy$y))
xy$GLAM.POT.BTH <- extract(glam_pot_bth_m,cbind(x=xy$x,y=xy$y))

xy$GLAM.FRM.RFD <- extract(glam_frm_rfd_m,cbind(x=xy$x,y=xy$y))
xy$GLAM.FRM.IRR <- extract(glam_frm_irr_m,cbind(x=xy$x,y=xy$y))
xy$GLAM.FRM.BTH <- extract(glam_frm_bth_m,cbind(x=xy$x,y=xy$y))

xy$ECROP <- extract(ecrp_cl,cbind(x=xy$x,y=xy$y))
xy <- xy[which(!is.na(xy$ECROP)),]


#####################################################################
#exploratory plots
#scatter plots
plot(xy$GLAM.POT.RFD,xy$ECROP,pch=20)
points(xy$GLAM.POT.RFD[which(xy$ZONE==1)],xy$ECROP[which(xy$ZONE==1)],col="red",pch=20)
points(xy$GLAM.POT.RFD[which(xy$ZONE==2)],xy$ECROP[which(xy$ZONE==2)],col="blue",pch=20)
points(xy$GLAM.POT.RFD[which(xy$ZONE==3)],xy$ECROP[which(xy$ZONE==3)],col="orange",pch=20)
points(xy$GLAM.POT.RFD[which(xy$ZONE==4)],xy$ECROP[which(xy$ZONE==4)],col="pink",pch=20)
points(xy$GLAM.POT.RFD[which(xy$ZONE==5)],xy$ECROP[which(xy$ZONE==5)],col="grey 50",pch=20)

plot(xy$GLAM.POT.IRR,xy$ECROP,pch=20,col="red")
points(xy$GLAM.POT.BTH,xy$ECROP,pch=20,col="blue")

points(xy$GLAM.FRM.RFD,xy$ECROP,pch=20,col="red")
points(xy$GLAM.FRM.IRR,xy$ECROP,pch=20,col="red")
points(xy$GLAM.FRM.BTH,xy$ECROP,pch=20,col="blue")

#density plots to see normality
dp_glam <- density(scale(xy$GLAM.POT.RFD))
dp_glam$y <- dp_glam$y/max(dp_glam$y)

dp_ecoc <- density(scale(xy$ECROP))
dp_ecoc$y <- dp_ecoc$y/max(dp_ecoc$y)


#####################################################################
#####################################################################
#some analyses and plots here to see how the data looked like
#plot(dp_glam$x,dp_glam$y,ty="l",col="blue")
#lines(dp_ecoc$x,dp_ecoc$y,col="red")

# plot(xy$GLAM.POT.RFD/max(xy$GLAM.POT.RFD),xy$ECROP/100,pch=20)
# abline(0,1,col="red")
# 
# x <- xy$GLAM.POT.RFD[which(xy$ECROP<100)]/max(xy$GLAM.POT.RFD[which(xy$ECROP<100)])
# y <- xy$ECROP[which(xy$ECROP<100)]/100
# 
# #plots per zones
# plot(xy$GLAM.POT.RFD[which(xy$ZONE==1)],xy$ECROP[which(xy$ZONE==1)],col="red",pch=20)
# plot(xy$GLAM.POT.RFD[which(xy$ZONE==2)],xy$ECROP[which(xy$ZONE==2)],col="blue",pch=20)
# plot(xy$GLAM.POT.RFD[which(xy$ZONE==3)],xy$ECROP[which(xy$ZONE==3)],col="orange",pch=20)
# plot(xy$GLAM.POT.RFD[which(xy$ZONE==4)],xy$ECROP[which(xy$ZONE==4)],col="pink",pch=20)
# plot(xy$GLAM.POT.RFD[which(xy$ZONE==5)],xy$ECROP[which(xy$ZONE==5)],col="grey 50",pch=20)
# 
# #difference between expected perfect 1:1 fit and actual predictions
# a <- c(0,1)
# b <- c(0,1)
# p_fit <- lm(b~a)
# 
# yp <- predict(p_fit,newdata=data.frame(a=x))
# plot(x,y)
# points(x,yp,col="red")
# yd <- y-yp
# plot(x,yd,col="blue")
# 
# lfit <- lm(y~x-1)
# sfit <- summary(lfit)
# plot(x,y)
# #y2 <- lfit$coefficients[1]+x*lfit$coefficients[2]
# y2 <- lfit$coefficients[1]*x
# lines(x,y2,col="red")
# abline(0,1,col="red",lty=2)
# cor.test(y2,y)$estimate

#write.csv(xy,paste(ec_dir,"/xydata.csv",sep=""),quote=T,row.names=F)


########################################################################
########################################################################
#extract data for all years and gridcells for all further
#analyses ##get data of all gridcells

glam_data <- extract(glam_pot_rfd,cbind(x=xy$x,y=xy$y))
eco_data <- extract(ecrp_yr,cbind(x=xy$x,y=xy$y))

cell_data <- lapply(as.numeric(xy$CELL),FUN=get_cell_data,xy,glam_data,eco_data)
cell_data <- do.call("rbind",cell_data)


########################################################################
########################################################################
### analysis of suitability residuals

# out_all <- data.frame()
# for (cell in c(291,328,886,921,992)) {
#   reg_cell <- regress_cell(cell=cell,cell_data,crop_dir,wth_dir,exp)
#   out_all <- rbind(out_all,reg_cell)
# }

#get cell weather data
cell_wth_data <- lapply(as.numeric(unique(cell_data$CELL)),
                        FUN=get_wth_data,cell_data,crop_dir,wth_dir,exp)
cell_wth_data.df <- do.call("rbind",cell_wth_data)

out_all <- lapply(cell_wth_data,FUN=regress_cell,fit_par="ALL")
oall_reg <- do.call("rbind",out_all)

nparam <- apply(oall_reg[,3:(ncol(oall_reg)-1)],1,FUN=function(x) {length(which(x!=0))})
oall_reg$NPAR <- nparam
oall_reg$NPAR_PER <- oall_reg$NPAR / length(3:(ncol(oall_reg)-1)) * 100

#make rasters of correlations, and of number of regression terms
ccoef_rs <- raster(glam_pot_rfd)
ccoef_rs[oall_reg$CELL] <- oall_reg$CCOEF

npars_rs <- raster(glam_pot_rfd)
npars_rs[oall_reg$CELL] <- oall_reg$NPAR

ppars_rs <- raster(glam_pot_rfd)
ppars_rs[oall_reg$CELL] <- oall_reg$NPAR_PER

###here second iteration
#count number of times each term is included in a regression
#

pfreq <- as.numeric(apply(oall_reg,2,FUN=function(x) {length(which(x!=0))}))
pfreq <- data.frame(PAR=names(oall_reg),FREQ=pfreq)
pfreq$RFREQ <- pfreq$FREQ / nrow(oall_reg) * 100
pfreq <- pfreq[which(pfreq$PAR %in% names(oall_reg[,3:(ncol(oall_reg)-3)])),]
pfreq <- pfreq[order(pfreq$RFREQ,decreasing=T),]

#re_run the fitting algorithm for all gridcells but only 
#with variables that were found to be important

fit_par <- paste(pfreq$PAR[which(pfreq$RFREQ >= 50)])

out_all2 <- lapply(cell_wth_data,FUN=regress_cell,fit_par)
oall_reg2 <- do.call("rbind",out_all2)

nparam <- apply(oall_reg2[,3:(ncol(oall_reg2)-1)],1,FUN=function(x) {length(which(x!=0))})
oall_reg2$NPAR <- nparam
oall_reg2$NPAR_PER <- oall_reg2$NPAR / length(3:(ncol(oall_reg2)-1)) * 100

#make rasters of correlations, and of number of regression terms
ccoef_rs2 <- raster(glam_pot_rfd)
ccoef_rs2[oall_reg2$CELL] <- oall_reg2$CCOEF

npars_rs2 <- raster(glam_pot_rfd)
npars_rs2[oall_reg2$CELL] <- oall_reg2$NPAR

ppars_rs2 <- raster(glam_pot_rfd)
ppars_rs2[oall_reg2$CELL] <- oall_reg2$NPAR_PER

#count number of times each term is included in a regression
#
pfreq2 <- as.numeric(apply(oall_reg2,2,FUN=function(x) {length(which(x!=0))}))
pfreq2 <- data.frame(PAR=names(oall_reg2),FREQ=pfreq2)
pfreq2$RFREQ <- pfreq2$FREQ / nrow(oall_reg2) * 100
pfreq2 <- pfreq2[which(pfreq2$PAR %in% names(oall_reg2[,3:(ncol(oall_reg2)-3)])),]
pfreq2 <- pfreq2[order(pfreq2$RFREQ,decreasing=T),]
pfreq2 <- pfreq2[which(pfreq2$RFREQ > 0),]







########################################################################
########################################################################
###analysis of average suitability per yield quantile
y_quan <- quantile(cell_data$YIELD,probs=seq(0,1,by=0.1))
q_mat <- data.frame(QUANTILE=seq(0,1,by=0.1),Y_VALUE=as.numeric(y_quan),YIELD=NA,SUIT=NA)

cell_data$Y_QUANT <- NA
for (i in 1:nrow(q_mat)) {
  if (i == 1) {
    cell_data$Y_QUANT[which(cell_data$YIELD<=q_mat$Y_VALUE[i])] <- i
    
    q_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$YIELD<=q_mat$Y_VALUE[i])],na.rm=T)
    q_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$YIELD<=q_mat$Y_VALUE[i])],na.rm=T)
  } else {
    cell_data$Y_QUANT[which(cell_data$YIELD<=q_mat$Y_VALUE[i] & cell_data$YIELD>q_mat$Y_VALUE[i-1])] <- i
    
    q_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$YIELD<=q_mat$Y_VALUE[i] & cell_data$YIELD>q_mat$Y_VALUE[i-1])],na.rm=T)
    q_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$YIELD<=q_mat$Y_VALUE[i] & cell_data$YIELD>q_mat$Y_VALUE[i-1])],na.rm=T)
  }
}


########################################################################
########################################################################
###analysis of average yield per suitability class
c_mat <- data.frame(CLASS=seq(1,11,by=1),S_VALUE=seq(0,100,by=10),YIELD=NA,SUIT=NA)
cell_data$S_CLASS <- NA
for (i in 1:nrow(c_mat)) {
  if (i == 1) {
    cell_data$S_CLASS[which(cell_data$SUIT<=c_mat$S_VALUE[i])] <- i
    
    c_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$SUIT<=c_mat$S_VALUE[i])],na.rm=T)
    c_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$SUIT<=c_mat$S_VALUE[i])],na.rm=T)
  } else {
    cell_data$S_CLASS[which(cell_data$SUIT<=c_mat$S_VALUE[i] & cell_data$SUIT>c_mat$S_VALUE[i-1])] <- i
    
    c_mat$YIELD[i] <- mean(cell_data$YIELD[which(cell_data$SUIT<=c_mat$S_VALUE[i] & cell_data$SUIT>c_mat$S_VALUE[i-1])],na.rm=T)
    c_mat$SUIT[i] <- mean(cell_data$SUIT[which(cell_data$SUIT<=c_mat$S_VALUE[i] & cell_data$SUIT>c_mat$S_VALUE[i-1])],na.rm=T)
  }
}


#make include the above plot using the most water or temperature stressed years
#(5% or 95% quantiles)

#most temperature stressed years
      #top 95% and low 5% of HTS1
      #top 95% and low 5% of TSTD (much more extremes)
hts1_top <- quantile(cell_wth_data.df$HTS1,probs=c(0.95))
hts1_bot <- quantile(cell_wth_data.df$HTS1,probs=c(0.05))

tstd_top <- quantile(cell_wth_data.df$TSTD,probs=c(0.95))
tstd_bot <- quantile(cell_wth_data.df$TSTD,probs=c(0.05))

#most drought stress years
      #5/95% of RAIN
rain_top <- quantile(cell_wth_data.df$RAIN,probs=c(0.95))
rain_bot <- quantile(cell_wth_data.df$RAIN,probs=c(0.05))

      #5/95% of RD.0
rd.0_top <- quantile(cell_wth_data.df$RD.0,probs=c(0.95))
rd.0_bot <- quantile(cell_wth_data.df$RD.0,probs=c(0.05))

      #5/95% of RSTD (much more extremes)
rstd_top <- quantile(cell_wth_data.df$RSTD,probs=c(0.95))
rstd_bot <- quantile(cell_wth_data.df$RSTD,probs=c(0.05))

      #5/95% of ERATIO.50 (water stress)
ea50_top <- quantile(cell_wth_data.df$ERATIO.50,probs=c(0.95))
ea50_bot <- quantile(cell_wth_data.df$ERATIO.50,probs=c(0.05))

      #5/95% of ERATIO.25 (water stress)
ea25_top <- quantile(cell_wth_data.df$ERATIO.25,probs=c(0.95))
ea25_bot <- quantile(cell_wth_data.df$ERATIO.25,probs=c(0.05))

#best and worst years overall
      #5/95% of EFF.GD
efgd_top <- quantile(cell_wth_data.df$EFF.GD,probs=c(0.95))
efgd_bot <- quantile(cell_wth_data.df$EFF.GD,probs=c(0.05))

#construct table with the above information
extr_val <- data.frame(PAR=c("HTS1","TSTD","RAIN","RD.0","RSTD","ERATIO.25","ERATIO.50","EFF.GD"),
                       TOP95=c(hts1_top,tstd_top,rain_top,rd.0_top,rstd_top,ea25_top,ea50_top,efgd_top),
                       BOT05=c(hts1_bot,tstd_bot,rain_bot,rd.0_bot,rstd_bot,ea25_bot,ea50_bot,efgd_bot))

#make table for heatmap
cell_merg_wth <- cell_data
cell_merg_wth$YIELD <- NULL; cell_merg_wth$SUIT <- NULL
cell_merg_wth <- merge(cell_wth_data.df,cell_merg_wth,by=c("CELL","YEAR"),sort=F,all=T)
for (i in 1:nrow(c_mat)) {
  subs_merg <- cell_merg_wth[which(cell_merg_wth$S_CLASS == i),]
  
  ymo_df <- as.data.frame(matrix(nrow=3,ncol=1))
  sdo_df <- as.data.frame(matrix(nrow=3,ncol=1))
  #loop through accounting variables
  for (j in 1:nrow(extr_val)) {
    tpar <- paste(extr_val$PAR[j])
    subs_merg$QUANT <- "MID"
    subs_merg$QUANT[which(subs_merg[,tpar] <= extr_val$BOT05[j])] <- "B05"
    subs_merg$QUANT[which(subs_merg[,tpar] >= extr_val$TOP95[j])] <- "T95"
    
    #mean values
    ym1 <- mean(subs_merg$YIELD[which(subs_merg$QUANT == "B05")],na.rm=T)
    sd1 <- sd(subs_merg$YIELD[which(subs_merg$QUANT == "B05")],na.rm=T)
    ym2 <- mean(subs_merg$YIELD[which(subs_merg$QUANT == "MID")],na.rm=T)
    sd2 <- sd(subs_merg$YIELD[which(subs_merg$QUANT == "MID")],na.rm=T)
    ym3 <- mean(subs_merg$YIELD[which(subs_merg$QUANT == "T95")],na.rm=T)
    sd3 <- sd(subs_merg$YIELD[which(subs_merg$QUANT == "T95")],na.rm=T)
    yma <- mean(subs_merg$YIELD,na.rm=T)
    sda <- sd(subs_merg$YIELD,na.rm=T)
    
    #output
    ymo <- c(ym1/yma,ym2/yma,ym3/yma)
    ymo_df <- cbind(ymo_df,ymo)
    
    sdo <- c(sd1/sda,sd2/sda,sd3/sda)
    sdo_df <- cbind(sdo_df,sdo)
  }
  
  names(ymo_df) <- c("QUANT",paste("MEAN.",extr_val$PAR,sep=""))
  sdo_df$V1 <- NULL; names(sdo_df) <- paste("SD.",extr_val$PAR,sep="")
  ymo_df$QUANT <- c("B05","MID","T95")
  ymo_df <- cbind(CLASS=i,ymo_df)
  
  if (i==1) {
    o_class <- cbind(ymo_df,sdo_df)
  } else {
    o_class <- rbind(o_class,cbind(ymo_df,sdo_df))
  }
}




########################################################################
########################################################################
#write results dir
res_dir <- paste(crop_dir,"/ecg_analyses/results",sep="")
if (!file.exists(res_dir)) {dir.create(res_dir)}

#write all results into files
write.csv(oall_reg,paste(res_dir,"/regression_residuals.csv",sep=""),quote=T,row.names=F)
write.csv(oall_reg2,paste(res_dir,"/regression_residuals_i2.csv",sep=""),quote=T,row.names=F)
write.csv(pfreq,paste(res_dir,"/parameter_importance.csv",sep=""),quote=T,row.names=F)
write.csv(pfreq2,paste(res_dir,"/parameter_importance_i2.csv",sep=""),quote=T,row.names=F)
write.csv(cell_data,paste(res_dir,"/cell_data_yield_suit.csv",sep=""),quote=F,row.names=F)
write.csv(cell_wth_data.df,paste(res_dir,"/cell_data_wth.csv",sep=""),quote=T,row.names=F)
write.csv(q_mat,paste(res_dir,"/yield_quant.csv",sep=""),quote=F,row.names=F)
write.csv(c_mat,paste(res_dir,"/suit_class.csv",sep=""),quote=F,row.names=F)
write.csv(extr_val,paste(res_dir,"/extreme_values.csv",sep=""),quote=F,row.names=F)
write.csv(o_class,paste(res_dir,"/heatmap_extremes.csv",sep=""),quote=F,row.names=F)



ccoef_rs <- writeRaster(ccoef_rs,paste(res_dir,"/ccoef_residuals.asc",sep=""),format="ascii")
npars_rs <- writeRaster(npars_rs,paste(res_dir,"/npars_residuals.asc",sep=""),format="ascii")
ppars_rs <- writeRaster(ppars_rs,paste(res_dir,"/ppars_residuals.asc",sep=""),format="ascii")

ccoef_rs2 <- writeRaster(ccoef_rs2,paste(res_dir,"/ccoef_residuals_i2.asc",sep=""),format="ascii")
npars_rs2 <- writeRaster(npars_rs2,paste(res_dir,"/npars_residuals_i2.asc",sep=""),format="ascii")
ppars_rs2 <- writeRaster(ppars_rs2,paste(res_dir,"/ppars_residuals_i2.asc",sep=""),format="ascii")


########################################################################
########################################################################
### produce needed plots

#spatial importance of parameters
tiff(paste(res_dir,"/parameter_importance.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(6,5,1,1),cex=1)
barplot(height=pfreq$RFREQ,names.arg=pfreq$PAR,las=2,
        ylab="Percent gridcells where significant (%)",
        xlab=NA,ylim=c(0,100))
grid()
abline(h=50,col="red")
abline(h=30,col="red",lty=2)
dev.off()

#spatial importance of parameters (second iteration)
tiff(paste(res_dir,"/parameter_importance_i2.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(6,5,1,1),cex=1)
barplot(height=pfreq2$RFREQ,names.arg=pfreq2$PAR,las=2,
        ylab="Percent gridcells where significant (%)",
        xlab=NA,ylim=c(0,100))
grid()
abline(h=50,col="red")
abline(h=30,col="red",lty=2)
dev.off()

#yield quantiles vs. suitability
tiff(paste(res_dir,"/yield_quantiles_xy.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=1)
plot(q_mat$YIELD,q_mat$SUIT,ty="l",
     ylab="Mean suitability (%)",xlab="Mean GLAM yield (kg/ha)")
points(q_mat$YIELD,q_mat$SUIT,pch=20)
grid()
dev.off()

tiff(paste(res_dir,"/yield_quantiles_boxplot.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=1)
boxplot(cell_data$SUIT~cell_data$Y_QUANT,pch=NA,cex=0.75,col="grey 80",
        ylab="Mean suitability (%)",xlab="GLAM yield quantile")
grid()
dev.off()

### here make this boxplot in a loop for each of the parameters listed
### in the data frame extr_val (extreme values)
for (i in 1:nrow(extr_val)) {
  tpar <- paste(extr_val$PAR[i])
  
  #years with values in the bottom of distribution
  subs_merg <- cell_merg_wth[which(cell_merg_wth[,tpar] <= extr_val$BOT05[i] | cell_merg_wth[,tpar] >= extr_val$TOP95[i]),]
  subs_merg$TOP_BOT <- NA
  subs_merg$TOP_BOT[which(subs_merg[,tpar] <= extr_val$BOT05[i])] <- "BOT"
  subs_merg$TOP_BOT[which(subs_merg[,tpar] >= extr_val$TOP95[i])] <- "TOP"
  
  
  tiff(paste(res_dir,"/yield_quantiles_boxplot-",tpar,"-bottom.tiff",sep=""),res=300,pointsize=10,
       width=1500,height=1300,units="px",compression="lzw")
  par(mar=c(5,5,1,1),cex=1,las=2)
  boxplot(subs_merg$SUIT~subs_merg$Y_QUANT,pch=NA,cex=0.75,col="grey 80",
          ylab="Mean suitability (%)",xlab="GLAM yield quantile",
          xlim=c(0,11))
  grid()
  dev.off()
  
  #years with values in the top of distribution
  subs_merg <- cell_merg_wth[which(cell_merg_wth[,tpar] >= extr_val$TOP95[i]),]
  tiff(paste(res_dir,"/yield_quantiles_boxplot-",tpar,"-top.tiff",sep=""),res=300,pointsize=10,
       width=1500,height=1300,units="px",compression="lzw")
  par(mar=c(5,5,1,1),cex=1)
  boxplot(subs_merg$SUIT~subs_merg$Y_QUANT,pch=NA,cex=0.75,col="grey 80",
          ylab="Mean suitability (%)",xlab="GLAM yield quantile",
          xlim=c(0,11))
  grid()
  dev.off()
}

#plot(subs_merg$YIELD_NORM,subs_merg$SUIT_NORM,pch=20,col="blue",xlim=c(0,1),ylim=c(0,1))
#points(subs_merg$YIELD_NORM,subs_merg$SUIT_NORM,pch=20,col="red")

#suitability classess vs. yield
tiff(paste(res_dir,"/suit_classes_xy.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=1)
plot(c_mat$SUIT,c_mat$YIELD,ty="p",pch=20,
     ylab="Mean suitability (%)",xlab="Mean GLAM yield (kg/ha)")
abline(400,(1200-400)/100)
grid()
dev.off()

tiff(paste(res_dir,"/suit_classes_boxplot.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=1)
boxplot(cell_data$YIELD~cell_data$S_CLASS,pch=NA,cex=0.75,col="grey 80",
        ylab="Mean GLAM yield (kg/ha)",xlab="Suitability class",ylim=c(0,3000))
grid()
dev.off()


##loop through extreme values
for (i in 1:nrow(extr_val)) {
  tpar <- paste(extr_val$PAR[i])
  
  #years with values in the bottom of distribution
  subs_merg <- cell_merg_wth[which(cell_merg_wth[,tpar] <= extr_val$BOT05[i] | cell_merg_wth[,tpar] >= extr_val$TOP95[i]),]
  subs_merg$TOP_BOT <- NA
  subs_merg$TOP_BOT[which(subs_merg[,tpar] <= extr_val$BOT05[i])] <- "BOT"
  subs_merg$TOP_BOT[which(subs_merg[,tpar] >= extr_val$TOP95[i])] <- "TOP"
  
  
  tiff(paste(res_dir,"/suit_classes_boxplot-",tpar,"-bottom.tiff",sep=""),res=300,pointsize=10,
       width=1500,height=1300,units="px",compression="lzw")
  par(mar=c(5,5,1,1),cex=1)
  boxplot(subs_merg$YIELD~subs_merg$S_CLASS,pch=NA,cex=0.75,col="grey 80",
          ylab="Mean GLAM yield (kg/ha)",xlab="Suitability class",
          ylim=c(0,5000),xlim=c(0,11))
  grid()
  dev.off()
  
  #years with values in the top of distribution
  subs_merg <- cell_merg_wth[which(cell_merg_wth[,tpar] >= extr_val$TOP95[i]),]
  tiff(paste(res_dir,"/suit_classes_boxplot-",tpar,"-top.tiff",sep=""),res=300,pointsize=10,
       width=1500,height=1300,units="px",compression="lzw")
  par(mar=c(5,5,1,1),cex=1)
  boxplot(subs_merg$YIELD~subs_merg$S_CLASS,pch=NA,cex=0.75,col="grey 80",
          ylab="Mean GLAM yield (kg/ha)",xlab="Suitability class",
          ylim=c(0,5000),xlim=c(0,11))
  grid()
  dev.off()
}




#plot of historical mean comparisons
tiff(paste(res_dir,"/yield_vs_suit_climatology.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=1)
plot(xy$GLAM.POT.RFD,xy$ECROP,pch=20,
     xlab="Rainfed GLAM yield, no YGP-limited (kg/ha)",
     ylab="Crop suitability (%)")
grid()
dev.off()

#plot of yearly data
tiff(paste(res_dir,"/yield_vs_suit_yearly.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1,1),cex=1)
plot(cell_data$YIELD, cell_data$SUIT,pch=20,cex=0.75,
     xlab="Rainfed GLAM yield, no YGP-limited (kg/ha)",
     ylab="Crop suitability (%)")
grid()
dev.off()

#some kernel density stuff
# plot(cell_data$YIELD, cell_data$SUIT, xlim = c(0,5000), ylim = c(0,100),pch=20,cex=0.75)
f2 <- kde2d(cell_data$YIELD, cell_data$SUIT, 
            n = 100, lims = c(-50, 5000, -10, 150),
            h = c(1000,75))
cols <- colorRampPalette(c("grey 80","grey 10"))(25)
tiff(paste(res_dir,"/yield_vs_suit_density.tiff",sep=""),res=300,pointsize=10,
     width=1500,height=1300,units="px",compression="lzw")
par(mar=c(5,5,1.5,1.5),cex=1)
image(f2,ylim=c(0,100),xlim=c(0,4000),col=cols,useRaster=T,
      xlab="GLAM yield, no YGP-limited (kg/ha)",
      ylab="Crop suitability (%)")
#points(cell_data$YIELD, cell_data$SUIT, pch=20,cex=0.5)
grid(col="white")
dev.off()


### plot rasters here
ht <- 1000
fct <- (ccoef_rs@extent@xmin-ccoef_rs@extent@xmax)/(ccoef_rs@extent@ymin-ccoef_rs@extent@ymax)
wt <- ht*(fct+.1)

##plot correlation coefficient
brks <- seq(0,1,by=0.1)
brks.lab <- round(brks,2)
cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))

tiffName <- paste(res_dir,"/ccoef.tiff",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
par(mar=c(3,3,1,3.5))
plot(ccoef_rs,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
plot(wrld_simpl,add=T)
grid()
dev.off()

tiffName <- paste(res_dir,"/ccoef_i2.tiff",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
par(mar=c(3,3,1,3.5))
plot(ccoef_rs2,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
plot(wrld_simpl,add=T)
grid()
dev.off()


##plot of number of explanatory terms
brks <- seq(1,max(npars_rs[],na.rm=T),by=1)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))

tiffName <- paste(res_dir,"/num_params.tiff",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
par(mar=c(3,3,1,3.5))
plot(npars_rs,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
plot(wrld_simpl,add=T)
grid()
dev.off()

##plot of number of explanatory terms
brks <- seq(1,max(npars_rs2[],na.rm=T),by=1)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))

tiffName <- paste(res_dir,"/num_params_i2.tiff",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
par(mar=c(3,3,1,3.5))
plot(npars_rs2,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
plot(wrld_simpl,add=T)
grid()
dev.off()

##plot of percent of explanatory terms
brks <- seq(0,100,by=10)
brks.lab <- round(brks,0)
cols <- c(colorRampPalette(c("grey 80","grey 10"))(length(brks)))

tiffName <- paste(res_dir,"/per_params.tiff",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
par(mar=c(3,3,1,3.5))
plot(ppars_rs,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
plot(wrld_simpl,add=T)
grid()
dev.off()

tiffName <- paste(res_dir,"/per_params_i2.tiff",sep="")
tiff(tiffName,res=300,compression="lzw",height=ht,width=wt,pointsize=5)
par(mar=c(3,3,1,3.5))
plot(ppars_rs2,col=cols,breaks=brks,lab.breaks=brks.lab,horizontal=T,legend.shrink=0.8)
plot(wrld_simpl,add=T)
grid()
dev.off()








