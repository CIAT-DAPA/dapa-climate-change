#Julian Ramirez-Villegas
#UoL / CIAT / CCAFS
#Dec 2012

library(raster)
library(maptools); data(wrld_simpl)

#source directories
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD"
#src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD"

source(paste(src.dir,"/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))

#config details
cropName <- "gnut"
ver <- "v6"
gcmChars <- read.table(paste(src.dir,"/0008-CMIP5/data/CMIP5gcms.tab",sep=""),header=T,sep="\t")

#directories
bDir <- "W:/eejarv/PhD-work/crop-modelling"
#bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling"
glamDir <- paste(bDir,"/GLAM",sep="")
clmDir <- paste(bDir,"/climate-data/gridcell-data",sep="")
cropDir <- paste(glamDir,"/model-runs/",toupper(cropName),sep="")
glamInDir <- paste(cropDir,"/inputs",sep="")

#data directories
obsDir <- paste(clmDir,"/IND",sep="")
hisDir <- paste(clmDir,"/IND_CMIP5",sep="")
rcpDir <- paste(clmDir,"/IND_RCP45",sep="")

#output directories
outDir <- paste(clmDir,"/IND_GCM_cal_bc",sep="")
if (!file.exists(outDir)) {dir.create(outDir)}

#locations
cells <- read.csv(paste(glamInDir,"/calib-cells-selection-",ver,".csv",sep=""))
rsdum <- raster(paste(cropDir,"/calib/exp-33_outputs/general/calib_results_spat/y_obs.asc",sep=""))
rsdum[] <- NA

#list of GCMs
gcmList_his <- list.files(paste(glamInDir,"/ascii/wth-cmip5_hist",sep=""),pattern="_ENS_")
gcmList_rcp <- list.files(paste(glamInDir,"/ascii/wth-cmip5_rcp45",sep=""),pattern="_ENS_")
gcmList <- gcmList_his[gcmList_his %in% gcmList_rcp]

#per grid cell apply these methods to maximum and minimum temperature and 
#then produce Ed's plot. Using these then decide what to use (use CF)

#calculate delta/correction for Jun-Dec period for
#mean maximum and mean minimum temperature

#variable selection
obs_vn <- "cru_tmx"
gcm_vn <- "tasmax"

#### functions
#calculate monthly totals in model
calc_month <- function(x,wleap) {
  yr <- x[1]
  dg <- createDateGridCMIP5(year=yr,whatLeap=wleap)
  daily <- x[2:(nrow(dg)+1)]
  dg$VALUE <- daily
  dg$DAY <- NULL; dg$MTH.STR <- NULL; dg$DAY.STR <- NULL; dg$MTH.DAY <- NULL; dg$JD <- NULL
  mthVals <- as.numeric(tapply(dg$VALUE,dg$MONTH,FUN=mean,na.rm=T))
  return(mthVals)
}


#use period 1961-1980 as baseline
#use period 1981-2000 as future

#gcm and ensemble member
for (gcm_ens in gcmList) {
  #gcm_ens <- gcmList[1]
  gcm <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[1]
  ens <- unlist(strsplit(gcm_ens,"_ENS_",fixed=T))[2]
  wlp <- paste(gcmChars$has_leap[gcmChars$GCM == gcm & gcmChars$Ensemble == ens])
  
  cat(gcm_ens,"\n")
  
  #gcm outdir
  gcm_oDir <- paste(outDir,"/",gcm_ens,"/",gcm_vn,sep="")
  if (!file.exists(gcm_oDir)) {dir.create(gcm_oDir,recursive=T)}
  
  if (!file.exists(paste(gcm_oDir,"/",gcm_vn,"_CF_2020-2039.tif",sep=""))) {
    #calculate mean temperatures for JJA period in GCM and in obs for both periods
    outDat <- data.frame()
    for (i in 1:nrow(cells)) {
      #i<-1
      loc <- cells$CELL[i]
      #cat(loc,"\n")
      
      obsDat <- read.csv(paste(obsDir,"/",obs_vn,"/cell-",loc,".csv",sep=""))
      hisDat <- read.csv(paste(hisDir,"/",gcm,"/",ens,"/",gcm_vn,"/cell-",loc,".csv",sep=""))
      rcpDat <- read.csv(paste(rcpDir,"/",gcm,"/",ens,"/",gcm_vn,"/cell-",loc,".csv",sep=""))
      
      #take only years of interest
      obsDat <- obsDat[obsDat$YEAR >= 1961 & obsDat$YEAR <= 2000,]
      hisDat <- hisDat[hisDat$YEAR >= 1961 & hisDat$YEAR <= 2000,]
      rcpDat <- rcpDat[rcpDat$YEAR >= 2020 & rcpDat$YEAR <= 2039,]
      
      #calculate seasonal totals
      obsSum <- data.frame(YEAR=obsDat$YEAR,VALUE=rowMeans(obsDat[,paste("MONTH",6:12,sep="")]))
      hisSum <- as.data.frame(t(apply(hisDat,1,FUN=calc_month,wlp)))
      hisSum <- data.frame(YEAR=hisDat$YEAR,VALUE=rowMeans(hisSum[,6:12]))
      rcpSum <- as.data.frame(t(apply(rcpDat,1,FUN=calc_month,wlp)))
      rcpSum <- data.frame(YEAR=rcpDat$YEAR,VALUE=rowMeans(rcpSum[,6:12]))
      
      #use some years to calculate the others
      p1_obs <- mean(obsSum$VALUE[which(obsSum$YEAR >= 1961 & obsSum$YEAR <= 1980)],na.rm=T)
      p2_obs <- mean(obsSum$VALUE[which(obsSum$YEAR >= 1981 & obsSum$YEAR <= 2000)],na.rm=T)
      p1_gcm <- mean(hisSum$VALUE[which(hisSum$YEAR >= 1961 & hisSum$YEAR <= 1980)],na.rm=T)
      p2_gcm <- mean(hisSum$VALUE[which(hisSum$YEAR >= 1981 & hisSum$YEAR <= 2000)],na.rm=T)
      p3_gcm <- mean(rcpSum$VALUE,na.rm=T)
      
      p2_bc <- p2_gcm + (p1_obs-p1_gcm)
      p2_cf <- p1_obs + (p2_gcm-p1_gcm)
      p3_bc <- p3_gcm + (p1_obs-p1_gcm)
      p3_cf <- p1_obs + (p3_gcm-p1_gcm)
      
      odf <- data.frame(CELL=loc,GCM.P1=p1_gcm,GCM.P2=p2_gcm,GCM.P3=p3_gcm,
                        OBS.P1=p1_obs,OBS.P2=p2_obs,BC.P2=p2_bc,BC.P3=p3_bc,
                        CF.P2=p2_cf,CF.P3=p3_cf)
      outDat <- rbind(outDat,odf)
    }
    
    #write all rasters in there
    rs1_gcm <- rsdum; rs1_gcm[outDat$CELL] <- outDat$GCM.P1
    rs1_gcm <- writeRaster(rs1_gcm,paste(gcm_oDir,"/",gcm_vn,"_GCM_1961-1980.tif",sep=""),format="GTiff")
    
    rs2_gcm <- rsdum; rs2_gcm[outDat$CELL] <- outDat$GCM.P2
    rs2_gcm <- writeRaster(rs2_gcm,paste(gcm_oDir,"/",gcm_vn,"_GCM_1981-2000.tif",sep=""),format="GTiff")
    
    rs3_gcm <- rsdum; rs3_gcm[outDat$CELL] <- outDat$GCM.P3
    rs3_gcm <- writeRaster(rs3_gcm,paste(gcm_oDir,"/",gcm_vn,"_GCM_2020-2039.tif",sep=""),format="GTiff")
    
    rs1_obs <- rsdum; rs1_obs[outDat$CELL] <- outDat$OBS.P1
    rs1_obs <- writeRaster(rs1_obs,paste(gcm_oDir,"/",gcm_vn,"_OBS_1961-1980.tif",sep=""),format="GTiff")
    
    rs2_obs <- rsdum; rs2_obs[outDat$CELL] <- outDat$OBS.P2
    rs2_obs <- writeRaster(rs2_obs,paste(gcm_oDir,"/",gcm_vn,"_OBS_1981-2000.tif",sep=""),format="GTiff")
    
    rs2_bc <- rsdum; rs2_bc[outDat$CELL] <- outDat$BC.P2
    rs2_bc <- writeRaster(rs2_bc,paste(gcm_oDir,"/",gcm_vn,"_BC_1981-2000.tif",sep=""),format="GTiff")
    
    rs3_bc <- rsdum; rs3_bc[outDat$CELL] <- outDat$BC.P3
    rs3_bc <- writeRaster(rs3_bc,paste(gcm_oDir,"/",gcm_vn,"_BC_2020-2039.tif",sep=""),format="GTiff")
    
    rs2_cf <- rsdum; rs2_cf[outDat$CELL] <- outDat$CF.P2
    rs2_cf <- writeRaster(rs2_cf,paste(gcm_oDir,"/",gcm_vn,"_CF_1981-2000.tif",sep=""),format="GTiff")
    
    rs3_cf <- rsdum; rs3_cf[outDat$CELL] <- outDat$CF.P3
    rs3_cf <- writeRaster(rs3_cf,paste(gcm_oDir,"/",gcm_vn,"_CF_2020-2039.tif",sep=""),format="GTiff")
  } else {
    rs1_gcm <- raster(paste(gcm_oDir,"/",gcm_vn,"_GCM_1961-1980.tif",sep=""))
    rs2_gcm <- raster(paste(gcm_oDir,"/",gcm_vn,"_GCM_1981-2000.tif",sep=""))
    rs3_gcm <- raster(paste(gcm_oDir,"/",gcm_vn,"_GCM_2020-2039.tif",sep=""))
    rs1_obs <- raster(paste(gcm_oDir,"/",gcm_vn,"_OBS_1961-1980.tif",sep=""))
    rs2_obs <- raster(paste(gcm_oDir,"/",gcm_vn,"_OBS_1981-2000.tif",sep=""))
    rs2_bc <- raster(paste(gcm_oDir,"/",gcm_vn,"_BC_1981-2000.tif",sep=""))
    rs3_bc <- raster(paste(gcm_oDir,"/",gcm_vn,"_BC_2020-2039.tif",sep=""))
    rs2_cf <- raster(paste(gcm_oDir,"/",gcm_vn,"_CF_1981-2000.tif",sep=""))
    rs3_cf <- raster(paste(gcm_oDir,"/",gcm_vn,"_CF_2020-2039.tif",sep=""))
  }
  
  #make figures
  if (!file.exists(paste(gcm_oDir,"/fig_",gcm_vn,".tif",sep=""))) {
    #plot characteristics  
    ht <- 2000
    #fct <- (rsdum@extent@xmin-rsdum@extent@xmax)/(rsdum@extent@ymin-rsdum@extent@ymax)
    wt <- 3000 #ht*(fct+.1)
    wld <- list("sp.polygons",wrld_simpl,lwd=0.8,first=F)
    grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))
    grli <- list("sp.lines",grat,lwd=0.5,lty=2,first=F)
    
    #legend
    all_vals <- c(rs1_gcm[],rs2_gcm[],rs3_gcm[],rs1_obs[],rs2_obs[],rs2_bc[],rs3_bc[],rs2_cf[],rs3_cf[])
    min_val <- min(all_vals,na.rm=T)
    max_val <- max(all_vals,na.rm=T)
    
    #brks <- unique(c(seq(min_val,max_val,length.out=20),max_val))
    brks <- unique(c(quantile(all_vals,probs=c(seq(0,1,by=0.05)),na.rm=T),max_val))
    brks.lab <- round(brks,2)
    cols <- c(colorRampPalette(c("yellow","orange","red","purple"))(length(brks)))
    layt <- list(wld,grli)
    
    tiffName <- paste(gcm_oDir,"/fig_",gcm_vn,".tif",sep="")
    trellis.device("tiff",file=tiffName,res=300,compression="lzw",height=ht,width=wt) 
    rp1_gcm <- spplot(rs1_gcm,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp2_gcm <- spplot(rs2_gcm,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp3_gcm <- spplot(rs3_gcm,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp1_obs <- spplot(rs1_obs,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp2_obs <- spplot(rs2_obs,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp2_bc <- spplot(rs2_bc,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp3_bc <- spplot(rs3_bc,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp2_cf <- spplot(rs2_cf,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=F,pretty=brks.lab)
    rp3_cf <- spplot(rs3_cf,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),at=brks,colorkey=list(at=brks.lab),pretty=brks.lab)
    
    print(rp1_gcm,split=c(1,1,4,3),more=T)
    print(rp2_gcm,split=c(1,2,4,3),more=T)
    print(rp3_gcm,split=c(1,3,4,3),more=T)
    print(rp1_obs,split=c(2,1,4,3),more=T)
    print(rp2_obs,split=c(2,2,4,3),more=T)
    print(rp2_bc,split=c(3,2,4,3),more=T)
    print(rp3_bc,split=c(3,3,4,3),more=T)
    print(rp2_cf,split=c(4,2,4,3),more=T)
    print(rp3_cf,split=c(4,3,4,3),more=F)
    
    dev.off()
  }
#   tiffName <- paste(gcm_oDir,"/fig_tasmin_GCM_1961-1980.tif",sep="")
#   tiff(tiffName,res=300,compression="lzw",height=ht,width=wt)
#   par(mfrow=c(3,4))
#   x <- spplot(rs1_gcm,sp.layout=layt,col.regions=cols,par.settings=list(fontsize=list(text=8)),
#               at=brks,colorkey=list(at=brks.lab),pretty=brks.lab)
#   print(x)
#   dev.off()
#   
  
}








