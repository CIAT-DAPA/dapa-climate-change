#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#summarise and make charts of skill of randomised optimisation runs

#local
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
bDir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"
maxiter <- 10
version <- "c"
selection <- "v3"
base_exp <- 10 #change if you have done any other experiment

#run <- 1
#expID <- "10"

#eljefe
# src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
# bDir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"
# maxiter <- 10
# version <- "d"
# selection <- "v4"
# base_exp <- 31 #change if you have done any other experiment

cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

####list of seeds to randomise parameter list
set.seed(512)
seeds <- c(NA,sample(1:9999,20))
#seeds <- c(NA)

expIDs <- c(base_exp:((base_exp-1)+length(seeds)))
expIDs[which(expIDs<10)] <- paste("0",expIDs,sep="")
expIDs <- paste(expIDs)

#list of runs to be performed
runs_ref <- data.frame(SID=1:length(seeds),SEED=seeds,EXPID=expIDs)

#output summary folders
sum_out <- paste(cropDir,"/calib/results_exp",sep="")
set_odir <- paste(sum_out,"/summary_exp_",runs_ref$EXPID[1],"-",runs_ref$EXPID[nrow(runs_ref)],sep="")

sum_spatial <- paste(set_odir,"/spatial",sep="")
if (!file.exists(sum_spatial)) {dir.create(sum_spatial)}

### get summaries and do some plots
if (!file.exists(paste(sum_spatial,"/ccoef_y_ysd_spat.csv",sep=""))) {
  skill <- data.frame()
  scount <- 1
  for (seed in seeds) {
    #seed <- seeds[2]
    if (is.na(seed)) {
      idexp <- runs_ref$EXPID[which(is.na(runs_ref$SEED))][1]
    } else {
      idexp <- runs_ref$EXPID[which(runs_ref$SEED == seed)][1]
    }
    
    cal_dir <- paste(cropDir,"/calib/exp-",idexp,sep="")
    
    #load calib_all_cells.csv
    cal_data <- read.csv(paste(cal_dir,"/calib_all_cells.csv",sep=""))
    r_ym <- cor.test(cal_data$Y_OBS,cal_data$Y_PRED,na.rm=T)$estimate
    p_ym <- cor.test(cal_data$Y_OBS,cal_data$Y_PRED,na.rm=T)$p.value
    
    r_ys <- cor.test(cal_data$YSD_OBS,cal_data$YSD_PRED,na.rm=T)$estimate
    p_ys <- cor.test(cal_data$YSD_OBS,cal_data$YSD_PRED,na.rm=T)$p.value
    
    odf <- data.frame(EXPID=idexp,CCOEF_Y=r_ym,PVAL_Y=p_ym,CCOEF_YSD=r_ys,PVAL_YSD=p_ys)
    skill <- rbind(skill,odf)
    
    #produce density plots of each skil parameter
    dp <- density(cal_data$YGP,na.rm=T)
    dp$y <- dp$y/max(dp$y)
    if (scount==1) {
      tiff(paste(sum_spatial,"/ygp.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
      par(mar=c(5,5,1,1))
      plot(dp$x,dp$y,ty="l",xlab="YGP",ylab="Normalised PDF",col="grey 60",lwd=0.75)
      dev.next()
    } else {
      dev.set(which=2)
      lines(dp$x,dp$y,col="grey 60",lwd=0.75)
    }
    
    dp <- density(cal_data$CCOEF,na.rm=T)
    dp$y <- dp$y/max(dp$y)
    if (scount==1) {
      tiff(paste(sum_spatial,"/ccoef.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
      par(mar=c(5,5,1,1))
      plot(dp$x,dp$y,ty="l",xlab="CCOEF",ylab="Normalised PDF",col="grey 60",lwd=0.75)
      dev.next()
    } else {
      dev.set(which=3)
      lines(dp$x,dp$y,col="grey 60",lwd=0.75)
    }
    
    dp <- density(cal_data$RMSE,na.rm=T)
    dp$y <- dp$y/max(dp$y)
    if (scount==1) {
      tiff(paste(sum_spatial,"/rmse.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
      par(mar=c(5,5,1,1))
      plot(dp$x,dp$y,ty="l",xlab="RMSE (kg/ha)",ylab="Normalised PDF",col="grey 60",lwd=0.75)
      dev.next()
    } else {
      dev.set(which=4)
      lines(dp$x,dp$y,col="grey 60",lwd=0.75)
    }
    
    dp <- density(cal_data$P_RMSE,na.rm=T)
    dp$y <- dp$y/max(dp$y)
    if (scount==1) {
      tiff(paste(sum_spatial,"/prmse.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
      par(mar=c(5,5,1,1))
      plot(dp$x,dp$y,ty="l",xlab="RMSE (%)",ylab="Normalised PDF",col="grey 60",lwd=0.75)
      dev.next()
    } else {
      dev.set(which=5)
      lines(dp$x,dp$y,col="grey 60",lwd=0.75)
    }
    
    scount <- scount+1
  }
  
  for (gdev in 2:5) {
    dev.set(which=gdev)
    grid(lwd=0.5)
  }
  graphics.off()
  
  #plot the spread of randomised optimisation runs in a scattergram
  tiff(paste(sum_spatial,"/random_skill_xy.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
  par(mar=c(5,5,1,1))
  plot(skill$CCOEF_Y,skill$CCOEF_YSD,pch=20,cex=0.75,xlab="CCOEF (Mean yield)",ylab="CCOEF (SD yield)")
  grid(lwd=0.75)
  dev.off()
  
  dp <- density(skill$CCOEF_Y,na.rm=T)
  dp$y <- dp$y/max(dp$y)
  tiff(paste(sum_spatial,"/random_skill_pdf.tif",sep=""),res=300,compression="lzw",height=1000,width=1250,pointsize=8)
  par(mar=c(5,5,1,1))
  plot(dp$x,dp$y,lwd=1,lty=1,ty="l",xlab="CCOEF (spatial)",ylab="Normalised PDF",xlim=c(min(skill$CCOEF_Y,skill$CCOEF_YSD),max(skill$CCOEF_Y,skill$CCOEF_YSD)))
  dp <- density(skill$CCOEF_YSD,na.rm=T)
  dp$y <- dp$y/max(dp$y)
  lines(dp$x,dp$y,lwd=1,lty=2)
  grid(lwd=0.5)
  dev.off()
  
  write.csv(skill,paste(sum_spatial,"/ccoef_y_ysd_spat.csv",sep=""),quote=T,row.names=F)
} else {
  skill <- read.csv(paste(sum_spatial,"/ccoef_y_ysd_spat.csv",sep=""))
}





