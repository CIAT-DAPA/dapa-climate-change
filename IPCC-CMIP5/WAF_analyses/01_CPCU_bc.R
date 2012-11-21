#bias correction to CPCU data
#JRV Nov 2012
#CCAFS funded UoL-ANACIM project

b_dir <- "V:/eejarv/WAF_analysis"
src.dir <- "D:/_tools/dapa-climate-change/trunk"

source(paste(src.dir,"/PhD/0007-crop-modelling/scripts/cmip5/06.bc_rain-functions.R",sep=""))
source(paste(src.dir,"/PhD/0006-weather-data/scripts/GHCND-GSOD-functions.R",sep=""))
source(paste(src.dir,"/PhD/0008-CMIP5/scripts/CMIP5-functions.R",sep=""))

wd <- paste(b_dir,"/gridded_product/daily/senegal",sep="")

yi <- 1979
yf <- 2011

#load all years and locations
obs_all <- data.frame()
sat_all <- data.frame()
for (yr in yi:yf) {
  #yr <- yi
  cat("year:",yr,"\n")
  nd <- leap(yr)
  
  if (nd==366) {ffor <- "368F8"} else {ffor <- "367F8"}
  
  obs_dat <- read.fortran(paste(wd,"/senobs",yr,".txt",sep=""),format=ffor,na.strings="-99.00")
  names(obs_dat) <- c("LON","LAT",paste("JD.",1:nd,sep=""))
  if (nd==365) {obs_dat$JD.366 <- NA}
  obs_dat <- cbind(YEAR=yr,LOC=paste("LOC",1:nrow(obs_dat),sep=""),obs_dat)
  obs_all <- rbind(obs_all,obs_dat)
  
  sat_dat <- read.fortran(paste(wd,"/ncpcu",yr,".txt",sep=""),format=ffor,na.strings="-999.00")
  names(sat_dat) <- c("LON","LAT",paste("JD.",1:nd,sep=""))
  if (nd==365) {sat_dat$JD.366 <- NA}
  sat_dat <- cbind(YEAR=yr,LOC=paste("LOC",1:nrow(obs_dat),sep=""),sat_dat)
  sat_all <- rbind(sat_all,sat_dat)
}


#do the bias correction per location
ulocs <- unique(obs_all$LOC)

#object to store data and metrics
all_metr <- data.frame()
fit_loci <- data.frame()
bc_odata <- data.frame()

#loop locations
for (i in 1:length(ulocs)) {
  loc <- paste(ulocs[i]) #3 and 16
  cat("\nlocation",loc,"\n")
  #get and organise data
  loc_obs <- obs_all[which(obs_all$LOC == loc),]
  lon <- loc_obs$LON[1]; lat <- loc_obs$LAT[1]
  loc_obs$LOC <- NULL; loc_obs$LON <- NULL; loc_obs$LAT <- NULL
  obs_ml <- mk_mth_list(loc_obs,dg_fun="createDateGrid",dg_args=NA,yi=yi,yf=yf)
  
  loc_sat <- sat_all[which(sat_all$LOC == loc),]
  loc_sat$LOC <- NULL; loc_sat$LON <- NULL; loc_sat$LAT <- NULL
  sat_ml <- mk_mth_list(loc_sat,dg_fun="createDateGrid",dg_args=NA,yi=yi,yf=yf)
  
  #do the correction
  ofit_file <- paste(wd,"/biascorr/output/loci_fit_",loc,".csv",sep="")
  if (!file.exists(ofit_file)) {
    loci_mets <- loci_cal(obs_ml,sat_ml,wdt_obs=0.1,iter_step=0.0001)
    write.csv(loci_mets,paste(wd,"/biascorr/output/loci_fit_",loc,".csv",sep=""),quote=F,row.names=F)
  } else {
    loci_mets <- read.csv(paste(wd,"/biascorr/output/loci_fit_",loc,".csv",sep=""))
  }
  sat_ml <- loci_correct(sat_ml,loci_mets)
  sat_bc <- remake_daily(sat_ml,loc_sat,wleap="yes",yi,yf)
  
  #put data into single vector
  bcdata <- as.numeric(as.matrix(sat_bc[2:ncol(sat_bc)]))
  rwdata <- as.numeric(as.matrix(loc_sat[2:ncol(loc_sat)]))
  obdata <- as.numeric(as.matrix(loc_obs[2:ncol(loc_obs)]))
  
  #calculate correlation
  rwcor <- cor.test(obdata,rwdata,method="spearman",na.rm=T)$estimate
  bccor <- cor.test(obdata,bcdata,method="spearman",na.rm=T)$estimate
  
  #calculate bias
  bcbias <- mean(obdata-bcdata,na.rm=T)
  bcabias <- mean(abs(obdata-bcdata),na.rm=T)
  rwbias <- mean(obdata-rwdata,na.rm=T)
  rwabias <- mean(abs(obdata-rwdata),na.rm=T)
  
  #calculate rmse
  bcrmse <- (obdata-bcdata)^2
  bcrmse <- bcrmse[which(!is.na(bcrmse))]
  bcrmse <- sqrt(sum(bcrmse)/length(bcrmse))
  rwrmse <- (obdata-rwdata)^2
  rwrmse <- rwrmse[which(!is.na(rwrmse))]
  rwrmse <- sqrt(sum(rwrmse)/length(rwrmse))
  
  #calculate annual cycle
  bcacy <- colMeans(sat_bc[,2:ncol(sat_bc)],na.rm=T)
  rwacy <- colMeans(loc_sat[,2:ncol(loc_sat)],na.rm=T)
  obacy <- colMeans(loc_obs[,2:ncol(loc_obs)],na.rm=T)
  
  #correlation of annual cycle
  bccor_acy <- cor(obacy,bcacy)
  rwcor_acy <- cor(obacy,rwacy)
  
  #final output data
  out_row <- data.frame(LOC=loc,RHO_BC=bccor,RHO_RW=rwcor,BIAS_BC=bcbias,BIAS_RW=rwbias,
                        ABIAS_BC=bcabias,ABIAS_RW=rwabias,RMSE_BC=bcrmse,RMSE_RW=rwrmse,
                        RAC_BC=bccor_acy,RAC_RW=rwcor_acy)
  all_metr <- rbind(all_metr,out_row)
  loci_mets <- cbind(LOC=loc,LON=lon,LAT=lat,loci_mets)
  fit_loci <- rbind(fit_loci,loci_mets)
  sat_bc <- cbind(LOC=loc,LON=lon,LAT=lat,sat_bc)
  bc_odata <- rbind(bc_odata,sat_bc)
  
  #plot pdf of all days and all months
  xx <- obdata[which(obdata>0 & !is.na(obdata))]
  hob <- hist(xx,breaks=c(seq(0,40,by=1),300),plot=F)
  xv <- hob$mids
  hob <- hob$counts/sum(hob$counts)*100
  
  xx <- bcdata[which(bcdata>0 & !is.na(bcdata))]
  hbc <- hist(xx,breaks=c(seq(0,40,by=1),300),plot=F)
  hbc <- hbc$counts/sum(hbc$counts)*100
  
  xx <- rwdata[which(rwdata>0 & !is.na(rwdata))]
  hrw <- hist(xx,breaks=c(seq(0,40,by=1),300),plot=F)
  hrw <- hrw$counts/sum(hrw$counts)*100
  
  tiff(paste(wd,"/biascorr/figures/pdf_",loc,".tif",sep=""),
       res=300,compression="lzw",pointsize=6,width=1024,height=1024)
  par(mar=c(5,5,1,1))
  plot(xv,hob,ty="l",main=NA,ylab="PDF (%)",xlab="rain (mm/day)",col="black",ylim=c(0,50),xlim=c(0,40))
  lines(xv,hbc,main=NA,col="blue")
  lines(xv,hrw,main=NA,col="red")
  grid()
  dev.off()
}

write.csv(all_metr,paste(wd,"/biascorr/output/bc_skill.csv",sep=""),quote=T,row.names=F)
write.csv(fit_loci,paste(wd,"/biascorr/output/fit_data.csv",sep=""),quote=T,row.names=F)
write.csv(bc_odata,paste(wd,"/biascorr/output/bc_data.csv",sep=""),quote=T,row.names=F)


# #plot(loci_mets$MTH,loci_mets$WDT_MOD,ty="l",ylim=c(0,6))
# #abline(h=1,col="red")
# for (j in 1:nrow(sat_bc)) {
#   plot(1:366,sat_bc[j,2:ncol(sat_bc)],ty="l",col="blue",main=sat_bc$YEAR[j])
#   #plot(1:366,loc_sat[5,2:ncol(loc_sat)],ty="l",col="red")
#   lines(1:366,loc_obs[j,2:ncol(loc_obs)],ty="l",col="black")
# }


