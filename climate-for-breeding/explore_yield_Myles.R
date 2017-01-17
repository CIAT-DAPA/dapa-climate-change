#JRV May 2015
#explore some yield-climate relationships in Toshi's data

library(raster); library(rasterVis); library(maptools); data(wrld_simpl)

#working directory
wd <- "/nfs/a101/earjr/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")

#output directory
out_dir <- "~/Leeds-work/climate-for-breeding/yield_analysis"

#load initial conditions
load(paste(mdata_dir,"/initial_conditions_major_dssat.RData",sep=""))

#load rates of change
rates_list <- list()
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  #rcp <- "rcp85"
  for (bs in c("best","worst")) {
    #bs <- "best"
    rs <- raster(paste(out_dir,"/dur_change_tifs/dur_change_map_",rcp,"_",bs,"case.tif",sep=""))
    rates_list[[paste(rcp,".",bs,sep="")]] <- rs; rm(rs)
  }
}

#load all_data
all_data <- read.table(paste(out_dir,"/dssat_yield_response_mjf/all_data.tab",sep=""),header=T,sep="\t")

#loop global runs and construct relationship
out_all <- data.frame()
for (i in 1:max(all_data$GLORUN)) {
  #i <- 14
  cat("...calculating for glorun=",i,"out of 25\n")
  rundata <- all_data[which(all_data$GLORUN == i),] #get glorun data
  rundata <- rundata[which(rundata$HWAM != 0),] #remove zero yields, probably due to some stress
  rundata$PYEAR <- as.numeric(substr(rundata$PDAT,1,4)) #get year of planting
  rundata$PDAY <- as.numeric(substr(rundata$PDAT,5,7)) #get day of planting
  rundata$HYEAR <- as.numeric(substr(rundata$HDAT,1,4)) #get year of harvest (=maturity)
  rundata$HDAY <- as.numeric(substr(rundata$HDAT,5,7)) #get day of harvest (=maturity)
  
  #calculate crop duration
  calcdur <- function(pyear,pday,hyear,hday) {
    x <- pday; y <- hday
    if (hyear == pyear) {
      dur <- length(c(x:y))
    } else if (hyear == (pyear+1)) {
      dur <- length(c(x:365,1:y))
    } else if (hyear == (pyear+2)) {
      dur <- length(c(x:365,1:y)) + 365
    } else if (hyear == (pyear+3)) {
      dur <- length(c(x:365,1:y)) + 365*2
    }
    return(dur)
  }
  rundata$CDUR <- mapply(calcdur, pyear=rundata$PYEAR, pday=rundata$PDAY, hyear=rundata$HYEAR, hday=rundata$HDAY)
  #rundata <- rundata[which(rundata$CDUR <= 250),]
  
  #significance of rsquare
  rsqall <- c()
  for (j in 1:1000) {
    #j <- 1
    yvals <- runif(nrow(rundata),-100,100)
    x1 <- runif(nrow(rundata),-100,100)
    x2 <- runif(nrow(rundata),-100,100)
    fit <- lm(yvals~x1+x2)
    rsq <- summary(fit)$r.squared
    rsqall <- c(rsqall, rsq)
  }
  rsq_thresh <- as.numeric(quantile(rsqall, probs=0.95))
  
  #construct fit
  plot(rundata$CDUR, rundata$HWAH, pch=20,main=paste("glorun=",i,sep=""))
  fit <- lm(HWAH ~ CDUR, data=rundata)
  sfit <- summary(fit)
  
  #append data
  row_out <- data.frame(glorun=i,elev=rundata$ELEVATION[1],lat=rundata$LATITUDE[1],n=nrow(rundata),
                        rsq=sfit$r.squared,rsq_thresh=rsq_thresh,int=as.numeric(coef(fit)[1]),
                        durfact=as.numeric(coef(fit)[2]),medianyield=median(rundata$HWAH),
                        mediandur=median(rundata$CDUR))
  out_all <- rbind(out_all, row_out)
}

#for each location in the xy_main data.frame associate a durfact
xy_main$DURFAC <- xy_main$INT <- xy_main$GLORUN <- xy_main$YIELDM<- xy_main$CDURM <- xy_main$VALID <- NA
for (i in 1:nrow(xy_main)) {
  #i <- 1
  ilat <- xy_main$y[i]; ielev <- xy_main$ELEV[i]
  latdist <- sqrt((out_all$lat - ilat)^2)
  latzone <- out_all[which(latdist == min(latdist)),]
  elevdist <- sqrt((latzone$elev - ielev)^2)
  selzone <- latzone[which(elevdist == min(elevdist)),]
  if (selzone$rsq[1] >= selzone$rsq_thresh[1]) {tvalid <- 1} else {tvalid <- 0}
  xy_main$GLORUN[i] <- selzone$glorun[1]
  xy_main$INT[i] <- selzone$int[1]
  xy_main$DURFAC[i] <- selzone$durfact[1]
  xy_main$YIELDM[i] <- selzone$medianyield[1]
  xy_main$CDURM[i] <- selzone$mediandur[1]
  xy_main$VALID[i] <- tvalid
}

### plots
#load needed libraries
library(raster); library(maptools); library(rasterVis); data(wrld_simpl)

###
#function to plot maps
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T,colours=NA,now_inc=T,panel_names=NA) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
    if (!is.na(colours[1])) {pal <- colours}
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
    if (!is.na(colours[1])) {pal <- colours}
  }
  if (rev) {pal <- rev(pal)}
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  if (is.na(panel_names)) {panel_names <- names(rsin)}
  
  #set theme
  this_theme <- custom.theme(fill = pal, region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin), names.attr=panel_names,
                             xlab='', ylab='') + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}

#figure details
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=20), norths=seq(-90,90,by=20))


#load base raster
rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))

#plots for response of yield change per variety
ncycles <- 1
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  for (bs in c("best","worst")) {
    #rcp <- "rcp85"; bs <- "best"
    trs <- rates_list[[paste(rcp,".",bs,sep="")]]
    xy_main$CHFAC <- extract(trs, xy_main[,c("x","y")])
    yieldsens <- yieldsens_p <- raster(rs_ref)
    yieldsens[xy_main$LOC] <- xy_main$DURFAC * ncycles * xy_main$CHFAC
    yieldsens[xy_main$LOC[which(xy_main$VALID == 0)]] <- NA
    yieldsens_p[xy_main$LOC] <- (xy_main$DURFAC * ncycles * xy_main$CHFAC) / xy_main$YIELDM * 100
    yieldsens_p[xy_main$LOC[which(xy_main$VALID == 0)]] <- NA
    
    #for boxplot
    xy_main$YSENS <- extract(yieldsens, xy_main[,c("x","y")])
    xy_main$YSENS_P <- extract(yieldsens_p, xy_main[,c("x","y")])
    boxplot(xy_main$YSENS_P~xy_main$ME, pch=20)
    
    if (rcp == "rcp85") {
      pdf(paste(out_dir,"/boxplot_yield_response_ME_",rcp,"_",bs,"case_dssat_AfricaInputs.pdf",sep=""),
          height=5,width=5)
      par(mar=c(3,5,1,1),las=1)
      boxplot(xy_main$YSENS_P~xy_main$ME, pch=20, names=c("WUMA","WLMA","DMA","WL","DL"),
              ylab="Yield change per cycle [%]",ylim=c(-10,0))
      grid()
      dev.off()
    }
    
    #plot here
    tplot <- rs_levplot2(yieldsens,zn=NA,zx=NA,nb=NA,brks=seq(-700,700,by=100),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-700,700,by=100),labels=paste(seq(-700,700,by=100))),
                         panel_names=c("abs. yield sensitivity"))
    pdf(paste(out_dir,"/yield_response_dssat_gdd_chg_",rcp,"_",bs,"case_abs.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
    
    #plot yield sensitivity to 1-day duration reduction (relative to median yield of all cultivars)
    tplot <- rs_levplot2(yieldsens_p,zn=NA,zx=NA,nb=NA,brks=seq(-10,10,by=2),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-10,10,by=2),labels=paste(seq(-10,10,by=2))),
                         panel_names=c("rel. yield sensitivity"))
    pdf(paste(out_dir,"/yield_response_dssat_gdd_chg_",rcp,"_",bs,"case_rel.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
  }
}

# #load and put data in raster
# yieldsens <- yieldsens_p <- raster(rs_ref)
# yieldsens[xy_main$LOC] <- xy_main$DURFAC * -1
# yieldsens[xy_main$LOC[which(xy_main$VALID == 0)]] <- NA
# yieldsens_p[xy_main$LOC] <- (xy_main$DURFAC * -1) / xy_main$YIELDM * 100
# yieldsens_p[xy_main$LOC[which(xy_main$VALID == 0)]] <- NA
# 
# ###
# #function to plot maps
# rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T,colours=NA,now_inc=T,panel_names=NA) {
#   if (scale %in% row.names(brewer.pal.info)) {
#     pal <- rev(brewer.pal(ncol, scale))
#     if (!is.na(colours[1])) {pal <- colours}
#   } else {
#     pal <- colorRampPalette(c(col_i,col_f))(ncol)
#     if (!is.na(colours[1])) {pal <- colours}
#   }
#   if (rev) {pal <- rev(pal)}
#   if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
#   if (is.na(panel_names)) {panel_names <- names(rsin)}
#   
#   #set theme
#   this_theme <- custom.theme(fill = pal, region = pal,
#                              bg = "white", fg = "grey20", pch = 14)
#   
#   p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
#                              at = brks, maxpixels=ncell(rsin), names.attr=panel_names,
#                              xlab='', ylab='') + 
#     layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
#     layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
#   return(p)
# }
# 
# #figure details
# grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=20), norths=seq(-90,90,by=20))
# 
# #plot yield sensitivity to 1-day duration reduction (absolute)
# tplot <- rs_levplot2(yieldsens,zn=NA,zx=NA,nb=NA,brks=seq(-100,100,by=20),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-100,100,by=20),labels=paste(seq(-100,100,by=20))),
#                      panel_names=c("abs. yield sensitivity"))
# pdf(paste(out_dir,"/yield_sensitivity_1day_absolute.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# #plot yield sensitivity to 1-day duration reduction (relative to median yield of all cultivars)
# tplot <- rs_levplot2(yieldsens_p,zn=NA,zx=NA,nb=NA,brks=seq(-1.25,1.25,by=0.25),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-1.25,1.25,by=0.25),labels=paste(seq(-1.25,1.25,by=0.25))),
#                      panel_names=c("rel. yield sensitivity"))
# pdf(paste(out_dir,"/yield_sensitivity_1day_relative.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()


