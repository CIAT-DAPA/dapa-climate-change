#JRV May 2015
#explore some yield-climate relationships in Toshi's data
stop("!")

#load libraries
library(raster)

#source scripts
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
source(paste(src.dir,"/meteo/extract_weather.R",sep=""))

#working directory
wd <- "/nfs/a101/earjr/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")

# #fix what Maisa sent
# rs1 <- t(rs)
# > plot(rs1)
# > rs1 <- flip(rs1, direction="y")
# > plot(rs1)
# > rs1 <- flip(rs1, direction="x")
# > plot(rs1)
# > extent(rs1) <- c(-180,180,-90,90)

#output directory
out_dir <- "~/Leeds-work/climate-for-breeding/yield_analysis"

#load initial conditions, yield, and GDD/prec data
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))
prt_stk <- stack(paste(out_dir,"/output_TTR_prcp_obs/RData/PRCP_TOT_obs_1980_2000_",c(3:21),".tif",sep=""))
gdd_stk <- stack(paste(out_dir,"/output_TTR_prcp_obs/RData/TTR_obs_1980_2000_",c(3:21),".tif",sep=""))

#load rates of change
rates_list <- list()
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  #rcp <- "rcp85"
  for (bs in c("best","worst")) {
    #bs <- "best"
    rs <- raster(paste(out_dir,"/dur_change_tifs/GDD_change_map_",rcp,"_1995_2060_",bs,"case.tif",sep=""))
    rates_list[[paste(rcp,".",bs,sep="")]] <- rs; rm(rs)
  }
}

#monte-carlo estimate of r-squared significance
rsqall <- c()
for (i in 1:1000) {
  #i <- 1
  yvals <- runif(18,-100,100)
  x1 <- runif(18,-100,100)
  x2 <- runif(18,-100,100)
  fit <- lm(yvals~x1+x2)
  rsq <- summary(fit)$r.squared
  rsqall <- c(rsqall, rsq)
}
rsq_thresh <- quantile(rsqall, probs=0.95)

#list of locations
loc_list <- list.files(paste(met_dir,"/ascii_extract_raw/obs_hist_WFD",sep=""),pattern="meteo_cell-")
loc_list <- gsub("meteo_cell-","",loc_list)
loc_list <- as.numeric(gsub(".met","",loc_list))

#load output if exists
if (file.exists(paste(out_dir,"/yield_models_AfricaInputs.RData",sep=""))) {load(paste(out_dir,"/yield_models_AfricaInputs.RData",sep=""))}

#loop locations if objects dont exist
if (!exists("out_all")) {
  out_all <- data.frame()
  data_all <- data.frame()
  iyear <- 1982; fyear <- 2000
  for (loc in loc_list) {
    #loc <- loc_list[120]
    cat("...processing loc=",loc,"(",which(loc_list%in%loc),"of",length(loc_list),")","\n")
    xy_loc <- xy_main[which(xy_main$LOC==loc),]
    
    #obs data
    yieldsr <- as.numeric(xy_main_yield[which(xy_main_yield$x == xy_loc$x & xy_main_yield$y == xy_loc$y),paste("Y.",iyear:fyear,sep="")])
    yielddf <- yieldsr[2:length(yieldsr)] - yieldsr[1:(length(yieldsr)-1)]
    logyield <- log(yieldsr)
    
    #get gdd and total rain
    gdd_loc <- as.numeric(extract(gdd_stk, xy_loc[,c("x","y")]))
    prt_loc <- as.numeric(extract(prt_stk, xy_loc[,c("x","y")]))
    
    outdf <- data.frame(year=iyear:fyear,dec=NA,period=NA,gdd=gdd_loc,prt=prt_loc)
    
    #decade and 5-year technology periods
    outdf$dec[which(outdf$year < 1990)] <- 1980
    outdf$dec[which(outdf$year >= 1990 & outdf$year < 2000)] <- 1990
    outdf$dec[which(outdf$year >= 2000)] <- 2000
    outdf$period[which(outdf$year < 1985)] <- 1980
    outdf$period[which(outdf$year >= 1985 & outdf$year < 1990)] <- 1985
    outdf$period[which(outdf$year >= 1990 & outdf$year < 1995)] <- 1990
    outdf$period[which(outdf$year >= 1995 & outdf$year < 2000)] <- 1995
    outdf$period[which(outdf$year >= 2000 & outdf$year < 2005)] <- 2000
    
    #multiple location data.frame
    out_row <- cbind(loc=loc, x=xy_loc$x, y=xy_loc$y, me=xy_loc$ME, me_new=xy_loc$ME_NEW, outdf, 
                     yield=yieldsr, logyield=logyield)
    
    #append yield
    outdf$yield <- yieldsr
    outdf$logyield <- logyield
    
    #first differences data.frame
    fdifs <- data.frame(year=((iyear+1):fyear),yield=yielddf, prt=(outdf$prt[2:nrow(outdf)] - outdf$prt[1:(nrow(outdf)-1)]),
                        gdd=(outdf$gdd[2:nrow(outdf)] - outdf$gdd[1:(nrow(outdf)-1)]))
    
    #construct glm
    fit <- lm(yield ~ gdd + prt, data=fdifs, na.action=na.omit)
    fitsum <- summary(fit)
    rsq_m <- fitsum$r.squared
    coeffs <- as.data.frame(fitsum$coefficients)
    int_est <- coeffs$Estimate[1]; int_p <- coeffs[1,4]
    gdd_est <- coeffs$Estimate[2]; gdd_p <- coeffs[2,4]
    prt_est <- coeffs$Estimate[3]; prt_p <- coeffs[3,4]
    #plot(fdifs$yield, fitted(fit),xlim=c(-400,400),ylim=c(-400,400),pch=20)
    #abline(0,1,col="grey 50",lty=2)
    #cor.test(fdifs$yield, fitted(fit), method="pearson")$estimate^2
    
    #row output
    row_out <- data.frame(loc=loc,rsq=rsq_m,adj_rsq=fitsum$adj.r.squared,int_est=int_est,
                          int_pval=int_p,gdd_est=gdd_est,gdd_pval=gdd_p,prt_est=prt_est,
                          prt_pval=prt_p,meany=mean(outdf$yield[which(outdf$year > 1995 & outdf$year <= 2000)],na.rm=T))
    
    #calculate change in absolute terms and also relative to 1996-2000 observed yield levels
    for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
      for (bs in c("best","worst")) {
        chg_val <- as.numeric(extract(rates_list[[paste(rcp,".",bs,sep="")]], xy_loc[,c("x","y")]))
        ychg_abs <- chg_val * gdd_est #absolute
        ychg_rel <- ychg_abs / mean(outdf$yield[which(outdf$year > 1995 & outdf$year <= 2000)],na.rm=T) * 100 #relative (in %)
        row_out <- cbind(row_out, abs=ychg_abs, rel=ychg_rel)
        names(row_out)[c((ncol(row_out)-1),ncol(row_out))] <- paste(c("abs.","rel."),rcp,bs,sep="")
      }
    }
    
    #append output
    out_all <- rbind(out_all, row_out)
    data_all <- rbind(data_all, out_row)
  }
  #xx <- out_all
  sig_models <- out_all[which(out_all$rsq > rsq_thresh),]
  xy_tried <- xy_main[which(xy_main$LOC%in%loc_list),]
  xy_sig <- xy_main[which(xy_main$LOC%in%sig_models$loc),]
}

par(mar=c(3,3,1,1))
plot(xy_main$x, xy_main$y, pch=21,xlab=NA,ylab=NA)
points(xy_tried$x, xy_tried$y, pch=20,col="red")
points(xy_sig$x, xy_sig$y, pch=20,col="black")


######### source plotting stuff
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
  if (is.na(panel_names[1])) {panel_names <- names(rsin)}
  
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

###
#get the coefficients of gridcell specific regressions and rsquare into rasters and plot
rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
gddcoef <- prtcoef <- rsqcoef <- raster(rs_ref)
gddcoef[out_all$loc] <- out_all$gdd_est
gddcoef[which(gddcoef[] < -5)] <- -5; gddcoef[which(gddcoef[] > 5)] <- 5
prtcoef[out_all$loc] <- out_all$prt_est
prtcoef[which(prtcoef[] < -5)] <- -5
rsqcoef[out_all$loc] <- out_all$rsq

#regression coefficients
coefstk <- stack(gddcoef, prtcoef)
tplot <- rs_levplot2(coefstk,zn=NA,zx=NA,nb=NA,brks=seq(-5,5,by=0.5),scale="RdYlBu",col_i=NA,
                     col_f=NA,ncol=9,rev=T,leg=list(at=seq(-5,5,by=1),labels=c("<-5",paste(seq(-4,4,by=1)),">5")),
                     panel_names=c("gdd response","seasonal precip. response"))
pdf(paste(out_dir,"/per_grid_cell_gdd_prtot_response_AfricaInputs.pdf",sep=""), height=4.5,width=10,pointsize=16)
print(tplot)
dev.off()

#regression coefficients only for significant areas
coefstk <- stack(gddcoef, prtcoef)
coefstk[which(rsqcoef[] < rsq_thresh)] <- NA
tplot <- rs_levplot2(coefstk,zn=NA,zx=NA,nb=NA,brks=seq(-5,5,by=0.5),scale="RdYlBu",col_i=NA,
                     col_f=NA,ncol=9,rev=T,leg=list(at=seq(-5,5,by=1),labels=c("<-5",paste(seq(-4,4,by=1)),">5")),
                     panel_names=c("gdd response","seasonal precip. response"))
pdf(paste(out_dir,"/per_grid_cell_gdd_prtot_response_sig_models_AfricaInputs.pdf",sep=""), height=4.5,width=10,pointsize=16)
print(tplot)
dev.off()

#rsquare areas
tplot <- rs_levplot2(rsqcoef,zn=NA,zx=NA,nb=NA,brks=c(0,rsq_thresh,1),scale="RdYlBu",col_i=NA,
                     col_f=NA,ncol=9,rev=T,leg=list(at=c(0,rsq_thresh,1),labels=paste(c(0,round(rsq_thresh,2),1))),
                     panel_names=c("r-square"))
pdf(paste(out_dir,"/per_grid_cell_rsquare_areas_AfricaInputs.pdf",sep=""), height=5,width=6,pointsize=16)
print(tplot)
dev.off()

#rsquare areas
rsqcoef[which(rsqcoef[] < rsq_thresh)] <- NA
tplot <- rs_levplot2(rsqcoef,zn=NA,zx=NA,nb=NA,brks=seq(0.25,1,by=0.05),scale="Blues",col_i=NA,
                     col_f=NA,ncol=9,rev=T,leg=list(at=seq(0.25,1,by=0.05),labels=paste(seq(0.25,1,by=0.05))),
                     panel_names=c("r-square"))
pdf(paste(out_dir,"/per_grid_cell_rsquare_sig_models_AfricaInputs.pdf",sep=""), height=5,width=6,pointsize=16)
print(tplot)
dev.off()

#plots for response of yield change per variety
rsqcoef[out_all$loc] <- out_all$rsq
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  for (bs in c("best","worst")) {
    #rcp <- "rcp85"; bs <- "worst"
    trs <- raster(rs_ref)
    trs[out_all$loc] <- out_all[,paste("rel.",rcp,bs,sep="")]
    trs[which(rsqcoef[] < rsq_thresh)] <- NA
    trs[which(trs[] > 50)] <- 50; trs[which(trs[] < -50)] <- -50
    tplot <- rs_levplot2(trs,zn=NA,zx=NA,nb=NA,brks=seq(-50,50,by=10),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-50,50,by=10),labels=c("<-50",paste(seq(-40,40,by=10)),">50")),
                         panel_names=c("rate of change"))
    pdf(paste(out_dir,"/per_grid_cell_rate_yield_",rcp,"_",bs,"case_AfricaInputs.pdf",sep=""), height=4.5,width=10,pointsize=16)
    print(tplot)
    dev.off()
  }
}


###
### construct the fixed-effect model with all_data, 
### * model: random effects for ME_NEW and PERIOD
library(lme4)

fitdata <- data_all
if (!exists("fit2")) {
  fit0a <- lmer(logyield ~ 1 + (1|period), data=fitdata)
  plot(fitdata$logyield, fitted(fit0a), pch=20); abline(0,1)
  fit0b <- lmer(logyield ~ 1 + (1|me_new), data=fitdata)
  plot(fitdata$logyield, fitted(fit0b), pch=20); abline(0,1)
  fit0c <- lmer(logyield ~ 1 + (1|period) + (1|me_new), data=fitdata)
  plot(fitdata$logyield, fitted(fit0c), pch=20); abline(0,1)
  
  #fit using linear gdd term only
  fit1 <- lmer(logyield ~ 1 + gdd + prt + (1|me_new) + (1|period), data=fitdata)
  plot(fitdata$logyield, fitted(fit1), pch=20); abline(0,1)
  cor.test(fitdata$logyield, fitted(fit1))$estimate^2
  summary(fit1)
  
  #fit using polynomial term for gdd
  fit2 <- lmer(logyield ~ 1 + poly(gdd,2) + prt + (1|me_new) + (1|period), data=fitdata)
  plot(fitdata$logyield, fitted(fit2), pch=20); abline(0,1)
  cor.test(fitdata$logyield, fitted(fit2))$estimate^2
  summary(fit2)
  
  #testing for significance against less complex models
  anova(fit0a, fit1); anova(fit0b, fit1); anova(fit0c, fit1)
  anova(fit0a, fit2); anova(fit0b, fit2); anova(fit0c, fit2)
  anova(fit1,fit2)
  
  save(list=c("out_all","data_all","rsq_thresh","sig_models","xy_tried","xy_sig","fit0a","fit0b",
              "fit0c","fit1","fit2"),file=paste(out_dir,"/yield_models_AfricaInputs.RData",sep=""))
}

### plotting
#head(fitdata)
ncycles <- 1 #1 cycle
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  for (bs in c("best","worst")) {
    #rcp <- "rcp85"; bs <- "best"
    trs <- rates_list[[paste(rcp,".",bs,sep="")]]
    testdata <- fitdata
    testdata$gdd_chg <- extract(trs, testdata[,c("x","y")])
    testdata$gdd <- testdata$gdd + testdata$gdd_chg * ncycles
    testdata$logyield_fut <- predict(fit1, testdata)
    testdata$yield_fut <- exp(testdata$logyield_fut)
    testdata$fitlogyield <- fitted(fit1)
    testdata$fityield <- exp(testdata$fitlogyield)
    
    #yield response per grid cell
    yieldresp <- aggregate(testdata[,c("x","y","me","me_new","gdd","prt","yield","logyield","logyield_fut","yield_fut","fitlogyield","fityield")],
                           by=list(loc=testdata$loc),FUN=function(x) {mean(x,na.rm=T)})
    yieldresp$perch_yield <- (yieldresp$yield_fut - yieldresp$fityield) / yieldresp$fityield * 100
    yieldresp$absch_yield <- yieldresp$yield_fut - yieldresp$fityield
    
    if (rcp == "rcp85") {
      pdf(paste(out_dir,"/boxplot_yield_response_ME_",rcp,"_",bs,"case_fit1_AfricaInputs.pdf",sep=""),
          height=5,width=5)
      par(mar=c(3,5,1,1),las=1)
      boxplot(yieldresp$perch_yield ~ yieldresp$me, pch=20, names=c("WUMA","WLMA","DMA","WL","DL"),
              ylab="Yield change per cycle [%]",ylim=c(-3.5,0))
      grid()
      dev.off()
    }
    
    #create rasters
    rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
    perch_yield <- absch_yield <- raster(rs_ref)
    perch_yield[yieldresp$loc] <- yieldresp$perch_yield
    absch_yield[yieldresp$loc] <- yieldresp$absch_yield
    #plot(perch_yield)
    
    #plot figure (relative change)
    tplot <- rs_levplot2(perch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-5,5,by=1),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-5,5,by=1),labels=paste(seq(-5,5,by=1))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit1_rel_AfricaInputs.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
    
    #plot figure (absolute change)
    tplot <- rs_levplot2(absch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-50,50,by=10),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-50,50,by=10),labels=paste(seq(-50,50,by=10))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit1_abs_AfricaInputs.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
  }
}


### fit2
ncycles <- 1 #1 cycle
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  for (bs in c("best","worst")) {
    #rcp <- "rcp85"; bs <- "best"
    trs <- rates_list[[paste(rcp,".",bs,sep="")]]
    testdata <- fitdata
    testdata$gdd_chg <- extract(trs, testdata[,c("x","y")])
    testdata$gdd <- testdata$gdd + testdata$gdd_chg * ncycles
    testdata$logyield_fut <- predict(fit2, testdata)
    testdata$yield_fut <- exp(testdata$logyield_fut)
    testdata$fitlogyield <- fitted(fit2)
    testdata$fityield <- exp(testdata$fitlogyield)
    
    #yield response per grid cell
    yieldresp <- aggregate(testdata[,c("x","y","me","me_new","gdd","prt","yield","logyield","logyield_fut","yield_fut","fitlogyield","fityield")],
                           by=list(loc=testdata$loc),FUN=function(x) {mean(x,na.rm=T)})
    yieldresp$perch_yield <- (yieldresp$yield_fut - yieldresp$fityield) / yieldresp$fityield * 100
    yieldresp$absch_yield <- yieldresp$yield_fut - yieldresp$fityield
    
    if (rcp == "rcp85") {
      pdf(paste(out_dir,"/boxplot_yield_response_ME_",rcp,"_",bs,"case_fit2_AfricaInputs.pdf",sep=""),
          height=5,width=5)
      par(mar=c(3,5,1,1),las=1)
      boxplot(yieldresp$perch_yield ~ yieldresp$me, pch=20, names=c("WUMA","WLMA","DMA","WL","DL"),
              ylab="Yield change per cycle [%]",ylim=c(-15,10))
      grid()
      dev.off()
    }
    
    #create rasters
    rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
    perch_yield <- absch_yield <- raster(rs_ref)
    perch_yield[yieldresp$loc] <- yieldresp$perch_yield
    absch_yield[yieldresp$loc] <- yieldresp$absch_yield
    #plot(perch_yield)
    
    #plot figure (relative change)
    tplot <- rs_levplot2(perch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-20,20,by=5),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-20,20,by=5),labels=paste(seq(-20,20,by=5))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit2_rel_AfricaInputs.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
    
    #plot figure (absolute change)
    tplot <- rs_levplot2(absch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-350,350,by=50),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-350,350,by=50),labels=paste(seq(-350,350,by=50))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit2_abs_AfricaInputs.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
  }
}


