#JRV May 2015
#explore some yield-climate relationships in Toshi's data
stop("!")

#source scripts
src.dir <- "~/Repositories/dapa-climate-change/trunk/robustness"
source(paste(src.dir,"/glam-utils/make_wth.R",sep=""))
source(paste(src.dir,"/meteo/extract_weather.R",sep=""))

#working directory
wd <- "/nfs/a101/earjr/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")

#output directory
out_dir <- "~/Leeds-work/climate-for-breeding/yield_analysis"

#load initial conditions and yield data
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

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
  yvals <- runif(19,-100,100)
  x1 <- runif(19,-100,100)
  x2 <- runif(19,-100,100)
  fit <- lm(yvals~x1+x2)
  rsq <- summary(fit)$r.squared
  rsqall <- c(rsqall, rsq)
}
rsq_thresh <- quantile(rsqall, probs=0.95)

#function to calc tt
ttfun <- function(tmean,tb,to) {
  if (tmean<to & tmean>tb) {
    teff <- tmean-tb
  } else if (tmean>=to) {
    teff <- to-tb
  } else if (tmean<=tb) {
    teff <- 0
  }
  return(teff)
}

#list of locations
loc_list <- list.files(paste(met_dir,"/ascii_extract_raw/obs_hist_WFD",sep=""),pattern="meteo_cell-")
loc_list <- gsub("meteo_cell-","",loc_list)
loc_list <- as.numeric(gsub(".met","",loc_list))

#create meteorology for missing locations
xy_sel <- xy_main[which(!xy_main$LOC%in%loc_list),]
if (nrow(xy_sel)>0) {
  for (i in 1:nrow(xy_sel)) {
   loc <- xy_sel$LOC[i]; x <- xy_sel$x[i]; y <- xy_sel$y[i]
   wval <- extract_weather(cellid=loc, lon=x, lat=y, met_dir=met_dir, data_type="obs", dataset="WFD", 
                           sce="hist", years=1950:2001)
  }
}

#load output if exists
if (file.exists(paste(out_dir,"/yield_models.RData",sep=""))) {load(paste(out_dir,"/yield_models.RData",sep=""))}

#loop locations if objects dont exist
if (!exists("out_all")) {
  out_all <- data.frame()
  data_all <- data.frame()
  for (loc in loc_list) {
    #loc <- loc_list[120]
    cat("...processing loc=",loc,"(",which(loc_list%in%loc),"of",length(loc_list),")","\n")
    xy_loc <- xy_main[which(xy_main$LOC==loc),]
    
    #obs data
    sowdate <- round(mean(c(xy_loc$SOW_DATE1,xy_loc$SOW_DATE2)),0)
    hardate <- round(mean(c(xy_loc$HAR_DATE1,xy_loc$HAR_DATE1)),0)
    if (hardate<sowdate) {growper <- c(sowdate:365,1:hardate)} else {growper <- c(sowdate:hardate)}
    cropdur <- length(growper)
    yieldsr <- as.numeric(xy_main_yield[which(xy_main_yield$x == xy_loc$x & xy_main_yield$y == xy_loc$y),paste("Y.",1982:2001,sep="")])
    yielddf <- yieldsr[2:length(yieldsr)] - yieldsr[1:(length(yieldsr)-1)]
    logyield <- log(yieldsr)
    
    #load meteorology for this grid cell
    metdata <- read.table(paste(met_dir,"/ascii_extract_raw/obs_hist_WFD/meteo_cell-",xy_loc$LOC,".met",sep=""),header=T)
    if (hardate<sowdate) {
      iyear <- 1981; fyear <- 2000
      metdata <- metdata[metdata$year%in%c(iyear:(fyear+1)),]
    } else {
      iyear <- 1982; fyear <- 2001
      metdata <- metdata[metdata$year%in%c(iyear:fyear),]
    }
    
    
    #calculate gdd for fixed period and totrain per gs
    outdf <- data.frame()
    yrcount <- 1
    for (yr in iyear:fyear) {
      #yr <- iyear
      #get gs meteorology for different types of sowing-harvest conditions
      if (hardate < sowdate) {
        yrmet1 <- metdata[metdata$year%in%yr,]; gsmet1 <- yrmet1[yrmet1$jday%in%c(sowdate:365),]
        yrmet2 <- metdata[metdata$year%in%(yr+1),]; gsmet2 <- yrmet2[yrmet2$jday%in%c(1:hardate),]
        gsmet <- rbind(gsmet1, gsmet2)
      } else {
        yrmet <- metdata[metdata$year%in%yr,]
        gsmet <- yrmet[yrmet$jday%in%growper,]
      }  
      gsmet$tas <- (gsmet$tasmin + gsmet$tasmax) * 0.5
      ttgs <- sapply(gsmet$tas, ttfun, tb=8, to=32.5)
      ttgs <- sum(ttgs)
      gsmet$pr[which(gsmet$pr < 0)] <- 0; prt <- sum(gsmet$pr)
      
      #decade and 5year technological periods
      if (yr < 1990) {dec <- 1980}
      if (yr >= 1990 & yr < 2000) {dec <- 1990}
      if (yr >=  2000) {dec <- 2000}
      if (yr < 1985) {tecper <- 1980}
      if (yr >= 1985 & yr < 1990) {tecper <- 1985}
      if (yr >= 1990 & yr < 1995) {tecper <- 1990}
      if (yr >= 1995 & yr < 2000) {tecper <- 1995}
      if (yr >= 2000 & yr < 2005) {tecper <- 2000}
      
      out_row <- data.frame(year=yr,dec=dec,period=tecper,gdd=ttgs,prt=prt)
      outdf <- rbind(outdf, out_row)
      
      out_row <- cbind(loc=loc, x=xy_loc$x, y=xy_loc$y, me=xy_loc$ME, me_new=xy_loc$ME_NEW, out_row, 
                       yield=yieldsr[yrcount], logyield=logyield[yrcount])
      data_all <- rbind(data_all, out_row)
      yrcount <- yrcount+1
    }
    
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
        #rcp <- "rcp85"; bs <- "worst"
        chg_val <- as.numeric(extract(rates_list[[paste(rcp,".",bs,sep="")]], xy_loc[,c("x","y")]))
        ychg_abs <- chg_val * gdd_est #absolute
        ychg_rel <- ychg_abs / mean(outdf$yield[which(outdf$year > 1995 & outdf$year <= 2000)],na.rm=T) * 100 #relative (in %)
        row_out <- cbind(row_out, abs=ychg_abs, rel=ychg_rel)
        names(row_out)[c((ncol(row_out)-1),ncol(row_out))] <- paste(c("abs.","rel."),rcp,bs,sep="")
      }
    }
    
    #append output
    out_all <- rbind(out_all, row_out)
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
pdf(paste(out_dir,"/per_grid_cell_gdd_prtot_response.pdf",sep=""), height=4.5,width=10,pointsize=16)
print(tplot)
dev.off()

#regression coefficients only for significant areas
coefstk <- stack(gddcoef, prtcoef)
coefstk[which(rsqcoef[] < rsq_thresh)] <- NA
tplot <- rs_levplot2(coefstk,zn=NA,zx=NA,nb=NA,brks=seq(-5,5,by=0.5),scale="RdYlBu",col_i=NA,
                     col_f=NA,ncol=9,rev=T,leg=list(at=seq(-5,5,by=1),labels=c("<-5",paste(seq(-4,4,by=1)),">5")),
                     panel_names=c("gdd response","seasonal precip. response"))
pdf(paste(out_dir,"/per_grid_cell_gdd_prtot_response_sig_models.pdf",sep=""), height=4.5,width=10,pointsize=16)
print(tplot)
dev.off()

#rsquare areas
tplot <- rs_levplot2(rsqcoef,zn=NA,zx=NA,nb=NA,brks=c(0,rsq_thresh,1),scale="RdYlBu",col_i=NA,
                     col_f=NA,ncol=9,rev=T,leg=list(at=c(0,rsq_thresh,1),labels=paste(c(0,round(rsq_thresh,2),1))),
                     panel_names=c("r-square"))
pdf(paste(out_dir,"/per_grid_cell_rsquare_areas.pdf",sep=""), height=5,width=6,pointsize=16)
print(tplot)
dev.off()

#rsquare areas
rsqcoef[which(rsqcoef[] < rsq_thresh)] <- NA
tplot <- rs_levplot2(rsqcoef,zn=NA,zx=NA,nb=NA,brks=seq(0.25,1,by=0.05),scale="Blues",col_i=NA,
                     col_f=NA,ncol=9,rev=T,leg=list(at=seq(0.25,1,by=0.05),labels=paste(seq(0.25,1,by=0.05))),
                     panel_names=c("r-square"))
pdf(paste(out_dir,"/per_grid_cell_rsquare_sig_models.pdf",sep=""), height=5,width=6,pointsize=16)
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
    pdf(paste(out_dir,"/per_grid_cell_rate_yield_",rcp,"_",bs,"case.pdf",sep=""), height=4.5,width=10,pointsize=16)
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
              "fit0c","fit1","fit2"),file=paste(out_dir,"/yield_models.RData",sep=""))
}


###
### plotting
#head(fitdata)
ncycles <- 1 #1 cycle
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  for (bs in c("best","worst")) {
    #rcp <- "rcp85"; bs <- "worst"
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
    
    #create rasters
    rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
    perch_yield <- absch_yield <- raster(rs_ref)
    perch_yield[yieldresp$loc] <- yieldresp$perch_yield
    absch_yield[yieldresp$loc] <- yieldresp$absch_yield
    #plot(perch_yield)
    
    #plot figure (relative change)
    tplot <- rs_levplot2(perch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-1.5,1.5,by=0.25),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-1.5,1.5,by=0.25),labels=paste(seq(-1.5,1.5,by=0.25))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit1_rel.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
    
    #plot figure (absolute change)
    tplot <- rs_levplot2(absch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-20,20,by=5),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-20,20,by=5),labels=paste(seq(-20,20,by=5))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit1_abs.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
  }
}


### fit2
ncycles <- 1 #1 cycle
for (rcp in c("rcp26","rcp45","rcp60","rcp85")) {
  for (bs in c("best","worst")) {
    #rcp <- "rcp85"; bs <- "worst"
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
    
    #create rasters
    rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
    perch_yield <- absch_yield <- raster(rs_ref)
    perch_yield[yieldresp$loc] <- yieldresp$perch_yield
    absch_yield[yieldresp$loc] <- yieldresp$absch_yield
    #plot(perch_yield)
    
    #plot figure (relative change)
    tplot <- rs_levplot2(perch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-10,10,by=2.5),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-10,10,by=2.5),labels=paste(seq(-10,10,by=2.5))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit2_rel.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
    
    #plot figure (absolute change)
    tplot <- rs_levplot2(absch_yield,zn=NA,zx=NA,nb=NA,brks=seq(-80,80,by=20),scale="RdYlBu",col_i=NA,
                         col_f=NA,ncol=9,rev=T,leg=list(at=seq(-80,80,by=20),labels=paste(seq(-80,80,by=20))),
                         panel_names=c("rate of change in yield"))
    pdf(paste(out_dir,"/yield_response_gdd_chg_",rcp,"_",bs,"case_fit2_abs.pdf",sep=""), height=5,width=6,pointsize=16)
    print(tplot)
    dev.off()
  }
}


# ###
# ## response to changes in gdd, for fit1
# #test response data to 5% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.05
# fitdata$logyield05p <- predict(fit1, testdata)
# fitdata$yield05p <- exp(fitdata$logyield05p)
# 
# #test response data to 10% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.1
# fitdata$logyield10p <- predict(fit1, testdata)
# fitdata$yield10p <- exp(fitdata$logyield10p)
# 
# #response to 20% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.2
# fitdata$logyield20p <- predict(fit1, testdata)
# fitdata$yield20p <- exp(fitdata$logyield20p)
# 
# #response to 50% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.5
# fitdata$logyield50p <- predict(fit1, testdata)
# fitdata$yield50p <- exp(fitdata$logyield50p)
# 
# #append fitted yield
# fitdata$fitlogyield <- fitted(fit1)
# fitdata$fityield <- exp(fitdata$fitlogyield)
# 
# #calculate averages of all variables per location
# yieldresp <- aggregate(fitdata[,c("x","y","me","me_new","gdd","prt","yield","logyield","logyield05p","yield05p","logyield10p","yield10p","logyield20p","yield20p","logyield50p","yield50p","fitlogyield","fityield")],
#                 by=list(loc=fitdata$loc),FUN=function(x) {mean(x,na.rm=T)})
# yieldresp$perch_gdd05p <- (yieldresp$yield05p - yieldresp$fityield) / yieldresp$fityield * 100
# yieldresp$perch_gdd10p <- (yieldresp$yield10p - yieldresp$fityield) / yieldresp$fityield * 100
# yieldresp$perch_gdd20p <- (yieldresp$yield20p - yieldresp$fityield) / yieldresp$fityield * 100
# yieldresp$perch_gdd50p <- (yieldresp$yield50p - yieldresp$fityield) / yieldresp$fityield * 100
# 
# #create rasters with these responses
# rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
# perch_gdd05p <- raster(rs_ref)
# perch_gdd05p[yieldresp$loc] <- yieldresp$perch_gdd05p
# plot(perch_gdd05p)
# 
# perch_gdd10p <- raster(rs_ref)
# perch_gdd10p[yieldresp$loc] <- yieldresp$perch_gdd10p
# plot(perch_gdd10p)
# 
# perch_gdd20p <- raster(rs_ref)
# perch_gdd20p[yieldresp$loc] <- yieldresp$perch_gdd20p
# plot(perch_gdd20p)
# 
# perch_gdd50p <- raster(rs_ref)
# perch_gdd50p[yieldresp$loc] <- yieldresp$perch_gdd50p
# plot(perch_gdd50p)
# 
# #save responses to increasing GDD
# save(list=c("yieldresp","perch_gdd50p","perch_gdd20p","perch_gdd10p","perch_gdd05p"),
#      file=paste(out_dir,"/yield_response_increase_gdd_fit1.RData",sep=""))
# 
# tplot <- rs_levplot2(perch_gdd05p,zn=NA,zx=NA,nb=NA,brks=seq(-2,2,by=0.5),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-2,2,by=0.5),labels=paste(seq(-2,2,by=0.5))),
#                      panel_names=c("gdd +5%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_05per_fit1.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# tplot <- rs_levplot2(perch_gdd10p,zn=NA,zx=NA,nb=NA,brks=seq(-3,3,by=0.5),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-3,3,by=0.5),labels=paste(seq(-3,3,by=0.5))),
#                      panel_names=c("gdd +10%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_10per_fit1.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# tplot <- rs_levplot2(perch_gdd20p,zn=NA,zx=NA,nb=NA,brks=seq(-6,6,by=1),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-6,6,by=1),labels=paste(seq(-6,6,by=1))),
#                      panel_names=c("gdd +20%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_20per_fit1.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# tplot <- rs_levplot2(perch_gdd50p,zn=NA,zx=NA,nb=NA,brks=seq(-15,15,by=2.5),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-15,15,by=2.5),labels=paste(seq(-15,15,by=2.5))),
#                      panel_names=c("gdd +50%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_50per_fit1.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# 
# ###
# ## response to changes in gdd, for fit2
# #test response data to 5% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.05
# fitdata$logyield05p <- predict(fit2, testdata)
# fitdata$yield05p <- exp(fitdata$logyield05p)
# 
# #test response data to 10% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.1
# fitdata$logyield10p <- predict(fit2, testdata)
# fitdata$yield10p <- exp(fitdata$logyield10p)
# 
# #response to 20% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.2
# fitdata$logyield20p <- predict(fit2, testdata)
# fitdata$yield20p <- exp(fitdata$logyield20p)
# 
# #response to 50% increase in gdd
# testdata <- fitdata; testdata$gdd <- fitdata$gdd * 1.5
# fitdata$logyield50p <- predict(fit2, testdata)
# fitdata$yield50p <- exp(fitdata$logyield50p)
# 
# #append fitted yield
# fitdata$fitlogyield <- fitted(fit2)
# fitdata$fityield <- exp(fitdata$fitlogyield)
# 
# #calculate averages of all variables per location
# yieldresp <- aggregate(fitdata[,c("x","y","me","me_new","gdd","prt","yield","logyield","logyield05p","yield05p","logyield10p","yield10p","logyield20p","yield20p","logyield50p","yield50p","fitlogyield","fityield")],
#                        by=list(loc=fitdata$loc),FUN=function(x) {mean(x,na.rm=T)})
# yieldresp$perch_gdd05p <- (yieldresp$yield05p - yieldresp$fityield) / yieldresp$fityield * 100
# yieldresp$perch_gdd10p <- (yieldresp$yield10p - yieldresp$fityield) / yieldresp$fityield * 100
# yieldresp$perch_gdd20p <- (yieldresp$yield20p - yieldresp$fityield) / yieldresp$fityield * 100
# yieldresp$perch_gdd50p <- (yieldresp$yield50p - yieldresp$fityield) / yieldresp$fityield * 100
# 
# #create rasters with these responses
# rs_ref <- raster(paste(wd,"/data/maize_MEs/maizeMESglobal_lowres_me_final_splitted.tif",sep=""))
# perch_gdd05p <- raster(rs_ref)
# perch_gdd05p[yieldresp$loc] <- yieldresp$perch_gdd05p
# plot(perch_gdd05p)
# 
# perch_gdd10p <- raster(rs_ref)
# perch_gdd10p[yieldresp$loc] <- yieldresp$perch_gdd10p
# plot(perch_gdd10p)
# 
# perch_gdd20p <- raster(rs_ref)
# perch_gdd20p[yieldresp$loc] <- yieldresp$perch_gdd20p
# plot(perch_gdd20p)
# 
# perch_gdd50p <- raster(rs_ref)
# perch_gdd50p[yieldresp$loc] <- yieldresp$perch_gdd50p
# plot(perch_gdd50p)
# 
# #save responses to increasing GDD
# save(list=c("yieldresp","perch_gdd50p","perch_gdd20p","perch_gdd10p","perch_gdd05p"),
#      file=paste(out_dir,"/yield_response_increase_gdd_fit2.RData",sep=""))
# 
# tplot <- rs_levplot2(perch_gdd05p,zn=NA,zx=NA,nb=NA,brks=seq(-20,20,by=5),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-20,20,by=5),labels=paste(seq(-20,20,by=5))),
#                      panel_names=c("gdd +5%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_05per_fit2.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# tplot <- rs_levplot2(perch_gdd10p,zn=NA,zx=NA,nb=NA,brks=seq(-40,40,by=10),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-40,40,by=10),labels=paste(seq(-40,40,by=10))),
#                      panel_names=c("gdd +10%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_10per_fit2.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# tplot <- rs_levplot2(perch_gdd20p,zn=NA,zx=NA,nb=NA,brks=seq(-60,60,by=20),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-60,60,by=20),labels=paste(seq(-60,60,by=20))),
#                      panel_names=c("gdd +20%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_20per_fit2.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()
# 
# tplot <- rs_levplot2(perch_gdd50p,zn=NA,zx=NA,nb=NA,brks=seq(-100,100,by=25),scale="RdYlBu",col_i=NA,
#                      col_f=NA,ncol=9,rev=T,leg=list(at=seq(-100,100,by=25),labels=paste(seq(-100,100,by=25))),
#                      panel_names=c("gdd +50%"))
# pdf(paste(out_dir,"/yield_response_gdd_increase_50per_fit2.pdf",sep=""), height=5,width=6,pointsize=16)
# print(tplot)
# dev.off()


