#JRV May 2015
#explore some yield-climate relationships in Toshi's data
stop("!")

rm(list=ls())

#load libraries
library(raster); library(ncdf4)

#name of crop to analyse
cropname <- "Maize"

#working directory
#nfsDir <- "/nfs/a101/earjr"
nfsDir <- "~/Leeds-work"
wd <- paste(nfsDir,"/ToE-global-crops-seasons",sep="")
toe_dir <- paste(wd,"/yield_responses/toe_data",sep="")
mdata_dir <- paste(wd,"/yield_responses/analysis_data",sep="")
met_dir <- paste(wd,"/yield_responses/meteorology",sep="")

#output directory
out_dir <- paste(wd,"/yield_responses/results",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

#load rates of change
rates_list <- list()
for (rcp in c("RCP26","RCP45","RCP60","RCP85")) {
  #rcp <- "RCP85"
  if (tolower(cropname) == "soybean") {
    rs <- raster(paste(toe_dir,"/pp_change_atTOE_",rcp,"_Soy.nc",sep=""),varname="pr_change")
  } else {
    rs <- raster(paste(toe_dir,"/pp_change_atTOE_",rcp,"_",cropname,".nc",sep=""),varname="pr_change")
  }
  rs1 <- t(rs)
  rs1 <- flip(rs1, direction="y")
  rs1 <- flip(rs1, direction="x")
  extent(rs1) <- c(-180,180,-90,90)
  rates_list[[rcp]] <- rs1; rm(list=c("rs","rs1"))
}

#load output
load(file=paste(mdata_dir,"/analysis_data_",tolower(cropname),".RData",sep=""))
data_all <- out_data

#add "management" random factor (country) to this matrix
if (tolower(cropname) == "soybean") {
  coun_rs <- raster(paste(wd,"/data/growing_season_prod_countries_Soybeans.nc",sep=""), varname="countries")
} else {
  coun_rs <- raster(paste(wd,"/data/growing_season_prod_countries_",cropname,".nc",sep=""), varname="countries")
}
data_all$country <- extract(coun_rs, data_all[,c("x","y")])
na_coun <- which(is.na(data_all$country))
non_na <- data_all[which(!is.na(data_all$country)),]
for (nai in na_coun) {
  #nai <- na_coun[1]
  pdist <- pointDistance(data_all[nai,c("x","y")], non_na[which(non_na$year == 1986),c("x","y")], lonlat=T)
  newcoun <- non_na$country[which(pdist == min(pdist,na.rm=T))[1]]
  data_all$country[nai] <- newcoun
}

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
### construct the fixed-effect model with all_data, 
### * model: random effects for ME_NEW and PERIOD
library(lme4)

fitdata <- data_all
if (!file.exists(paste(out_dir,"/yield_models_",tolower(cropname),".RData",sep=""))) {
  fit0a <- lmer(logyield ~ 1 + (1|period), data=fitdata)
  plot(fitdata$logyield, fitted(fit0a), pch=20); abline(0,1)
  fit0b <- lmer(logyield ~ 1 + (1|country), data=fitdata)
  plot(fitdata$logyield, fitted(fit0b), pch=20); abline(0,1)
  fit0c <- lmer(logyield ~ 1 + (1|period) + (1|country), data=fitdata)
  plot(fitdata$logyield, fitted(fit0c), pch=20); abline(0,1)
  
  #fit using linear gdd term only
  fit1 <- lmer(logyield ~ 1 + gdd + prec + (1|country) + (1|period), data=fitdata)
  plot(fitdata$logyield, fitted(fit1), pch=20); abline(0,1)
  plot(fitdata$gdd, fitted(fit1), pch=20)
  plot(fitdata$prec, fitted(fit1), pch=20)
  cor.test(fitdata$logyield, fitted(fit1))$estimate^2
  summary(fit1)
  
  #fit using polynomial term for gdd
  fit2 <- lmer(logyield ~ 1 + poly(gdd,2) + poly(prec,2) + (1|country) + (1|period), data=fitdata)
  plot(fitdata$logyield, fitted(fit2), pch=20); abline(0,1)
  plot(fitdata$gdd, fitted(fit2), pch=20)
  plot(fitdata$prec, fitted(fit2), pch=20)
  cor.test(fitdata$logyield, fitted(fit2))$estimate^2
  summary(fit2)
  
  #testing for significance against less complex models
  anova(fit0a, fit1); anova(fit0b, fit1); anova(fit0c, fit1)
  anova(fit0a, fit2); anova(fit0b, fit2); anova(fit0c, fit2)
  anova(fit1,fit2)
  
  save(list=c("data_all","fit0a","fit0b","fit0c","fit1","fit2"),
       file=paste(out_dir,"/yield_models_",tolower(cropname),".RData",sep=""))
} else {
  load(file=paste(out_dir,"/yield_models_",tolower(cropname),".RData",sep=""))
}

### plots
if (tolower(cropname) == "maize") {finalfit <- fit2}
if (tolower(cropname) == "wheat") {finalfit <- fit1}
if (tolower(cropname) == "soybean") {finalfit <- fit2}
if (tolower(cropname) == "rice") {finalfit <- fit2}

for (rcp in c("RCP26","RCP45","RCP60","RCP85")) {
  #rcp <- "RCP26"
  trs <- rates_list[[rcp]]
  testdata <- fitdata
  testdata$prec_chg <- extract(trs, testdata[,c("x","y")]) * 0.01
  testdata$prec <- testdata$prec * (1 + testdata$prec_chg)
  testdata$logyield_fut <- predict(finalfit, testdata)
  testdata$yield_fut <- exp(testdata$logyield_fut)
  testdata$fitlogyield <- fitted(finalfit)
  testdata$fityield <- exp(testdata$fitlogyield)
  testdata <- testdata[complete.cases(testdata),]
  
  #yield response per grid cell
  yieldresp <- aggregate(testdata[,c("x","y","country","gdd","prec","tmean","yield_ton_ha","logyield","logyield_fut","yield_fut","fitlogyield","fityield")],
                         by=list(loc=testdata$cell_id),FUN=function(x) {mean(x,na.rm=T)})
  yieldresp$perch_yield <- (yieldresp$yield_fut - yieldresp$fityield) / yieldresp$fityield * 100
  yieldresp$absch_yield <- yieldresp$yield_fut - yieldresp$fityield
  
  #create rasters
  perch_yield <- absch_yield <- raster(trs)
  perch_yield[cellFromXY(trs, yieldresp[,c("x","y")])] <- yieldresp$perch_yield
  absch_yield[cellFromXY(trs, yieldresp[,c("x","y")])] <- yieldresp$absch_yield
  cat(range(perch_yield[],na.rm=T),"\n")
  summary(perch_yield[])
  mean(perch_yield[which(trs[] < 0)],na.rm=T)
  range(perch_yield[which(trs[] < 0)],na.rm=T)
  mean(perch_yield[which(trs[] > 0)],na.rm=T)
  range(perch_yield[which(trs[] > 0)],na.rm=T)
  plot(quantile(perch_yield[],probs=seq(0,1,by=0.01),na.rm=T),0:100,ty="l")
  #plot(perch_yield)
  
  #save file as a netcdf
  if (!file.exists(paste(out_dir,"/yield_change_at_ToE_",rcp,"_",tolower(cropname),".nc",sep=""))) {
    cat("creating NetCDF file \n")
    mv <- 1.e20 #missing value
    dim_lon <- ncdim_def("lon","degrees_east",seq(-179,180,len=360))
    dim_lat <- ncdim_def("lat","degrees_north",seq(-90,90,len=181))
    ncp <- ncvar_def("yield_change_per","%",list(dim_lon,dim_lat),mv,
                     longname=paste("Projected change in yield at ToE [%], ",rcp,sep=""),
                     compression=9)
    ncfg <- nc_create(paste(out_dir,"/yield_change_at_ToE_",rcp,"_",tolower(cropname),".nc",sep=""),list(ncp))
    ncatt_put(ncfg,varid=0,"title","Projected changes in yield [%] using a mixed effects linear model")
    ncatt_put(ncfg,varid=0,"comment1","Areas with missing data is either because of no signal emergence, or missing yield data")
    ncatt_put(ncfg,varid=0,"comment2","Yield data from Iizumi et al. (2013) Global Dataset of Historical Yields")
    ncatt_put(ncfg,varid=0,"comment3","file written by J. Ramirez-Villegas, Jan 2017")
    mapp <- matrix(nrow=360,ncol=181,data=perch_yield[],byrow=F)
    mapp[is.na(mapp)] <- mv
    ncvar_put(ncfg,ncp,mapp[,181:1])
    nc_close(ncfg)
  }
  
  #plot figure (relative change)
  zlim <- c(-10,10); zby <- 2
  perch_yield[which(perch_yield[] > 10)] <- 10
  perch_yield[which(perch_yield[] < -10)] <- -10
  tplot <- rs_levplot2(perch_yield,zn=NA,zx=NA,nb=NA,brks=seq(zlim[1],zlim[2],by=zby),scale="RdYlBu",col_i=NA,
                       col_f=NA,ncol=9,rev=T,leg=list(at=seq(zlim[1],zlim[2],by=zby),labels=paste(seq(zlim[1],zlim[2],by=zby))),
                       panel_names=c("change in yield [%]"))
  pdf(paste(out_dir,"/yield_response_prec_chg_",tolower(cropname),"_",rcp,"case_rel.pdf",sep=""), height=5,width=8,pointsize=16)
  print(tplot)
  dev.off()
}


