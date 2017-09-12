#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")
#############################################################################################
####### See wfd_wfdei_checks.R. I realised the annual precip totals were ok, so calculated
####### totals of precip for the Sacks et al. (2010) defined growing season.
#############################################################################################

#packages
library(raster); library(rgdal); library(ncdf); library(rasterVis); library(maptools)
data(wrld_simpl)

#input directories
wd <- "~/Leeds-work/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")
met_dir <- paste(wd,"/data/meteorology",sep="")
yi_dir <- paste(wd,"/data/yield_data_maize",sep="")

#load objects
load(paste(mdata_dir,"/initial_conditions_major.RData",sep=""))
load(paste(mdata_dir,"/yield_major.RData",sep=""))

#determine extent to cut the resampled netcdfs (of CRU and WorldClim)
yrs <- raster(paste(yi_dir,"/descriptive_stats/mean_ModelYld500.tif",sep=""))
bbox <- extent(yrs)

#define dataset, period, variable
dataset <- "WFDEI" #WFD, WFDEI
years <- 1982:2005 #1982:2001 for WFD, 1982:2005 for WFDEI

#/o directory for figs
fig_dir <- paste(wd,"/text/wfd_wfdei_checks/",tolower(dataset),sep="")

#/o dir
out_dir <- paste(met_dir,"/baseline_climate/Rainf_gseason_",dataset,"_GPCC",sep="")
if (!file.exists(out_dir)) {dir.create(out_dir)}

sow_plot_dir <- paste(out_dir,"/gridcell_sowdate_check",sep="")
if (!file.exists(sow_plot_dir)) {dir.create(sow_plot_dir)}

###
#function to return precip value for growing season of a particular year
calc_season_pr <- function(x, stk_y, yr, verification=F) {
#2. get various planting date values using the range and runif()
  #x <- as.numeric(in_data[466,])
  
  #setting values of variables
  loc <- x[1]; sow_i <- x[2]; sow_f <- x[3]; har_i <- x[4]; har_f <- x[5]; lon <- x[6]; lat <- x[7]
  yield <- x[8:length(x)]; yield <- yield[yr-1982+1]
  cat("loc=",loc,"\n")
  
  #if sow_i and sow_f, or har_i and har_f are in different years
  if (sow_i > sow_f) {sow_f <- sow_f + 365; cat("sow_i=",sow_i,"/ sow_f=",sow_f,"\n")}
  if (har_i > har_f) {har_f <- har_f + 365; cat("har_i=",har_i,"/ har_f",har_f,"\n")}
  
  #array of planting / harvesting dates
  #while loop to ensure periods is large enough
  #sow_dates <- round(rnorm(500, mean=mean(c(sow_i,har_i)), sd=(0.25*(sow_f-sow_i))), 0) #Toshi's method
  #har_dates <- round(rnorm(500, mean=mean(c(har_i,har_i)), sd=(0.25*(har_f-har_i))), 0) #Toshi's method
  periods <- data.frame(); i <- 0
  while (nrow(periods)<500) {
    sow_dates <- round(runif(1000+i*1000, sow_i, sow_f), 0) #double as needed, runif instead of rnorm
    har_dates <- round(runif(1000, har_i, har_f), 0) #double as needed, runif instead of rnorm
    periods <- data.frame(sow=sow_dates, har=har_dates)
    periods$dur <- apply(periods, 1, FUN=function(x) {a <- x[1]; b <- x[2]; if (a > b) {b <- b+365}; c <- b-a; return(c)})
    periods <- periods[which(periods$dur > 60),] #remove unrealistically low values
    periods <- periods[which(periods$dur < 180),] #remove unrealistically high values
    #if no values are in range 60-180 then it means sow/har dates are wrong, hence correct har
    if (nrow(periods) == 0) {har_i <- har_i+60; har_f <- har_f+180}
    i <- i+1
    #cat(i,"\t")
  }
  #cat("\n")
  periods <- periods[sample(1:nrow(periods),size=500),] #only need 500 values
  row.names(periods) <- 1:nrow(periods)
  
  #3. with planting date, calculate total precipitation for growing season
  #precip time series
  prvals <- as.numeric(extract(stk_y, cbind(x=lon,y=lat)))
  prvals <- data.frame(name=names(stk_y),value=prvals)
  prvals$value[which(prvals$value < 0)] <- 0
  prvals$year <- apply(prvals,1,FUN=function(x) {a <- x[1]; return(as.numeric(gsub("y.","",unlist(strsplit(a,"_",fixed=T))[1])))})
  prvals$doy <- apply(prvals,1,FUN=function(x) {a <- x[1]; return(as.numeric(gsub("day.","",unlist(strsplit(a,"_",fixed=T))[2])))})
  prvals$name <- NULL
  prvals$day_cont <- 1:nrow(prvals)
  
  if (verification) {
    if (!file.exists(paste(sow_plot_dir,"/loc-",loc,"_",yr,".pdf",sep=""))) {
      pdf(paste(sow_plot_dir,"/loc-",loc,"_",yr,".pdf",sep=""),height=3.5,width=10,pointsize=14)
      par(mar=c(5,5,1,1),las=1,lwd=1.25)
      plot(prvals$day_cont,prvals$value,ty="l",xlab="day count",ylab="precipitation (mm/day)")
      abline(v=365,col="red"); abline(v=365*2,col="red")
      abline(v=sow_i,col="blue"); abline(v=sow_f,col="blue")
      abline(v=mean(periods$sow),col="blue",lty=2)
      abline(v=har_i,col="green"); abline(v=har_f,col="green")
      abline(v=mean(periods$har),col="green",lty=2)
      dev.off()
    }
    outval <- NA
  }
  
  if (!verification) {
    #calculate prec total for each of the 500 periods
    get_prval <- function(z, prvals, yr) {
      sow_p <- z[1]; har_p <- z[2]
      #cat(sow_p,har_p,"\n")
      #if sow > har it means crop was harvested this year, but sown in previous (relatively late, e.g. in Dec)
      if (sow_p > har_p) {prv <- sum(prvals$value[which(prvals$day_cont >= sow_p & prvals$day_cont <= (har_p+365))])}
      
      #if sow < har AND har is below 365 it means crop was sown and harvested in the same year
      if (sow_p < har_p & har_p <= 365) {prv <- sum(prvals$value[which(prvals$year == yr & prvals$doy >= sow_p & prvals$doy <= har_p)])}
      
      #if sow < har AND har is above 365 it means crop was harvested this year but sown not too late in previous (e.g. in Jul/Aug.)
      if (sow_p < har_p & har_p > 365) {prv <- sum(prvals$value[which(prvals$day_cont >= sow_p & prvals$day_cont <= har_p)])}
      return(prv)
    }
    pr_periods <- as.numeric(apply(periods, 1, get_prval, prvals, yr))
    
    #4. take mean
    pr_mean <- mean(pr_periods)
    
    #5. calculate ratio to this year's yield
    yratio <- yield / (pr_mean * 10)
    
    #return vector
    outval <- c(pr_mean,yratio)
  }
  
  #return both things
  return(outval)
}

####
#### Note: i noticed that the sowing dates were dodgy in places. This was caused by 1.125 grid cell
####       means being calculated from grid cells with dates at the end and beginning of year
####       simultaneously. After a few tries for generalising i noticed it was rather difficult
####       hence i decided to plot every grid cell and apply a manual correction on the sowing
####       and harvesting dates as needed.
####

####
#verification run with some particular year (e.g. 1982)
yr <- 1982

#1. load this year's and previous year's data
stk_y1 <- c(); stk_y2 <- c()
for (m in 1:12) {
  #m <- 1
  cat("...loading m=",m,"\n")
  ifil1 <- paste(met_dir,"/baseline_climate/Rainf_daily_",dataset,"_GPCC/afr_Rainf_daily_",dataset,"_GPCC_",(yr-1),sprintf("%1$02d",m),".nc",sep="")
  ifil2 <- paste(met_dir,"/baseline_climate/Rainf_daily_",dataset,"_GPCC/afr_Rainf_daily_",dataset,"_GPCC_",yr,sprintf("%1$02d",m),".nc",sep="")
  stk_m1 <- stack(ifil1); stk_m2 <- stack(ifil2)
  stk_y1 <- c(stk_y1,stk_m1); stk_y2 <- c(stk_y2,stk_m2)
}
#building stacks
stk_y1 <- stack(stk_y1); stk_y2 <- stack(stk_y2)

#naming rasters in stacks
names(stk_y1) <- paste("y.",yr-1,"_day.",1:nlayers(stk_y1),sep="")
names(stk_y2) <- paste("y.",yr,"_day.",1:nlayers(stk_y2),sep="")

#single stack of both stacks
stk_y <- stack(stk_y1,stk_y2)

#remove grid cells where no yield data exists
true_cells <- cellFromXY(stk_y,xy_main_yield[,c("x","y")])
stk_y[!(1:ncell(stk_y)) %in% true_cells] <- NA

#calculate precip in mm
stk_y <- stk_y * 3600 * 24 #kg m-2 s-1 to mm/day

#verify
in_data <- cbind(xy_main[,c("LOC","SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")],xy_main_yield)
row.names(in_data) <- 1:nrow(in_data)
xx <- apply(in_data, 1, FUN=calc_season_pr, stk_y, yr, verification=T)

#loop years
for (yr in years) {
  #yr <- years[1]
  cat("...processing year",yr,"\n")
  
  if (!file.exists(paste(out_dir,"/afr_Rainf_gseason_",yr,"_yield_ratio.tif",sep=""))) {
    #1. load this year's and previous year's data
    stk_y1 <- c(); stk_y2 <- c()
    for (m in 1:12) {
      #m <- 1
      cat("...loading m=",m,"\n")
      ifil1 <- paste(met_dir,"/baseline_climate/Rainf_daily_",dataset,"_GPCC/afr_Rainf_daily_",dataset,"_GPCC_",(yr-1),sprintf("%1$02d",m),".nc",sep="")
      ifil2 <- paste(met_dir,"/baseline_climate/Rainf_daily_",dataset,"_GPCC/afr_Rainf_daily_",dataset,"_GPCC_",yr,sprintf("%1$02d",m),".nc",sep="")
      stk_m1 <- stack(ifil1); stk_m2 <- stack(ifil2)
      stk_y1 <- c(stk_y1,stk_m1); stk_y2 <- c(stk_y2,stk_m2)
    }
    #building stacks
    stk_y1 <- stack(stk_y1); stk_y2 <- stack(stk_y2)
    
    #naming rasters in stacks
    names(stk_y1) <- paste("y.",yr-1,"_day.",1:nlayers(stk_y1),sep="")
    names(stk_y2) <- paste("y.",yr,"_day.",1:nlayers(stk_y2),sep="")
    
    #single stack of both stacks
    stk_y <- stack(stk_y1,stk_y2)
    
    #remove grid cells where no yield data exists
    true_cells <- cellFromXY(stk_y,xy_main_yield[,c("x","y")])
    stk_y[!(1:ncell(stk_y)) %in% true_cells] <- NA
    
    #calculate precip in mm
    stk_y <- stk_y * 3600 * 24 #kg m-2 s-1 to mm/day
    
    #calculate gs precip
    in_data <- cbind(xy_main[,c("LOC","SOW_DATE1","SOW_DATE2","HAR_DATE1","HAR_DATE2")],xy_main_yield)
    row.names(in_data) <- 1:nrow(in_data)
    xx <- apply(in_data, 1, FUN=calc_season_pr, stk_y, yr, verification=F)
    
    #output growing season precip and ratio rasters
    pr_rs <- raster(stk_y); pr_rs[true_cells] <- xx[1,]
    yratio_rs <- raster(stk_y); yratio_rs[true_cells] <- xx[2,]
    
    #write rasters
    pr_rs <- writeRaster(pr_rs, paste(out_dir,"/afr_Rainf_gseason_",yr,"_total_rain.tif",sep=""),format="GTiff")
    yratio_rs <- writeRaster(yratio_rs, paste(out_dir,"/afr_Rainf_gseason_",yr,"_yield_ratio.tif",sep=""),format="GTiff")
  }
}

# rs_sow1 <- raster(stk_y); rs_sow1[cellFromXY(rs_sow1,xy_main[,c("x","y")])] <- xy_main$SOW_DATE1
# rs_har1 <- raster(stk_y); rs_har1[cellFromXY(rs_har1,xy_main[,c("x","y")])] <- xy_main$HAR_DATE1

################################################################################
################################################################################
###
#plot maps of seasonal precip and ratio yield/precip
###

### functions
#functions
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
  }
  if (rev) {pal <- rev(pal)}
  
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  
  #set theme
  this_theme <- custom.theme(fill = pal,region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin)) + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}

#figure details
ht <- 6
rs <- yrs
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=15), norths=seq(-90,90,by=15))


#load all years precipitation and yield ratio
pr_rs <- stack(paste(out_dir,"/afr_Rainf_gseason_",years,"_total_rain.tif",sep=""))
yratio_rs <- stack(paste(out_dir,"/afr_Rainf_gseason_",years,"_yield_ratio.tif",sep=""))

#calculate mean, sd and c.v. and plot
pr_mean <- calc(pr_rs, fun=function(x) {mean(x,na.rm=T)})
pr_sd <- calc(pr_rs, fun=function(x) {sd(x,na.rm=T)})
pr_cv <- pr_sd / pr_mean * 100

yratio_mean <- calc(yratio_rs, fun=function(x) {mean(x,na.rm=T)})
yratio_sd <- calc(yratio_rs, fun=function(x) {sd(x,na.rm=T)})
yratio_cv <- yratio_sd / yratio_mean * 100

#plotting precip
trs <- pr_mean
tplot <- rs_levplot2(trs,zn=0,zx=2600,nb=13,brks=NA,scale="Spectral",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/gseason_total_rain_mean_Rainf_",dataset,".pdf",sep=""), height=3.5,width=4.5,pointsize=14)
print(tplot)
dev.off()

trs <- pr_sd
tplot <- rs_levplot2(trs,zn=0,zx=1500,nb=13,brks=NA,scale="Spectral",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/gseason_total_rain_sd_Rainf_",dataset,".pdf",sep=""), height=3.5,width=4.5,pointsize=14)
print(tplot)
dev.off()

trs <- pr_cv; trs[which(trs[]>100)] <- 100
tplot <- rs_levplot2(trs,zn=0,zx=100,nb=10,brks=NA,scale="Blues",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/gseason_total_rain_cv_Rainf_",dataset,".pdf",sep=""), height=3.5,width=4.5,pointsize=14)
print(tplot)
dev.off()

#plotting yield ratio
trs <- yratio_mean
tplot <- rs_levplot2(trs,zn=0,zx=2.5,nb=25,brks=NA,scale="YlGnBu",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/gseason_yield_ratio_mean_Rainf_",dataset,".pdf",sep=""), height=3.5,width=4.5,pointsize=14)
print(tplot)
dev.off()

trs <- yratio_sd
tplot <- rs_levplot2(trs,zn=0,zx=2.5,nb=10,brks=NA,scale="Spectral",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/gseason_yield_ratio_sd_Rainf_",dataset,".pdf",sep=""), height=3.5,width=4.5,pointsize=14)
print(tplot)
dev.off()

trs <- yratio_cv; trs[which(trs[]>100)] <- 100
tplot <- rs_levplot2(trs,zn=0,zx=100,nb=10,brks=NA,scale="Blues",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/gseason_yield_ratio_cv_Rainf_",dataset,".pdf",sep=""), height=3.5,width=4.5,pointsize=14)
print(tplot)
dev.off()

