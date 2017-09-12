#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015
stop("!")

#baseline and future yields, yield s.d., and yield c.v.

#load libraries
library(sp); library(maptools); library(raster); library(rgeos); library(rasterVis)
data(wrld_simpl)
#library(ggplot2); library(grid); library(gridExtra)

#source code directory
src.dir <- "~/Repositories/dapa-climate-change/rice-future-tpe"

#directories
#wd <- "/nfs/a101/earjr/rice-future-tpe"
wd <- "~/Leeds-work/rice-future-tpe"
res_dir <- paste(wd,"/oryza_output",sep="")
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")
fig_dir <- paste(wd,"/figures",sep="")
an_dir <- paste(res_dir,"/analysis",sep="")

#load functions
source(paste(src.dir,"/thiessen_polygons.R",sep=""))

#load Brazil shapefile
bra_shp <- readRDS(paste(wd,"/data/BRA_adm1.rds",sep=""))
bra_shp <- bra_shp[which(bra_shp$ID_1 == 7 | bra_shp$ID_1 == 9 | bra_shp$ID_1 == 12 | bra_shp$ID_1 == 22 | bra_shp$ID_1 == 27),]

#construct list of final weather stations
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))
loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_dir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]
loc_list <- loc_list[which(loc_list$id != ".IPGO.00007"),]
row.names(loc_list) <- NULL

#create thiessen polygons from weather stations locations
wst_xy <- loc_list[,c("lon","lat")]; names(wst_xy) <- c("x","y")
thiepol <- voronoipolygons(wst_xy)
proj4string(thiepol)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
f_thiepol <- gIntersection(thiepol, bra_shp, byid=T)
id_list <- data.frame(ID=sapply(slot(f_thiepol,'polygons'),function(x) {slot(x,'ID')}))
id_list$WST_ID <- sapply(id_list$ID, function(x) {as.numeric(unlist(strsplit(paste(x)," ",fixed=T))[1])})
id_list$OBJECTID <- sapply(id_list$ID, function(x) {as.numeric(unlist(strsplit(paste(x)," ",fixed=T))[2])})
wst_xy$WST_ID <- row.names(wst_xy)
id_list <- merge(id_list, wst_xy, by="WST_ID")
names(id_list)[4:5] <- c("x_wst","y_wst")
row.names(id_list) <- id_list$ID; id_list <- id_list[,c("ID","WST_ID","OBJECTID","x_wst","y_wst")]
names(id_list)[1] <- "id"
fpols <- SpatialPolygonsDataFrame(f_thiepol, id_list, match.ID=T)

#reference raster
rs_ref <- raster(paste(an_dir,"/rs_reference.tif",sep=""))

#lists of factors
bclist <- c("cf","del")
gcmlist <- list.files(paste(gcm_dir,"/loc_CNPAF1/gcm",sep=""))
gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]
rcplist <- c("rcp26","rcp45","rcp60","rcp85")
co2list <- c("High","Low")

#load historical raster data
load(file=paste(an_dir,"/yield_historical_raster_data.RData",sep=""))
rm(yield_rs)

#######
#plotting function
rs_levplot <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T,this_colours=NA) {
  #dummy check of raster
  rsdum <- raster(rsin); rsdum[] <- rsin[]; rsin <- rsdum; rm(rsdum)
  
  #display.brewer.all() #for colours
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
    if (!is.na(this_colours[1])) {pal <- this_colours}
  } else {
    pal <- colorRampPalette(c(col_i,col_f))(ncol)
    if (!is.na(this_colours[1])) {pal <- this_colours}
  }
  if (rev) {pal <- rev(pal)}
  
  if (is.na(brks[1])) {brks <- do.breaks(c(zn,zx),nb)}
  
  #set theme
  this_theme <- custom.theme(fill = pal, region = pal,
                             bg = "white", fg = "grey20", pch = 14)
  
  p <- rasterVis:::levelplot(rsin, margin=F, par.settings = this_theme, colorkey=leg,
                             at = brks, maxpixels=ncell(rsin)) + 
    layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
    layer(sp.polygons(fpols,lwd=0.8,col="black"))
  return(p)
}

#figure details
ht <- 6
fct <- (rs_ref@extent@xmin-rs_ref@extent@xmax)/(rs_ref@extent@ymin-rs_ref@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=10), norths=seq(-90,90,by=15))

#make figure for historical (yield mean)
tplot <- rs_levplot(rs_ymean,zn=500,zx=4500,nb=4,brks=NA,scale="GnBu",col_i=NA,col_f=NA,
                     ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/fig_3_yield_mean_historical.pdf",sep=""), height=7,width=10)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_3_yield_mean_historical.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_yield_mean_historical.png",sep=""))
setwd("~")

#make figure for historical (yield std)
tplot <- rs_levplot(rs_ystd,zn=0,zx=2000,nb=4,brks=NA,scale="YlOrBr",col_i=NA,col_f=NA,
                    ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/fig_3_yield_std_historical.pdf",sep=""), height=7,width=10)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_3_yield_std_historical.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_yield_std_historical.png",sep=""))
setwd("~")

#make figure for historical (yield c.v.)
tplot <- rs_levplot(rs_ycv,zn=0,zx=100,nb=5,brks=NA,scale="YlOrBr",col_i=NA,col_f=NA,
                    ncol=9,rev=T,leg=T)
pdf(paste(fig_dir,"/fig_3_yield_cv_historical.pdf",sep=""), height=7,width=10)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_3_yield_cv_historical.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_yield_cv_historical.png",sep=""))
setwd("~")

#logicals for processing
calc_all_ensemble <- T
calc_per_co2 <- T

#compute change in mean yield for each projection
#loop to get output as rasters
for (rcp in rcplist) {
  #rcp <- rcplist[1]
  cat("...processing rcp=",rcp,"\n")
  
  #load data
  load(file=paste(an_dir,"/yield_mean_rasters_",rcp,".RData",sep=""))
  load(file=paste(an_dir,"/yield_std_rasters_",rcp,".RData",sep=""))
  load(file=paste(an_dir,"/yield_cv_rasters_",rcp,".RData",sep=""))
  
  chg_ym <- chg_ysd <- chg_ycv <- list()
  #calculate change with respect to historical
  for (i in 1:length(ymean_list)) {
    #i <- 1
    chg_ym[[i]] <- (ymean_list[[i]] - rs_ymean) #/ rs_ymean * 100
    chg_ysd[[i]] <- (ystd_list[[i]] - rs_ystd) #/ rs_ystd * 100
    chg_ycv[[i]] <- ycv_list[[i]] - rs_ycv
  }
  
  #create stacks
  chg_ym <- stack(chg_ym); chg_ysd <- stack(chg_ysd); chg_ycv <- stack(chg_ycv)
  
  #function to print everything
  printall_fun <- function(suffix="") {
    #now produce plots for future scenarios
    #mean yield change
    chg_ym1[which(chg_ym1[] > 600)] <- 600
    chg_ym1[which(chg_ym1[] < -600)] <- -600
    tplot <- rs_levplot(chg_ym1,zn=-600,zx=600,nb=6,brks=NA,scale="RdYlBu",col_i=NA,col_f=NA,
                        ncol=11,rev=T,leg=T)
    pdf(paste(fig_dir,"/fig_3_future_yield_chg_",suffix,rcp,".pdf",sep=""), height=7,width=10)
    print(tplot)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_3_future_yield_chg_",suffix,rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_future_yield_chg_",suffix,rcp,".png",sep=""))
    setwd("~")
    
    #mean yield change agreement (%)
    tplot <- rs_levplot(chg_ym2,zn=0,zx=100,nb=5,brks=NA,scale="Greens",col_i=NA,col_f=NA,
                        ncol=9,rev=T,leg=T)
    pdf(paste(fig_dir,"/fig_3_future_yield_unc_",suffix,rcp,".pdf",sep=""), height=7,width=10)
    print(tplot)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_3_future_yield_unc_",suffix,rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_future_yield_unc_",suffix,rcp,".png",sep=""))
    setwd("~")
    
    #s.d. yield change
    chg_sd1[which(chg_sd1[] > 500)] <- 500
    chg_sd1[which(chg_sd1[] < -500)] <- -500
    tplot <- rs_levplot(chg_sd1,zn=-500,zx=500,nb=10,brks=NA,scale="RdYlBu",col_i=NA,col_f=NA,
                        ncol=11,rev=T,leg=T)
    pdf(paste(fig_dir,"/fig_3_future_ystd_chg_",suffix,rcp,".pdf",sep=""), height=7,width=10)
    print(tplot)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_3_future_ystd_chg_",suffix,rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_future_ystd_chg_",suffix,rcp,".png",sep=""))
    setwd("~")
    
    #mean s.d. change agreement (%)
    tplot <- rs_levplot(chg_sd2,zn=0,zx=100,nb=5,brks=NA,scale="Greens",col_i=NA,col_f=NA,
                        ncol=9,rev=T,leg=T)
    pdf(paste(fig_dir,"/fig_3_future_ystd_unc_",suffix,rcp,".pdf",sep=""), height=7,width=10)
    print(tplot)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_3_future_ystd_unc_",suffix,rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_future_ystd_unc_",suffix,rcp,".png",sep=""))
    setwd("~")
    
    #mean c.v. change
    chg_cv1[which(chg_cv1[] > 30)] <- 30
    chg_cv1[which(chg_cv1[] < -30)] <- -30
    tplot <- rs_levplot(chg_cv1,zn=-30,zx=30,nb=6,brks=NA,scale="RdYlBu",col_i=NA,col_f=NA,
                        ncol=11,rev=T,leg=T)
    pdf(paste(fig_dir,"/fig_3_future_ycv_chg_",suffix,rcp,".pdf",sep=""), height=7,width=10)
    print(tplot)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_3_future_ycv_chg_",suffix,rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_future_ycv_chg_",suffix,rcp,".png",sep=""))
    setwd("~")
    
    #mean yield change agreement (%)
    tplot <- rs_levplot(chg_cv2,zn=0,zx=100,nb=5,brks=NA,scale="Greens",col_i=NA,col_f=NA,
                        ncol=9,rev=T,leg=T)
    pdf(paste(fig_dir,"/fig_3_future_ycv_unc_",suffix,rcp,".pdf",sep=""), height=7,width=10)
    print(tplot)
    dev.off()
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 fig_3_future_ycv_unc_",suffix,rcp,".pdf -quality 100 -sharpen 0x1.0 -alpha off fig_3_future_ycv_unc_",suffix,rcp,".png",sep=""))
    setwd("~")
  }
  
  #calculate ensemble means and agreement
  if (calc_all_ensemble) {
    chg_ym1 <- calc(chg_ym, fun=function(x) {median(x,na.rm=T)})
    chg_ym2 <- calc(chg_ym, fun=function(x) {xmean <- median(x, na.rm=T); tval <- length(which(sign(x) == sign(xmean))) / length(x) * 100; return(tval)})
    chg_sd1 <- calc(chg_ysd, fun=function(x) {median(x,na.rm=T)})
    chg_sd2 <- calc(chg_ysd, fun=function(x) {xmean <- median(x, na.rm=T); tval <- length(which(sign(x) == sign(xmean))) / length(x) * 100; return(tval)})
    chg_cv1 <- calc(chg_ycv, fun=function(x) {median(x,na.rm=T)})
    chg_cv2 <- calc(chg_ycv, fun=function(x) {xmean <- median(x, na.rm=T); tval <- length(which(sign(x) == sign(xmean))) / length(x) * 100; return(tval)})
    printall_fun()
  }
  
  #calculate per co2 level
  if (calc_per_co2) {
    for (co2p in co2list) {
      cat("...plotting co2=",co2p,"\n")
      #co2p <- co2list[1]
      chg_ym1 <- calc(chg_ym[[grep(co2p,names(ymean_list))]], fun=function(x) {median(x,na.rm=T)})
      chg_ym2 <- calc(chg_ym[[grep(co2p,names(ymean_list))]], fun=function(x) {xmean <- median(x, na.rm=T); tval <- length(which(sign(x) == sign(xmean))) / length(x) * 100; return(tval)})
      chg_sd1 <- calc(chg_ysd[[grep(co2p,names(ymean_list))]], fun=function(x) {median(x,na.rm=T)})
      chg_sd2 <- calc(chg_ysd[[grep(co2p,names(ymean_list))]], fun=function(x) {xmean <- median(x, na.rm=T); tval <- length(which(sign(x) == sign(xmean))) / length(x) * 100; return(tval)})
      chg_cv1 <- calc(chg_ycv[[grep(co2p,names(ymean_list))]], fun=function(x) {median(x,na.rm=T)})
      chg_cv2 <- calc(chg_ycv[[grep(co2p,names(ymean_list))]], fun=function(x) {xmean <- median(x, na.rm=T); tval <- length(which(sign(x) == sign(xmean))) / length(x) * 100; return(tval)})
      printall_fun(suffix=paste(co2p,"_",sep=""))
    }
  }
}


