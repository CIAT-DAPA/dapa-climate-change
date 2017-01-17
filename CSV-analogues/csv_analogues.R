#JRV 2016 CSV analogues analysis
#CIAT / CCAFS

#load packages
library(raster); library(maptools); library(ncdf4); library(rasterVis)
data("wrld_simpl")

#working directory
wd <- "/nfs/workspace_cluster_9/ClimateAnalogues/Analisis_TeSACs_LAC"
fig_dir <- paste("~/CIAT-work/CSV-analogues/figures",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir, recursive=T)}

#analyses to be conducted
#1. global map of change in representativeness (fut_fut - his_his), for agricultural areas (both crops + livestock)
#2. graph of representativeness value and cumulative area covered for crop, for pasture, for fut_fut and for his_his
#3. scatter plot of similarity of site with its own and similarity between site and all others (backwards and forwards separate)

#load and prepare crop and pasture area
ext_glob <- extent(-180, 180, -60, 90)
carea_rs <- crop(raster(paste(wd,"/crop_pasture_area/Cropland2000_5min.nc", sep=""), level=1), ext_glob)
parea_rs <- crop(raster(paste(wd,"/crop_pasture_area/Pasture2000_5min.nc", sep=""), level=1), ext_glob)

#add them up
tarea_rs <- carea_rs + parea_rs

#compute physical area for total, crop and pasture
pharea_rs <- area(tarea_rs)
pharea_rs <- tarea_rs * pharea_rs
phcrop_rs <- area(carea_rs); phcrop_rs <- carea_rs * phcrop_rs
phpast_rs <- area(parea_rs); phpast_rs <- parea_rs * phpast_rs

#make binary raster out of total crop+pasture area
barea_rs <- tarea_rs; barea_rs[which(barea_rs[] != 0)] <- 1
barea_rs[which(barea_rs[] == 0)] <- NA

#load site database
sites <- read.csv(paste(wd,"/Result/sitios.csv",sep=""))
sp.sites <- data.frame(x=sites$Long, y=sites$Lat)
coordinates(sp.sites) <- ~ x + y
proj4string(sp.sites) <- paste(tarea_rs@crs)

#####
#1. global map of change in representativeness (fut_fut - his_his), for agricultural areas (both crops + livestock)
nonehh_rs <- raster(paste(wd,"/Result/none_max.tif",sep=""))
noneff_rs <- raster(paste(wd,"/Result/None_futuro_futuro_max.tif",sep=""))

#remove non crop or pasture areas
nonehh_rs <- nonehh_rs * barea_rs
noneff_rs <- noneff_rs * barea_rs

#compute change in similarity
chgsim_rs <- noneff_rs - nonehh_rs

#plot the three maps
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
    layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
  return(p)
}

#figure details
ht <- 6
fct <- (ext_glob@xmin-ext_glob@xmax)/(ext_glob@ymin-ext_glob@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=40), norths=seq(-90,90,by=40))

#map of change in similarity
tplot <- rs_levplot(chgsim_rs,zn=NA,zx=NA,nb=NA,brks=c(-.6,-.3,-.15,-.05,-.01,.01,.05,.15,.3,.6),scale="RdYlBu",col_i=NA,col_f=NA,ncol=11,rev=T,leg=T)
tplot <- tplot + layer(sp.points(sp.sites, pch=20, cex=0.5, col="blue"))
pdf(paste(fig_dir,"/fig_change_similarity.pdf",sep=""), height=4.75,width=9)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_change_similarity.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_change_similarity.png",sep=""))
setwd("~")

#map of his_his similarity
tplot <- rs_levplot(nonehh_rs,zn=0,zx=1,nb=5,brks=NA,scale="YlOrRd",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.points(sp.sites, pch=20, cex=0.5, col="blue"))
pdf(paste(fig_dir,"/fig_similarity_his_his.pdf",sep=""), height=4.75,width=9)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_similarity_his_his.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_similarity_his_his.png",sep=""))
setwd("~")

#map of fut_fut similarity
tplot <- rs_levplot(noneff_rs,zn=0,zx=1,nb=5,brks=NA,scale="YlOrRd",col_i=NA,col_f=NA,ncol=9,rev=T,leg=T)
tplot <- tplot + layer(sp.points(sp.sites, pch=20, cex=0.5, col="blue"))
pdf(paste(fig_dir,"/fig_similarity_fut_fut.pdf",sep=""), height=4.75,width=9)
print(tplot)
dev.off()
setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_similarity_fut_fut.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_similarity_fut_fut.png",sep=""))
setwd("~")


####
#2. graph of representativeness value and cumulative area covered for crop, for pasture, for fut_fut and for his_his
#   area covered vs. level of representativeness

#sequence of thresholds
rep_th <- rev(seq(0,1,by=0.01))
out_data <- data.frame()

for (th in rep_th) {
  #th <- rep_th[1]
  cat("threshold th=",th,"\n")
  
  #select grid cells that are below or equal to threshold
  cells_hh <- which(nonehh_rs[] >= th)
  cells_ff <- which(noneff_rs[] >= th)
  
  #compute area in [km^2] and in [%] for this threshold
  crop_hh <- sum(phcrop_rs[cells_hh])
  crop_hhr <- crop_hh / sum(phcrop_rs[],na.rm=T)
  
  crop_ff <- sum(phcrop_rs[cells_ff])
  crop_ffr <- crop_ff / sum(phcrop_rs[],na.rm=T)
  
  past_hh <- sum(phpast_rs[cells_hh])
  past_hhr <- past_hh / sum(phpast_rs[],na.rm=T)
  
  past_ff <- sum(phpast_rs[cells_ff])
  past_ffr <- past_ff / sum(phpast_rs[],na.rm=T)
  
  #create data frame
  out_row <- data.frame(threshold=th, crop_his_km2=crop_hh, crop_his_rel=crop_hhr, crop_fut_km2=crop_ff, 
                        crop_fut_rel=crop_ffr, past_his_km2=past_hh, past_his_rel=past_hhr, past_fut_km2=past_ff,
                        past_fut_rel=past_ffr)
  out_data <- rbind(out_data, out_row)
}

pdf(paste(fig_dir,"/fig_cumulative_area.pdf",sep=""), height=5,width=6)
par(mar=c(5,5,1,1), las=1)
plot(1:nrow(out_data), out_data$past_his_km2 / 10^6, ty="l", xlab="Similarity value [unitless]", 
     ylab=expression(paste("Cumulative area [", km^2, "]", " x ", 10^6)), axes=F, lwd=2,ylim=c(0,30))
axis(1, at=seq(0,100,by=10), labels=rev(seq(0,1,by=0.1)))
axis(2, at=seq(0,30,by=5), labels=seq(0,30,by=5))
box(lwd=1.5)
lines(1:nrow(out_data), out_data$crop_his_km2 / 10^6, col="red", lwd=2)
lines(1:nrow(out_data), out_data$crop_fut_km2 / 10^6, col="red", lty=2, lwd=2)
lines(1:nrow(out_data), out_data$past_fut_km2 / 10^6, col="black", lty=2, lwd=2)
grid()
legend(0.5,30,legend=c("Pasture","Crops"),lty=c(1,1),col=c("black","red"), bg="white", box.lwd=1, cex=0.8)
legend(0.5,25,legend=c("Historical","Future"),lty=c(1,2),col=c("black","black"), bg="white", box.lwd=1, cex=0.8)
dev.off()

setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_cumulative_area.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_cumulative_area.png",sep=""))
setwd("~")

####
#3. scatter plot of similarity of site with its own and similarity between site and all others (backwards and forwards separate)

df_allsites <- data.frame()
for (i in 1:nrow(sites)) {
  #i <- 1
  cat("processing site i=",i,"out of",nrow(sites),"\n")
  sitef_rs <- raster(paste(wd,"/Result/Forwards/",sites$Country[i],sites$identificador[i],"_forwards.tif",sep=""))
  siteb_rs <- raster(paste(wd,"/Result/Backwards/",sites$Country[i],sites$identificador[i],"_backwards.tif",sep=""))
  
  #extract distance from rasters
  distf <- extract(sitef_rs, cbind(x=sites$Long, y=sites$Lat))
  distf_i <- distf[i]
  distf_x <- max(distf[-i], na.rm=T)
  distf_n <- min(distf[-i], na.rm=T)
  distf_m <- mean(distf[-i], na.rm=T)
  
  distb <- extract(siteb_rs, cbind(x=sites$Long, y=sites$Lat))
  distb_i <- distb[i]
  distb_x <- max(distb[-i], na.rm=T)
  distb_n <- min(distb[-i], na.rm=T)
  distb_m <- mean(distb[-i], na.rm=T)
  
  out_row <- data.frame(site=sites$identificador[i], country=sites$Country[i], dist_fi=distf_i, dist_fmax=distf_x,
                        dist_fmin=distf_n, dist_fmean=distf_m, dist_bi=distb_i, dist_bmax=distb_x, dist_bmin=distb_n,
                        dist_bmean=distb_m)
  df_allsites <- rbind(df_allsites, out_row)
}

#assign region to countries
df_allsites$region <- NA
df_allsites$region[which(df_allsites$country == "Kenya")] <- "EAF"
df_allsites$region[which(df_allsites$country == "Uganda")] <- "EAF"
df_allsites$region[which(df_allsites$country == "Ethiopia")] <- "EAF"
df_allsites$region[which(df_allsites$country == "Tanzania")] <- "EAF"
df_allsites$region[which(df_allsites$country == "Burkina Faso")] <- "WAF"
df_allsites$region[which(df_allsites$country == "Ghana")] <- "WAF"
df_allsites$region[which(df_allsites$country == "Mali")] <- "WAF"
df_allsites$region[which(df_allsites$country == "Niger")] <- "WAF"
df_allsites$region[which(df_allsites$country == "Senegal")] <- "WAF"
df_allsites$region[which(df_allsites$country == "Bangladesh")] <- "SAS"
df_allsites$region[which(df_allsites$country == "India")] <- "SAS"
df_allsites$region[which(df_allsites$country == "Nepal")] <- "SAS"
df_allsites$region[which(df_allsites$country == "Vietnam")] <- "SEA"
df_allsites$region[which(df_allsites$country == "Lao PDR")] <- "SEA"
df_allsites$region[which(df_allsites$country == "Cambodia")] <- "SEA"
df_allsites$region[which(df_allsites$country == "Philippines")] <- "SEA"
df_allsites$region[which(df_allsites$country == "Colombia")] <- "LAM"
df_allsites$region[which(df_allsites$country == "Honduras")] <- "LAM"
df_allsites$region[which(df_allsites$country == "Guatemala")] <- "LAM"
df_allsites$region[which(df_allsites$country == "Nicaragua")] <- "LAM"

#produce figure
#forward
pdf(paste(fig_dir,"/fig_fwd_similarity_between_sites.pdf",sep=""), height=5,width=6)
par(mar=c(5,5,1,1), las=1)
plot(df_allsites$dist_fi, df_allsites$dist_fmax, xlim=c(0.35,0.9), ylim=c(0.35,0.9), pch=20, cex=0.5,
     xlab="Forward site self-similarity [unitless]", ylab="Maximum forward community similarity [unitless]")
points(df_allsites$dist_fi[which(df_allsites$region == "EAF")], df_allsites$dist_fmax[which(df_allsites$region == "EAF")],
       col="red", pch=20)
points(df_allsites$dist_fi[which(df_allsites$region == "WAF")], df_allsites$dist_fmax[which(df_allsites$region == "WAF")],
       col="blue", pch=20)
points(df_allsites$dist_fi[which(df_allsites$region == "SAS")], df_allsites$dist_fmax[which(df_allsites$region == "SAS")],
       col="orange", pch=20)
points(df_allsites$dist_fi[which(df_allsites$region == "SEA")], df_allsites$dist_fmax[which(df_allsites$region == "SEA")],
       col="grey 50", pch=20)
points(df_allsites$dist_fi[which(df_allsites$region == "LAM")], df_allsites$dist_fmax[which(df_allsites$region == "LAM")],
       col="black", pch=20)
abline(0,1)
grid()
legend(0.35,0.9,col=c("red","blue","orange","grey 50","black"),legend=c("EAF","WAF","SAS","SEA","LAM"),
       pch=20,bg="white",cex=1)
dev.off()

setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_fwd_similarity_between_sites.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_fwd_similarity_between_sites.png",sep=""))
setwd("~")

#backward
#produce figure
pdf(paste(fig_dir,"/fig_bwd_similarity_between_sites.pdf",sep=""), height=5,width=6)
par(mar=c(5,5,1,1), las=1)
plot(df_allsites$dist_bi, df_allsites$dist_bmax, xlim=c(0.35,0.9), ylim=c(0.35,0.9), pch=20, cex=0.5,
     xlab="Backward site self-similarity [unitless]", ylab="Maximum backward community similarity [unitless]")
points(df_allsites$dist_bi[which(df_allsites$region == "EAF")], df_allsites$dist_bmax[which(df_allsites$region == "EAF")],
       col="red", pch=20)
points(df_allsites$dist_bi[which(df_allsites$region == "WAF")], df_allsites$dist_bmax[which(df_allsites$region == "WAF")],
       col="blue", pch=20)
points(df_allsites$dist_bi[which(df_allsites$region == "SAS")], df_allsites$dist_bmax[which(df_allsites$region == "SAS")],
       col="orange", pch=20)
points(df_allsites$dist_bi[which(df_allsites$region == "SEA")], df_allsites$dist_bmax[which(df_allsites$region == "SEA")],
       col="grey 50", pch=20)
points(df_allsites$dist_bi[which(df_allsites$region == "LAM")], df_allsites$dist_bmax[which(df_allsites$region == "LAM")],
       col="black", pch=20)
abline(0,1)
grid()
legend(0.35,0.9,col=c("red","blue","orange","grey 50","black"),legend=c("EAF","WAF","SAS","SEA","LAM"),
       pch=20,bg="white",cex=1)
dev.off()

setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_bwd_similarity_between_sites.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_bwd_similarity_between_sites.png",sep=""))
setwd("~")

#write table of site-site similarity
write.csv(df_allsites, paste(fig_dir,"/site_site_similarity.csv",sep=""),row.names=F)


#######
####### extract wcl data for each site
wcl_dir <- "/nfs/data_cluster_4/observed/gridded_products/worldclim/Global_2_5min"
bio_stk <- stack(paste(wcl_dir,"/bio_",1:19,sep=""))
bio_data <- extract(bio_stk, cbind(x=sites$Long, y=sites$Lat))
bio_data <- cbind(sites, as.data.frame(bio_data))

#name regions
bio_data$region <- NA
bio_data$region[which(bio_data$Country == "Kenya")] <- "EAF"
bio_data$region[which(bio_data$Country == "Uganda")] <- "EAF"
bio_data$region[which(bio_data$Country == "Ethiopia")] <- "EAF"
bio_data$region[which(bio_data$Country == "Tanzania")] <- "EAF"
bio_data$region[which(bio_data$Country == "Burkina Faso")] <- "WAF"
bio_data$region[which(bio_data$Country == "Ghana")] <- "WAF"
bio_data$region[which(bio_data$Country == "Mali")] <- "WAF"
bio_data$region[which(bio_data$Country == "Niger")] <- "WAF"
bio_data$region[which(bio_data$Country == "Senegal")] <- "WAF"
bio_data$region[which(bio_data$Country == "Bangladesh")] <- "SAS"
bio_data$region[which(bio_data$Country == "India")] <- "SAS"
bio_data$region[which(bio_data$Country == "Nepal")] <- "SAS"
bio_data$region[which(bio_data$Country == "Vietnam")] <- "SEA"
bio_data$region[which(bio_data$Country == "Lao PDR")] <- "SEA"
bio_data$region[which(bio_data$Country == "Cambodia")] <- "SEA"
bio_data$region[which(bio_data$Country == "Philippines")] <- "SEA"
bio_data$region[which(bio_data$Country == "Colombia")] <- "LAM"
bio_data$region[which(bio_data$Country == "Honduras")] <- "LAM"
bio_data$region[which(bio_data$Country == "Guatemala")] <- "LAM"
bio_data$region[which(bio_data$Country == "Nicaragua")] <- "LAM"

#convert units
bio_data$bio_1 <- bio_data$bio_1 * 0.1
bio_data$bio_2 <- bio_data$bio_2 * 0.1
bio_data$bio_3 <- bio_data$bio_3 * 0.01
bio_data$bio_4 <- bio_data$bio_4 * 0.001
bio_data$bio_5 <- bio_data$bio_5 * 0.1
bio_data$bio_6 <- bio_data$bio_6 * 0.1
bio_data$bio_7 <- bio_data$bio_7 * 0.1
bio_data$bio_8 <- bio_data$bio_8 * 0.1
bio_data$bio_9 <- bio_data$bio_9 * 0.1
bio_data$bio_10 <- bio_data$bio_10 * 0.1
bio_data$bio_11 <- bio_data$bio_11 * 0.1

#produce figure
pdf(paste(fig_dir,"/fig_temp_vs_precip.pdf",sep=""), height=5,width=6)
par(mar=c(5,5,1,1), las=1)
plot(bio_data$bio_1, bio_data$bio_12, xlim=c(8.5, 30), ylim=c(400, 3500), pch=20, cex=0.5,
     xlab="Annual mean temperature [Celsius]", ylab="Total annual precipitation [mm/year]")
points(bio_data$bio_1[which(bio_data$region == "EAF")], bio_data$bio_12[which(bio_data$region == "EAF")],
       col="red", pch=20)
points(bio_data$bio_1[which(bio_data$region == "WAF")], bio_data$bio_12[which(bio_data$region == "WAF")],
       col="blue", pch=20)
points(bio_data$bio_1[which(bio_data$region == "SAS")], bio_data$bio_12[which(bio_data$region == "SAS")],
       col="orange", pch=20)
points(bio_data$bio_1[which(bio_data$region == "SEA")], bio_data$bio_12[which(bio_data$region == "SEA")],
       col="grey 50", pch=20)
points(bio_data$bio_1[which(bio_data$region == "LAM")], bio_data$bio_12[which(bio_data$region == "LAM")],
       col="black", pch=20)
grid()
legend(8.5,3500,col=c("red","blue","orange","grey 50","black"),legend=c("EAF","WAF","SAS","SEA","LAM"),
       pch=20,bg="white",cex=1)
dev.off()

setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_temp_vs_precip.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_temp_vs_precip.png",sep=""))
setwd("~")


#produce figure
pdf(paste(fig_dir,"/fig_dtr_vs_prCV.pdf",sep=""), height=5,width=6)
par(mar=c(5,5,1,1), las=1)
plot(bio_data$bio_2, bio_data$bio_15, xlim=c(6, 16), ylim=c(30, 150), pch=20, cex=0.5,
     xlab="Mean diurnal range [Celsius]", ylab="Precipitation C.V. [%]")
points(bio_data$bio_2[which(bio_data$region == "EAF")], bio_data$bio_15[which(bio_data$region == "EAF")],
       col="red", pch=20)
points(bio_data$bio_2[which(bio_data$region == "WAF")], bio_data$bio_15[which(bio_data$region == "WAF")],
       col="blue", pch=20)
points(bio_data$bio_2[which(bio_data$region == "SAS")], bio_data$bio_15[which(bio_data$region == "SAS")],
       col="orange", pch=20)
points(bio_data$bio_2[which(bio_data$region == "SEA")], bio_data$bio_15[which(bio_data$region == "SEA")],
       col="grey 50", pch=20)
points(bio_data$bio_2[which(bio_data$region == "LAM")], bio_data$bio_15[which(bio_data$region == "LAM")],
       col="black", pch=20)
grid()
legend(6,150,col=c("red","blue","orange","grey 50","black"),legend=c("EAF","WAF","SAS","SEA","LAM"),
       pch=20,bg="white",cex=1)
dev.off()

setwd(fig_dir)
system(paste("convert -verbose -density 300 fig_dtr_vs_prCV.pdf -quality 100 -sharpen 0x1.0 -alpha off fig_dtr_vs_prCV.png",sep=""))
setwd("~")

write.csv(bio_data, file=paste(fig_dir,"/bio_data.csv",sep=""), quote=T, row.names=F)

#save all objects
save.image(file=paste(fig_dir,"/csv_analogues.RData",sep=""))

#load(file=paste(fig_dir,"/csv_analogues.RData",sep=""))

