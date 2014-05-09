#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")

#load packages
library(rgdal); library(raster); library(maptools); library(dismo); data(wrld_simpl)
library(rasterVis)

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
figDir <- paste(bDir,"/paper_figures_v2/area_yield",sep="")
m3Dir <- "~/Leeds-work/datasets/yield_data/Monfreda2008/NetCDF"

#create raster of world
rs_wrld <- rasterize(wrld_simpl, raster(ncols=720, nrows=1440))
tab_wrld <- wrld_simpl@data

#loop crops
for (crop_name in c("maize","groundnut","rice","cassava","soybean")) {
  #crop_name <- "groundnut"
  setwd(m3Dir)
  ifil <- paste(crop_name,"_5min.nc.zip",sep="")
  ncfil <- gsub("\\.zip","",ifil)
  
  #extract data
  if (!file.exists(ncfil)) {system(paste("unzip ",ifil,sep=""))}
  
  #read data from netcdf files
  rs_ahar <- raster(ncfil, level=1)
  rs_yield <- raster(ncfil, level=2)
  
  #create data frame with everything
  xy_all <- as.data.frame(xyFromCell(rs_ahar,which(!is.na(rs_ahar[]))))
  xy_all$ahar <- extract(rs_ahar, xy_all[,c("x","y")])
  xy_all$yield <- extract(rs_yield, xy_all[,c("x","y")])
  xy_all <- xy_all[which(!is.na(xy_all$yield)),]
  xy_all <- xy_all[which(xy_all$ahar > 0),]
  
  #xy_plot <- xy_all[sample(x=(1:nrow(xy_all)), size=10000),]
  #plot(xy_plot$ahar, xy_plot$yield, pch=20, cex=0.75)
  
  #extract values of countries for points
  xy_all$ctry <- extract(rs_wrld, xy_all[,c("x","y")])
  
  #list of countries
  maize_list <- c("US","CN","BR","MX","IN","ZA","NG")
  groundnut_list <- c("US","CN","IN","AR","NE","SN","NG")
  rice_list <- c("IN","CN","TH","BD","UY","ID")
  soybean_list <- c("US","BR","IN","CN","AR","CA")
  cassava_list <- c("BR","TH","IN","NG","VN","CN")
  ctry_list <- get(paste(crop_name,"_list",sep=""))
  
  #loop countries
  i <- 1; cols <- c("black","red","green","yellow","orange","pink")
  for (ctry in ctry_list) {
    #ctry <- ctry_list[1]
    xy_ctry <- xy_all[which(xy_all$ctry == which(tab_wrld$ISO2 == ctry)),]
    xy_ctry$ahar_norm <- (xy_ctry$ahar - min(xy_ctry$ahar)) / (max(xy_ctry$ahar) - min(xy_ctry$ahar))
    xy_ctry$yield_norm <- (xy_ctry$yield - min(xy_ctry$yield)) / (max(xy_ctry$yield) - min(xy_ctry$yield))
    
    pdf(paste(figDir,"/",crop_name,"_",ctry,".pdf",sep=""), height=6,width=8,pointsize=16)
    par(mar=c(5,5,1,1),las=1,lwd=1.75)
    plot(xy_ctry$ahar_norm, xy_ctry$yield_norm, 
         pch=20, cex=0.5, xlim=c(0,1), ylim=c(0,1),
         xlab="Normalised area harvested", ylab="Normalised crop yield")
    text(0.9,0.1,labels=ctry)
    text(0.9,0.05,round(cor(xy_ctry$ahar_norm,xy_ctry$yield_norm),4))
    grid()
    dev.off()
    
    #convert fig into png
    setwd(figDir)
    system(paste("convert -verbose -density 300 ",crop_name,"_",ctry,".pdf -quality 100 -sharpen 0x1.0 -alpha off ",crop_name,"_",ctry,".png", sep=""))
    setwd(m3Dir)
    
    i <- i+1
  }
  
  #remove netcdf file after processing
  if (file.exists(ncfil)) {system(paste("rm -f ",ncfil,sep=""))}
}


# ahar_val <- sort(xy_plot$ahar) #sort(xy_all$ahar) #sort(xy_plot$ahar)
# plot(1:length(ahar_val), ahar_val, ty="l")
# abline(h=0.2)
# 
# #determine threshold by calculating rate of change from previous. then max rate of change
# chg_rate <- data.frame(ahar=ahar_val[2:length(ahar_val)])
# chg_rate$prev <- ahar_val[1:nrow(chg_rate)]
# chg_rate$chg <- (chg_rate$ahar - chg_rate$prev) / chg_rate$prev
# chg_rate$ahar[which(chg_rate$chg == max(chg_rate$chg)[1])]
# 
# xy_plot2 <- xy_all[which(xy_all$ahar >= 0.1),]
# plot(density(xy_plot2$yield),ylim=c(0,0.25))
# lines(density(xy_plot$yield),col="red")
# abline(v=mean(xy_plot2$yield),col="black") #niche
# abline(v=mean(xy_plot$yield),col="red") #all


