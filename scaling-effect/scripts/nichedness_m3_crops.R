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

######################################################################################
### scatter and box plots for selected crops and countries

#create raster of world
rs_wrld <- rasterize(wrld_simpl, raster(ncols=720, nrows=1440))
tab_wrld <- wrld_simpl@data

#loop crops
for (crop_name in c("maize","groundnut","rice","cassava","soybean")) {
  #crop_name <- "maize"
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
  i <- 1
  for (ctry in ctry_list) {
    #ctry <- ctry_list[1]
    xy_ctry <- xy_all[which(xy_all$ctry == which(tab_wrld$ISO2 == ctry)),]
    xy_ctry$ahar_norm <- (xy_ctry$ahar - min(xy_ctry$ahar)) / (max(xy_ctry$ahar) - min(xy_ctry$ahar))
    xy_ctry$yield_norm <- (xy_ctry$yield - min(xy_ctry$yield)) / (max(xy_ctry$yield) - min(xy_ctry$yield))
    
    #assign classes for boxplot
    xy_ctry$class <- NA; xy_ctry$class_lab <- NA
    xy_ctry$class[which(xy_ctry$ahar_norm >= 0 & xy_ctry$ahar_norm <= 0.025)] <- 1
    xy_ctry$class_lab[which(xy_ctry$ahar_norm >= 0 & xy_ctry$ahar_norm <= 0.025)] <- "0.00 - 0.025"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.025 & xy_ctry$ahar_norm <= 0.05)] <- 2
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.025 & xy_ctry$ahar_norm <= 0.05)] <- "0.025 - 0.05"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.05 & xy_ctry$ahar_norm <= 0.1)] <- 3
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.05 & xy_ctry$ahar_norm <= 0.1)] <- "0.05 - 0.10"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.1 & xy_ctry$ahar_norm <= 0.2)] <- 4
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.1 & xy_ctry$ahar_norm <= 0.2)] <- "0.10 - 0.20"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.2 & xy_ctry$ahar_norm <= 0.3)] <- 5
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.2 & xy_ctry$ahar_norm <= 0.3)] <- "0.20 - 0.30"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.3 & xy_ctry$ahar_norm <= 0.4)] <- 6
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.3 & xy_ctry$ahar_norm <= 0.4)] <- "0.30 - 0.40"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.4 & xy_ctry$ahar_norm <= 0.5)] <- 7
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.4 & xy_ctry$ahar_norm <= 0.5)] <- "0.40 - 0.50"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.5 & xy_ctry$ahar_norm <= 0.75)] <- 8
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.5 & xy_ctry$ahar_norm <= 0.75)] <- "0.50 - 0.75"
    xy_ctry$class[which(xy_ctry$ahar_norm > 0.75 & xy_ctry$ahar_norm <= 1.0)] <- 9
    xy_ctry$class_lab[which(xy_ctry$ahar_norm > 0.75 & xy_ctry$ahar_norm <= 1.0)] <- "0.75 - 1.00"
    
    xy_ctry$class <- factor(xy_ctry$class, levels=c(1:9), 
                            labels=c("0.00 - 0.025","0.025 - 0.05","0.05 - 0.10","0.10 - 0.20",
                                     "0.20 - 0.30","0.30 - 0.40","0.40 - 0.50","0.50 - 0.75",
                                     "0.75 - 1.00"))
    
    #boxplot
    pdf(paste(figDir,"/boxplot_",crop_name,"_",ctry,".pdf",sep=""), height=6,width=8,pointsize=16)
    par(mar=c(6,5,1,1),las=2,lwd=1.75)
    boxplot(xy_ctry$yield_norm ~ xy_ctry$class, pch=20, cex=0.5,
            xlab=NA, ylab="Normalised crop yield",
            ylim=c(-0.1,1), xlim=c(0.5,9.5), 
            #names=unique(xy_ctry$class_lab),
            outcol="red",medcol="red",boxcol="blue",col="white",border="black")
    grid()
    boxplot(xy_ctry$yield_norm ~ xy_ctry$class, pch=20, cex=0.75,add=T, axes=F, 
            outcol="red",medcol="red",boxcol="blue",col="white",border="black")
    for (j in unique(xy_ctry$class)) {text((which(unique(xy_ctry$class) %in% j)),-0.06,labels=paste("n=",length(which(paste(xy_ctry$class) == paste(j))),sep=""),cex=0.7,col="grey 30")}
    dev.off()
    
    #scatterplot
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
    system(paste("convert -verbose -density 300 boxplot_",crop_name,"_",ctry,".pdf -quality 100 -sharpen 0x1.0 -alpha off boxplot_",crop_name,"_",ctry,".png", sep=""))
    setwd(m3Dir)
    
    i <- i+1
  }
  
  #remove netcdf file after processing
  if (file.exists(ncfil)) {system(paste("rm -f ",ncfil,sep=""))}
}


######################################################################################
### scatter and box plots for selected crops and countries

#create raster of world using a given crop
setwd(m3Dir)
ifil <- "maize_5min.nc.zip"
ncfil <- gsub("\\.zip","",ifil)
if (!file.exists(ncfil)) {system(paste("unzip ",ifil,sep=""))}
rs_wrld <- rasterize(wrld_simpl, raster(ncfil, level=1))
tab_wrld <- wrld_simpl@data
if (file.exists(ncfil)) {system(paste("rm -f ",ncfil,sep=""))}

#loop crops
for (crop_name in c("maize","groundnut","rice","soybean")) {
  #crop_name <- "maize"
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
  
  #extract values of countries for points
  xy_all$ctry <- extract(rs_wrld, xy_all[,c("x","y")])
  
  if (!file.exists(paste(figDir,"/nichedness_",crop_name,"_cut_v2.tif",sep=""))) {
    #raster for nichedness of crop
    rs_crop <- raster(rs_wrld)
    
    #loop countries to calculate differences
    for (i in 1:nrow(tab_wrld)) {
      #i <- 6
      cat("...processing country",paste(tab_wrld$NAME[i]),"\n")
      xy_ctry <- xy_all[which(xy_all$ctry == i),]
      
      if (nrow(xy_ctry) >= 100) {
        xy_ctry$ahar_norm <- (xy_ctry$ahar - min(xy_ctry$ahar)) / (max(xy_ctry$ahar) - min(xy_ctry$ahar))
        xy_ctry$yield_norm <- (xy_ctry$yield - min(xy_ctry$yield)) / (max(xy_ctry$yield) - min(xy_ctry$yield))
        perc <- as.numeric(quantile(xy_ctry$ahar_norm, probs=c(0.1, 0.9))) #percentiles
        yield_low <- mean(xy_ctry$yield_norm[which(xy_ctry$ahar_norm <= perc[1])]) #low area category
        yield_hig <- mean(xy_ctry$yield_norm[which(xy_ctry$ahar_norm >= perc[2])]) #high area category
        frac_diff <- (yield_hig - yield_low) / yield_low * 100 #difference between both
        rs_crop[which(rs_wrld[] == i)] <- frac_diff #put values into raster
      }
    }
    
    #remove NA grid cells in country raster
    rs_crop2 <- rs_crop
    true_cells <- cellFromXY(rs_crop, xy_all[,c("x","y")])
    rs_crop2[!(1:ncell(rs_crop2)) %in% true_cells] <- NA
    
    #write nichedness raster
    writeRaster(rs_crop, paste(figDir,"/nichedness_",crop_name,"_raw_v2.tif",sep=""),format="GTiff")
    writeRaster(rs_crop2, paste(figDir,"/nichedness_",crop_name,"_cut_v2.tif",sep=""),format="GTiff")
  }
  
  #remove netcdf file
  if (file.exists(ncfil)) {system(paste("rm -f ",ncfil,sep=""))}
}

###
#produce plots
#functions to plot the stuff
rs_levplot2 <- function(rsin,zn,zx,nb,brks=NA,scale="YlOrRd",ncol=9,col_i="#CCECE6",col_f="#00441B",rev=F,leg=T,colours=NA) {
  if (scale %in% row.names(brewer.pal.info)) {
    pal <- rev(brewer.pal(ncol, scale))
    if (!is.na(colours[1])) {pal <- colours}
  } else {
    pal <- c("grey 50",colorRampPalette(c(col_i,col_f))(ncol))
    if (!is.na(colours[1])) {pal <- colours}
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
rs <- rs_wrld
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=20), norths=seq(-90,90,by=20))

#loop crops
for (crop_name in c("maize","groundnut","rice","soybean")) {
  #crop_name <- "maize"
  cat("...processing",crop_name,"\n")
  rs_crop <- raster(paste(figDir,"/nichedness_",crop_name,"_raw_v2.tif",sep=""))
  rs_crop2 <- raster(paste(figDir,"/nichedness_",crop_name,"_cut_v2.tif",sep=""))
  
  #plot uncut one
  trs <- rs_crop; trs[which(trs[] < 0)] <- -5; trs[which(trs[] > 100)] <- 100
  tplot <- rs_levplot2(trs,zn=-5,zx=100,nb=21,brks=NA,scale=NA,col_i="#FFC8C8",col_f="#FF0000",ncol=21,rev=F,leg=T)
  pdf(paste(figDir,"/global_nichedness_",crop_name,".pdf",sep=""), height=4.5,width=7,pointsize=16)
  print(tplot)
  dev.off()
  setwd(figDir)
  system(paste("convert -verbose -density 300 global_nichedness_",crop_name,".pdf -quality 100 -sharpen 0x1.0 -alpha off global_nichedness_",crop_name,".png",sep=""))
  setwd("~")
  
  #plot cut one
  trs <- rs_crop2; trs[which(trs[] < 0)] <- -5; trs[which(trs[] > 100)] <- 100
  tplot <- rs_levplot2(trs,zn=-5,zx=100,nb=21,brks=NA,scale=NA,col_i="#FFC8C8",col_f="#FF0000",ncol=21,rev=F,leg=T)
  pdf(paste(figDir,"/global_nichedness_",crop_name,"_cropped_area.pdf",sep=""), height=4.5,width=7,pointsize=16)
  print(tplot)
  dev.off()
  setwd(figDir)
  system(paste("convert -verbose -density 300 global_nichedness_",crop_name,"_cropped_area.pdf -quality 100 -sharpen 0x1.0 -alpha off global_nichedness_",crop_name,"_cropped_area.png",sep=""))
  setwd("~")
}


#rstk <- stack(paste(figDir,"/nichedness_",c("maize","groundnut","rice","soybean"),"_raw_v2.tif",sep=""))

