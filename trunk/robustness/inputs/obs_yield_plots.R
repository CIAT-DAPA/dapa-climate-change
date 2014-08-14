#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Jan 2014
stop("!")
#read in Iizumi et al. (2013) yield data, cut to Africa and plot for some individual countries

#packages
library(sp); library(raster); library(rgdal); library(maptools); library(rasterVis)
data(wrld_simpl)

#settings
wd <- "~/Leeds-work"
dataDir <- paste(wd,"/datasets",sep="")
yieldDir <- paste(dataDir,"/yield_data/gdhy/gdhy_2013JAN29_maize",sep="")
robDir <- paste(wd,"/quest-for-robustness",sep="")

seasons <- c("_","_major_","_second_")
years <- c(1982:2006)

#read Africa shapefile
shp <- readShapePoly(paste(dataDir,"/shapefiles/Africa/africa-adm0.shp",sep=""))

#choose / loop season
for (seas in seasons) {
  #seas <- seasons[1]
  cat("\nprocessing season:",seas,"\n",sep="")
  
  #output raster dir
  orsDir <- paste(yieldDir,"/raster",seas,"maize",sep="")
  if (!file.exists(orsDir)) {dir.create(orsDir)}
  
  if ((length(list.dirs(orsDir))-1) != length(years)) {
    #choose / loop year
    for (yr in years) {
      #yr <- years[1]
      cat("\n...processing year: ",yr,"\n",sep="")
      
      #output year dir
      orsyrDir <- paste(orsDir,"/rs_",yr,sep="")
      if (!file.exists(orsyrDir)) {dir.create(orsyrDir)}
      
      #read table
      ydata <- read.table(paste(yieldDir,"/gdhy_2013JAN29_maize",seas,yr,".dat",sep=""),sep="",header=T)
      
      #get grid names to info
      grdnames <- names(ydata)[5:ncol(ydata)]
      grdnames_f <- gsub("t.ha.","",grdnames)
      grdnames_f <- gsub("\\.","",grdnames_f)
      
      #get lats and lons for grid creation
      longs <- unique(ydata$Long.deg..)
      lats <- unique(ydata$Lati.deg..)
      
      #create raster and put data into it (lon,lat are center)
      rs <- raster(nrows=length(lats), ncols=length(longs), xmn=-0.5625, xmx=359.4375, ymn=-90, ymx=90)
      #rs2 <- raster(nrows=length(lats), ncols=length(longs), xmn=0, xmx=360, ymn=-90, ymx=90)
      ydata$CELL <- cellFromXY(rs, xy=cbind(x=ydata$Long.deg..,y=ydata$Lati.deg..))
      
      for (gn in 1:length(grdnames)) {
        #gn <- 1
        cat("processing",grdnames_f[gn],"\n")
        if (!file.exists(paste(orsyrDir,"/gdhy_2013JAN29_maize",seas,grdnames_f[gn],"_",yr,".tif",sep=""))) {
          trs <- rs
          trs[ydata$CELL] <- ydata[,grdnames[gn]]
          trs[which(trs[] == -999)] <- NA
          if (gn == 1) {trs[which(trs[] == 0)] <- NA}
          trs <- rotate(trs)
          trs <- writeRaster(trs, paste(orsyrDir,"/gdhy_2013JAN29_maize",seas,grdnames_f[gn],"_",yr,".tif",sep=""),format="GTiff")
          rm(trs)
        }
      }
    }
  }
}


###
#1. crop all yield data for Africa and store in robustness folder
#2. create a figure for each of the years, cropping seasons (only median yield)
#3. calculate mean yield, s.d. and c.v. --plot
#4. calculate the mean for whole Africa and put 050 and 095 confidence intervals, plot as time series
#5. do the same as in (4), but for each country


###
#1. first crop all yield data to Africa and store locally
robDataDir <- paste(robDir,"/data",sep="")
if (!file.exists(robDataDir)) {dir.create(robDataDir)}

af_extn <- extent(-20,55,-40,40)

#for whole-Africa calculation and also country-based calcs
af_rs <- rasterize(shp, raster(paste(yieldDir,"/raster_maize/rs_1982/gdhy_2013JAN29_maize_ModelYld025_1982.tif",sep="")))
af_rs <- crop(af_rs, af_extn)

for (seas in seasons) {
  #seas <- seasons[1]
  cat("\nprocessing season:",seas,"\n",sep="")
  
  #output seasons dir
  orsDir <- paste(robDataDir,"/yield_data",seas,"maize",sep="")
  if (!file.exists(orsDir)) {dir.create(orsDir)}
  
  for (yr in years) {
    #yr <- years[1]
    cat("...processing year: ",yr,"\n",sep="")
    
    #output year dir
    orsyrDir <- paste(orsDir,"/rs_",yr,sep="")
    if (!file.exists(orsyrDir)) {dir.create(orsyrDir)}
    
    rsList <- list.files(paste(yieldDir,"/raster",seas,"maize/rs_",yr,sep=""),pattern="\\.tif")
    for (irs in rsList) {
      #irs <- rsList[1]
      if (!file.exists(paste(orsyrDir,"/",irs,sep=""))) {
        trs <- raster(paste(yieldDir,"/raster",seas,"maize/rs_",yr,"/",irs,sep=""))
        trs <- crop(trs, af_extn)
        trs <- writeRaster(trs,paste(orsyrDir,"/",irs,sep=""),format="GTiff")
      }
    }
  }
}


###
# 2. create figure for each season and year
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


rs_print <- function(p,pdfName) {
  pdf(pdfName,height=ht,width=wt,pointsize=14)
  print(p)
  dev.off()
}

#figure details
ht <- 6
rs <- af_rs
fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
wt <- ht*(fct+.1)
grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=5), norths=seq(-90,90,by=5))


#loop sasons and years for plotting
dodgypoints <- cbind(x=c(28.125,21.375,20.250),y=c(-23.0625,-26.4375,-25.3125))
maxvals <- c()
for (seas in seasons) {
  #seas <- seasons[1]
  cat("\nprocessing season:",seas,"\n",sep="")
  
  #output seasons dir
  orsDir <- paste(robDataDir,"/yield_data",seas,"maize",sep="")
  
  #output fig dir
  ofigDir <- paste(orsDir,"/yearly_plots",sep="")
  if (!file.exists(ofigDir)) {dir.create(ofigDir)}
  
  for (yr in years) {
    #yr <- years[1]
    cat("...processing year: ",yr,"\n",sep="")
    
    #output year dir
    orsyrDir <- paste(orsDir,"/rs_",yr,sep="")
    
    #open raster i want to plot
    trs <- raster(paste(orsyrDir,"/gdhy_2013JAN29_maize",seas,"ModelYld500_",yr,".tif",sep=""))
    ssa_extn <- af_extn
    ssa_extn@ymax <- 20
    trs <- crop(trs, ssa_extn)
    
    #remove dodgypoints
    trs[cellFromXY(trs,dodgypoints)] <- NA
    trs[which(trs[] > 6)] <- 6
    
    #calculating max value
    #cat("ceiling at",ceiling(max(trs[],na.rm=T)),"\n")
    maxvals <- c(maxvals, max(trs[],na.rm=T))
    
    #make figure
    pdf(paste(ofigDir,"/ModelYld500_",yr,".pdf",sep=""), height=8,width=10,pointsize=14)
    tplot <- rs_levplot2(trs,zn=0,zx=6,nb=12,brks=NA,scale="YlOrRd",col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
    print(tplot)
    dev.off()
  }
}


###
#3. calculate mean yield, s.d. and c.v. --plot
for (seas in seasons) {
  #seas <- seasons[1]
  #seasons dir
  orsDir <- paste(robDataDir,"/yield_data",seas,"maize",sep="")
  
  #output dir
  outsumDir <- paste(orsDir,"/descriptive_stats",sep="")
  if (!file.exists(outsumDir)) {dir.create(outsumDir)}
  
  #load all years data
  trs <- stack(paste(orsDir,"/rs_",years,"/gdhy_2013JAN29_maize",seas,"ModelYld500_",years,".tif",sep=""))
  
  #remove dodgypoints
  trs[cellFromXY(trs,dodgypoints)] <- NA
  
  #calculate mean and other stuff
  trs_mean <- calc(trs,fun=function(x) {mean(x,na.rm=T)})
  trs_sd <- calc(trs,fun=function(x) {sd(x,na.rm=T)})
  trs_cv <- trs_sd / trs_mean * 100
  
  #cut to ssa extent
  ssa_extn <- af_extn
  ssa_extn@ymax <- 20
  
  trs_mean <- crop(trs_mean, ssa_extn)
  trs_sd <- crop(trs_sd, ssa_extn)
  trs_cv <- crop(trs_cv, ssa_extn)
  
  #write rasters
  trs_mean <- writeRaster(trs_mean, paste(outsumDir,"/mean_ModelYld500.tif",sep=""),format="GTiff")
  trs_sd <- writeRaster(trs_sd, paste(outsumDir,"/sd_ModelYld500.tif",sep=""),format="GTiff")
  trs_cv <- writeRaster(trs_cv, paste(outsumDir,"/cv_ModelYld500.tif",sep=""),format="GTiff")
  
  #make figure
  pdf(paste(outsumDir,"/mean_ModelYld500.pdf",sep=""), height=8,width=10,pointsize=14)
  tplot <- rs_levplot2(trs_mean,zn=0,zx=6,nb=12,brks=NA,scale="YlOrRd",col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
  print(tplot)
  dev.off()
  
  pdf(paste(outsumDir,"/sd_ModelYld500.pdf",sep=""), height=8,width=10,pointsize=14)
  tplot <- rs_levplot2(trs_sd,zn=0,zx=2,nb=20,brks=NA,scale="YlOrRd",col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
  print(tplot)
  dev.off()
  
  pdf(paste(outsumDir,"/cv_ModelYld500.pdf",sep=""), height=8,width=10,pointsize=14)
  tplot <- rs_levplot2(trs_cv,zn=0,zx=100,nb=20,brks=NA,scale="reds",col_i="red",col_f="#FEE0D2",ncol=9,rev=T,leg=T)
  print(tplot)
  dev.off()
}


#4. calculate the mean for whole Africa and put 050 and 095 confidence intervals, plot as time series
#create data.frame with af_rs and perform required calculations
xy_all <- data.frame(cell=which(!is.na(af_rs[])))
xy_all <- cbind(xy_all,xyFromCell(af_rs, xy_all$cell))

#loop seasons to calculate mean
seasList <- list()
for (seas in seasons) {
  #seas <- seasons[1]
  cat("\nprocessing season:",seas,"\n",sep="")
  orsDir <- paste(robDataDir,"/yield_data",seas,"maize",sep="")
  
  yrstat <- data.frame()
  for (yr in years) {
    #yr <- years[1]
    cat("...processing year: ",yr,"\n",sep="")
    
    #load 050 raster
    orsyrDir <- paste(orsDir,"/rs_",yr,sep="")
    
    qntmeans <- c()
    for (qnt in c("050","500","950")) {
      #qnt <- "050"
      trs <- raster(paste(orsyrDir,"/gdhy_2013JAN29_maize",seas,"ModelYld",qnt,"_",yr,".tif",sep=""))
      #ssa_extn <- af_extn
      #ssa_extn@ymax <- 20
      #trs <- crop(trs, ssa_extn)
      trs[cellFromXY(trs,dodgypoints)] <- NA
      xy_all$VALUE <- extract(trs, xy_all[,c("x","y")])
      qntmeans <- c(qntmeans, mean(xy_all$VALUE,na.rm=T))
      xy_all$VALUE <- NULL
    }
    
    #append final data.frame
    yrstat <- rbind(yrstat, data.frame(year=yr, q050=qntmeans[1], q500=qntmeans[2], q950=qntmeans[3]))
  }
  seasList[[seas]] <- yrstat
}


#plot the season data
tsfigDir <- paste(robDataDir,"/timeseries_plots",sep="")
if (!file.exists(tsfigDir)) {dir.create(tsfigDir)}

#read in FAO statistical data
faoyield <- read.csv(paste(tsfigDir,"/StatisticalData.csv",sep=""),header=T)

#plot the timeseries
pdf(paste(tsfigDir,"/AFR_timeseries.pdf",sep=""), height=8,width=10,pointsize=15)
par(mar=c(5,5,1,1),las=1,lwd=1.75)
plot(years, seasList[[1]]$q500, ty="l", ylim=c(0.5,3), col="black", lwd=1.5,
     xlab="Year", ylab="Crop yield (ton/ha)")
polx <- c(years,rev(years))
poly <- c(seasList[[1]]$q050, rev(seasList[[1]]$q950))
polygon(polx,poly,col=rgb(red=0,green=0,blue=0,50,maxColorValue=255),border=NA)

poly <- c(seasList[[2]]$q050, rev(seasList[[2]]$q950))
polygon(polx,poly,col=rgb(red=0,green=0,blue=255,50,maxColorValue=255),border=NA)
lines(years, seasList[[2]]$q500, ty="l", col="blue", lwd=1.5)

poly <- c(seasList[[3]]$q050, rev(seasList[[3]]$q950))
polygon(polx,poly,col=rgb(red=255,green=0,blue=0,50,maxColorValue=255),border=NA)
lines(years, seasList[[3]]$q500, ty="l", col="red", lwd=1.5)

#plot all-Africa yield mean
fao_afr <- as.numeric(faoyield[nrow(faoyield),paste("X",years,sep="")]) / 10000
lines(years, fao_afr/(mean(fao_afr)/mean(seasList[[1]]$q500)), col="black", lty=2, lwd=2)

grid()
legend(1982,3,legend=c("mean","major","second","FAO"),col=c("black","blue","red","black"),
       lty=c(1,1,1,2),lwd=c(1.5,1.5,1.5,2.5),cex=1.2,bg="white",border=NA)

dev.off()

#5. do the same as in (4), but for each country
correldf <- data.frame()
for (i in 1:nrow(af_rs@data@attributes[[1]])) {
  #i <- 2
  ctryid <- af_rs@data@attributes[[1]]$ID[i]
  ctryname <- af_rs@data@attributes[[1]]$name.sort[i]
  
  if (i == 41) {
    ctryfips <- "SM"
  } else {
    ctryfips <- af_rs@data@attributes[[1]]$fips.cntry[i]
  }
  
  xy_ctry <- data.frame(cell=which(af_rs[] == ctryid))
  xy_ctry <- cbind(xy_ctry, xyFromCell(af_rs, xy_ctry$cell))
  
  #loop seasons to calculate mean
  seasList <- list()
  for (seas in seasons) {
    #seas <- seasons[1]
    cat("\nprocessing season:",seas,"\n",sep="")
    orsDir <- paste(robDataDir,"/yield_data",seas,"maize",sep="")
    
    yrstat <- data.frame()
    for (yr in years) {
      #yr <- years[1]
      cat("...processing year: ",yr,"\n",sep="")
      
      #load 050 raster
      orsyrDir <- paste(orsDir,"/rs_",yr,sep="")
      
      qntmeans <- c()
      for (qnt in c("050","500","950")) {
        #qnt <- "050"
        trs <- raster(paste(orsyrDir,"/gdhy_2013JAN29_maize",seas,"ModelYld",qnt,"_",yr,".tif",sep=""))
        trs[cellFromXY(trs,dodgypoints)] <- NA
        xy_ctry$VALUE <- extract(trs, xy_ctry[,c("x","y")])
        qntmeans <- c(qntmeans, mean(xy_ctry$VALUE,na.rm=T))
        xy_ctry$VALUE <- NULL
      }
      
      #append final data.frame
      yrstat <- rbind(yrstat, data.frame(year=yr, q050=qntmeans[1], q500=qntmeans[2], q950=qntmeans[3]))
    }
    seasList[[seas]] <- yrstat
  }
  
  #determine min/max
  alldata <- do.call("rbind",seasList)
  alldata <- c(alldata[,2],alldata[,3],alldata[,4])
  
  if (length(which(!is.na(alldata))) > 0) {
    maxval <- max(c(alldata,3),na.rm=T); minval <- min(c(0.5,alldata),na.rm=T)
    
    #plot the timeseries
    pdf(paste(tsfigDir,"/",ctryfips,"_timeseries.pdf",sep=""), height=8,width=10,pointsize=15)
    par(mar=c(5,5,1,1),las=1,lwd=1.75)
    
    tpdata <- seasList[[1]]
    tpdata <- tpdata[which(!is.na(tpdata[,2])),]; tpdata <- tpdata[which(!is.na(tpdata[,3])),]; tpdata <- tpdata[which(!is.na(tpdata[,4])),]
    poly <- c(tpdata$q050, rev(tpdata$q950))
    plot(tpdata$year, tpdata$q500, ty="l", ylim=c(minval,maxval), col="black", lwd=1.5,
         xlab="Year", ylab="Crop yield (ton/ha)")
    polx <- c(tpdata$year,rev(tpdata$year))
    poly <- c(tpdata$q050, rev(tpdata$q950))
    polygon(polx,poly,col=rgb(red=0,green=0,blue=0,50,maxColorValue=255),border=NA)
    
    tpdata <- seasList[[2]]
    tpdata <- tpdata[which(!is.na(tpdata[,2])),]; tpdata <- tpdata[which(!is.na(tpdata[,3])),]; tpdata <- tpdata[which(!is.na(tpdata[,4])),]
    poly <- c(tpdata$q050, rev(tpdata$q950))
    polygon(c(tpdata$year,rev(tpdata$year)),poly,col=rgb(red=0,green=0,blue=255,50,maxColorValue=255),border=NA)
    lines(tpdata$year, tpdata$q500, ty="l", col="blue", lwd=1.5)
    
    tpdata <- seasList[[3]]
    tpdata <- tpdata[which(!is.na(tpdata[,2])),]; tpdata <- tpdata[which(!is.na(tpdata[,3])),]; tpdata <- tpdata[which(!is.na(tpdata[,4])),]
    poly <- c(tpdata$q050, rev(tpdata$q950))
    polygon(c(tpdata$year,rev(tpdata$year)),poly,col=rgb(red=255,green=0,blue=0,50,maxColorValue=255),border=NA)
    lines(tpdata$year, tpdata$q500, ty="l", col="red", lwd=1.5)
    
    #plot FAO yield (scaled with some sort of CYG)
    if (length(which(faoyield$fips == paste(ctryfips))) > 0) {
      fao_ctry <- as.numeric(faoyield[which(faoyield$fips == paste(ctryfips)),paste("X",years,sep="")]) / 10000
      lines(years, fao_ctry/(mean(fao_ctry,na.rm=T)/mean(seasList[[1]]$q500,na.rm=T)), col="black", lty=2, lwd=2)
      
      #calculate correlation and pseudo-cyg
      fao_ctry <- data.frame(year=years,yield=fao_ctry)
      fao_ctry <- merge(fao_ctry, seasList[[1]], by="year")
      
      if (length(which(!is.na(fao_ctry$yield))) > 2) {
        correl <- cor.test(fao_ctry$yield,fao_ctry$q500,na.rm=T,method="pearson")
        correldf <- rbind(correldf, data.frame(id=i,fips=ctryfips,name=ctryname, 
                                               rpearson=correl$estimate, pval=correl$p.value,
                                               pcyg=(mean(fao_ctry$yield,na.rm=T)/mean(seasList[[1]]$q500,na.rm=T))))
      } else {
        correldf <- rbind(correldf, data.frame(id=i,fips=ctryfips,name=ctryname, rpearson=NA, 
                                               pval=NA,pcyg=NA))
      }
    }
    grid()
    legend(1982,maxval,legend=c("mean","major","second","FAO"),col=c("black","blue","red","black"),
           lty=c(1,1,1,2),lwd=c(1.5,1.5,1.5,2.5),cex=1,bg="white",border=NA)
    dev.off()
  }
}

write.csv(correldf,paste(tsfigDir,"/correlations.csv",sep=""),quote=T,row.names=F)

