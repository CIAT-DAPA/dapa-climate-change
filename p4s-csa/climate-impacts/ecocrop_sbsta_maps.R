#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2015
#stop("!")

#plot maps for grops for all Africa
library(raster); library(maptools); library(rasterVis)
library(maptools); data(wrld_simpl)

#directories
wd <- "~/Leeds-work/p4s-csa/climate-impacts"
src_dir <- paste(wd,"/scripts",sep="")
sbsta_dir <- "~/Leeds-work/SBSTA-vulnerability"
dat_dir <- paste(sbsta_dir,"/model_data",sep="")
runs_dir <- paste(sbsta_dir,"/model_runs",sep="")
impact_dir <- paste(sbsta_dir,"/impacts",sep="")

#output figures dir
fig_dir <- paste(wd,"/figures/africa_maps",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir,recursive=T)}

#list of GCMs, emission scenarios, periods and variables
gcm_list <- c("gfdl-esm2m","hadgem2-es","ipsl-cm5a-lr","miroc-esm-chem","noresm1-m")
sce_list <- c("rcp26","rcp45","rcp60","rcp85")

#load list of crops, parameters, and thresholds
params <- read.table(paste(dat_dir,"/crop_parameters.tab",sep=""),header=T)
crop_list <- c(paste(params$Crop[1:6]),"sorghum","maize","yam")
thresholds <- read.csv(paste(dat_dir,"/thresholds.csv",sep=""))
thresholds <- aggregate(thresholds[,c("AUC","MinROCdist","MaxKappa","MaxSensSpec")],by=list(crop=thresholds$crop),FUN=function(x) {mean(x,na.rm=T)})

for (sce in sce_list) {
  #sce <- sce_list[1]
  for (per in c("2040-2069","2070-2099")) {
    #per <- "2040-2069"
    for (crop in crop_list) {
      #crop <- crop_list[1]
      cat("...plotting crop=",crop,"/ scenario=",sce,"/ period=",per,"\n")
      
      #get threshold value
      if (crop == "fmillet") {
        thresh <- thresholds$MaxSensSpec[which(thresholds$crop == "fmillet_EAF_SAF")]
      } else if (crop == "yam") {
        thresh <- thresholds$MaxSensSpec[which(thresholds$crop == "yam_WAF")]
      } else {
        thresh <- thresholds$MaxSensSpec[which(thresholds$crop == crop)]
      }
      
      #load baseline
      rstk <- stack(paste(runs_dir,"/",gcm_list,"/hist/1971-2000/",crop,"_suit.tif",sep=""))
      rstk <- calc(rstk, fun=function(x) {median(x,na.rm=T)})
      rstk[which(rstk[] < thresh*100)] <- NA
      
      #general config
      ht <- 6
      rs <- rstk
      fct <- (rs@extent@xmin-rs@extent@xmax)/(rs@extent@ymin-rs@extent@ymax)
      wt <- ht*(fct+.1)
      grat <- gridlines(wrld_simpl, easts=seq(-180,180,by=7.5), norths=seq(-90,90,by=7.5))
      
      pal <- c(rgb(255,119,1,255,maxColorValue=255),rgb(255,187,0,255,maxColorValue=255),
               rgb(255,255,0,255,maxColorValue=255),rgb(164,196,0,255,maxColorValue=255),
               rgb(86,145,1,255,maxColorValue=255),rgb(0,97,0,255,maxColorValue=255))
      brks <- c(40,50,60,70,80,90,100)
      this_theme <- custom.theme(fill = pal, region = pal, bg = "white", fg = "grey20", pch = 14)
      
      #plot baseline
      if (!file.exists(paste(fig_dir,"/",crop,"_baseline.pdf",sep=""))) {
        tplot <- rasterVis:::levelplot(rstk, margin=F, par.settings = this_theme, colorkey=T,
                                       at = brks, maxpixels=ncell(rstk)) + 
          layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
          layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
        
        #print to pdf
        pdf(paste(fig_dir,"/",crop,"_baseline.pdf",sep=""), height=6,width=6.5,pointsize=16)
        print(tplot)
        dev.off()
        
        #convert figure to PNG
        setwd(fig_dir)
        system(paste("convert -verbose -density 300 ",crop,"_baseline.pdf -quality 100 -sharpen 0x1.0 -alpha off ",crop,"_baseline.png",sep=""))
        setwd("~")
      }
      
      #future projection for change map
      stk_fut <- stack(paste(runs_dir,"/",gcm_list,"/",sce,"/",per,"/",crop,"_suit.tif",sep=""))
      stk_fut <- calc(stk_fut, fun=function(x) {median(x,na.rm=T)})
      
      rs_class <- raster(stk_fut)
      rs_class[] <- NA
      rs_class[which(rstk[] < thresh*100 & stk_fut[] >= thresh*100)] <- 0.5 #become suitable
      rs_class[which(stk_fut[] >= thresh*100 & stk_fut[] > rstk[] & rstk[] >= thresh*100)] <- 1.5 #stay suitable but above current
      rs_class[which(stk_fut[] >= thresh*100 & stk_fut[] == rstk[] & rstk[] >= thresh*100)] <- 2.5 #stay suitable equal to current
      rs_class[which(stk_fut[] >= thresh*100 & stk_fut[] < rstk[] & rstk[] >= thresh*100)] <- 3.5 #stay suitable but below current
      rs_class[which(rstk[] >= thresh*100 & stk_fut[] < thresh*100 )] <- 4.5 #become unsuitable
      
      #fig. config
      pal <- c(rgb(0,77,167,255,maxColorValue=255),rgb(38,115,0,255,maxColorValue=255),
               rgb(255,235,190,255,maxColorValue=255),rgb(255,170,1,255,maxColorValue=255),
               rgb(168,0,0,255,maxColorValue=255))
      brks <- c(0:5)
      this_theme <- custom.theme(fill = pal, region = pal, bg = "white", fg = "grey20", pch = 14)
      
      #produce plot
      tplot <- rasterVis:::levelplot(rs_class, margin=F, par.settings = this_theme, 
                                     colorkey=F,#list(at=c(0.5,1.5,2.5,3.5,4.5),labels=c("New areas","Suitable (above hist.)","Suitable (equal to hist.)","Suitable (below hist.)","Lost areas")),
                                     at = brks, maxpixels=ncell(rs_class)) + 
        layer(sp.lines(grat,lwd=0.5,lty=2,col="grey 50")) +
        layer(sp.polygons(wrld_simpl,lwd=0.8,col="black"))
      
      #print to pdf
      pdf(paste(fig_dir,"/",crop,"_change_",sce,"_",per,".pdf",sep=""), height=6,width=6.5,pointsize=16)
      print(tplot)
      dev.off()
      
      #convert figure to PNG
      setwd(fig_dir)
      system(paste("convert -verbose -density 300 ",crop,"_change_",sce,"_",per,".pdf -quality 100 -sharpen 0x1.0 -alpha off ",crop,"_change_",sce,"_",per,".png",sep=""))
      setwd("~")
    }
  }
}






