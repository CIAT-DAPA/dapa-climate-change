#Julian Ramirez-Villegas
#UoL / CCAFS
#Jan 2015
#stop("!")

#Impact analyses for a given country in Africa

#load libraries
library(sp); library(raster); library(rgdal); library(maptools)
library(ggplot2); library(reshape2) #; library(grid); library(rgeos); library(plyr)
data(wrld_simpl)

####################################################
### selected config. (country and y-axis limits) ###
####################################################
ylims <- data.frame(scountry=c("ETH","UGA","GHA","NER","SEN","MLI"),
                    yn=c(-100,-100,-100,-100,-100,-100),yx=c(250,50,100,250,160,150),
                    spacing=c(50,15,20,50,40,25))
scountry <- "MLI"
coun_ylims <- c(ylims$yn[which(ylims$scountry == scountry)], 
                ylims$yx[which(ylims$scountry == scountry)])
coun_spac <- ylims$spacing[which(ylims$scountry == scountry)]
####################################################
####################################################

#input directories
wd <- "~/Leeds-work/p4s-csa/climate-impacts"
src_dir <- paste(wd,"/scripts",sep="")
sbsta_dir <- "~/Leeds-work/SBSTA-vulnerability"
dat_dir <- paste(sbsta_dir,"/model_data",sep="")
runs_dir <- paste(sbsta_dir,"/model_runs",sep="")
impact_dir <- paste(sbsta_dir,"/impacts",sep="")

#output figures dir
fig_dir <- paste(wd,"/figures/country_plots",sep="")
if (!file.exists(fig_dir)) {dir.create(fig_dir,recursive=T)}

#list of GCMs, emission scenarios, periods and variables
gcm_list <- c("gfdl-esm2m","hadgem2-es","ipsl-cm5a-lr","miroc-esm-chem","noresm1-m")
sce_list <- c("rcp26","rcp45","rcp60","rcp85")

#load list of crops, parameters, and thresholds
params <- read.table(paste(dat_dir,"/crop_parameters.tab",sep=""),header=T)
crop_list <- c(paste(params$Crop[1:6]),"sorghum","maize","yam")
thresholds <- read.csv(paste(dat_dir,"/thresholds.csv",sep=""))

#make rasters for each of the regions: NAF, SAH, WAF, CAF, EAF, SAF
#reference raster
rs_ref <- raster(paste(runs_dir,"/",gcm_list[1],"/hist/1971-2000/banana_suit.tif",sep=""))
rs_ref[which(!is.na(rs_ref[]))] <- 1
afr_rs <- rs_ref #all Africa mask

###
#map of countries with indicator
load(file=paste(impact_dir,"/change_suitable_areas_all_countries.RData",sep=""))
out_africa$crop <- factor(out_africa$crop, levels=c("banana","cassava","beans","fmillet","groundnut","pmillet","sorghum","maize","yam"),
                          labels=c("Banana","Cassava","Bean","F. millet","Groundnut","P. millet","Sorghum","Maize","Yam"))

for (sce in sce_list) {
  #sce <- sce_list[1]
  for (per in c("2040-2069","2070-2099")) {
    #per <- "2040-2069"
    tdata <- out_africa[which(out_africa$country == scountry & out_africa$rcp == sce & out_africa$period == per),]
    tdata <- tdata[which(!is.na(tdata$area_chg)),]
    
    #the next line is for some countries only (all other than WAF ones)
    if (!scountry %in% c("SEN","CIV","GIN","GNB","GMB","LBR","NGA","BEN","TGO","GHA","SLE")) {
      tdata <- tdata[which(tdata$crop != "Yam"),]
    }
    
    #factors for plotting
    tdata$crop <- as.factor(paste(tdata$crop))
    tdata$crop <- ordered(tdata$crop, levels=levels(tdata$crop)[order(as.numeric(by(tdata$area_chg, tdata$crop, median)),decreasing=T)])
    
    #plotting
    pdf(paste(fig_dir,"/",scountry,"_boxplot_",sce,"_",per,".pdf",sep=""),height=5,width=5)
    par(las=1,mar=(c(5,5.5,1,1)))
    boxplot(tdata$area_chg ~ tdata$crop, pch=20, horizontal=T, ylim=coun_ylims, axes=F,
            xlab=paste("Projected change in suitable area [",per,"] (%)",sep=""),
            outcol="red",medcol="red",boxcol="blue",col="white",border="black")
    axis(side=1,at=seq(-100,500,by=25))
    axis(side=2,at=seq(1,length(unique(tdata$crop)),by=1),labels=paste(levels(tdata$crop)),las=1)
    box()
    grid(lwd=1)
    abline(v=0, lwd=1)
    dev.off()
    
    #convert figure to PNG
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 ",scountry,"_boxplot_",sce,"_",per,".pdf -quality 100 -sharpen 0x1.0 -alpha off ",scountry,"_boxplot_",sce,"_",per,".png",sep=""))
    setwd("~")
    
    #barplot with error bars
    bplot_data <- aggregate(tdata[,c("area_chg","area_loss","area_gain")], by=list(crop=tdata$crop), FUN=function(x) {median(x,na.rm=T)})
    area_chg_p5 <- aggregate(tdata[,c("area_chg")], by=list(crop=tdata$crop), FUN=function(x) {quantile(x,probs=0.05,na.rm=T)})
    area_chg_p95 <- aggregate(tdata[,c("area_chg")], by=list(crop=tdata$crop), FUN=function(x) {min(c(quantile(x,probs=0.95,na.rm=T),249))})
    bplot_data$area_chg_p5 <- area_chg_p5$x; bplot_data$area_chg_p95 <- area_chg_p95$x
    
    p <- ggplot(bplot_data, aes(x = crop, y = area_chg)) + 
      geom_bar(width=0.99,stat="identity",size=0.5, fill="grey 70", colour="black") + 
      geom_errorbar(aes(x=crop, ymin = area_chg_p5, ymax = area_chg_p95), width=0.1,size=0.5) +
      scale_x_discrete(name="") + 
      scale_y_continuous(paste("Projected change in suitable area [",per,"] (%)",sep=""), 
                         limits = coun_ylims, breaks=seq(coun_ylims[1], coun_ylims[2], by = coun_spac)) + 
      theme_bw() +
      theme(axis.text.x=element_text(size=14),
            axis.text.y=element_text(size=14),
            axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15),
            legend.position="right",
            legend.title = element_text(size=15),
            legend.text = element_text(size=15),
            legend.key =  element_rect(color="white")) +
      coord_flip()
    #print(p)
    
    pdf(paste(fig_dir,"/",scountry,"_barplot_",sce,"_",per,".pdf",sep=""),height=6,width=7)
    print(p)
    dev.off()
    
    #convert figure to PNG
    setwd(fig_dir)
    system(paste("convert -verbose -density 300 ",scountry,"_barplot_",sce,"_",per,".pdf -quality 100 -sharpen 0x1.0 -alpha off ",scountry,"_barplot_",sce,"_",per,".png",sep=""))
    setwd("~")
    
  }
}



