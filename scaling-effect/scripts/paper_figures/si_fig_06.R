#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Feb 2014
stop("!")

###
#heatmaps for 12km all and niche for maize
###

#load packages
library(rgdal); library(raster); library(maptools); library(rasterVis); data(wrld_simpl)
library(ggplot2); library(reshape2)

#i/o directories and details
bDir <- "~/Leeds-work/scaling-effect"
clmDir <- paste(bDir,"/climate_data",sep="")
runDir <- paste(bDir,"/model-runs",sep="")
lsmDir <- paste(bDir,"/lsm",sep="")

sensDir <- paste(runDir,"/sens",sep="")

#figure dir is local (on mbp)
figDir <- paste(bDir,"/paper_figures_v2",sep="")

#model run details
trial <- 6
crop_name <- "maiz"

#write sensitivity output
outsens <- read.csv(paste(sensDir,"/sensitivity_result.csv",sep=""))
outsens <- outsens[which(outsens$temp >= 0 & outsens$temp <= 5),]

#heatmap of sensitivity of 12km runs
hplot_df <- outsens[,c("prec","temp","reldiff_all")]
hplot_df$prec <- as.factor(hplot_df$prec*100)
hplot_df$temp <- as.factor(hplot_df$temp)

p <- ggplot(data=hplot_df, aes(temp, prec)) + geom_tile(aes(fill = reldiff_all), colour = NA)
p <- p + scale_fill_gradient2(name="", low = "red", mid="white", high = "blue", 
                              midpoint=0, limits=c(-100,100),guide="colourbar")
p <- p + theme(legend.key.height=unit(2.75,"cm"),legend.key.width=unit(1,"cm"),
               legend.text=element_text(size=13),
               panel.background=element_rect(fill="white",colour="black"),
               axis.text=element_text(colour="black",size=13),
               axis.title=element_text(colour="black",size=15,face="bold"))
p <- p + labs(x = "Temperature change (K)", y = "Precipitation change (%)")

pdf(paste(figDir,"/SI-Fig06d_ecocrop_12km_all.pdf",sep=""), height=6,width=8,pointsize=12,family="Helvetica")
print(p)
dev.off()

###
#niche areas
hplot_df <- outsens[,c("prec","temp","reldiff_har")]
hplot_df$prec <- as.factor(hplot_df$prec*100)
hplot_df$temp <- as.factor(hplot_df$temp)

p <- ggplot(data=hplot_df, aes(temp, prec)) + geom_tile(aes(fill = reldiff_har), colour = NA)
p <- p + scale_fill_gradient2(name="", low = "red", mid="white", high = "blue", 
                              midpoint=0, limits=c(-100,100),guide="colourbar")
p <- p + theme(legend.key.height=unit(2.75,"cm"),legend.key.width=unit(1,"cm"),
               legend.text=element_text(size=13),
               panel.background=element_rect(fill="white",colour="black"),
               axis.text=element_text(colour="black",size=13),
               axis.title=element_text(colour="black",size=15,face="bold"))
p <- p + labs(x = "Temperature change (K)", y = "Precipitation change (%)")

pdf(paste(figDir,"/SI-Fig06e_ecocrop_12km_niche.pdf",sep=""), height=6,width=8,pointsize=12,family="Helvetica")
print(p)
dev.off()


###
#3 deg heatmap
sensDir <- paste(runDir,"/sens_3deg-12km_exp_bil",sep="")

#read in sensitivity output
outsens <- read.csv(paste(sensDir,"/sensitivity_result.csv",sep=""))
outsens <- outsens[which(outsens$temp >= 0 & outsens$temp <= 5),]

#heatmap of sensitivity of 12km runs
hplot_df <- outsens[,c("prec","temp","reldiff_all")]
hplot_df$prec <- as.factor(hplot_df$prec*100)
hplot_df$temp <- as.factor(hplot_df$temp)

p <- ggplot(data=hplot_df, aes(temp, prec)) + geom_tile(aes(fill = reldiff_all), colour = NA)
p <- p + scale_fill_gradient2(name="", low = "red", mid="white", high = "blue", 
                              midpoint=0, limits=c(-100,100),guide="colourbar")
p <- p + theme(legend.key.height=unit(2.75,"cm"),legend.key.width=unit(1,"cm"),
               legend.text=element_text(size=13),
               panel.background=element_rect(fill="white",colour="black"),
               axis.text=element_text(colour="black",size=13),
               axis.title=element_text(colour="black",size=15,face="bold"))
p <- p + labs(x = "Temperature change (K)", y = "Precipitation change (%)")

pdf(paste(figDir,"/SI-Fig06f_ecocrop_3deg_all.pdf",sep=""), height=6,width=8,pointsize=12,family="Helvetica")
print(p)
dev.off()


