####################################################
library(lubridate); library(ggplot2); library(reshape)
cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "bean", "rice", "cocoa", "sugar_cane", "panela_cane")
cropLs_names <- c("Maize", "Cassava", "Plantain", " Beans", "Rice", "Cocoa", "Sugar Cane", "Panela Cane")
iDir <- "Z:/DATA/WP2/05_EcoCrop_runs/impact"
oDir <- "Z:/DATA/WP2/05_EcoCrop_runs/evaluation/boxplots"

if(!file.exists(oDir)){
  dir.create(oDir, recursive = T)
}

f_names <- list("rcp26"="RCP 2.6", "rcp45"="RCP 4.5", "rcp60"="RCP 6.0", "rcp85"="RCP 8.5", "Napo"="Napo", "Ucayali"="Ucayali")
f_labeller <- function(variable, value){return(f_names[value])}
flabel <- "Average Suitability Change (%)"

for (i in 1:length(cropLs)){
  
  ## Define output metrics file
  mFile <- read.csv(paste0(iDir, "/impacts-", cropLs[i],"/impacts-amz-sub.csv"))
  mFile <- mFile[which(mFile$THRESHOLD == 0), ]
  mFile <- mFile[which(mFile$SCEN == "un.mig"), ]
  
  # if (!file.exists(paste0(iDir, "/avg_suitchange_", crop, ".tif"))) {
        
    tiff(paste0(oDir, "/avg_suitchange_", cropLs[i], ".tif"), width=800, height=600, pointsize=8, compression='lzw',res=100)
        
    f <- ggplot(data=mFile, aes(x=PERIOD, y=AV.SUIT.CHG, fill=PERIOD)) +
          theme(panel.background = element_rect(fill = 'gray92'), axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks=element_blank(), legend.title=element_blank(), axis.title.y = element_text(size = rel(1.2))) +
          # scale_fill_manual(values=c(green2, orange, red, red2, blue2, blue3, "white")) +
          ggtitle(paste0(cropLs_names[i])) +   
          geom_boxplot(outlier.size = 1) +
          labs(x="Scenario", y=flabel) +
          facet_grid(COUNTRY~RCP, drop=T)
        
    # Plot
    print(f)
    dev.off()
  # }
}



## Boxplots for overall suitability change, PIA/NIA ratio, percent change in suitable area

cDir <- "E:/EcoCrop-development/Modelling/groundnut/analyses/impacts"
library(Rcmdr)
#3. make boxplots for regions of interest
#   open the file "impacts-starea-countries.shp.csv"
imets <- read.csv(paste(cDir,"/impacts-starea-countries.shp.csv",sep=""))

#   sub-select SCEN=no.mig and THRESHOLD=0
imets <- imets[imets$SCEN=="no.mig",]
imets <- imets[imets$THRESHOLD==0,]

#   open the file "Country-regions-LO2008.csv"
regs <- read.csv("E:/EcoCrop-development/EcoCrop-src/Country-regions-groundnutR.csv")

#   merge the region names with the other data
x <- merge(imets,regs,by="CID")
write.csv(x, paste(cDir, "/tmp.csv", sep=""), row.names=F, quote=T)

#   open "current-area-starea-countries.shp.csv" and merge it with the first one
ca <- read.csv(paste(cDir,"/current-area-starea-countries.shp.csv",sep=""))
ca <- ca[which(ca$THRESH==0),]
x <- merge(x,ca,by="CID")



#   open "future-area-starea-countries.shp.csv" and merge it with the first one
fa <- read.csv(paste(cDir,"/future-area-starea-countries.shp.csv",sep=""))
fa <- fa[which(fa$THRESH==0),]
x <- merge(x,fa,by=c("CID","GCM"))

#   remove extra fields
x$COUNTRY.y <- NULL
x$COUNTRY <- NULL
x$THRESH.x <- NULL; x$THRESH.y <- NULL
x$Country <- NULL
write.csv(x, paste(cDir, "/tmp.csv", sep=""), row.names=F, quote=T)
#   calculate percent change in area (future-current)/current*100
x$CHG.AREA <- (x$AREA.SUIT-x$AREA.SUIT.y)/x$AREA.SUIT.y * 100

#   make the boxplots for three variables: 
#           overall suitability change

count=numSummary(x$AV.SUIT.CHG,groups=x$Region,statistics=c("mean"))
valids=row.names(count$n[count$n>6])

tiff(paste(cDir,"/osc-boxplot.tif",sep=""),res=300,pointsize=10,width=800,
     height=1000,units="px",compression="lzw")
par(mar=c(6,4,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(x$AV.SUIT.CHG[x$Region %in%valids]~as.character(x$Region[x$Region %in%valids]),col="grey",boxwex=0.5,
        pch=20,outwex=0.2,horizontal=T,ylim=c(-100,100),
        xlab="Overall suitability change (%)")
abline(v=0,lwd=0.7,lty=2)
abline(v=seq(-100,100,by=25),lwd=0.6,lty=2,col="grey50")
dev.off()

#           PIA/NIA ratio
x$PIA_NIA_RATIO <- x$AREA.SUIT.INC/x$AREA.SUIT.DEC
count=numSummary(x$PIA_NIA_RATIO,groups=x$Region,statistics=c("mean"))
valids=row.names(count$n[count$n>6])



tiff(paste(cDir,"/pia_nia_ratio-boxplot.tif",sep=""),res=300,pointsize=10,width=800,
     height=1000,units="px",compression="lzw")
par(mar=c(6,4,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(x$PIA_NIA_RATIO[x$Region %in%valids]~as.character(x$Region[x$Region %in%valids]),col="grey",boxwex=0.5,
        pch=20,outwex=0.2,horizontal=T,ylim=c(0,100),
        xlab="Positively to negatively impacted area ratio")
abline(v=0,lwd=0.7,lty=2)
abline(v=seq(0,100,by=20),lwd=0.6,lty=2,col="grey50")
dev.off()


#           percent change in suitable area
tiff(paste(cDir,"/chg_area-boxplot.tif",sep=""),res=300,pointsize=10,width=800,
     height=1000,units="px",compression="lzw")
par(mar=c(6,4,1,1),cex=0.6,las=2,lwd=0.6)
boxplot(x$CHG.AREA[x$Region %in%valids]~as.character(x$Region[x$Region %in%valids]),col="grey",boxwex=0.5,
        pch=20,outwex=0.2,horizontal=T,ylim=c(-100,325),
        xlab="Change in area suitable (%)")
abline(v=0,lwd=0.7,lty=2)
abline(v=seq(-100,325,by=25),lwd=0.6,lty=2,col="grey50")
dev.off()

