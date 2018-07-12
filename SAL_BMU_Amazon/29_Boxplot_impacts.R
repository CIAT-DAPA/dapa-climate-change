###########################################################################################
## Boxplots for overall suitability change, PIA/NIA ratio, percent change in suitable area
## Author: Carlos Navarro, Marcela Beltran
## Based on previous J. Ramirez-Villegas script
###########################################################################################

# Set libraries
library(lubridate); library(ggplot2); library(reshape); library(Rcmdr); library(maptools)

# Set parameteres
cropLs <- c("cassava", "maize", "plantain")
cropLs_names <- c("Cassava", "Maize", "Plantain")
crop_experiment <- c("cassava", "maize_eitzinger_kai", "plantain_reggata_german")
iDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/impact"
oDir <- "D:/OneDrive - CGIAR/CIAT/Articles/mbeltran_crop_exposure/boxplots"
rDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/outputs"
shpSubLevel <- "Z:/WORK_PACKAGES/WP2/00_geodata/tnc_terrestial_ecoregions_napo.shp"
shp <- readShapePoly(shpSubLevel)

if(!file.exists(oDir)){  dir.create(oDir, recursive = T)}

f_names <- list("rcp26"="RCP 2.6", "rcp45"="RCP 4.5", "rcp85"="RCP 8.5")
f_labeller <- function(variable, value){return(f_names[value])}
flabel <- "Average suitablility change (%)"

#function to take countries and make mask and run all the above
suitArea <- function(csr, shp) {
  res <- (csr@extent@xmax - csr@extent@xmin)/(csr@ncols) #Resolution
  #Looping polygons
  nPol <- length(shp@polygons)
  for (p in 1:nPol) {
    cat("Pol", p, "\n")
    cname <- shp@data$COUNTRY[p]
    pol <- shp@polygons[p] #extract single polygon
    sh <- SpatialPolygons(pol) #create SP object from extracted feature
    rs <- createMask(sh, res) #create a raster from the SP object
    xy <- xyFromCell(rs, which(!is.na(rs[]))) #extract xy values from raster cells
    cv <- extract(csr, xy)
    cu <- rs; cu[which(!is.na(cu[]))] <- cv; rm(cv)
    #running impact functions
    as.nz <- areaSuitable(cu, threshold=0) #impact for g0 mask
    as.th <- areaSuitable(cu, threshold=50) #impact for g50 mask
    asm <- c(as.nz, as.th) #merge both matrices
    asm <- cbind(CID=rep(p,times=length(asm)), COUNTRY=rep(cname,times=length(asm)), THRESH=c(0,50), AREA.SUIT=asm)		
    rm(cu); rm(rs); rm(pol); rm(sh); rm(xy); gc()
    #create the matrix
    if (p == 1) {outasm <- asm} else {outasm <- rbind(outasm, asm)}
  }
  return(outasm)
}

crop_stat <- data.frame()

for (i in 1:length(cropLs)){
  
  ## Define output metrics file
  mFile <- read.csv(paste0(iDir, "/", "impacts-amz-sub_", cropLs[i], ".csv"))
  mFile <- mFile[which(mFile$THRESHOLD == 50), ]
  mFile <- mFile[which(mFile$SCEN == "no.mig"), ]
  ##mFile <- aggregate (cbind(AV.SUIT.CHG, AV.SUIT.INC, AV.SUIT.DEC)  ~RCP+PERIOD+GCM , FUN = mean, data = mFile) 
  mFile <- mFile[!(mFile$RCP == "rcp60"), ]
  
  csr <- raster(paste0(rDir, "/", crop_experiment[i], "/runs/", crop_experiment[i], "_suit.tif"))
  ca <- suitArea(csr, shp)
  ca <- as.data.frame(ca)
  ca <- ca[which(ca$THRESH==50),]
  
  
  x <- merge(mFile,ca,by="CID")
  
  x$CROP <- cropLs_names[i]
  x$CHG.AREA <- (x$AREA.SUIT.x - x$AREA.SUIT.y)/x$AREA.SUIT.y * 100
  x$PIA_NIA_RATIO <- x$AREA.SUIT.INC/x$AREA.SUIT.DEC
  
  crop_stat <- rbind(crop_stat, x)
}

write.csv(stats, paste(oDir, "/all_crops_stats_boxplot.csv", sep=""), row.names=F, quote=T)


# Make the boxplots for three variables: 
crop_stat <- read.csv(paste(oDir, "/all_crops_stats_boxplot.csv", sep=""), header = T)
# crop_stat <- crop_stat[which(crop_stat$RCP == "rcp45"), ]

##### Overall suitability changev #####

if (!file.exists(paste0(iDir, "/osc_all_crops.tif"))) {
  
  tiff(paste0(oDir, "/osc_all_crops.tif"), width=3000, height=1020, pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=CROP, y=AV.SUIT.CHG, fill=COUNTRY.x)) +
    scale_fill_manual(values=c("white", "gray")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    geom_boxplot(color="black", outlier.size = 1) +
    geom_hline(aes(yintercept=0), linetype = 2, size=0.5) +
    facet_grid(~PERIOD, drop=T, scales="free_y")+ 
    labs(x="Crop", y="OSC (%)") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_blank(), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_blank(),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_blank(), 
          legend.position="none"
    )
  
  # Plot
  print(f)
  dev.off()
  
}

# PIA/NIA ratio

if (!file.exists(paste0(iDir, "/pia_nia_ratio_all_crops.tif"))) {
  
  tiff(paste0(oDir, "/pia_nia_ratio_all_crops.tif"), width=2994, height=1020, pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=CROP, y=PIA_NIA_RATIO, fill=COUNTRY.x)) +
    scale_fill_manual(values=c("white", "gray")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    geom_boxplot(color="black", outlier.size = 1) +
    # geom_hline(aes(yintercept=0), linetype = 2, size=0.5) +
    facet_grid(~PERIOD, drop=T)+
    coord_cartesian(ylim=c(0,100))+
    scale_y_sqrt() +
    labs(x="Crop", y="PIA / NIA") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_blank(), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_blank(),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_blank(), 
          legend.position="none"
    ) 
  
  # Plot
  print(f)
  dev.off()
  
}


# Percent change in suitable area
# Calculate percent change in area (future-current)/current*100

if (!file.exists(paste0(iDir, "/chg_area_all_crops.tif"))) {
  
  tiff(paste0(oDir, "/chg_area_all_crops.tif"), width=2976, height=1020, units = "px", pointsize=12, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=CROP, y=CHG.AREA, fill=COUNTRY.x)) +
    scale_fill_manual(values=c("white", "gray")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    geom_boxplot(color="black", outlier.size = 1) +
    # geom_hline(aes(yintercept=0), linetype = 2, size=0.5) +
    facet_grid(~PERIOD, drop=T, scales="free_y")+ 
    labs(x="Crop", y="CSA (%)") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_blank(), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_blank(),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_blank(),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_blank(), 
          legend.position="none"
    ) 
  
  # Plot
  print(f)
  dev.off()
  
}
