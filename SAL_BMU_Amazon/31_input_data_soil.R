# Processing historical data in order to calculate climatic indices by counties
# H. Achicanoy, C. Navarro
# CIAT, 2017

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #
#                                                            Processing data
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= #

# ===================================================================== #
# SOIL process
# ===================================================================== #

# Load packages
options(warn=-1)
library(raster)
library(ncdf)
library(ncdf4)
library(maptools)
library(ff)
library(data.table)
library(miscTools)
library(GSIF)

#Set variables
iDir <- "Z:/DATA/WP2"
county <- "Ucayali"
region <- "amz"
obsPer <- "1981_2010"

#Load RData for coordinates 
load(paste0(iDir, "/06_Clustering_analyses/data/input_tables_lr/", county, "/prec/prec.RData"))
hist.obs <- chirps_year
rm(chirps_year)

#Load historical observations (looping through months...)
cellId <- hist.obs[[1]]$cellID
lat <- hist.obs[[1]]$lat
lon <- hist.obs[[1]]$lon
pts <- cbind(lon, lat)

## Load WISE 2015 GIS file
soil <- raster(paste0(iDir, "/02_Gridded_data/soil_30s/wise30sec_fin"))
soil_pts <- as.data.frame(extract(soil, pts, factors=TRUE))
colnames(soil_pts) <- "VALUE"
soil_pts <- cbind("ID"= cellId, "LON"= lon, "LAT"=lat, soil_pts)

# Load clasess code and merge
cod_data <- read.csv(paste0(iDir, "/02_Gridded_data/soil_30s/wise30sec.csv"), header = T)
soil_pts_class <- merge(soil_pts, cod_data, by = "VALUE", all = FALSE)

# Load characteristics and merge
soil_char <- read.csv(paste0(iDir, "/02_Gridded_data/soil_30s/HW30s_FULL.csv"), header = T)
soil_pts_class_data <- merge(soil_pts_class, soil_char, by = "NEWSUID", all = FALSE)
soil_pts_class_data <- soil_pts_class_data[order(soil_pts_class_data$ID, soil_pts_class_data$SCID, soil_pts_class_data$Layer),]

# Load mask region
rs_prj <- raster(paste(iDir, '/06_Clustering_analyses/data/', region, '_regions_rst/', county, '_base_lr.tif', sep=''))

soil_data <- as.data.frame(pts)
soil_data$id_coarse <- cellId

# 01 - ERDICM calc
erdicm <- c()
for (i in 1:length(cellId)){
  
  soil_i <- soil_pts_class_data[which(soil_pts_class_data$ID == cellId[i]), ]
  soil_i <- soil_i[which(soil_i$SCID == 1), ]
  
  UHDICM = soil_i$TopDep
  LHDICM = soil_i$BotDep
  SNDPPT = soil_i$SDTO
  SLTPPT = soil_i$STPC
  CLYPPT = soil_i$CLPC
  CRFVOL = soil_i$CFRAG
  BLD = soil_i$BULK *1000
  ORCDRC = soil_i$ORGC
  PHIHOX = soil_i$PHAQ
  CEC = soil_i$CECS
  ENA = soil_i$CECS * soil_i$ESP/100 
  EACKCL = soil_i$ECEC / soil_i$ALSA
  if (length(EACKCL[which(EACKCL == "Inf")]) > 3){
    EACKCL[which(EACKCL == "Inf")] <- 0
  } else {
    EACKCL[which(EACKCL == "Inf")] <- NA  
  }
  
  EXB = soil_i$TEB
  DRAINFAO = soil_i$Drain
  
  ECN = soil_i$ELCO
  CRB = soil_i$TCEQ
  GYP = soil_i$GYPS
  # cbind(UHDICM=UHDICM, LHDICM=LHDICM, SNDPPT=SNDPPT, SLTPPT=SLTPPT, CLYPPT=CLYPPT, CRFVOL=CRFVOL, BLD=BLD, ORCDRC=ORCDRC, PHIHOX=PHIHOX,CEC=CEC, ENA=ENA, EACKCL=EACKCL,EXB=EXB)
  
  
  x <- LRI(UHDICM=UHDICM, LHDICM=LHDICM, SNDPPT=SNDPPT, 
           SLTPPT=SLTPPT, CLYPPT=CLYPPT, CRFVOL=CRFVOL, 
           BLD=BLD, ORCDRC=ORCDRC, PHIHOX=PHIHOX,CEC=CEC, 
           ENA=ENA, EACKCL=EACKCL, EXB=EXB, 
           ECN=ECN, CRB=CRB, GYP=GYP, 
           print.thresholds=TRUE)

  sel <- x==FALSE
  if(!all(sel==FALSE)){ 
    UHDICM[which(sel==TRUE)[1]] 
  } else {
    max(LHDICM)
  }
  
  xI <- attr(x, "minimum.LRI")
  ## derive Effective rooting depth:
  erdicm_i <- ERDICM(UHDICM=UHDICM, LHDICM=LHDICM, minimum.LRI=xI, DRAINFAO=DRAINFAO )
  
  erdicm <- c(erdicm, erdicm_i)
}

erdicm <- cbind(id_coarse=cellId, erdicm)
soil_data <- merge(soil_data, erdicm, by="id_coarse")


# 02 - Extract soil water holding capacity data on soil_data data.frame
depths <- c(25,100,225,450,800,1500)
rs <- stack(paste(iDir,"/02_Gridded_data/soil_30s/AWCh2_M_sl",1:6,"_1km_ll.tif",sep=""))
rs <- crop(rs, extent(rs_prj))
rs_res <- resample(rs, rs_prj, method="ngb")
rs_res_pts <- extract(rs_res, pts)
colnames(rs_res_pts) <- paste("d.",depths,sep="")
rs_res_pts <- cbind(id_coarse=cellId, rs_res_pts)
  
soil_data <- merge(soil_data, rs_res_pts, by="id_coarse")


# 03 - Soil capacity
soilcap_calc_mod <- function(x, minval, maxval) {
  if(!is.na(x[4])){
    rdepth <- max(c(x[4],minval)) #cross check
    rdepth <- min(c(rdepth,maxval)) #cross-check
    wc_df <- data.frame(depth=c(2.5,10,22.5,45,80,150),wc=(x[5:10])*.01)
    if (!rdepth %in% wc_df$depth) {
      wc_df1 <- wc_df[which(wc_df$depth < rdepth),]
      wc_df2 <- wc_df[which(wc_df$depth > rdepth),]
      y1 <- wc_df1$wc[nrow(wc_df1)]; y2 <- wc_df2$wc[1]
      x1 <- wc_df1$depth[nrow(wc_df1)]; x2 <- wc_df2$depth[1]
      ya <- (rdepth-x1) / (x2-x1) * (y2-y1) + y1
      wc_df <- rbind(wc_df1,data.frame(depth=rdepth,wc=ya),wc_df2)
    }
    wc_df <- wc_df[which(wc_df$depth <= rdepth),]
    wc_df$soilthick <- wc_df$depth - c(0,wc_df$depth[1:(nrow(wc_df)-1)])
    wc_df$soilcap <- wc_df$soilthick * wc_df$wc
    soilcp <- sum(wc_df$soilcap) * 10 #in mm
    return(soilcp)
  } else {
    soilcp <- NA
    return(soilcp)
  }
}

# Calculate soil water holding capacity in mm, minval and maxval taken from
# Fatondji et al. (2012) --in: Kihara, J. et al. Improving soil fert. recommendation using DSSAT
soil_data$soilcp <- apply(soil_data, 1, FUN=soilcap_calc_mod, minval=45, maxval=100)
soil_data_county <- cbind(cellId, lon, lat, soil_data)
names(soil_data_county) <- c("cellID", "lon.x", "lat.x", "id_coarse", "lon.y", "lat.y", "rdepth", "d.25", "d.100", "d.225", "d.450", "d.800", "d.1500", "soilcp")

## Save results
counDir <- paste(iDir, "/06_Clustering_analyses/data/input_tables_lr/", county, "/soil", sep='')
if(!dir.exists(counDir)){ dir.create(counDir, recursive=T) }
save(soil_data_county, file=paste(counDir, '/soil_data.RData', sep=''))
  