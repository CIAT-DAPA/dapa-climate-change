#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015
stop("!")

#1. Extract daily GCM data (HIS and all RCPs) for Brazilian locations for future TPE study
#2. Get observations into format needed by CN/JT scripts
#3. Run bias correction, summary stats / plots
#4. Create oryza2000 files as needed

library(sp); library(maptools); library(raster); library(rgeos)
library(soiltexture); library(lubridate)
#library(ggplot2); library(tools); library(reshape); require(grid)

#source code directory
src.dir <- "~/Repositories/dapa-climate-change/rice-future-tpe"

#directories
#wd <- "/nfs/a101/earjr/rice-future-tpe"
wd <- "~/Leeds-work/rice-future-tpe"
res_dir <- paste(wd,"/oryza_output",sep="")
obs_dir <- paste(wd,"/obs_meteorology",sep="")
gcm_dir <- paste(wd,"/gcm_meteorology",sep="")
an_dir <- paste(res_dir,"/analysis",sep="")
if (!file.exists(an_dir)) {dir.create(an_dir)}

#load functions
source(paste(src.dir,"/thiessen_polygons.R",sep=""))

#load Brazil shapefile
bra_shp <- readRDS(paste(wd,"/data/BRA_adm1.rds",sep=""))
bra_shp <- bra_shp[which(bra_shp$ID_1 == 7 | bra_shp$ID_1 == 9 | bra_shp$ID_1 == 12 | bra_shp$ID_1 == 22 | bra_shp$ID_1 == 27),]

#location list
loc_list <- read.csv(paste(obs_dir,"/all_wst_locs.csv",sep=""))

#construct list of final weather stations
#remove those that were not processed for this analysis, and
#remove CALDAS NOVAS (.IPGO.00007), as it was not used in Heinemann et al. (2015) JXB
#number of stations per state from JXB paper: 3 TO; 7 RO; 16 MT; 25 GO
loc_list$allyears <- T
for (wst in loc_list$id) {
  #wst <- paste(loc_list$id[1])
  wst_name <- gsub(".","",wst,fixed=T)
  wst_odir <- paste(gcm_dir,"/loc_",wst_name,"/obs",sep="")
  if (!file.exists(wst_odir)) {loc_list$allyears[which(loc_list$id == wst)] <- F}
}
loc_list <- loc_list[which(loc_list$allyears),]
loc_list <- loc_list[which(loc_list$id != ".IPGO.00007"),]
row.names(loc_list) <- NULL

#create thiessen polygons from weather stations locations
wst_xy <- loc_list[,c("lon","lat")]; names(wst_xy) <- c("x","y")
thiepol <- voronoipolygons(wst_xy)
proj4string(thiepol)=CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
f_thiepol <- gIntersection(thiepol, bra_shp, byid=T)
id_list <- data.frame(ID=sapply(slot(f_thiepol,'polygons'),function(x) {slot(x,'ID')}))
id_list$WST_ID <- sapply(id_list$ID, function(x) {as.numeric(unlist(strsplit(paste(x)," ",fixed=T))[1])})
id_list$OBJECTID <- sapply(id_list$ID, function(x) {as.numeric(unlist(strsplit(paste(x)," ",fixed=T))[2])})
wst_xy$WST_ID <- row.names(wst_xy)
id_list <- merge(id_list, wst_xy, by="WST_ID")
names(id_list)[4:5] <- c("x_wst","y_wst")
row.names(id_list) <- id_list$ID; id_list <- id_list[,c("ID","WST_ID","OBJECTID","x_wst","y_wst")]
names(id_list)[1] <- "id"
fpols <- SpatialPolygonsDataFrame(f_thiepol, id_list, match.ID=T)

#load soil data
soil_data <- read.csv(paste(wd,"/data/BrazilSoilDB_08VI05_NAfilled.csv",sep=""))

#extract historical simulated data from 7z file
setwd(res_dir)
system("7z x Historical_Files.7z -aos")

#load results files
op_data <- read.csv("OP_historical.csv") #this is the seasonal output (for end-of-season yield)
rs_data <- read.csv("RES_historical.csv", sep=";") #this is the 5-day output (for stress profile)
st_data <- read.csv("env_cluster.csv", sep=";")

#convert ID from factors to character
rs_data$ID <- paste(rs_data$ID)
st_data$ID <- paste(st_data$ID)

#determine if the same profiles are present
rs_un <- unique(paste(rs_data$ID))
st_un <- unique(paste(st_data$ID))
miss <- rs_un[which(!rs_un %in% st_un)] #there are 579 missing stress_profile due to failed season

#merge res data.frame and stress_profile data.frame
rs_data <- merge(rs_data, st_data[,c("ID","clust")], by="ID", all.x=T)

#add Year and DOY to rs_data
rs_data$Year <- as.numeric(sapply(rs_data$ID, FUN=function(x) {y <- unlist(strsplit(x,"_",fixed=T)); y <- as.numeric(y[length(y)]); return(y)}))
rs_data$DOY <- as.numeric(format(as.Date(paste(rs_data$data),format="%d/%m/%Y"), "%j"))
names(rs_data)[grep("clust",names(rs_data))] <- "stress_profile"

#correct coordinates of Catalao and Cristalina
op_data$Lat[which(op_data$Station == "Catalao")] <- loc_list$lat[which(loc_list$municipio == "CATALAO")]
op_data$Long[which(op_data$Station == "Catalao")] <- loc_list$lon[which(loc_list$municipio == "CATALAO")]
op_data$Lat[which(op_data$Station == "Cristalina")] <- loc_list$lat[which(loc_list$municipio == "CRISTALINA")]
op_data$Long[which(op_data$Station == "Cristalina")] <- loc_list$lon[which(loc_list$municipio == "CRISTALINA")]


#make a spatial layer that can be used to plot yields for any given scenario
#1. for each soil profile, extract the weather station they belong to, create a table that says
#   weather_station, OrgProfID, latitude of soil profile, longitude of soil profile
#2. construct a raster where grid cell size is the minimum distance between any two soil profiles
#3. construct a data.frame assign each grid cell the values from the closest profile x weather
#   station combination.

#notes:
#* soil profile depth is 50 cm for Oryza model, so just get for each profile the average 
#  Silt, Clay and Sand contents up to this depth. Then use the soil triangle to get the 
#  soil class.

#unique soil profiles
soil_pts <- unique(soil_data[,c("Long","Lat")])

#reference soil classes; only those that are in op_data
soil_class <- data.frame(soil_sim=c("C","CL","SC","SCL","SL","S","L"), 
                         soil_txt=c("Cl","ClLo","SaCl","SaClLo","SaLo","Sa","Lo"))

#1. construct data frame with soil texture, soil profile data, and weather station information
if (!file.exists(paste(an_dir,"/soil_texture_locations.csv",sep=""))) {
  #loop points to perform procedure
  soil_txt <- data.frame()
  for (s_i in 1:nrow(soil_pts)) {
    #s_i <- 1
    #get coordinates
    lon <- soil_pts$Long[s_i]; lat <- soil_pts$Lat[s_i]
    
    #get weather station where belongs
    coords <- SpatialPoints(data.frame(longitude=lon,latitude=lat))
    spdf_pts <- SpatialPointsDataFrame(coords, data.frame(longitude=lon,latitude=lat))
    proj4string(spdf_pts) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    wst_pol <- over(spdf_pts, fpols)
    
    #if the soil profile is within a wst polygon
    if (!is.na(wst_pol$id)) {
      #get soil profile at depth < 50cm
      this_soil <- soil_data[which(soil_data$Long == lon & soil_data$Lat == lat),c("OrgProfID","HzSimb","HzDeIn","HzDeFn","HzVal","Silt","Clay","Sand")]
      
      #check for NAs
      this_soil <- this_soil[which(!is.na(this_soil$HzDeIn)),]
      this_soil <- this_soil[which(!is.na(this_soil$HzDeFn)),]
      this_soil <- this_soil[which(!is.na(this_soil$Silt)),]
      this_soil <- this_soil[which(!is.na(this_soil$Sand)),]
      this_soil <- this_soil[which(!is.na(this_soil$Clay)),]
      
      if (nrow(this_soil) > 0) {
        cat("...processing soil profile=",s_i,"out of n=",nrow(soil_pts),"\n")
        
        #do horizon stuff, Oryza model was run with rooting depth of 50cm
        if (min(this_soil$HzDeIn) <= 50) {
          this_soil <- this_soil[which(this_soil$HzDeIn <= 50),]
          this_soil$HzDeFn[which(this_soil$HzDeFn > 50)] <- 50
        }
        this_soil$HzThck <- this_soil$HzDeFn - this_soil$HzDeIn
        
        #if more than 1 horizon calculate weighted average of silt, clay and sand
        if (nrow(this_soil) > 1) {
          this_soil$silt_w <- this_soil$Silt * this_soil$HzThck
          this_soil$sand_w <- this_soil$Sand * this_soil$HzThck
          this_soil$clay_w <- this_soil$Clay * this_soil$HzThck
          silt <- sum(this_soil$silt_w) / sum(this_soil$HzThck)
          sand <- sum(this_soil$sand_w) / sum(this_soil$HzThck)
          clay <- sum(this_soil$clay_w) / sum(this_soil$HzThck)
        } else {
          silt <- this_soil$Silt; sand <- this_soil$Sand; clay <- this_soil$Clay
        }
        
        #if total > 100 then correct
        tot_cont <- sum(c(silt,sand,clay))
        if (tot_cont != 100) {
          silt <- silt + (100-tot_cont)/3
          sand <- sand + (100-tot_cont)/3
          clay <- clay + (100-tot_cont)/3
          cat("   ...had to correct since total was=",tot_cont,"% instead of 100 %\n")
        }
        
        #get soil texture
        soiltex <- TT.points.in.classes(tri.data=data.frame(CLAY=clay, SILT=silt, SAND=sand), class.sys="USDA.TT")
        soiltex <- colnames(soiltex)[which(soiltex[1,] != 0)]
        if (length(soiltex) > 1) {soiltex <- soiltex[1]}
        
        #output
        odf <- data.frame(OrgProfID=this_soil$OrgProfID[1], Long=lon, Lat=lat, Silt=silt, Clay=clay,
                          Sand=sand,Texture=soiltex)
        odf <- cbind(odf, wst_pol)
        soil_txt <- rbind(soil_txt, odf)
      }
    }
  }
  
  loc_list2 <- loc_list[,c("lon","lat","id","elev","uf","municipio")]
  names(loc_list2) <- c("x_wst","y_wst","id_wst","elev_wst","uf_wst","municipio_wst")
  soil_txt <- merge(soil_txt, loc_list2, by=c("x_wst","y_wst")); rm(loc_list2)
  soil_txt <- soil_txt[,c("OrgProfID","Long","Lat","Silt","Clay","Sand","Texture","id","WST_ID",
                          "OBJECTID","x_wst","y_wst","id_wst","elev_wst","uf_wst","municipio_wst")]
  
  #select only those textures in the reference table
  table(soil_txt$Texture)
  soil_txt <- soil_txt[which(soil_txt$Texture %in% soil_class$soil_txt),]
  soil_txt$Texture <- paste(soil_txt$Texture)
  
  #write file
  write.csv(soil_txt, paste(an_dir,"/soil_texture_locations.csv",sep=""),quote=T,row.names=F)
} else {
  soil_txt <- read.csv(paste(an_dir,"/soil_texture_locations.csv",sep=""))
}
#plot(fpols); points(soil_txt$Long, soil_txt$Lat, pch=20,col="blue")

###
#based on post-hoc analysis of correspondence between simulations and available soil profiles
#per weather region, remove those soil profiles x weather stations that were not simulated

if (!file.exists(paste(an_dir,"/soil_texture_locations_clean.csv",sep=""))) {
  tab_spat <- as.matrix(table(unique(soil_txt[,c("municipio_wst","Texture")])))
  tab_spat <- as.data.frame(tab_spat[,])
  tab_spat <- cbind(Station=row.names(tab_spat),tab_spat)
  row.names(tab_spat) <- NULL
  names(tab_spat) <- c("Station",paste(names(tab_spat)[2:length(names(tab_spat))],"_spat",sep=""))
  
  tab_runs <- as.matrix(table(unique(op_data[,c("Station","Soil")])))
  tab_runs <- as.data.frame(tab_runs[,])
  tab_runs <- cbind(Station=row.names(tab_runs),tab_runs)
  row.names(tab_runs) <- NULL
  names(tab_runs) <- c("Station",paste(names(tab_runs)[2:length(names(tab_runs))],"_runs",sep=""))
  
  sclass_list <- c("Cl","ClLo","Lo","Sa","SaCl","SaClLo","SaLo")
  for (i in 1:nrow(tab_runs)) {
    #i <- 24
    wst_name <- paste(tab_spat$Station[i])
    row_spat <- tab_spat[i,]
    row_runs <- tab_runs[i,]
    row_sum <- as.numeric(row_runs[,2:ncol(tab_runs)] - row_spat[,2:ncol(tab_spat)])
    sclass <- sclass_list[which(row_sum == -1)]
    if (length(sclass) >= 1) {
      cat("...removing soil(s)=",sclass,"from wst=",wst_name,"due to missing simulations\n")
      for (scl in sclass) {
        soil_txt <- soil_txt[-which(soil_txt$municipio_wst == wst_name & soil_txt$Texture == scl),]
      }
    }
  }
  write.csv(soil_txt, paste(an_dir,"/soil_texture_locations_clean.csv",sep=""),quote=T,row.names=F)
} else {
  soil_txt <- read.csv(paste(an_dir,"/soil_texture_locations_clean.csv",sep=""))
}

#2. construct raster
if (!file.exists(paste(an_dir,"/rs_reference.tif",sep=""))) {
  #minimum distance between any two profiles in the soil database for study area
  mindist <- pointDistance(soil_txt[,c("Long","Lat")], lonlat=F)
  mindist <- unique(as.vector(mindist))
  mindist <- mindist[which(mindist != 0)]
  mindist <- min(mindist)
  
  #make raster
  rs_ext <- extent(fpols)
  rs_ext@xmin <- rs_ext@xmin - 0.5; rs_ext@xmax <- rs_ext@xmax + 0.5
  rs_ext@ymin <- rs_ext@ymin - 0.5; rs_ext@ymax <- rs_ext@ymax + 1.5
  rs_ref <- raster(ext=rs_ext, resolution=mindist)
  rs_ref[] <- 1:ncell(rs_ref)
  rs_ref <- rasterize(fpols, rs_ref)
  rs_ref[which(!is.na(rs_ref[]))] <- 1
  rs_ref <- writeRaster(rs_ref,paste(an_dir,"/rs_reference.tif",sep=""),format="GTiff")
} else {
  rs_ref <- raster(paste(an_dir,"/rs_reference.tif",sep=""))
}
#plot(rs_ref); plot(fpols,add=T); plot(bra_shp,add=T)

#3. create lookup data frame so rasters of any variable can be plotted easily
if (!file.exists(paste(an_dir,"/df_reference.RData",sep=""))) {
  df_ref <- as.data.frame(xyFromCell(rs_ref, which(!is.na(rs_ref[]))))
  df_ref <- cbind(cell_id=which(!is.na(rs_ref[])), df_ref)
  
  #function to merge both data.frames (soils x weather stations & raster)
  merge_df <- function(df_i, ref_df, soil_df) {
    #df_i <- 1
    rs_x <- ref_df$x[df_i]; rs_y <- ref_df$y[df_i]
    pdist <- pointDistance(cbind(lon=rs_x, lat=rs_y), soil_df[,c("Long","Lat")], lonlat=F)
    tsoil <- soil_df[which(pdist == min(pdist)),c("Long","Lat","Silt","Clay","Sand","WST_ID","OBJECTID","x_wst","y_wst","elev_wst")]
    if (nrow(tsoil) > 1) {tsoil <- tsoil[1,]}
    outrow <- cbind(ref_df[df_i,],tsoil)
    names(outrow) <- c("cell_id","x","y","x_soil","y_soil","Silt","Clay","Sand","WST_ID","OBJECTID","x_wst","y_wst","elev_wst")
    return(outrow)
  }
  
  ref_out <- as.data.frame(t(sapply(1:nrow(df_ref), FUN=merge_df, ref_df=df_ref, soil_df=soil_txt)))
  #ref_out <- as.data.frame(t(sapply(1:100, FUN=merge_df, ref_df=df_ref, soil_df=soil_txt)))
  
  #fix data.frame
  ref_out$cell_id <- as.numeric(paste(ref_out$cell_id))
  ref_out$x <- as.numeric(paste(ref_out$x))
  ref_out$y <- as.numeric(paste(ref_out$y))
  ref_out$x_soil <- as.numeric(paste(ref_out$x_soil))
  ref_out$y_soil <- as.numeric(paste(ref_out$y_soil))
  ref_out$Silt <- as.numeric(paste(ref_out$Silt))
  ref_out$Clay <- as.numeric(paste(ref_out$Clay))
  ref_out$Sand <- as.numeric(paste(ref_out$Sand))
  ref_out$WST_ID <- as.numeric(paste(ref_out$WST_ID))
  ref_out$OBJECTID <- as.numeric(paste(ref_out$OBJECTID))
  ref_out$x_wst <- as.numeric(paste(ref_out$x_wst))
  ref_out$y_wst <- as.numeric(paste(ref_out$y_wst))
  ref_out$elev_wst <- as.numeric(paste(ref_out$elev_wst))
  
  save(ref_out, file=paste(an_dir,"/df_reference.RData",sep=""))
} else {
  load(file=paste(an_dir,"/df_reference.RData",sep=""))
}


#############################################################################################
### create raster of yield (or of any variable -just change WRR14 for variable of interest)
#############################################################################################

if (!file.exists(paste(an_dir,"/yield_historical_raster_data.RData",sep=""))) {
  op_data$Em_Dat <- paste(op_data$Em_Dat)
  for (emd in unique(op_data$Em_Dat)) {
    #emd <- unique(op_data$Em_Dat)[1]
    emd_data <- op_data[which(op_data$Em_Dat == emd),]
    emd_data$Em_Dat <- NULL
    
    #aggregate
    emd_mean <- aggregate(emd_data[,c("Lat","Long","WRR14")], by=list(Station=emd_data$Station, Soil=emd_data$Soil), FUN=function(x) {mean(x,na.rm=T)})
    emd_std <- aggregate(emd_data[,c("Lat","Long","WRR14")], by=list(Station=emd_data$Station, Soil=emd_data$Soil), FUN=function(x) {sd(x,na.rm=T)})
    emd_cv <- aggregate(emd_data[,c("Lat","Long","WRR14")], by=list(Station=emd_data$Station, Soil=emd_data$Soil), FUN=function(x) {sd(x,na.rm=T)/mean(x,na.rm=T)*100})
    emd_mean$WRR14sd <- emd_std$WRR14; emd_mean$WRR14cv <- emd_cv$WRR14
    rm(list=c("emd_std","emd_cv"))
    
    #rename columns
    names(emd_mean)[grep("WRR14",names(emd_mean))] <- paste(names(emd_mean)[grep("WRR14",names(emd_mean))],"_",emd,sep="")
    
    if (emd == unique(op_data$Em_Dat)[1]) {
      mean_op <- emd_mean
    } else {
      mean_op <- merge(mean_op, emd_mean, by=c("Station","Soil","Lat","Long"),all.x=T,all.y=T)
    }
  }
  
  mean_op <- merge(mean_op, soil_class, by.x="Soil", by.y="soil_sim")
  mean_op$soil_txt <- paste(mean_op$soil_txt)
  names(mean_op)[3:4] <- c("y_wst","x_wst")
  
  #merge data so that a single data.frame with yield values can be obtained
  yield_rs <- merge(ref_out, soil_txt[,c("Long","Lat","OrgProfID","Texture","id","WST_ID","OBJECTID","x_wst","y_wst","id_wst","uf_wst","municipio_wst")], 
                    by.x=c("x_soil","y_soil","WST_ID","OBJECTID","x_wst","y_wst"),
                    by.y=c("Long","Lat","WST_ID","OBJECTID","x_wst","y_wst"))
  yield_rs <- merge(yield_rs, mean_op, by.x=c("x_wst","y_wst","Texture"), 
                    by.y=c("x_wst","y_wst","soil_txt"),all.x=T)
  
  #mean yield of all planting dates
  yield_rs$YIELD_mean <- rowMeans(yield_rs[,grep("WRR14_",names(yield_rs),fixed=T)], na.rm=T)
  yield_rs$YIELD_std <- rowMeans(yield_rs[,grep("WRR14sd_",names(yield_rs),fixed=T)], na.rm=T)
  yield_rs$YIELD_cv <- rowMeans(yield_rs[,grep("WRR14cv_",names(yield_rs),fixed=T)], na.rm=T)
  
  #make rasters
  rs_ymean <- rs_ystd <- rs_ycv  <- rs_ref
  rs_ymean[yield_rs$cell_id] <- yield_rs$YIELD_mean
  rs_ystd[yield_rs$cell_id] <- yield_rs$YIELD_std
  rs_ycv[yield_rs$cell_id] <- yield_rs$YIELD_cv
  #plot(rs_ymean); plot(fpols, add=T)
  #plot(rs_ystd); plot(fpols, add=T)
  #plot(rs_ycv); plot(fpols, add=T)
  
  #save objects
  save(list=c("mean_op","yield_rs","rs_ymean","rs_ystd","rs_ycv"),file=paste(an_dir,"/yield_historical_raster_data.RData",sep=""))
} else {
  load(file=paste(an_dir,"/yield_historical_raster_data.RData",sep=""))
}


#############################################################################################
### probability plot
#############################################################################################

#put stress_profile into op_data
st_data$station <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[1]}))
st_data$soil <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[2]}))
st_data$sdate <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[3]}))
st_data$lat <- as.numeric(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[4]}))
st_data$lon <- as.numeric(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[5]}))
st_data$municipio <- as.vector(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[6]}))
st_data$year <- as.numeric(sapply(st_data$ID, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[7]}))
st_data$sdate[which(st_data$sdate == "1/11")] <- "01/11"

if (!file.exists(paste(an_dir,"/yield_probability_historical.RData",sep=""))) {
  run_data <- op_data; run_data$Em_Dat <- paste(run_data$Em_Dat)
  run_data <- merge(run_data, st_data[,c("clust","station","soil","sdate","year")],
                    by.x=c("Station","Soil","Em_Dat","Harvest_Year"),
                    by.y=c("station","soil","sdate","year"), all.x=T)
  names(run_data)[grep("clust",names(run_data),fixed=T)] <- "stress_cluster"
  names(run_data)[grep("Cluster",names(run_data),fixed=T)] <- "env_cluster"
  
  print(table(run_data[,c("env_cluster","stress_cluster")]))
  
  #loop clusters and stress profiles to get cum. frequency plot
  quant_df <- data.frame()
  for (clus in unique(run_data$env_cluster)) {
    #clus <- unique(run_data$env_cluster)[1]
    clus_data <- run_data[which(run_data$env_cluster == clus),]
    for (stress in c(0,unique(clus_data$stress_cluster))) {
      #stress <- c(0,unique(run_data$stress_cluster))[1]
      if (!is.na(stress)) {
        if (stress == 0) {stress_data <- clus_data} else {stress_data <- clus_data[which(clus_data$stress_cluster == stress),]}
        qdf <- as.numeric(quantile(stress_data$WRR14, probs=seq(0,1,by=0.01), na.rm=T))
        qdf <- as.data.frame(t(as.matrix(qdf)))
        names(qdf) <- paste("p",0:100,sep="")
        qdf <- cbind(env_cluster=clus, stress_cluster=stress, qdf)
        quant_df <- rbind(quant_df, qdf)
      }
    }
  }
  save(list=c("quant_df"),file=paste(an_dir,"/yield_probability_historical.RData",sep=""))
  #par(mar=c(5,5,1,1),lwd=1.5,las=1)
  #plot(as.numeric(quant_df[1,paste("p",0:100,sep="")]), 0:100/100, xlim=c(0,7000), ylim=c(0,1), ty="l", lwd=2.5, col="red", xlab=expression(Yield~(kg~ha^{-1})), ylab="Cumulative probability")
  #lines(as.numeric(quant_df[4,paste("p",0:100,sep="")]), 0:100/100, col="dark green", lwd=2.5)
  #lines(as.numeric(quant_df[8,paste("p",0:100,sep="")]), 0:100/100, col="blue", lwd=2.5)
  #box(); abline(h=0.5); grid()
} else {
  load(file=paste(an_dir,"/yield_probability_historical.RData",sep=""))
}




