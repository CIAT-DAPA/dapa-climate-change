#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#July 2015
stop("!")

#load libraries
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

#soil data
soil_data <- read.csv(paste(wd,"/data/BrazilSoilDB_08VI05_NAfilled.csv",sep=""))
soil_pts <- unique(soil_data[,c("Long","Lat")])
soil_class <- data.frame(soil_sim=c("C","CL","SC","SCL","SL","S","L"), 
                         soil_txt=c("Cl","ClLo","SaCl","SaClLo","SaLo","Sa","Lo"))
soil_txt <- read.csv(paste(an_dir,"/soil_texture_locations_clean.csv",sep=""))

#load reference raster and data.frame
rs_ref <- raster(paste(an_dir,"/rs_reference.tif",sep=""))
load(file=paste(an_dir,"/df_reference.RData",sep=""))

#merge for base of yield raster
yield_df <- merge(ref_out, soil_txt[,c("Long","Lat","OrgProfID","Texture","id","WST_ID","OBJECTID","x_wst","y_wst","id_wst","uf_wst","municipio_wst")], 
                  by.x=c("x_soil","y_soil","WST_ID","OBJECTID","x_wst","y_wst"),
                  by.y=c("Long","Lat","WST_ID","OBJECTID","x_wst","y_wst"))

#lists of factors
bclist <- c("cf","del")
gcmlist <- list.files(paste(gcm_dir,"/loc_CNPAF1/gcm",sep=""))
gcmlist <- gcmlist[which(gcmlist != "cesm1_cam5")]
gcmlist <- gcmlist[which(gcmlist != "ncar_ccsm4")]
gcmlist <- gcmlist[which(gcmlist != "mri_cgcm3")]
gcmlist <- gcmlist[which(gcmlist != "ipsl_cm5a_lr")]
rcplist <- c("rcp26","rcp45","rcp60","rcp85")
co2list <- c("High","Low")

#1. produce map of mean yield for each GCM
#unzip j2 files
setwd(res_dir)
system("7z x Arquivos_J2.zip -aos")

#loop to get output as rasters
for (rcp in rcplist) {
  #rcp <- rcplist[1]
  
  if (!file.exists(paste(an_dir,"/yield_cv_rasters_",rcp,".RData",sep=""))) {
    #output master list for RCP
    alldata_list <- ymean_list <- ystd_list <- ycv_list <- list()
    
    for (gcm in gcmlist) {
      #gcm <- gcmlist[1]
      for (bc in bclist) {
        #bc <- bclist[1]
        for (co2p in co2list) {
          #co2p <- co2list[1]
          co2i <- substr(co2p,1,1)
          cat("...processing rcp=",rcp,"/ gcm=",gcm,"/ bc_method=",bc,"/ co2=",co2p,"\n")
          
          #read data
          fname <- paste(res_dir,"/Arquivos_J2/",co2p,"/","J2_op_",co2i,"_method_",bc,"_",rcp,"_",gcm,".csv",sep="")
          run_data <- read.csv(fname, sep=";")
          run_data$station <- as.vector(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[1]}))
          run_data$soil <- as.vector(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[2]}))
          run_data$sdate <- as.numeric(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[3]}))
          run_data$municipio <- as.vector(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[4]}))
          run_data <- merge(run_data, soil_class, by.x="soil", by.y="soil_sim")
          
          #put lon,lat into run_data
          loc_data <- loc_list
          loc_data$mun_locase <- gsub(" ", "", tolower(paste(loc_data$municipio)))
          run_data <- merge(run_data, loc_data[,c("lat","lon","mun_locase","id")], by.x="station", by.y="mun_locase")
          
          #calculate multi-year mean yield per planting date
          for (emd in unique(run_data$sdate)) {
            #emd <- unique(run_data$sdate)[1]
            emd_data <- run_data[which(run_data$sdate == emd),]
            emd_data$Em_Dat <- NULL
            emd_mean <- aggregate(emd_data[,c("lat","lon","WRR14")],by=list(station=emd_data$station, soil=emd_data$soil), FUN=function(x) {mean(x,na.rm=T)})
            emd_std <- aggregate(emd_data[,c("lat","lon","WRR14")],by=list(station=emd_data$station, soil=emd_data$soil), FUN=function(x) {sd(x,na.rm=T)})
            emd_cv <- aggregate(emd_data[,c("lat","lon","WRR14")],by=list(station=emd_data$station, soil=emd_data$soil), FUN=function(x) {sd(x,na.rm=T)/mean(x,na.rm=T)*100})
            emd_mean$WRR14sd <- emd_std$WRR14; emd_mean$WRR14cv <- emd_cv$WRR14
            rm(list=c("emd_std","emd_cv"))
            names(emd_mean)[grep("WRR14",names(emd_mean))] <- paste(names(emd_mean)[grep("WRR14",names(emd_mean))],"_",emd,sep="")
            
            if (emd == unique(run_data$sdate)[1]) {
              mean_run <- emd_mean
            } else {
              mean_run <- merge(mean_run, emd_mean, by=c("station","soil","lat","lon"), all.x=T, all.y=T)
            }
          }
          mean_run <- merge(mean_run, soil_class, by.x="soil", by.y="soil_sim")
          
          #merge data.frames for further making yield raster
          yield_rs <- merge(yield_df, mean_run, by.x=c("x_wst","y_wst","Texture"), 
                            by.y=c("lon","lat","soil_txt"),all.x=T)
          
          #mean yield of all planting dates
          yield_rs$YIELD_mean <- rowMeans(yield_rs[,grep("WRR14_",names(yield_rs),fixed=T)], na.rm=T)
          yield_rs$YIELD_std <- rowMeans(yield_rs[,grep("WRR14sd_",names(yield_rs),fixed=T)], na.rm=T)
          yield_rs$YIELD_cv <- rowMeans(yield_rs[,grep("WRR14cv_",names(yield_rs),fixed=T)], na.rm=T)
          
          #make rasters
          rs_ymean <- rs_ystd <- rs_ycv  <- rs_ref
          rs_ymean[yield_rs$cell_id] <- yield_rs$YIELD_mean
          rs_ystd[yield_rs$cell_id] <- yield_rs$YIELD_std
          rs_ycv[yield_rs$cell_id] <- yield_rs$YIELD_cv
          
          #put into master list
          alldata_list[[paste(gcm,"_",bc,"_",co2p,sep="")]] <- yield_rs
          ymean_list[[paste(gcm,"_",bc,"_",co2p,sep="")]] <- rs_ymean
          ystd_list[[paste(gcm,"_",bc,"_",co2p,sep="")]] <- rs_ystd
          ycv_list[[paste(gcm,"_",bc,"_",co2p,sep="")]] <- rs_ycv
          
          #remove objects
          rm(list=c("yield_rs","rs_ymean","rs_ystd","rs_ycv","mean_run","run_data")); g=gc(); rm(g)
        }
      }
    }
    
    #save objects per RCP
    save(list=c("alldata_list"),file=paste(an_dir,"/yield_future_spatial_df_",rcp,".RData",sep=""))
    save(list=c("ymean_list"),file=paste(an_dir,"/yield_mean_rasters_",rcp,".RData",sep=""))
    save(list=c("ystd_list"),file=paste(an_dir,"/yield_std_rasters_",rcp,".RData",sep=""))
    save(list=c("ycv_list"),file=paste(an_dir,"/yield_cv_rasters_",rcp,".RData",sep=""))
    
    #clean up
    rm(list=c("alldata_list","ymean_list","ystd_list","ycv_list")); g=gc(); rm(g)
  }
}


#############################################################################################
#calculate probability distribution of each climate model for each cluster and stress_profile
#############################################################################################
#loop to get output as rasters
for (rcp in rcplist) {
  #rcp <- rcplist[1]
  
  if (!file.exists(paste(an_dir,"/yield_probability_",rcp,".RData",sep=""))) {
    #output master list for RCP
    
    quant_df <- data.frame()
    for (gcm in gcmlist) {
      #gcm <- gcmlist[1]
      for (bc in bclist) {
        #bc <- bclist[1]
        for (co2p in co2list) {
          #co2p <- co2list[1]
          co2i <- substr(co2p,1,1)
          cat("...processing rcp=",rcp,"/ gcm=",gcm,"/ bc_method=",bc,"/ co2=",co2p,"\n")
          
          #read data
          fname <- paste(res_dir,"/Arquivos_J2/",co2p,"/","J2_op_",co2i,"_method_",bc,"_",rcp,"_",gcm,".csv",sep="")
          run_data <- read.csv(fname, sep=";")
          run_data$station <- as.vector(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[1]}))
          run_data$soil <- as.vector(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[2]}))
          run_data$sdate <- as.numeric(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[3]}))
          run_data$municipio <- as.vector(sapply(run_data$identificador, FUN=function(x) {unlist(strsplit(paste(x),"_",fixed=T))[4]}))
          run_data <- merge(run_data, soil_class, by.x="soil", by.y="soil_sim")
          
          #put lon,lat into run_data
          loc_data <- loc_list
          loc_data$mun_locase <- gsub(" ", "", tolower(paste(loc_data$municipio)))
          run_data <- merge(run_data, loc_data[,c("lat","lon","mun_locase","id")], by.x="station", by.y="mun_locase")
          
          #print cluster output
          print(table(run_data[,c("env_cluster","stress_cluster")]))
          
          #loop clusters and stress profiles to get cum. frequency plot
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
                qdf <- cbind(rcp=rcp, gcm=gcm, bc_method=bc, co2=co2p, env_cluster=clus, stress_cluster=stress, qdf)
                quant_df <- rbind(quant_df, qdf)
              }
            }
          }
          #par(mar=c(5,5,1,1),lwd=1.5,las=1)
          #plot(as.numeric(quant_df[1,paste("p",0:100,sep="")]), 0:100/100, xlim=c(0,7000), ylim=c(0,1), ty="l", xlab=expression(Yield~(kg~ha^{-1})), ylab="Cumulative probability")
          #lines(as.numeric(quant_df[4,paste("p",0:100,sep="")]), 0:100/100, col="red")
          #lines(as.numeric(quant_df[7,paste("p",0:100,sep="")]), 0:100/100, col="green")
          #abline(h=0.5)
        }
      }
    }
    save(list=c("quant_df"),file=paste(an_dir,"/yield_probability_",rcp,".RData",sep=""))
  }
}

#remove J2 files
setwd("~")
system(paste("rm -rf ",res_dir,"/Arquivos_J2",sep=""))

