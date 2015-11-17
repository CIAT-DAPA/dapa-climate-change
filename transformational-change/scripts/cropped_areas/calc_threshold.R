#Julian Ramirez-Villegas / Ulrike Rippke
#CIAT / CCAFS / UoL
#Jan 2015

##exclusion of forest from SPAM and Ecocrop data
library(raster); library(maptools); library(rgdal); library(sp); library(PresenceAbsence)

#working dir
bdir <- "/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM"
#bdir <- "//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM"
wd <- paste(bdir,"/modelling/Cul_de_sacs",sep="")
data_dir <- paste(wd,"/model_data",sep="")
runs_dir <- paste(wd,"/model_runs",sep="")
chk_dir <- paste(wd,"/check_figs",sep="")

#list of crops
crop_list <- paste(read.table(paste(data_dir,"/crop_parameters.tab",sep=""),header=T)$Crop[c(1:6,9:10,17)])

####crop rasters including forest areas
spam_crops = list.files(paste(wd,"/SPAM_data_africa",sep=""), full.names=TRUE, recursive=FALSE)
spam_crops = stack(spam_crops)

#cru or wcl
for (dset in c("cru","wcl")) {
  #dset <- "cru"
  cat("...processing dataset=",dset,"\n")
  eco_dir <- paste(runs_dir,"/",dset,"_hist",sep="")
  eco_out <- paste(runs_dir,"/",dset,"_hist_forest_excl",sep="")
  if (!file.exists(eco_out)) {dir.create(eco_out)}
  
  #30 min eco results
  eco_crops = stack(paste(eco_dir,"/",crop_list,"_suit.tif",sep=""))
  
  #exclude forest in ecocrop rasters and adjust extent and resolution of forest
  if (!file.exists(paste(data_dir,"/forest.tif",sep=""))) {
    forest = raster("/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/GIS_maps/forest_shp/FAO_forest_of_the_world2010/Extract_rast71.tif")
    forest = resample(forest, eco_crops[[1]],method="ngb")
    forest = writeRaster(forest,paste(data_dir,"/forest.tif",sep=""),format="GTiff")
  } else {
    forest = raster(paste(paste(data_dir,"/forest.tif",sep="")))
  }
  
  #remove forest from crop suit simulations
  for (ec in 1:nlayers(eco_crops)){
    #ec <- 1
    if (!file.exists(paste(eco_out,"/",names(eco_crops[[ec]]),".tif", sep=""))) {
      ecocrop <- eco_crops[[ec]] #forest_excl[[1]]
      ecocrop[which(forest[]==1)] <- NA 
      #plot(ecocrop, main=names(eco_crops[[ec]]))
      writeRaster(ecocrop, paste(eco_out,"/",names(eco_crops[[ec]]),".tif", sep=""), format="GTiff")
    }
  }
}

###SPAM res and extent adjustment
for (sp in 1:nlayers(spam_crops)){
  #sp <- 1
  if (!file.exists(paste(wd,"/SPAM_data_africa/",names(spam_crops[[sp]]),"_forest_excl.tif", sep=""))) {
    #if I dont rename the single rasters here the resample wouldnt work
    spamcrop =  spam_crops[[sp]]
    spamcrop= resample(spamcrop, forest, method="ngb")  
    spamcrop[which(forest[]==1)] <- NA  
    #plot(spamcrop, main=names(spamcrop))
    writeRaster(spamcrop,paste(wd,"/SPAM_data_africa/",names(spamcrop),"_forest_excl.tif", sep=""),format="GTiff")
  }
}


############################
## threshold determination

######################
#load shp and rasters for REGIONAL analysis for yam, finger millet and beans

##mask to regions
rs_ref = eco_crops[[1]]

#make raster for EAF
#shp_EAF <- readOGR(dsn="//dapadfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/GIS_maps/Africa_regions_shp", layer="EAF") 
shp_EAF <- readOGR(dsn="/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/GIS_maps/Africa_regions_shp", layer="EAF") 
rs_EAF <- crop(rs_ref, extent(shp_EAF))
rs_EAF <- rasterize(shp_EAF, rs_EAF)#turns crop raster into shapefile extent, myshp has 1, rest is NA - this creates the mask
rs_EAF[which(!is.na(rs_EAF[]))] <- 1

#make raster for SAF
shp_SAF <- readOGR(dsn="/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/GIS_maps/Africa_regions_shp", layer="SAF") 
rs_SAF <- crop(rs_ref, extent(shp_SAF))
rs_SAF <- rasterize(shp_SAF, rs_SAF)#turns crop raster into shapefile extent, myshp has 1, rest is NA - this creates the mask
rs_SAF[which(!is.na(rs_SAF[]))] <- 1

#make raster for EAF+SAF
rs_EAF_SAF = merge(rs_EAF,rs_SAF)

#make raster for WAF
shp_WAF <- readOGR(dsn="/nfs/workspace_cluster_6/VULNERABILITY_ANALYSIS_CC_SAM/ECOCROP_DEVELOPMENT_CC_SAM/ULI/GIS_maps/Africa_regions_shp", layer="WAF") 
rs_WAF <- crop(rs_ref, extent(shp_WAF))
rs_WAF <- rasterize(shp_WAF, rs_WAF)#turns crop raster into shapefile extent, myshp has 1, rest is NA - this creates the mask
rs_WAF[which(!is.na(rs_WAF[]))] <- 1

#loop datasets
if (!file.exists(paste(data_dir,"/thresholds.csv",sep=""))) {
  out_meval <- data.frame()
  for (dset in c("cru","wcl")) {
    #dset <- "cru"
    cat("...processing dataset=",dset,"\n")
    eco_dir <- paste(runs_dir,"/",dset,"_hist_forest_excl",sep="")
    eco_dir2 <- paste(runs_dir,"/",dset,"_hist",sep="")
    
    #30 min eco results
    eco_crops = stack(paste(eco_dir2,"/",crop_list,"_suit.tif",sep=""))
    
    #30 min forest-excluded eco results
    eco = stack(paste(eco_dir,"/",crop_list,"_suit.tif",sep=""))
    
    #load spam data
    spam = stack(paste(wd,"/SPAM_data_africa/",crop_list,"_SPAM_af_forest_excl.tif",sep=""))
    
    #loop crops to produce table with values
    for(ci in 1:length(crop_list)){
      #ci <- 3
      tspam = spam[[ci]]
      tspam[which(tspam[] > 0)] <- 1
      teco = eco[[ci]]
      spam_eco = stack(tspam, teco)
      
      ###whole SSA
      data=spam_eco[]
      data=data[!is.na(data[,1]),] ##Quita los NA
      data=data[!is.na(data[,2]),]
      data[,2]=data[,2]/100 ###the package requires the format of the values in decimals
      data <- cbind(1:nrow(data),data)
      
      thresh <- optimal.thresholds(data,model.names="eco",opt.methods=c("MinROCdist","MaxKappa","MaxSens+Spec"))
      auc <- presence.absence.accuracy(data,threshold=thresh$eco[which(thresh$Method=="MaxSens+Spec")])
      
      tteco <- eco_crops[[ci]]
      tteco[which(tteco[] < thresh$eco[3]*100)] <- NA
      plot(tteco,main=paste(crop_list[ci],"/",dset,sep=""))
      
      out_row <- data.frame(dataset=dset,crop=crop_list[ci],AUC=auc$AUC,MinROCdist=thresh$eco[1],
                            MaxKappa=thresh$eco[2],MaxSensSpec=thresh$eco[3])
      out_meval <- rbind(out_meval,out_row)
      
      if (crop_list[ci] == "bean" | crop_list[ci] == "fmillet") {
        for (regi in c("EAF","SAF","EAF_SAF")) {
          #regi <- "EAF"
          rs_reg <- get(paste("rs_",regi,sep=""))
          teco_reg <- crop(teco, rs_reg)
          teco_reg <- mask(teco_reg, rs_reg)
          tspam_reg <- crop(tspam, rs_reg)
          tspam_reg <- mask(tspam_reg, rs_reg)
          spam_eco = stack(tspam_reg, teco_reg)
          data=spam_eco[]
          data=data[!is.na(data[,1]),] ##Quita los NA
          data=data[!is.na(data[,2]),]
          data[,2]=data[,2]/100 ###the package requires the format of the values in decimals
          data <- cbind(ID=1:nrow(data),data)
          
          thresh <- optimal.thresholds(data,model.names="eco",opt.methods=c("MinROCdist","MaxKappa","MaxSens+Spec"))
          auc <- presence.absence.accuracy(data,threshold=thresh$eco[which(thresh$Method=="MaxSens+Spec")])
          out_row <- data.frame(dataset=dset,crop=paste(crop_list[ci],"_",regi,sep=""),AUC=auc$AUC,
                                MinROCdist=thresh$eco[1],MaxKappa=thresh$eco[2],
                                MaxSensSpec=thresh$eco[3])
          out_meval <- rbind(out_meval,out_row)
        }
      } else if (crop_list[ci] == "yam") {
        regi <- "WAF"
        rs_reg <- get(paste("rs_",regi,sep=""))
        teco_reg <- crop(teco, rs_reg)
        teco_reg <- mask(teco_reg, rs_reg)
        tspam_reg <- crop(tspam, rs_reg)
        tspam_reg <- mask(tspam_reg, rs_reg)
        spam_eco = stack(tspam_reg, teco_reg)
        data=spam_eco[]
        data=data[!is.na(data[,1]),] ##Quita los NA
        data=data[!is.na(data[,2]),]
        data[,2]=data[,2]/100 ###the package requires the format of the values in decimals
        data <- cbind(ID=1:nrow(data),data)
        
        thresh <- optimal.thresholds(data,model.names="eco",opt.methods=c("MinROCdist","MaxKappa","MaxSens+Spec"))
        auc <- presence.absence.accuracy(data,threshold=thresh$eco[which(thresh$Method=="MaxSens+Spec")])
        out_row <- data.frame(dataset=dset,crop=paste(crop_list[ci],"_",regi,sep=""),AUC=auc$AUC,
                              MinROCdist=thresh$eco[1],MaxKappa=thresh$eco[2],
                              MaxSensSpec=thresh$eco[3])
        out_meval <- rbind(out_meval,out_row)
      }
    }
  }
  write.csv(out_meval, paste(data_dir,"/thresholds.csv",sep=""), row.names=F)
}

##info: the package requires that the first column is index values, second column pres/abs (spam)
# and third column the ecocrop values in decimals

#produce plots of thresholded suitability
thresholds <- read.csv(paste(data_dir,"/thresholds.csv",sep=""))
for (dset in c("cru","wcl")) {
  #dset <- "cru"
  cat("...processing dataset=",dset,"\n")
  eco_dir <- paste(runs_dir,"/",dset,"_hist",sep="")
  
  #30 min eco results
  eco_crops = stack(paste(eco_dir,"/",crop_list,"_suit.tif",sep=""))
  for (ic in 1:length(crop_list)) {
    #ic <- 9
    crop_name <- crop_list[ic]
    if (crop_name == "fmillet") {
      threshval <- thresholds$MaxSensSpec[which(thresholds$dataset == dset & thresholds$crop == "fmillet_EAF_SAF")]
    } else if (crop_name == "yam") {
      threshval <- thresholds$MaxSensSpec[which(thresholds$dataset == dset & thresholds$crop == "yam_WAF")]
    } else {
      threshval <- thresholds$MaxSensSpec[which(thresholds$dataset == dset & thresholds$crop == crop_name)]
    }
    eco_crops[[ic]][which(eco_crops[[ic]][] < threshval*100)] <- NA
  }
  pdf(paste(chk_dir,"/eco_thresholded_hist_",dset,".pdf",sep=""),width=10,height=10)
  plot(eco_crops,zlim=c(0,100),col=rev(terrain.colors(20)),legend=F)
  dev.off()
}


