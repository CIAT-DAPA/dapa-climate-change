#JRV 2013
#CIAT / CCAFS
#stop("!")

#calculate variance inflation factor for bioclimatic indices to 
#produce subset of predictors for a given species. Four sets of 
#variables will be produced:

#1. subset climate
#2. subset climate + soil + topographic
#3. full climate
#4. full climate + soil + topographic

#where
#soil = drainage rate category (factor)
#topo = slope + aspect (continuous variable)

#load libraries
library(raster); library(rgdal); library(usdm)

######################################
#declare function for analysis of VIF (variance inflation factor)
vif_analysis <- function(sppName,bio_dir,vif_dir,soil_dir,topo_dir) {
  if (!file.exists(vif_dir)) {dir.create(vif_dir,recursive=T)}
  
  spp_dir <- paste(vif_dir,"/",sppName,sep="")
  if (!file.exists(spp_dir)) {dir.create(spp_dir)}
  
  if (!file.exists(paste(spp_dir,"/",sppName,"_output.RData",sep=""))) {
    cat("\nprocessing species",sppName,"\n")
    
    #load occurrences
    #beware: this script will always try to get the species data file from the 
    #directory named 'occurrences' inside the project's root directory
    
    #note names of LON,LAT fields HAVE to be LONGITUDE and LATITUDE (respectively)
    spp <- read.csv(paste("./occurrences/",sppName,".txt",sep=""),sep="\t")
    loc_data <- data.frame(x=spp$LONGITUDE,y=spp$LATITUDE)
    
    #keep only unique occurrences
    loc_data <- unique(loc_data)
    
    #load bioclim rasterstack
    bio_stk <- stack(c(paste(bio_dir,"/bio_",1:19,".tif",sep=""),paste(bio_dir,"/sind.tif",sep=""),paste(bio_dir,"/drymonths.tif",sep="")))
    
    #extract climate for location data
    cat("extracting climate predictors for",nrow(loc_data),"occurrences \n")
    bio_data <- as.data.frame(extract(bio_stk,loc_data))
    bio_data <- cbind(loc_data,bio_data)
    
    soil_stk <- stack(paste(soil_dir,"/soildrain_final.tif",sep=""))
    bio_data <- cbind(bio_data,as.data.frame(extract(soil_stk,loc_data)))
    
    top_stk <- stack(paste(topo_dir,"/",c("aspect.tif","slope.tif"),sep=""))
    bio_data <- cbind(bio_data,as.data.frame(extract(top_stk,loc_data)))
    
    #count NA and remove those with > 1 NA
    bio_data$NAs <- apply(bio_data,1,FUN=function(x) {nac <- length(which(is.na(x))); return(nac)})
    bio_data <- bio_data[which(bio_data$NAs == 0),]
    bio_data$NAs <- NULL
    
    #sind * 10000 for formatting reasons
    bio_data$sind <- bio_data$sind * 10000
    bio_data$slope <- bio_data$slope * 100
    
    #write data file (full climate)
    write.csv(bio_data,paste(spp_dir,"/",sppName,"_full.csv",sep=""),row.names=F,quote=T)
    
    #vif analysis
    vif_res <- vifstep(bio_data[,c(names(bio_data)[grep("bio_",names(bio_data))],"sind","drymonths")],th=5)
    
    #selecting predictors (+ bio_1 + bio_12)
    #vif_res <- vif_res[order(vif_res$VIF,decreasing=F),]
    sel_var <- unique(c(paste(vif_res@results$Variables),"bio_1","bio_12"))
    sel_var <- sort(sel_var)
    
    #filtering out useless variables in bio_data
    #!Remember sind has been scaled * 10000 
    #(so the projection raster has to be scaled * 10000 before projecting)
    bio_sel <- bio_data[,c("x","y",sel_var,"soildrain_final","aspect","slope")]
    
    #write sub-selected file (this is subset model)
    write.csv(bio_sel,paste(spp_dir,"/",sppName,"_subset.csv",sep=""),row.names=F,quote=T)
    
    #output object (to be stored as RData)
    out_obj <- list(SPP=sppName,RAW_OCC=spp,LOC_DATA=loc_data,BIO_FULL=bio_data,BIO_SUBSET=bio_sel)
    save(list=c("out_obj"),file=paste(spp_dir,"/",sppName,"_output.RData",sep=""))
    cat("done!, check object",paste(sppName,"_output.RData",sep=""),"\n")
  } else {
    cat("\nspecies",sppName,"existed. Loading... \n")
    load(paste(spp_dir,"/",sppName,"_output.RData",sep=""))
  }
  return(out_obj)
}

##### run analysis for example species
#working directory
#wd <- "/media/DATA/CIAT_work/DNP-biodiversity"
wd <- "/nfs/a102/eejarv/DNP-biodiversity"
setwd(wd)

#other dirs
var_dir <- paste(wd,"/env-data/bioclim_gtiff",sep="")
sol_dir <- paste(wd,"/env-data/soil",sep="")
top_dir <- paste(wd,"/env-data/topography",sep="")
out_dir <- paste(wd,"/vif-analysis",sep="")

spp_all <- c("Bixa_orel","Boro_pati","Caes_spin", "Cres_cuje","Geni_amer",
             "Indi_suff","Jaca_cauc","Just_pect","Lipp_alba","Mint_moll",
             "Myro_bals","Oeno_batu","Smil_moll")

for (spp_name in spp_all) {
  #spp name and dir
  #spp_name <- "Smil_moll"
  #run function
  
  out_data <- vif_analysis(sppName=spp_name,bio_dir=var_dir,vif_dir=out_dir,
                           soil_dir=sol_dir,topo_dir=top_dir)
}


