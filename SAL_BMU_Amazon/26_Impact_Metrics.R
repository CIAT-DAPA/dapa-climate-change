########  Calculate impact metrics for the study area  ######

library(raster);library(maptools);library(rgdal);library(sp)

src.dir <- "Z:/WORK_PACKAGES/WP2/00_scripts"
bd <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs" 
iDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/outputs"
oDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/impacts"
# cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "bean", "rice", "cocoa", "sugar_cane", "panela_cane")
# cropLs <- c("cocoa", "sugar_cane", "sugar_cane_eitzinger", "panela_cane", "coffee", "coffee_eitzinger", "palmito")
rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
periodLs <- c("2020_2049", "2040_2069", "2070_2099")
shpStudyArea <- "Z:/WORK_PACKAGES/WP2/00_geodata/tnc_terrestial_ecoregions_napo_diss.shp"
shpSubLevel <- "Z:/WORK_PACKAGES/WP2/00_geodata/tnc_terrestial_ecoregions_napo.shp"
sh <- readShapePoly(shpStudyArea)
cropLs <- c("palmito")

#Source scripts and libraries
stop("no")
source(paste(src.dir,"/src-ecocrop/createMask.R",sep=""))
source(paste(src.dir,"/src-ecocrop/futureRuns.tmp.R",sep=""))
source(paste(src.dir,"/src-ecocrop/impacts.R",sep=""))
source(paste(src.dir,"/src-ecocrop/uncertainty.R",sep=""))

cat("Calculate impact metrics for the area of study\n")

for(crop in cropLs){
  
  if (!file.exists(paste(bd, "/impact/impacts-", crop, "/impacts-amz.csv", sep=""))) {
    
  # crop <- cropLs[1]
  impDir <- paste(bd, "/impacts/impacts-", crop, sep="")
  if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
  
  for (rcp in rcpLs){
    
    rcpDir <- paste0(iDir, "/", crop, "/runs-", rcp)
    gls <- list.files(rcpDir, full.names = F, include.dirs = F)
    
    for(period in periodLs){
      
        for (gcm in gls) {
          
          cat("\tImpact StudyArea ", gcm, "\n")
          
          od <- paste(bd, "/impact/impacts-", crop, "/", gcm, "/", rcp, "_", period, sep="")  
          if (!file.exists(od)) {dir.create(od)}
          
          r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif"))
          r2 <- raster(paste0(rcpDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
          pp <- iMetrix(r1,r2,sh,od, chggrid=T, impact=T, classes=T)
          
          # im <- cbind(GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          # 
          # if (gcm == gls[1]) {
          #   res.im <- im
          # } else {
          #   res.im <- rbind(res.im, im)
          # }
          # 
          im <- cbind(RCP=rep(rcp,times=nrow(pp$IMPACT)),PERIOD=rep(period,times=nrow(pp$IMPACT)),GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          
          if (gcm == gls[1] && rcp == rcpLs[1] && period == periodLs[1]) {
            res.im <- im
          } else {
            res.im <- rbind(res.im, im)
          }
          
          
          rm(im) #; rm(cl)
        }
        

        
      } 
      
      
    }
    
  write.csv(res.im, paste(bd, "/impact/impacts-", crop, "/impacts-amz.csv", sep=""), quote=T, row.names=F)
  cat("Calcs impact metrics for the area of study done\n")
  
  } else {cat("Calcs impact metrics for the area of study done!\n")}
  
}





########  Calculate impact metrics for subadministrative levels  ######

library(raster);library(maptools);library(rgdal);library(sp)

# src.dir <- "Z:/DATA/WP2/00_scripts"
# bd <- "Z:/DATA/WP2/05_EcoCrop_runs" 
# iDir <- "Z:/DATA/WP2/05_EcoCrop_runs/outputs"
# oDir <- "Z:/DATA/WP2/05_EcoCrop_runs/impacts"
# # cropLs <- c("maize_eitzinger_kai", "cassava", "plantain_reggata_german", "bean", "rice" , "cocoa", "sugar_cane", "panela_cane")
# cropLs <- c("cocoa", "sugar_cane", "sugar_cane_eitzinger", "panela_cane", "coffee", "coffee_eitzinger", "palmito")
# rcpLs <- c("rcp26", "rcp45", "rcp60", "rcp85")
# periodLs <- c("2020_2049", "2040_2069", "2070_2099")
# shpStudyArea <- "Z:/GEODATA/AMAZON/SHAPEFILES/Ecoregions/tnc_terrestial_ecoregions_napo_dis.shp"
# # shpStudyAreaRs <- "Z:/GEODATA/AMAZON/SHAPEFILES/Ecoregions/tnc_terrestial_ecoregions_napo"
# shpSubLevel <- "Z:/GEODATA/AMAZON/SHAPEFILES/Ecoregions/tnc_terrestial_ecoregions_napo.shp"
sh <- readOGR(shpSubLevel)
# rs <- shpStudyAreaRs

#Source scripts and libraries
stop("no")
source(paste(src.dir,"/src-ecocrop/createMask.R",sep=""))
source(paste(src.dir,"/src-ecocrop/futureRuns.tmp.R",sep=""))
source(paste(src.dir,"/src-ecocrop/impacts.R",sep=""))
source(paste(src.dir,"/src-ecocrop/uncertainty.R",sep=""))

cat("Calculate impact metrics for subadministrative levels\n")
for(crop in cropLs){
  
  if (!file.exists(paste(bd, "/impact/impacts-", crop, "/impacts-amz-sub.csv", sep=""))) {
    
    impDir <- paste(bd, "/impact/impacts-", crop, sep="")
    if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
    
    for (rcp in rcpLs){
      
      rcpDir <- paste0(iDir, "/", crop, "/runs-", rcp)
      gls <- list.files(rcpDir, full.names = F, include.dirs = F)
      
      for(period in periodLs){
       
        for (gcm in gls) {
          
          cat("\tImpact StudyArea ", rcp, gcm, period, "\n")
          
          od <- paste(bd, "/impact/impacts-", crop, "/", gcm, "/", rcp, "_", period, sep="")  
          if (!file.exists(od)) {dir.create(od, recursive=T)}
          
          r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif"))
          r2 <- raster(paste0(rcpDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
          pp <- iMetrix(r1,r2,sh,od, chggrid=T, impact=T, classes=T)
          
          im <- cbind(RCP=rep(rcp,times=nrow(pp$IMPACT)),PERIOD=rep(period,times=nrow(pp$IMPACT)),GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          
          if (gcm == gls[1] && rcp == rcpLs[1] && period == periodLs[1]) {
            res.im <- im
          } else {
            res.im <- rbind(res.im, im)
          }
          rm(im) #; rm(cl)
        }

      }
      
    }
    
    write.csv(res.im, paste(bd, "/impact/impacts-", crop, "/impacts-amz-sub.csv", sep=""), quote=T, row.names=F)
    cat("Calcs impact metrics for subadministrative levels done!", crop, "\n")
    
  } else {cat("Calcs impact metrics for subadministrative levels done!", crop, "\n")}
}

