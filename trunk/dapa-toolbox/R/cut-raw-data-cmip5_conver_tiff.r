require(raster)
require(ncdf)
require(rgdal)


admin <- "D:/Request/Request_miguel/ensemble/rcp45/global_30s/2020_2049"
cutworld = "D:/Request/Request_miguel/worldclim/Global_30s"
outdir<-"D:/Request/Request_miguel/anomalies"

#coun = c("col_adm","per_adm","slv_adm","cri_adm","mex_adm","grd_adm")
coun = c("arg_adm")
bio = c("bio_1","bio_12")

for (sres in coun) {
  
  for (i in bio){
    cat(paste("\t ",   strsplit(sres, "_")[[1]], sep=""))
    ensem = raster(paste(admin, '/', sres, '/',i, sep=""))
    worldc = raster(paste(cutworld, '/', sres, '/',i, sep=""))
    anomali = ensem - worldc
    #plot(worldc)
 
    if (!file.exists(paste(outdir, "/", sres, sep=""))) 
    {dir.create(paste(outdir, "/", sres,sep=""), recursive=TRUE)}   
    
    if (!file.exists(paste(outdir, "/", sres,"/",i,".asc", sep=""))) {
      
      ascWrite <- writeRaster(anomali, paste(outdir, "/", sres,"/",i,".asc", sep=""), format="ascii", overwrite=F)
      cat(" ..done\n")
      
    } else {cat(" ..done\n")}   
    

  }
  
}
















require(raster)
require(ncdf)
require(rgdal)


baseDir <- "T:/gcm/cmip5/raw/monthly"
region <- extent(-40, 70,-55,50)
outDir <- "D:/Request/Request_ulrike/cmip5_fix"
startYr <- 2005
endYr <- 2007
ens <- "r1i1p1"
rcp <- "rcp85"
basemask <- raster("D:/Request/Request_ulrike/2007/prec_1.tif")
plot(basemask)



mask = shapefile("S:/admin_boundaries/adminFiles/world_adm0.shp")
ma = raster("T:/gcm/cmip5/raw/monthly/rcp60/lasg_fgoals_s2/r1i1p1/monthly-files/2014/tmean_01.nc")
test = raster('T:/gcm/cmip5/raw/monthly/rcp60/gfdl_esm2m/r1i1p1/monthly-files/2015/tmin_01.nc')

plot(raster("W:/Request/Request_ulrike/cmip5/rcp85/gfdl_esm2g/2006/prec_1.tif"))
plot(basemask)

plot(raster("D:/tmin_06.nc"))


  
  rcpList <- c("historical", "rcp26", "rcp45", "rcp60", "rcp85")
  
  for (rcp in rcpList) {
    
    cat(paste("\n\nProcessing ", rcp, sep=""))
    
  }
  
  rcpDir <- paste(baseDir, "/", rcp, sep="")
  
  gcmStats <- read.table(paste("D:/_scripts/dapa-climate-change/IPCC-CMIP5/data/cmip5-", rcp, "-monthly-data-summary.txt", sep=""), sep="\t", na.strings = "", header = TRUE)
  
  monthList <- c(paste(0,c(1:9),sep=""),paste(c(10:12)))
  monthListMod <- c(1:12)  
  ndays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  ndaymtx <- as.data.frame(cbind(monthList, ndays, monthListMod))
  names(ndaymtx) <- c("Month", "Ndays", "MonthMod")
  
  #varList <- c("prec", "tmax", "tmin")  
  varList <- c("prec", "tmax")  
  
  
  for (i in 1:nrow(gcmStats)){
    
    gcm <- paste(as.matrix(gcmStats)[i,2])
    ensAll <- paste(as.matrix(gcmStats)[i,3])
    
    if(!paste(as.matrix(gcmStats)[i,10]) == "ins-var" &  gcm == "gfdl_esm2g") {
      
      if(!paste(as.matrix(gcmStats)[i,10]) == "ins-yr"){
        
        
        if (ensAll == ens){
          
          yrList <- list.dirs(paste(baseDir, "/", rcp, "/", gcm, "/", ens, "/monthly-files", sep=""), recursive = FALSE, full.names = FALSE)
          
          for (yrDir in yrList) {
            
            yr <- basename(yrDir)
            
            if (yr > startYr) {
              
              if (yr < endYr) {
                
                cat(paste("\n\nProcessing ", rcp, " ", gcm, " ", yr, "\n\n", sep=""))
                
                ncList <- list.files(yrDir, pattern=".nc", full.names = TRUE)
                
                outAscDir <- paste(outDir, "/", rcp, "/", gcm, "/", yr, sep="")
                if (!file.exists(outAscDir)) {
                  dir.create(outDir)
                  dir.create(paste(outDir, "/", rcp, sep=""))
                  dir.create(paste(outDir, "/", rcp, "/", gcm, sep=""))
                  dir.create(outAscDir)
                }
                
                for(nc in ncList){
                  
                  ncName <- basename(nc)
                  var <- sapply(strsplit(ncName, '[_]'), "[[", 1)
                  ncNoExt <- sapply(strsplit(ncName, '[.]'), "[[", 1)
                  mth <- sapply(strsplit(ncNoExt, '[_]'), "[[", 2)
                  
                  mthMod <- as.numeric(paste((ndaymtx$MonthMod[which(ndaymtx$Month == mth)])))
                  name_mod <- paste(var,'_',mthMod,sep='')
                  
                  if (!var == "tmean") {
                    if (!var == "hur") {
                      if (!var == "rsds") {
                        
                        ncCrop <- crop(rotate(raster(nc)), region)
                      plot(raster(nc))
                        
                        s  <- resample(ncCrop, basemask, method='bilinear')
                        
                        
                        if (var == "prec"){
                          
                          daysmth <- as.numeric(paste((ndaymtx$Ndays[which(ndaymtx$Month == mth)])))
                          recorte<-mask(s,basemask) * 86400 * (daysmth)
                          projection(recorte) = projection(ncCrop)                            
                          
                        } else {
                          recorte<-mask(s,basemask) - 272.15
                          projection(recorte) = projection(ncCrop)                           
                        }                       
                        
                        cat(paste("\t ", ncName, sep=""))
                        ######### ncsplit <- strsplit(nc, '[.]')
                        ######## ncRaster <- rotate(raster(paste(regionDir, "/baseline/", nc, sep=""))) * 0 + 1
                        
                        if (!file.exists(paste(outAscDir, "/", ncName, sep=""))) {
                          
                          ascWrite <- writeRaster(recorte, paste(outAscDir, "/", name_mod, ".tif", sep=""), format="GTiff", overwrite=F)
                          cat(" ..done\n")
                          
                        } else {cat(" ..done\n")}
                        
                      }
                    }
                  }
                  
                }
                
              }
            }
            
          }
          
        }
        
      } 
    }
  }
  
  


