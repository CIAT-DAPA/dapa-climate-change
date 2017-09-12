#Add FPU data to crop_matrices

#Load libraries
library(maptools)
library(sp)

#Static data
cultivos = c('Maize','Potatoes','Rice','Soybeans','Wheat')
treat = c('riego','secano')
path <- "Z:/08-Cells_toRun/matrices_cultivo/"

# Load FPU shape file
worldFPU <- readShapePoly(fn="Z:/15_FPUs/fpu_shp/fpu.shp",proj4string=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

# Load crop pixel data
for (c in 1:5)  {  #loop through crops
  for (t in 1:2)  {  #loop through treatments
    # Organizing data to identify FPU categories for all coordinates
    load(paste0(path,cultivos[c],"_",treat[t],".Rdat"))
    if (t==1)  {crop = crop_riego}  else{crop = crop_secano}
    xy <- cbind(crop$x, crop$y)
    occ <- SpatialPoints(xy); rm(xy)
    proj4string(occ) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    
    # Extract FPU categories
    overFPU <- sp::over(occ,worldFPU)
    overFPU <- overFPU[,c("New_FPU","NFPU_INT","New_Basin","Basin_Name")]
    
    # Create new file with FPU categories
    crop <- cbind(crop,overFPU)
    if (t==1)  {crop_riego = crop
                save(crop_riego, file=paste(path,cultivos[c],"_",treat[t],".RDat",sep=''))}  else{
                  crop_secano = crop
                  save(crop_secano, file=paste(path,cultivos[c],"_",treat[t],".RDat",sep=''))}
    
    
  }
}

