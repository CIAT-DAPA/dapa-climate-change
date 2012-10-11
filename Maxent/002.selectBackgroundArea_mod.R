occFile <- "D:/Maxent_Nicaragua/occurrence_files/coffea_arabica.csv"
msk <- "D:/Maxent_Nicaragua/masks/mask.asc" 
outBackName <- "D:/Maxent_Nicaragua/background_selection/background.csv"

selectBack <- function(occFile, outBackName, msk, backFilesDir) {
require(raster)
require(rgdal)

globZones <- raster(msk)
spData <- read.csv(occFile)
coords=coordinates(globZones)
coord_pres=spData[,2:3]
pos_pres=cellFromXY(globZones,coord_pres)
unos=which(globZones[]==1)

pos_unos_pres=array(0,length(pos_pres))
for(i in 1:length(pos_pres)){
  if(table(pos_pres[i]==unos)[1]==152755){
pos_unos_pres[i]=which(unos==pos_pres[i])} else {pos_unos_pres[i]=0}}

coords_extrac=coords[unos,]
coords_extrac=coords_extrac[-pos_unos_pres,]
coords_final=coords_extrac[sample(1:dim(coords_extrac)[1],10000),]
write.csv(coords_final, outBackName, quote=F, row.names=F)
}
