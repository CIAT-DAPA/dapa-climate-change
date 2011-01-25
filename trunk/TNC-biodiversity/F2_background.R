######################################################################
# Author: Johannes Signer
# Contact: j.m.signer@gmail.com
# Date: 20110124
# Purpose: make SWD files for MaxEnt
#--------------------------------------------------------------------#

# Parameters:
# sp_id: id of the specie for which background points are extracted
# biomes: raster map of wwf global biomes
# v.all: results of getValues() from biomes
# no.background: the number of background points, default 10000

get.background <- function(sp_id, biomes, v.all, no.background=10000)
{
  points <- read.csv(paste(dir.out,"/",sp_id,"/training/species.csv", sep=""))

  v.ok <- unique(extract(biomes, points[,2:3])) # biomes from which to draw background points
  v.ok <- v.ok[!is.na(v.ok)] # remove occurrence locations that are in no biome
  v.new <- ifelse(match(v.all, v.ok),1,NA)

  if(any(!is.na(v.new))){
     rr <- setValues(biomes, v.new) # create raster rr with regions were background points can be drawn from
     bg <- sampleRandom(rr, no.background, sp=T)@coords
     write.table("species,longitude,latitude",paste(dir.out,"/",sp_id,"/training/background.csv", sep=""),col.names=F, row.names=F,append=F, quote=F)
     write.table(cbind("background", bg), paste(dir.out,"/",sp_id,"/training/background.csv",sep=""), col.names=F, row.names=F, sep=",", append=T, quote=F)

  } else {write(sp_id, paste(dir.out,"/no_bg_made.csv", sep=""), append=T)
    system(paste("mv ",dir.out, "/", sp_id, " ", dir.error.bg, sep=""))
  
   }
  
}
