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

get.background <- function(path,continents, biomes,no.background=10000, make.swd=F, write.where=F)
{
  points <- read.csv(paste(path, "/training/species.csv", sep=""))
  coordinates(points) = ~lon+lat

  biomes.ok <- unique(over(points, biomes)[,2])
  biomes.ok <- biomes.ok[!is.na(biomes.ok)]

  continents.ok <- unique(overlay(points,continents))
  continents.ok <- continents.ok[!is.na(continents.ok)]
   

  if(make.swd==T) {
  
     if(nrow(take.bg.from)>0) {
        
        take.bg.from <- expand.grid(continents.ok, biomes.ok)

        # random number between 1 and 20
        rn <- sample(1:20,1)

        files.to.load <- str_c(dir.bg,"/continent", take.bg.from[,1], "_biome", take.bg.from[,2], "sample", rn, ".txt")

        bg.pts <- lapply(files.to.load, read.csv)
        bg.pts <- ldply(bg.pts,rbind)

        bg.pts <- bg.pts[sample(1:nrow(bg.pts),no.background),]


         write.table("species,longitude,latitude",paste(dir.out,"/",sp_id,"/training/background.csv", sep=""),col.names=F, row.names=F,append=F, quote=F)
           write.table(cbind("background", bg.pts), paste(dir.out,"/",sp_id,"/training/background.csv",sep=""), col.names=F, row.names=F, sep=",", append=T, quote=F)

      } else {write(sp_id, paste(dir.out,"/no_bg_made.csv", sep=""), append=T)
       system(paste("mv ",dir.out, "/", sp_id, " ", dir.error.bg, sep=""))
     
      }
   }
   
   if (write.where==T){

      write(paste("biomes : ", paste(biomes.ok, collapse=","), sep=""), paste(path, "/info.txt",sep=""), append=T)
      write(paste("continents : ", paste(continents.ok, collapse=","), sep=""), paste(path, "/info.txt", sep=""), append=T)
      write(paste(Sys.time(),path, " ok", sep="|"), paste(dir.log, "/where_from_bg.txt",sep=""), append=T)
   }
  
  
}
