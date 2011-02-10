######################################################################
# TNC - Global plants model
#--------------------------------------------------------------------#
# Author: Hector J. Salazar & Johannes Signer
# Contact: hectorjaime26@gmail.com - j.m.signer@gmail.com
# Date: 201100204
# Purpose: make background points
#--------------------------------------------------------------------#
                                                
             
sp_id <- paste("species/",list.files(path="species"), sep="")

extract.bg <- function (path, no.bg.files,dir.bg) {
   # read the info.txt file for species that is being processed
   info <- read.table(paste(path, "/info.txt",sep=""), sep=":", stringsAsFactors=F)   

   # get the continents and biomes were this species is found
   x <- info[info[,1]=="continents",2] 
   continents <- as.numeric(strsplit(x, ",")[[1]])     
   y <- info[info[,1]=="biomes",2]
   biomes <- as.numeric(strsplit(y, ",")[[1]])  

   # load backgorund files for given biomes and continents
   count <- 1
   for(i in continents) {
      for(j in biomes)  {
         sample <- sample(1:no.bg.files,1)
         if (count == 1)  {
               results <- read.table(paste(dir.bg,"/continent",i,"_biome",j,"_sample",sample,".swd",sep=""), sep=",")
            } else {
               tmp <- read.table(paste(dir.bg,"/continent",i,"_biome",j,"_sample",sample,".swd",sep=""), sep=",")
               results <- rbind(results, tmp)                      
            }
         count <- count+1     
      }
   }   
   rs <- results[sample(1:nrow(results),10000),]         # select points for background
   write.csv(rs,paste(path, "/training/bg.csv",sep=""))  # write background file.
} 
 
      
       
