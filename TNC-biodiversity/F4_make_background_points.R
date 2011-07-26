######################################################################
# TNC - Global plants model
#--------------------------------------------------------------------#
# Author: Hector J. Salazar & Johannes Signer
# Contact: hectorjaime26@gmail.com - j.m.signer@gmail.com
# Date: 201100204
# Purpose: for each species extract background file from the same continents and biomes where pressence files are found.
#--------------------------------------------------------------------#
                                                

extract.bg <- function (path, no.bg.files, dir.bg, log) {

   tryCatch({
   # read the info.txt file for species that is being processed
   info <- read.table(paste(path, "/info.txt",sep=""), sep=":", stringsAsFactors=F)   

   # get the continents and biomes were this species is found
   x <- info[info[,1]=="continents ",2] 
   continents <- as.numeric(strsplit(x, ",")[[1]])     
   y <- info[info[,1]=="biomes ",2]
   biomes <- as.numeric(strsplit(y, ",")[[1]])  

   # load backgorund files for given biomes and continents
   count <- 1
   for(i in continents) {
      for(j in biomes)  {
         sample <- sample(1:no.bg.files,1)
         if (file.exists(paste(dir.bg,"/continent",i,"_biome",j,"sample",sample,"_swd.txt",sep=""))){
         if (count == 1)  {
               results <- read.table(paste(dir.bg,"/continent",i,"_biome",j,"sample",sample,"_swd.txt",sep=""), sep=",", stringsAsFactors=F, header=T)
            } else {
               tmp <- read.table(paste(dir.bg,"/continent",i,"_biome",j,"sample",sample,"_swd.txt",sep=""), sep=",", stringsAsFactors=F,header=T)
               results <- rbind(results, tmp)                      
            }
         count <- count+1     
         }
      }
   } 

   
   if(nrow(results < 10000)) {  
    rs <- results[sample(1:nrow(results),10000, replace=T),]         # select points for background
   } else  rs <- results[sample(1:nrow(results),10000),]  

   names(rs) <- c("sp","lon","lat","bio1","bio2","bio3","bio4","bio5","bio6","bio8","bio9","bio12","bio13","bio14","bio15","bio18","bio19")

  
   write.csv(rs,paste(path, "/training/bg.csv",sep=""), row.names=F, quote=F)  # write background file.
   write(paste(date(), path, "successfull"), log, append=T)}, 
   error=function(x){write(paste(date(), path, "error:", x), log, append=T)})
      
} 
 
      
       
