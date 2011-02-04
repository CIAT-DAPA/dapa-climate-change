######################################################################
# Author: Hector J. Salazar & Johannes Signer
# Contact: hectorjaime26@gmail.com - j.m.signer@gmail.com
# Date: 201100204
# Purpose: make background points
#--------------------------------------------------------------------#
                                                
             
      sp_id <- paste("species/",list.files(path="species"), sep="")
   
      Make.bg <- function (sp_id) {
      info <- read.table(paste(sp_id, "/info.txt",sep=""), sep=":", stringsAsFactors=F)   
      x <- info[info[,1]=="continents",2] 
      continents <- as.numeric(strsplit(x, ",")[[1]])     
      y <- info[info[,1]=="biomes",2]
      biomes <- as.numeric(strsplit(y, ",")[[1]])  
      count <- 1
        for(i in continents) {
           for(j in biomes)  {
              sample <- sample(1:10,1)
              if (count == 1)  {
                results <- read.table(paste("biomes/continent",i,"_biome",j,"_sample",sample,".txt.swd",sep=""), sep=",")
              } else {
                tmp <- read.table(paste("biomes/continent",i,"_biome",j,"_sample",sample,".txt.swd",sep=""), sep=",")
                results <- rbind(results, tmp)                      
              }
                count <- count+1     
           }
        }   
        rs <- results[sample(1:nrow(results),10000),]               ##  select random points       
       write.csv(rs,paste(sp_id, "/training/bg.csv",sep=""))        #### write a table with a background file                          
      } 
       
             #####  to execute___ for (i in sp_id) Make.bg(i)
            
             