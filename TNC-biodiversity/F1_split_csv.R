######################################################################
# Author:   Johannes Signer
# Contact:  j.m.signer@gmail.com
# Date:     20110124
#--------------------------------------------------------------------#

# required parameters

# x: name of the file with the species presence records

# return
# writes for each species (where # of unique occurrences >= min.Points) a directory with a csv and an info file. 

#--------------------------------------------------------------------#
# scripts

write.species.csv <- function(x,part, dir.species=dir.species, dir.out=dir.out)
{
   # read the file
   tt <- try(this.sp <- read.csv(paste(dir.species, "/", x, sep="")))
   if (class(tt)!= "try-error") {
      id <- strsplit(x, "\\.")[[1]][1]
      # create file
      dir.create(paste(dir.out,part,id,"training",sep="/"),recursive=T)
             
      write(paste("species id : ",x,
           "\nspecie : ",this.sp[1,'specie'],
           "\ngenus : ",this.sp[1,'genus'],
           "\ngenus_id : ",this.sp[1,'genus_id'],
           "\nfamily_id : ",this.sp[1,'family_id'], 
           "\nfamily : ", this.sp[1,'family'],
           "\nnumber.of.points : ",nrow(this.sp), sep=""), paste(dir.out,part,id,"info.txt", sep="/"))
     
     write.csv(this.sp[,c("specie_id", "lon", "lat")], paste(dir.out,"/",part,"/",id,"/training/species.csv",sep=""), row.names=F, quote=F)
  } else write(x, paste(dir.out, "/errors/make_files.txt",sep=""), append=T)
  print(x)
      
}
   
