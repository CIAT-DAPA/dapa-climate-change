#--------------------------------------------------------------------#
# Purpose:  script to merge all points and only select the unique ones
# Author:   Johannes Signer & Hector Salazar
# Date:     25.1.2011
#--------------------------------------------------------------------#
# Workflow:
# 2. For each part
#  a. Merge species
#  b. Merge background
#  c. Merge species and Background
#  d. Get unique
# 3. Merge junks (maybe not)
# 4. Extract swd
# 5. Write swd for species and background for every species.



swd.by.chunks <- function(files.list=fl, split.every=1000,make.swd=F) {
# 2. Split them into chunks of 1000 species
   f <- rep(1:ceiling(length(files.list)/split.every), each=split.every)[1:length(files.list)]
   files.splitted <- split(files.list,f)
   
   # call fuction merge.points
   pts <- merge.point(unlist(files.splitted))

# 3.only take unique points
  pts <- pts[!duplicated(pts$key),]

# 3a. write to a file
   write.csv(pts[,c("key", "lon", "lat","specie_id")], str_c(dir.out,"/points_all.csv"), row.names=F,quote=F)
   
# 4. Extract swd
   if (make.swd==T)
   system(str_c("java -cp ", dir.maxent, "/maxent.jar density.Getval ",dir.out,"/points_all.csv ", dir.env,"/bio??.asc > ",dir.out,"/all_points_swd.csv"),wait=T) 


# 5. Write swd for species and background for every species.
}

merge.point <- function(file.chunk) {

   # read all points were species if found
   sp <- lapply(file.chunk,read.csv, stringsAsFactors=F)
   sp <- ldply(sp,rbind)
   sp$key <- str_c(sp$lat, sp$lon, sep=":")   
   sp <- sp[!duplicated(sp$key),]

   return(sp)
}
   
