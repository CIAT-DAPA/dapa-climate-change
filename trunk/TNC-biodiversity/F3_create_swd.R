#--------------------------------------------------------------------#
# Purpose:  script to merge all points and only select the unique ones
# Author:   Johannes Signer & Hector Salazar
# Date:     25.1.2011
#--------------------------------------------------------------------#
# Workflow:
# 1. Get list of all folders
# 2. For each part
#  a. Merge species
#  b. Merge background
#  c. Merge species and Background
#  d. Get unique
# 3. Merge junks (maybe not)
# 4. Extract swd
# 5. Write swd for species and background for every species.


# Workflow:
# 1. Get list of all folders
fl <- list.files(path=dir.out, pattern="^[0-9].*.[0-9]$")

swd.by.chunks <- function(files.list=fl, split.every=1000, dir.out=dir.out) {
# 2. Split them into chunks of 1000 species
   f <- rep(1:ceiling(length(files.list)/split.every), each=split.every)[1:length(files.list)]
   files.splitted <- split(files.list,f)
   
   # call fuction merge.points
   pts <- lapply(files.splitted,function(x) merge.points(x,dir.out=dir.out))

# 3. Merge chunks
   merged.chunks <- ldply(pts,rbind)
   merged.chunks[!duplicated(merged.chunks$key),]

# 3a. write to a file
   write.csv(merged.chunks[,c("species", "lon", "lat")], str_c(dir.out,"/points_all.csv"), row.names=F)
   
# 4. Extract swd
   system(str_c("java -cp ", dir.maxent, "/maxent.jar density.Getval ",dir.out,"/points_all.csv ", dir.env,"/bio??.asc > ",dir.out,"/all_points_swd.csv"),wait=T) 


# 5. Write swd for species and background for every species.
}

merge.point <- function(file.chunk, dir.out) {

   # read all points were species if found
   sp <- lapply(file.chunk, function(x) read.csv(str_c(dir.out,"/",x, "/training/species.csv")))
   sp <- ldply(sp,rbind)
   sp$key <- str_c(sp$lat, sp$lon, sep=":")   
   sp <- sp[!duplicated(sp$key),]

   return(sp)
}
   
