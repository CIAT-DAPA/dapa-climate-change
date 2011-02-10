######################################################################
# TNC - Global plants model
#--------------------------------------------------------------------#
# Author: Johannes Signer
# Date: 24.1.2011
######################################################################

#--------------------------------------------------------------------#
# workflow:

# 1. Split species file by species ID (script src/scripts3/P02_species.sh
# 2. Only consider species with 10 or more points (script: src/scripts3/P02_species.sh)
# 3. For each species create a folder and copy the file with all occurrence records in the training folder (script: src/scripts3/P02_species.sh)
# 4. For each species look where presence and background points are located.
# 5. For all species merge presence points, extract swd and split again. 
# 6. Extract SWD from grids.
# 7. Extract all points of each biome and continent and create for each biome 10 files with 10000 samples. (located in: src/scripts3/P01_biomes.R)

# * Create for each species a background file with 10000 background points
# * Create a presence swd file  
# * Create a background swd file  (script src/scripts3/F7_make_background_points.R)
# * Train and run Maxent

#--------------------------------------------------------------------#
# load libraries
library(raster)
library(snowfall)
library(plyr)
library(stringr)
library(maptools)

# clean workspace
rm(list=ls())

# Load parameters and setup working directory assuming R session was onpend from iabin root directory
source("./parameters/parameters.R")

# save paramaters from current run
save(list=ls(), file=str_c(dir.out, "parameters.RData"))

#--------------------------------------------------------------------#
# 3. Read file with all presence points, split it and create a folder
#    for each species. The function is located in the file 

# Split databasae into single files for every species. To be done from bash shell.
# Execute commands from P02_species.sh

sp.list <- read.table(dir.sp.list,stringsAsFactors=F) # read list of species
n.part  <- ceiling(nrow(sp.list)/10000) # split list in parts of 10000, to overcome limitations in file system
id.part <- rep(paste("part.",1:n.part,sep=""),each=10000)
id.part <- id.part[1:nrow(sp.list)]
sp.list$part <- id.part

sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("write.species.csv")
sfExport("dir.out")
sfExport("dir.sp")
sfExport("id.part")
sfExport("sp.list")

system.time(sfSapply(sp.list[,1:2], function(i) write.species.csv(i,dir.species=dir.sp, dir.out=dir.out))) # function is located in (./src/scripts3/F1_split_csv.R)

sfStop()

#--------------------------------------------------------------------#
# 4. For each species look where the background points should be located with reagd to biome and continent

# read shapefile for biomes and continents
continents <- readShapePoly(continents.path)
biomes <- readShapePoly(biomes.path)

l <- lapply(list.files(dir.out, pattern="^p.*"), function(x) data.frame(x, list.files(path=paste(dir.out, x, sep="/"), pattern="^[0-9].*.[0-9]$"), stringsAsFactors=F))
ll <- ldply(l, rbind)
paths <- str_c(dir.out,"/", ll[,1], "/", ll[,2])

# Export variables to workers
sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("get.background")
sfLibrary(maptools)
sfExport("biomes")
sfExport("continents")
sfExport("me.no.background")
sfExport("dir.out")
sfExport("dir.error.bg")


system.time(sfSapply(paths, function(i) get.background(path=i, continents=continents, biomes=biomes, write.where=T))) # src/scripts/F2_background.csv

sfStop()

#--------------------------------------------------------------------#
# 5. For all species extract presence records


l <- lapply(list.files(dir.out, pattern="^p.*.[0-9]$"), function(x) data.frame(x, list.files(path=paste(dir.out, x, sep="/"), pattern="^[0-9].*.[0-9]$"), stringsAsFactors=F))
ll <- ldply(l, rbind)
paths <- str_c(dir.out,"/", ll[,1], "/", ll[,2], "/training/species.csv")

swd.by.chunks(files.list=paths, split.every=1000) # located in src/scripts/F3_create_swd.R; saves results in dir.out/points_all.csv

#--------------------------------------------------------------------#
# 6. extract the swd values

where <- read.csv(str_c(dir.out, "/points_all.csv"))

where$col <- round((where$lon --180)*(43200-1)/(180--180)+1)
where$row <- round((where$lat -(-90))*(1-21600)/(90-(-90))+21600) + 6 # +6 are to skip the header

# create lookup list
l <- list()
keys <- unique(where$row)
count <- 1
for (i in keys) {l[[i]] <- where[where$row==i,'col']; print(count <- count+1)}


sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("get.values.lg")
sfExport("dir.env")
sfExport("where")
sfExport("keys")
sfExport("l")

# for each species extract the biomes and continents were background points are located.
bio.vars <- c("bio12.asc", "bio15.asc", "bio1.asc", "bio4.asc", "bio8.asc", "bio13.asc", "bio18.asc", "bio2.asc", "bio5.asc", "bio9.asc", "bio14.asc", "bio19.asc", "bio3.asc", "bio6.asc")

system.time(sfSapply(bio.vars, function(i) get.values.lg(ascii=i, where=where))) # function located in F8_get_xy_from_large_grid.R


sfStop()


#--------------------------------------------------------------------#

# Function to create a background swd file for each species

extract.bg <- function (path, no.bg.files,dir.bg)

  # a. load the info.txt file from each splitted species folder
  # b. load the file that contents in which continent and biome the species is found 
  # c. load backgorund files for given biomes and continents
  # d. select points for background
  # e. write a backgorund swd file


#--------------------------------------------------------------------#

#--------------------------------------------------------------------#
# run Maxent


