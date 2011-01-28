######################################################################
# TNC - Global plants model
#--------------------------------------------------------------------#
# Author: Johannes Signer
# Date: 24.1.2011
######################################################################

#--------------------------------------------------------------------#
# workflow:

# 1. Load the *.csv file with all global plant points
# 2. Split the the files and make for each species a folder
# 3. Create for each species a background file with 10000 background points
# 4. Create a presence swd file
# 5. Create a background swd file
# 6. Train and run Maxent

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
# 1. Read file with all presence points, split it and create a folder
#    for each species. The function is located in the file ./src/scripts/F1_split_csv.R

# Split databasae into single files for every species. To be done from bash shell.

awk -F "," '{close(f);f=$1}{print > f".txt"}' World_filtered_Plantae.csv


sp <- apply(species.files.raw,1,write.species.csv,log.file=log.make.species.csv, min.points=pts.min,dir.out=dir.out)

#--------------------------------------------------------------------#
# Create background files 

# read shapefile for biomes and continents
continents <- load(continents.path)
biomes <- load(biomes.path)

# extract backgrounds from wwf biomes task 10
### Export variables to workers
sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("get.background")
sfLibrary(maptools)
sfExport("biomes")
sfExport("continents")
sfExport("me.no.background")
sfExport("dir.out")
sfExport("dir.error.bg")
for (i in 1:length(sp))
system.time(sfSapply(sp[[i]], function(i) get.background(sp_id=i, biomes=biomes, v.all=biomes.values, no.background=me.no.background)))
 
sfStop()


## Make swd files 
fl <- list.files(path=dir.out, pattern="^[0-9].*.[0-9]$")
swd.by.chunks(files.list=fl, split.every=1000, dir.out=dir.out)

