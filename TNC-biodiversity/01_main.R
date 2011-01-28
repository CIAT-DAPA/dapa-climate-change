######################################################################
# TNC - Global plants model
#--------------------------------------------------------------------#
# Author: Johannes Signer
# Date: 24.1.2011
######################################################################

#--------------------------------------------------------------------#
# workflow:
# Data Preparation
# ================

# * Split species file by species ID (script src/scripts/P02_species.sh
# * Only consider species with 10 or more points (script: src/scripts/P02_species.sh)
# * For each species create a folder and copy the training file into the folder.

# * Create for each species a background file with 10000 background points
# * Create a presence swd file
# * Create a background swd file
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
# 1. Read file with all presence points, split it and create a folder
#    for each species. The function is located in the file ./src/scripts/F1_split_csv.R

# Split databasae into single files for every species. To be done from bash shell.
# Execute commands from P02_species.sh

sp.list <- read.table(dir.sp.list,stringsAsFactors=F)
n.part <- ceiling(nrow(sp.list)/10000)
id.part <- rep(paste("part.",1:n.part,sep=""),each=10000)
id.part <- id.part[1:nrow(sp.list)]
sp.list$part <- id.part

sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("write.species.csv")
sfExport("dir.out")
sfExport("dir.sp")
sfExport("id.part")
sfExport("sp.list")

system.time(sfSapply(sp.list[,1:2], function(i) write.species.csv(i,dir.species=dir.sp, dir.out=dir.out)))

sfStop()

#--------------------------------------------------------------------#
# Create background files 

# read shapefile for biomes and continents
continents <- readShapePoly(continents.path)
biomes <- readShapePoly(biomes.path)

l <- lapply(list.files(dir.out, pattern="p.*"), function(x) data.frame(x, list.files(path=paste(dir.out, x, sep="/"), pattern="^[0-9].*.[0-9]$"), stringsAsFactors=F))
ll <- ldply(l, rbind)
paths <- str_c(dir.out,"/", ll[,1], "/", ll[,2])

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
sfExport("dir.log") # need to be included in the function
system.time(sfSapply(paths, function(i) get.background(path=i, continents=continents, biomes=biomes, write.where=T)))

sfStop()


## Make swd files 
fl <- list.files(path=dir.out, pattern="^[0-9].*.[0-9]$")
swd.by.chunks(files.list=fl, split.every=1000, dir.out=dir.out)

