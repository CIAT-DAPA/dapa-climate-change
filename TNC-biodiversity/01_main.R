######################################################################
# TNC - Global plants model
#--------------------------------------------------------------------#
# Author: Johannes Signer
# Date: 24.1.2011
######################################################################

#--------------------------------------------------------------------#
# Workflow:

# 1. Presence records:
#   a. Split species file by species ID (script src/scripts3/P02_species.sh
#   b. Only consider species with 10 or more points (script: src/scripts3/P02_species.sh)
#   c. For each species create a folder and copy the file with all occurrence records in the training folder (script: src/scripts3/P02_species.sh)
#   d. For each species look in which biome and continent presence points are located. This information is than used to extract bakcground points
# 2. SWD: Presence Records
#   a. For all species merge presence points, extract swd and split again. 
#   b. Extract each value of each environmental variable
#   c. Interpolate cases where the extraction (2b) has failed.
#   d. Merge different environmental varialbes into one file
#   e. For each species extract values of environmental variables at location where there are presence points
# 3. SWD: Background records
#   a. For each continent extract all values for each biome
#   b. For each continent and biome take 10 times a random sample of 10.000 points
#   c. Merge all files from 3b to gether
#   d. Extract value at at each point from 3c for each environmental variabel
#   e. Merge results from every variable into one file
#   f. For each species extract background files, according to 1d.
# 3. Run Maxent
# 4. Project current

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
# Background points

  for i in bg.*.csv; do awk -F, '{print $7}' $i > $i.val.only; echo $i; done

# coordinates
awk -F, '{print $3 "," $4}' $i > bg.coordinates
awk -F, '{print $3""$4}' $i > bg.keys



echo "lon,lat,bio1,bio2,bio3,bio4,bio5,bio6,bio8,bio9,bio12,bio13,bio14,bio15,bio18,bio19" > bg.values.all.var.csv

paste -d, bg.coordinates bg.keys bg.valuesbio1.asc.csv.val.only bg.valuesbio2.asc.csv.val.only bg.valuesbio3.asc.csv.val.only bg.valuesbio4.asc.csv.val.only bg.valuesbio5.asc.csv.val.only bg.valuesbio6.asc.csv.val.only  bg.valuesbio8.asc.csv.val.only bg.valuesbio9.asc.csv.val.only bg.valuesbio12.asc.csv.val.only bg.valuesbio13.asc.csv.val.only bg.valuesbio14.asc.csv.val.only bg.valuesbio15.asc.csv.val.only bg.valuesbio18.asc.csv.val.only bg.valuesbio19.asc.csv.val.only > bg.values.all.var.csv

# for each sample assign the nececcsary points. 
r <- read.csv("data/env/bg.values.all.var.csv", stringsAsFactors=F)
fl <- list.files(path="data/background", pattern="*sample*", full=T) 

sapply(fl, assign.swd.to.bg, all.pts=r)

assign.swd.to.bg <- function(bg.file, all.pts) {
   if(!file.exists(str_c(str_sub(bg.file, end=-5),"_swd.txt"))) {
   a <- read.csv(bg.file, stringsAsFactors=F)
   a$key <- paste(a$lon,a$lat, sep="")

   if (length(which(complete.cases(a)==T))!=10000) a <- a[which(complete.cases(a)==T),]

   a <- cbind(a[,1:3],all.pts[all.pts$lonlat %in% a$key,4:ncol(r)])
   names(a) <- c("sp","lon","lat","bio1","bio2","bio3","bio4","bio5","bio6","bio8","bio9","bio12","bio13","bio14","bio15","bio18","bio19")
   write.csv(a,str_c(str_sub(bg.file, end=-5),"_swd.txt"), row.names=F, quote=F)
   print("..")
  } else print(".")
}


#--------------------------------------------------------------------#
# Presence points

for i in filled.*; do awk -F, '{print $3}' $i > $i.val.only; echo $i; done
awk -F, '{print $1 "," $2}' $i > filled.coordinates
awk -F, '{print $1""$2}' $i > filled.keys

paste -d, filled.coordinates filled.pr.valuesbio1.asc.csv.val.only filled.pr.valuesbio2.asc.csv.val.only filled.pr.valuesbio3.asc.csv.val.only filled.pr.valuesbio4.asc.csv.val.only filled.pr.valuesbio5.asc.csv.val.only filled.pr.valuesbio6.asc.csv.val.only  filled.pr.valuesbio8.asc.csv.val.only filled.pr.valuesbio9.asc.csv.val.only filled.pr.valuesbio12.asc.csv.val.only filled.pr.valuesbio13.asc.csv.val.only filled.pr.valuesbio14.asc.csv.val.only filled.pr.valuesbio15.asc.csv.val.only filled.pr.valuesbio18.asc.csv.val.only filled.pr.valuesbio19.asc.csv.val.only filled.key > filled.pr.values.all.var.csv


#--------------------------------------------------------------------#

# Function to create a background swd file for each species

ll <- unlist(sapply(list.files(dir.out, pattern="part.*",full=T), list.files, full=T))

# dir.log
this.log <- str_c(dir.out, "/log_make_backgrounds.txt")

sfInit(parallel=sf.parallel, cpus=sf.cpus, type=sf.type)
sfExport("extract.bg")
sfExport("dir.bg")
sfExport("this.log")

system.time(sfSapply(ll, function(x) extract.bg(x,no.bg=10, dir.bg=dir.bg, log=this.log))) # function located in F4...


sfStop()

#--------------------------------------------------------------------#
# get swd for each species

ll <- unlist(sapply(list.files(dir.out, pattern="part.*",full=T), list.files, full=T))
all.points <- read.csv(paste(dir.env, "/species.swd/filled.pr.values.all.var.csv", sep="")) # needs to be copied there firest. 

# dir.log
this.log <- str_c(dir.out, "/log_make_species_swd.txt")


system.time(sapply(ll, function(x) get.sp.swd(x,all.pts=all.points, log=this.log))) # function located in F5 ...

#--------------------------------------------------------------------#
# run Maxent
sp <- unlist(sapply(list.files(dir.out, pattern="part.*",full=T), list.files, full=T))

split.list <- rep(1:sum(cores),each=ceiling(length(sp)/sum(cores)), length.out=length(sp))
per.core <- split(sp, split.list)
t.count <- 1 # tmp count
for (server in 1:length(servers))
{
  # save paramters
  if(!file.exists(paste(dir.out,servers[server],sep="/"))) dir.create(paste(dir.out,servers[server],sep="/"))
  for (core in 1:cores[server])
  {
    if(length(per.core)>=t.count)
    {
      # write species list    
      write.table(per.core[[t.count]], paste(dir.out,"/",servers[server],"/species_list_core",core,".txt",sep=""), row.names=F, col.names=F, quote=F)
      # write R batch file for each core
      write(paste("# load params\n",
        "load(\"",dir.out,"parameters.RData\")\n",
        "# load data\n",
        "files <- read.table(\"",dir.out,"/",servers[server],"/species_list_core",core,".txt\")\n",
        "# prepare the log file\n",
        "log.file <- \"",dir.out,"/log_run_me_", servers[server], "_core", core,".txt\"\n",
        "write(\"sp_id;proc_time;finished_at\", log.file, append=F)\n",
        "# run maxent\n",
        "sapply(files[,1], function(i) run.maxent(path=i,max.ram=me.max.ram, dir.maxent=dir.maxent, no.replicates=me.no.replicates, replicate.type=me.replicate.type,log.file=log.file))",sep=""),
        paste(dir.out,"/",servers[server],"/runmaxent_core",core,".R",sep=""))
       t.count <- t.count+1
    }
  }
}

# Start for instances of R to run Maxent use:
for i in 1 2 3 4 ; do screen -S core$i -d -m R CMD BATCH results/20110128/gbif/runmaxent_core$i.R ; done

# -S name of screen session
# - d -m to detach the screen session at start

