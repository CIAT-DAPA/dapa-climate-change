#Julian Ramirez-Villegas
#June 2012
#CIAT / CCAFS / UoL

#functions to summarise a given GLAM run based on a given inner folder
#and a given variable to be summarised

#irrigation fraction needs to be input

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
#b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"


#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling"
b_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"

#details of crop and base folder of runs and data
crop_name <- "gnut"
crop_dir <- paste(b_dir,"/model-runs/",toupper(crop_name),sep="")

#load variable names
varnames <- read.table(paste(src.dir,"/data/GLAM-varnames.tab",sep=""),header=T,sep="\t")

#details of experiment i want to get
exp_id <- 10
if (exp_id < 10) {exp_id <- paste("0",exp_id,sep="")} else {exp_id <- paste(exp_id)}
selection <- "v4"
cells <- read.csv(paste(crop_dir,"/inputs/calib-cells-selection-",selection,".csv",sep=""))

#folders
cal_dir <- paste(crop_dir,"/calib",sep="")
exp_dir <- paste(cal_dir,"/exp-",exp_id,"_outputs",sep="")

###
#select variable and gridcell
vid <- 8
cell <- cells$CELL[1]
vname <- paste(varnames$EOS[8])

#open out file


#set names


#capture values for all years


#transpose











