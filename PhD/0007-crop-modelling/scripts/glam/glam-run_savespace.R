#Julian Ramirez-Villegas
#June 2012
#CIAT / CCAFS / UoL


#functions to save space in the hard drive by only keeping strictly needed files 
#from a given GLAM optimisation run. If specific runs need to be re-done then
#it is best to just re-run the model again for that particular configuration.

#these functions should be run after a given optimisation run is completed and fully summarised

#local
#src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
#b_dir <- "W:/eejarv/PhD-work/crop-modelling/GLAM"

#eljefe
src.dir <- "~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
b_dir <- "/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM"


#sourcing functions
source(paste(src.dir,"/glam/glam-run_savespace-functions.R",sep=""))

#details of crop and base folder of runs and data
#crop_name <- "gnut"
#crop_name_long <- "groundnut"
#zones <- c(1:5)

#details of experiment i want to get
#exp_id <- 10

for (exp in 31:32) {
  x <- savespace(exp_id=exp,b_dir=b_dir,zones=c(1:5),crop_name="gnut",crop_name_long="groundnut",dump=T)
}









