#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))

bDir <- "F:/PhD-work/crop-modelling/GLAM"
cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

parDir <- paste(cropDir,"/params",sep="")
cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))

#get defaults
GLAM_params <- GLAM_get_default(x=cells,cell=636,parDir=parDir)

#parameter to optimise here
GLAM_params$glam_param.ygp$YGP$Value <- 0.5
GLAM_params$glam_param.ygp$YGP$NVAL <- 20
GLAM_params$glam_param.mod_mgt$ISYR <- 1966 #start year
GLAM_params$glam_param.mod_mgt$IEYR <- 1993 #end year

#write the model params
opfil <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-run.txt",sep="")
opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)






