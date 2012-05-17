#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glamParFile-functions.R",sep=""))

bDir <- "F:/PhD-work/crop-modelling/GLAM"

cropName <- "gnut"
cropDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")

parDir <- paste(cropDir,"/params",sep="")
dumFile <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-dum.txt",sep="")

GLAM_get_par <- function(parFile,retain="all") {
  ret_list <- list()
  
  if (retain=="all" | "ygp" %in% retain) glam_param.ygp <- get_ygp(parFile) #read ygp
  if (retain=="all" | "simC" %in% retain) glam_param.simC <- get_sc(parFile) #simulation controls
  glam_param.mmgtC <- get_mm(parFile) #model management
  glam_param.spmC <- get_spm(parFile) #spatial management and LAI
  glam_param.sspC <- get_ssp(parFile) #soil spatial parameters
  glam_param.dupC <- get_du(parFile) #drainage and uptake
  glam_param.etC <- get_et(parFile) #evaporation and transpiration
  glam_param.bmC <- get_bm(parFile) #biomass
  glam_param.pheC <- get_phe(parFile) #phenology
  glam_param.fswsow <- get_line(parFile,l=82,float=T) #intelligent sowing
  glam_param.addC <- get_add(parFile) #additional variables
  glam_param.awhea <- get_awht(parFile) #additional wheat variables
  glam_param.wwinC <- get_wwin(parFile) #additional winter wheat variables
  glam_param.maiC <- get_mai(parFile) #additional maize variables
  glam_param.ricC <- get_rice(parFile) #additional rice variables
  glam_param.spiC <- get_spi(parFile) #spare integer and real variables
  glam_param.sprC <- get_spr(parFile) #spare integer and real variables
  
  out_list <- list(get(ls(pattern="glam_param.")))
  
  return()
  
}




