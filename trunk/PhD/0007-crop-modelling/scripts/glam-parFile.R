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
dumFile <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-dum.txt",sep="")

#get the model parameters from the dummy file
GLAM_params <- GLAM_get_par(dumFile,retain=c("all"))

#parameter to optimise here
GLAM_params$glam_param.ygp$YGP$Value <- 0.5
GLAM_params$glam_param.ygp$YGP$NVAL <- 20

#simulation controls here
GLAM_params$glam_param.mod_mgt$MODE <- "HYP" #hypercube calibration
GLAM_params$glam_param.mod_mgt$SEASON <- "RFD" #rainfed season
GLAM_params$glam_param.mod_mgt$ISYR <- 1966 #start year
GLAM_params$glam_param.mod_mgt$IEYR <- 1994 #end year
GLAM_params$glam_param.mod_mgt$I_E <- 2 #I_E = 2, for GLAM-R2
GLAM_params$glam_param.mod_mgt$I_R <- 2 #I_R = 2, for GLAM-R2
GLAM_params$glam_param.sim_ctr$MMNO <- 1 #MMNO = 1, meaning calibration based on RMSE
GLAM_params$glam_param.sim_ctr$IMERF <- 0 #IMERF = 0 (local optimisation)
GLAM_params$glam_param.sim_ctr$SLA_INI <- 300 #SLA should be on!, SLA_INI = 300 (Challinor and Wheeler 2008)
GLAM_params$glam_param.sim_ctr$ZSMAX <- 210 #ZSMAX: depth of soil profile should be 210 cm, Challinor et al. (2004)
GLAM_params$glam_param.mod_mgt$HTS <- "+1" #turn on HTS subroutine, parameters are below
GLAM_params$glam_param.sim_ctr$IC02 <- 0 #Turn off CO2 enrichment routine
GLAM_params$glam_param.sim_ctr$ISHF <- 1 #ISHF = 1
GLAM_params$glam_param.sim_ctr$IUPT <- 1 #IUPT = 1
GLAM_params$glam_param.sim_ctr$TETRS <- "+1." #turn on heat stressed transpiration efficiency

#get the longitude of the selected gridcell
cell <- 636
cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
GLAM_params$glam_param.sim_ctr$SMLON <- round(cells$X[which(cells$CELL==cell)],1)
# windows()
# plot(cells$X,cells$Y,pch=20,cex=0.001)
# text(x=cells$X,y=cells$Y,labels=cells$CELL,cex=0.5)

GLAM_params$glam_param.soils$RLL$Value <- -99
GLAM_params$glam_param.soils$DUL$Value <- -99
GLAM_params$glam_param.soils$SAT$Value <- -99


#TDS should be on! [HIMIN,SWC_FAC], -99 on these values would turn off TDS
#SET 1: [0.1, 0.1]
#SET 2: [0.25, 0.01]
GLAM_params$glam_param.sparer$RSPARE1$Value <- 0.1 #HIMIN for TDS
GLAM_params$glam_param.sparer$RSPARE2$Value <- 0.1 #SWC_FAC for TDS
 
#HTS should be on! and use TOL (which is TMV2) Challinor et al. (2005)
# TCRITMIN = 34
# TCSLOPE = 0
# TLSLOPE = 2.5
# TLINT = 60
# PPCRIT = 0.6
# TLIMMIN = 40
#FD should be [width, offset], #probably good to pick F5
    #F1 = 5,0.2
    #F5 = 6,0.3
    #F6 = 10,0.5
GLAM_params$glam_param.hts_fut$TCRITMIN$Value <- 34
GLAM_params$glam_param.hts_fut$TCSLOPE$Value <- 0
GLAM_params$glam_param.hts_fut$TLSLOPE$Value <- 2.5
GLAM_params$glam_param.hts_fut$TLINT$Value <- 60
GLAM_params$glam_param.hts_fut$PPCRIT$Value <- 0.6
GLAM_params$glam_param.hts_fut$TLIMMIN$Value <- 40
GLAM_params$glam_param.hts_fut$IDURMAX$Value <- 6
GLAM_params$glam_param.hts_fut$IBAMAX$Value <- 6
GLAM_params$glam_param.hts_fut$IAAMAX$Value <- 12

#IC02 should be off (IC02==0), and TENFAC should be -99
#These would be activated for future simulations with high CO2 concentrations
GLAM_params$glam_param.hts_fut$TENFAC$Value <- -99

#SWFF_THR: damage to flowers due to water stress. Turned on. Challinor et al. (2006)
GLAM_params$glam_param.hts_fut$SWFF_THR$Value <- 0.2

#TETRS (reduction of transp. efficiency under heat stress)
#should be on, TETRS="+1"
#TETR1 = 35; TETR2 = 47
GLAM_params$glam_param.hts_fut$TETR1$Value <- 35
GLAM_params$glam_param.hts_fut$TETR2$Value <- 47

#write the model params
opfil <- paste(parDir,"/glam-r2-param-",tolower(cropName),"-hyp.txt",sep="")
opfil <- GLAM_create_parfile(params=GLAM_params,outfile=opfil,base_file=NA,overwrite=T)






