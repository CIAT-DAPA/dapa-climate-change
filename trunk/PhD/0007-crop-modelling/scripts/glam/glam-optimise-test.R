#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#May 2012

#optimise GLAM parameters using pre-selected inputs

#Read in a dummy GLAM parameter file and create a new one based on a new parameter for
#running and optimising GLAM
src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts"
source(paste(src.dir,"/glam-parFile-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-runfiles-functions.R",sep=""))
source(paste(src.dir,"/glam-soil-functions.R",sep=""))
source(paste(src.dir,"/glam-make_wth.R",sep=""))
source(paste(src.dir,"/glam-optimise-functions.R",sep=""))
source(paste(src.dir,"/climateSignals-functions.R",sep=""))


#input directories and model
bDir <- "F:/PhD-work/crop-modelling/GLAM"
cropName <- "gnut"
cDir <- paste(bDir,"/model-runs/",toupper(cropName),sep="")
pDir <- paste(cDir,"/params",sep="") #parameter files

#load cell details
cells <- read.csv(paste(bDir,"/climate-signals-yield/",toupper(cropName),"/signals/cells-process.csv",sep=""))
cell <- 636
method <- "lin"

#get defaults
params <- GLAM_get_default(x=cells,cell=636,parDir=pDir)
params$glam_param.mod_mgt$ISYR <- 1966 #start year
params$glam_param.mod_mgt$IEYR <- 1993 #end year

#now the optimisation routine
optimised <- list()
optimal <- list()

#NSL
#seq(5,100,length.out=20)
parname <- "NSL"; where <- "glam_param.mod_mgt"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=20,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
cat(parname,":",optimal[[parname]],"\n")
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#ISDAY
#seq(-50,-10,length.out=21)
parname <- "ISDAY"; where <- "glam_param.mod_mgt"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=21,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- -30 #optimal[[parname]]


#DLDTMX
#seq(0.01,0.1,length.out=10)
parname <- "DLDTMX"; where <- "glam_param.spt_mgt"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=10,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#EXTC
#seq(0.2,0.8,length.out=31)
parname <- "EXTC"; where <- "glam_param.spt_mgt"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=31,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#SWF_THRESH
#seq(0.5,1,length.out=26)
parname <- "SWF_THRESH"; where <- "glam_param.spt_mgt"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=26,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#EFV
#seq(1,2,length.out=26)
parname <- "EFV"; where <- "glam_param.soils"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=26,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#P_TRANS_MAX
#seq(0.15,0.4,length.out=26)
parname <- "P_TRANS_MAX"; where <- "glam_param.evap"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=26,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#TE
#seq(1,2.5,length.out=26)
parname <- "TE"; where <- "glam_param.bmass"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=26,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#TEN_MAX
#seq(1.5,5,length.out=36)
parname <- "TEN_MAX"; where <- "glam_param.bmass"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=36,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]] #3 from C2004


#GCPLFL
#seq(350,400,length.out=51)
parname <- "GCPLFL"; where <- "glam_param.phenol"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=51,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#TB (FLWR,PODF,LMAX,HARV)
#seq(8,12,length.out=17)
parname <- "TB"; where <- "glam_param.phenol"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=17,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[paste(parname,"FLWR",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"PODF",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"LMAX",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"HARV",sep="")]]$Value <- optimal[[parname]]


#TO (FLWR,PODF,LMAX,HARV)
#seq(28,37,length.out=19)
parname <- "TO"; where <- "glam_param.phenol"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=19,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[paste(parname,"FLWR",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"PODF",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"LMAX",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"HARV",sep="")]]$Value <- optimal[[parname]]


#TM (FLWR,PODF,LMAX,HARV)
#seq(40,50,length.out=21)
parname <- "TM"; where <- "glam_param.phenol"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=21,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[paste(parname,"FLWR",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"PODF",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"LMAX",sep="")]]$Value <- optimal[[parname]]
params[[where]][[paste(parname,"HARV",sep="")]]$Value <- optimal[[parname]]


#GCFLPF
#seq(310,330,length.out=21)
parname <- "GCFLPF"; where <- "glam_param.phenol"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=21,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#GCPFLM
#seq(200,300,length.out=51)
parname <- "GCPFLM"; where <- "glam_param.phenol"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=51,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]


#GCLMHA
#seq(500,750,length.out=51)
parname <- "GCLMHA"; where <- "glam_param.phenol"
optimised[[parname]] <- GLAM_optimise(GLAM_params=params,sect=where,param=parname,n.steps=51,
                                      cell=636,method="lin",cropName=cropName,bDir=bDir)
plot(optimised[[parname]]$VALUE,optimised[[parname]]$RMSE,ty="l",xlab="Parameter value",ylab="RMSE (kg/ha)",main=parname)
optimal[[parname]] <- optimised[[parname]]$VALUE[which(optimised[[parname]]$RMSE == min(optimised[[parname]]$RMSE))]
if (length(optimal[[parname]]) > 1) {optimal[[parname]] <- optimal[[parname]][round(length(optimal[[parname]])/2,0)]}
params[[where]][[parname]]$Value <- optimal[[parname]]



