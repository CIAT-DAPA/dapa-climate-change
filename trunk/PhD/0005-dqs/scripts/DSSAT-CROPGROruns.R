#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
stop("error")

################################################################################
#Create a function for using this data frame to parallelise the stuff
#control_list <- read.csv(paste(bd,"/bin/control/p_prec_climate_test.csv",sep="")) #load test control file

src.dir <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
source(paste(src.dir,"/DSSAT-functions.R",sep=""))

####################################################
#### THESE TWO NEED TO BE CHANGED IN A MACHINE ####
####################################################
bd <- "D:/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT"
nproc <- 5
####################################################
####################################################

library(snowfall) #load library
sfInit(parallel=T,cpus=nproc) #initiate cluster

#export functions
sfExport("accuracy")
sfExport("optSLPF")
sfExport("runCROPGRO")
sfExport("runCROPGROPS")
sfExport("runPSModel")
sfExport("writeSoilFile")

#export data
sfExport("bd")

#run the parallel function looping through perturbed values
control_list <- read.csv(paste(bd,"/bin/control/p_yield_season.csv",sep="")) #load control file
p_unique <- unique(control_list$P)
for (pval in p_unique) {
  cat("Process",pval,"\n")
  tp <- control_list$TYPE[1]; va <- control_list$VAR[1]; sca <- control_list$SCALE[1]
  if (is.na(pval)) {
    reduced_list <- control_list
  } else {
    reduced_list <- control_list[which(control_list$P==pval),]
  }
  s_list <- reduced_list$SEED
  controlPS <- function(i) { #define a new function
    #cat(paste(x[1],x[2],x[3],x[4],x[5]),"\n")
    runPSModel(bd,ty=tp,v=va,sc=sca,s=i,p=pval)
    oFILE <- paste(bd,"/bin/control/",tp,"_",va,"_",sca,".ctr",sep="")
    system(paste("echo s=",i,"p=",pval,">>",oFILE))
  }
  sfExport("tp");sfExport("va");sfExport("sca");sfExport("pval")
  system.time(sfSapply(as.vector(s_list), controlPS))
}

sfStop() #stop the cluster
