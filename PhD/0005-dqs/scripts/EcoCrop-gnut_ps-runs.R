#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL
#stop("error")

#Fist test of the function
#EcoCrop_ps(bd,ty="s",va="tmin",sc="seasonal",s=1000,p=NA)

#3. write the small looped wrapper
####################################################
#### THESE FIVE NEED TO BE CHANGED IN A MACHINE ####
####################################################
#bd <- "D:/CIAT_work/GLAM/PNAS-paper/EcoCrop-GNUT"
#src.dir.ps <- "D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts"
#src.dir <- "D:/_tools/dapa-climate-change/trunk/EcoCrop"
#nproc <- 5
#conList <- "s_prec_seasonal.csv"
####################################################
####################################################
library(snowfall) #load library
sfInit(parallel=T,cpus=nproc) #initiate cluster

#export directories
sfExport("bd")
sfExport("src.dir.ps")

control_list <- read.csv(paste(bd,"/bin/control/",conList,sep="")) #load control file
p_unique <- unique(control_list$P)
for (pval in p_unique) {
  cat("Process",pval,"\n")
  tp <- control_list$TYPE[1]; v <- control_list$VAR[1]; sca <- control_list$SCALE[1]
  if (is.na(pval)) {
    reduced_list <- control_list
  } else {
    reduced_list <- control_list[which(control_list$P==pval),]
  }
  s_list <- reduced_list$SEED
  controlPS <- function(i) { #define a new function
    system(paste("Rscript --vanilla --no-save D:/_tools/dapa-climate-change/trunk/PhD/0005-dqs/scripts/EcoCrop-PSBatchRun.R",
                 bd,src.dir.ps,src.dir,tp,v,sca,i,pval))
  }
  sfExport("tp");sfExport("v");sfExport("sca");sfExport("pval")
  system.time(sfSapply(as.vector(s_list), controlPS))
}

sfStop() #stop the cluster
