#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL

#to be called from batch mode (Rscript)
args=(commandArgs(TRUE))

bd <- args[[1]] #base directory
src.dir.ps <- args[[2]] #dir containing basic functions to source
tp <- args[[3]] #type of error (shuffle/perturb)
v <- args[[4]] #variable (tmin/prec/tmean)
sca <- args[[5]] #scale (spatial / seasonal)
i <- as.numeric(args[[6]]) #seed
pval <- as.numeric(args[[7]]) #perturbed value (can be NA)

setwd(bd)

source(paste(src.dir.ps,"/EcoCrop-gnut_ps-functions.R",sep=""))
EcoCrop_ps(bd,ty=tp,va=v,sc=sca,s=i,p=pval)

