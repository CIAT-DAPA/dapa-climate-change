#Julian Ramirez-Villegas
#November 2011
#CIAT / CCAFS / UoL

#to be called from batch mode (Rscript)
args=(commandArgs(TRUE))

bd <- args[[1]] #base directory
src.dir.ps <- args[[2]] #dir containing basic functions to source
src.dir <- args[[3]]
tp <- args[[4]] #type of error (shuffle/perturb)
v <- args[[5]] #variable (tmin/prec/tmean)
sca <- args[[6]] #scale (spatial / seasonal)
i <- as.numeric(args[[7]]) #seed
pval <- as.numeric(args[[8]]) #perturbed value (can be NA)

setwd(bd)

source(paste(src.dir,"/src/EcoCrop.R",sep=""))
source(paste(src.dir,"/src/accuracy.R",sep=""))
source(paste(src.dir.ps,"/EcoCrop-gnut_ps-functions.R",sep=""))
source(paste(src.dir.ps,"/EcoCrop-evaluation_ps.R",sep=""))
EcoCrop_ps(bd,ty=tp,va=v,sc=sca,s=i,p=pval)

