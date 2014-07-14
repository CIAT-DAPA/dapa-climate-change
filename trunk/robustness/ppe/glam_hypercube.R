#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")

#1. load list of parameters
#2. create sample of parameter sets
#3. store the matrix of combinations as a data.frame

library(lhs)

#input directories
wd <- "~/Leeds-work/quest-for-robustness"
#wd <- "/nfs/a101/earjr/quest-for-robustness"
mdata_dir <- paste(wd,"/data/model_data",sep="")

###
#1. load list of parameters and ranges (./data/model_data/parameter_list.txt)
param_list <- read.csv(paste(mdata_dir,"/parameter_list.txt",sep=""),sep="\t",header=T)

#2. create sample of parameter sets
#n=number of points (i.e. replicas) --how many values in each dimension?
#k=number of dimensions (i.e. parameters) --how many dimensions (i.e. parameters)?
nrep <- 100
set.seed(2303)
lhyp1 <- maximinLHS(n=nrep,k=nrow(param_list),dup=1)

#to generate the values in the actual parameter range
#where min and max are specific to parameter being sampled
out_df <- as.data.frame(matrix(NA, nrow=nrep, ncol=nrow(param_list)))
names(out_df) <- param_list$PARAM
for (i in 1:nrow(param_list)) {
  #i <- 1
  pvals <- qunif(lhyp1[1,],min=param_list$MIN[i],max=param_list$MAX[i])
  out_df[,i] <- pvals
}

