#Julian Ramirez-Villegas
#UoL / CCAFS / CIAT
#Apr 2014
stop("!")

#1. load list of parameters
#2. create sample of parameter sets

library(lhs)

#n=number of points (i.e. replicas) --how many values of each dimension?
#k=number of dimensions (i.e. parameters) --how many dimensions (i.e. parameters)?
set.seed(1234)
lhyp1 <- maximinLHS(n=20,k=5,dup=1)


