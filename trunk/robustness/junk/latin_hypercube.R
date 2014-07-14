#latin hypercube sampling

library(lhs)

#n=number of points (i.e. replicas) --how many values of each dimension?
#k=number of dimensions (i.e. parameters) --how many dimensions (i.e. parameters)?
set.seed(1234)
lhyp1 <- maximinLHS(n=20,k=5,dup=1)

plot(rep(1,ncol(lhyp1)),lhyp1[1,],xlim=c(1,nrow(lhyp1)),ylim=c(0,1),pch=c(1:ncol(lhyp1)),cex=0.8,
     xlab="Replication",ylab="Parameter value in U[0,1]")
abline(v=1,lty=2,col="grey 80")
for (i in 2:nrow(lhyp1)) {
  abline(v=i,lty=2,col="grey 80")
  points(rep(i,ncol(lhyp1)),lhyp1[i,],pch=c(1:ncol(lhyp1)),cex=0.8)
}

#to generate the values in the actual parameter range
#where min and max are specific to parameter being sampled
p1vals <- qunif(lhyp1[1,],min=1,max=2.5) 
