## Data retrieval

setwd("~/Leeds-work/quest-for-robustness")
data <- read.table("mixture.dat", as.is=T, header=T)
data <- data$y


## Random Walk Metropolis 
## ----------------------

## Run random walk MH sampler to estimate the proportion, p,
## in a simple mixture distribution problem.
## The likelihood is given in (7.6)
## prior for p is Uniform(0,1).
## Here, the sampler is run in transformed space, u=log(p/(1-p)).

## We demonstrate the performance of the random
## walk chain when using two very different error distributions in the
## proposal , Unif(-1,1) and Unif(-.01,.01).


target.log=function(p,x) {  
  ## computer the log of the likelihood (based on equation (7.6) on page
  ## 187) times the prior.  Here the prior for p is
  ## Uniform(0,1)=Beta(1,1), so it won't play a part in the computation
  ## of the MH ratio and is omitted.
  
  ## Inputs:  p=probability estimate (delta in equation 7.6)
  ## 	   x=data vector
  ## Outputs: the log likelihood
  sum(log(p*dnorm(x,7,.5)+(1-p)*dnorm(x,10,.5)))
}


## CASE 1: proposal is a random walk with errors generated from
## Uniform(-1,1). 

set.seed(2) #Set random seed 

num.its=10000		#Number of iterations  
u=rep(0,num.its)	#MCMC output: vector of u realizations   	   
u[1]= runif(1,-1,1)     #Starting value (random number from uniform distribution)
p=rep(0,num.its)        #MCMC output: vector of p realizations   	   
p[1]=exp(u[1])/(1+exp(u[1]))    #transform u to p, see page 190-191

accept <- 0  ## acceptance count

## This is the code for the Metropolis Hastings algorithm, random walk
## chain. 

for (i in 1:(num.its-1)) {
  ##Generate proposal (random walk)       
  u[i+1]=u[i]+runif(1,-1,1)      
  
  ##Transform u to p, see page 190-191
  p[i+1]=exp(u[i+1])/(1+exp(u[i+1]))
  
  ##Compute Metropolis-Hastings ratio   
  MH=exp(target.log(p[i+1],data)-target.log(p[i],data))*
    (exp(u[i])*(1+exp(u[i+1]))^2)/(exp(u[i+1])*(1+exp(u[i]))^2)
  
  ##Reject or accept proposal
  
  uMH <- runif(1) ## uniform variable to determine acceptance
  
  if(uMH < MH) ## accept the new value
  {
    accept=accept+1 ## update count, u[i] is already fixed
  }
  else{p[i+1]=p[i]; u[i+1]=u[i];}
  
}

P.rw.1=p  #Save the output to examine later
u.rw.1=u
accept.rw.1=accept/num.its

## CASE 2: proposal is a random walk with errors generated from
## Uniform(-.01,.01).

set.seed(2) #Set random seed 

num.its=10000		#Number of iterations  
u2=rep(0,num.its)	#MCMC output: vector of u realizations   	   
u2[1]= runif(1,-1,1)    #Starting value          
p2=rep(0,num.its)       #MCMC output: vector of u realizations   	   
p2[1]=exp(u2[1])/(1+exp(u2[1]))  #transform u to p, see page 190-191

accept <- 0  ## acceptance count

for (i in 1:(num.its-1)) {
  #Generate proposal (random walk)       
  u2[i+1]=u2[i]+runif(1,-.01,.01)
  
  #Transform u to p, see page 190-191
  p2[i+1]=exp(u2[i+1])/(1+exp(u2[i+1]))
  
  #Compute Metropolis-Hastings ratio   
  MH=exp(target.log(p2[i+1],data)-target.log(p2[i],data))*
    (exp(u2[i])*(1+exp(u2[i+1]))^2)/(exp(u2[i+1])*(1+exp(u2[i]))^2)
  
  #Reject or accept proposal
  
  uMH <- runif(1) ## uniform variable to determine acceptance
  
  if(uMH < MH) ## accept the new value
  {
    accept=accept+1 ## update count, u[i] is already fixed
  }
  else{p2[i+1]=p2[i]; u2[i+1]=u2[i];}
}

P.rw.01=p2  #Save the output to examine later
u.rw.01=u2
accept.rw.01=accept/num.its

#__________________________________________________________________
#Plots of the output of the MCMC run:

#Sample paths (similar to figure 7.5 on page 192
#Note:  you may wish to remove iterations as a burn-in period
par(mfrow=c(2,1))
plot(P.rw.1,type="l",ylab="p",xlab="Iteration",ylim=c(.3,.85))
plot(P.rw.01,type="l",ylab="p",xlab="Iteration",ylim=c(.3,.85))


#Histograms of the estimated parameter 
#Note:  you may wish to remove iterations as a burn-in period
par(mfrow=c(2,1))
hist(P.rw.1,xlim=c(range(c(P.rw.1,P.rw.01))),nclass=40)
hist(P.rw.01,xlim=c(range(c(P.rw.1,P.rw.01))),nclass=40)

################################
## Visual Inspection of the chain

par(mfrow=c(2,1))
plot(P.rw.1, type="l", main="Sample path for mixing parameter -- case 1")
plot(P.rw.01, type="l", main="Sample path for mixing parameter -- case 2")

#################################
## Burn in

## Let choose BURN=2000, for now just assessed empirically

BURN=2000

################################
## Cusum plots

par(mfrow=c(2,1))
plot(cumsum(P.rw.1[-(1:BURN)])/seq(1, (num.its-BURN), by=1)-mean(P.rw.1[-(1:BURN)]), type="l", main="Cumulative sum plot for the posterior mean of delta -- case 1", ylab="Cusum deviation" , xlab="iteration")
plot(cumsum(P.rw.01[-(1:BURN)])/seq(1, (num.its-BURN), by=1)-mean(P.rw.01[-(1:BURN)]), type="l", main="Cumulative sum plot for the posterior mean of delta -- case 2", ylab="Cusum deviation", xlab="iteration")

##########################
### Autocorrelation plots

par(mfrow=c(2,1))
acf(P.rw.1, main="Autocorrelation plot for case 1")
acf(P.rw.01, main="Autocorrelation plot for case 1")


######
###### another example (with beta dist)
a=2.7; b=6.3; c=2.669 # initial values
Nsim=5000
X=rep(runif(1),Nsim) # initialize the chain
for (i in 2:Nsim){
  Y=runif(1)
  rho=dbeta(Y,a,b)/dbeta(X[i-1],a,b)
  X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<rho)
  cat(rho,"/",X[i],"\n")
}

plot(X,ty="l")
hist(X,nclass=20)




