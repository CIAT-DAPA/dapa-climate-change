#Trying out Toshi Iizumi's method for MCMC of parameter values
#using simple linear regression

trueA <- 5
trueB <- 0
#trueSd <- 10
sampleSize <- 31

# create independent x-values 
x <- (-(sampleSize-1)/2):((sampleSize-1)/2)

# create dependent values according to ax + b + N(0,sd)
y <-  trueA * x + trueB #+ rnorm(n=sampleSize,mean=0,sd=trueSd)

### example
#we want to estimate distributions of parameters A, B, and Sd
#we evaluate against values of Y (target outcome)

#define number of iterations
iterations <- 30000

#step 1: choose initial value for first parameter and a sampler
a_i <- 0; b_i <- 2#; sd_i <- 8
sampler <- function(param) {retx <- param + runif(1,min=-0.5,max=0.5); return(retx)}

#do for parameter "a" first
chain <- rep(NA,times=(iterations+1))

chain[1] <- a_i
for (i in 1:iterations) {
  #i <- 1
  #step 2: propose a candidate
  a_c <- sampler(chain[i])
  
  #step 3: run model using this candidate
  y_c <- a_c * x + b_i #+ rnorm(n=sampleSize,mean=0,sd=sd_i)
  y_i <- chain[i] * x + b_i #+ rnorm(n=sampleSize,mean=0,sd=sd_i)
  
  #step 4: calculate likelihood function for both the candidate and the current value
  errfun_c <- sd(y_c) #sd(y-y_c)
  sumfun_c <- sum((y-y_c)^2)
  likfun_c <- (2*pi*errfun_c)^(-sampleSize/2)*exp((-1/(2*errfun_c^2))*sumfun_c)
  
  errfun_i <- sd(y_i) #sd(y-y_i)
  sumfun_i <- sum((y-y_i)^2)
  likfun_i <- (2*pi*errfun_i)^(-sampleSize/2)*exp((-1/(2*errfun_i^2))*sumfun_i)
  
  #calculate posterior likelihood
  #prior_c <- sum(dnorm(y,mean=y_c,sd=sd(y)))
  #prior_i <- sum(dnorm(y,mean=y_i,sd=sd(y)))
  priorratio <- 1
  
  #step 5: calculate ratios
  alpha <- (likfun_c/likfun_i)*priorratio
  
  #step 6: set value according to rule
  if (runif(1) < alpha){
    chain[i+1] = a_c
  } else {
    chain[i+1] = chain[i]
  }
}

plot(chain,ty="l")
burnIn <- 100000
a_mh <- mean(chain[-(1:burnIn)])
a_mhsd <- sd(chain[-(1:burnIn)])

hist(chain[-(1:burnIn)],nclass=40,xlim=c(4,5.5))
abline(v=a_mh,col="blue")
abline(v=trueA,col="red")



