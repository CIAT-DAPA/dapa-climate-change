#lambda != 0, positive or zero x
c1 <- function(x,lambda) {xt <- (((x+1)^(lambda))-1) / lambda; return(xt)}
c1i <- function(xt, lambda) {x <- ((xt*lambda+1)^(1/lambda))+1; return(x)}

#lambda == 0, positive or zero x
c2 <- function(x) {xt <- log(x+1); return(xt)}
c2i <- function(xt) {x <- exp(xt)-1; return(x)}

#lambda != 2, negative x
c3 <- function(x,lambda) {xt <- (((1-x)^(2-lambda)) - 1) / (lambda-2); return(xt)}
c3i <- function(xt, lambda) {x <- 1 - (((xt*(lambda-2)) + 1) ^ (1/(2-lambda))); return(x)}

#lambda == 2, negative x
c4 <- function(x) {xt <- -log(1-x); return(xt)}
c4i <- function(xt) {x <- 1-exp(-xt); return(x)}

#Condition 1 values, x=1.5, lambda=0.2 -OK!
vt <- yjohn(1.5, 0.2)
v <- yjohn.inv(vt, 0.2)
cat(vt, v, "\n")

#Condition 2 values, x=1.5, lambda=0 -OK!
vt <- yjohn(1.5, 0)
v <- yjohn.inv(vt, 0)
cat(vt, v, "\n")

#Condition 3 values, x=-1.5, lambda=0, 0.2 -OK!
vt <- yjohn(-1.5, 0)
v <- yjohn.inv(vt, 0)
cat(vt, v, "\n")

vt <- yjohn(-1.5, 0.2)
v <- yjohn.inv(vt, 0.2)
cat(vt, v, "\n")

#Condition 4 values, x=-1.5, lambda=2
vt <- yjohn(-1.5, 2)
v <- yjohn.inv(vt, 2)
cat(vt, v, "\n")

yjohn <- function(x, lambda) { #YEO-JOHNSON
	if (lambda != 0 & x >= 0) {
		cat("Condition 1 \n")
		xt <- ((x+1)^lambda-1)/lambda
	} else if (lambda == 0 & x >= 0) {
		cat("Condition 2 \n")
		xt <- log(x+1)
	} else if (lambda != 2 & x < 0) {
		cat("Condition 3 \n")
		xt <- (((1-x)^(2-lambda))-1) / (lambda-2)
	} else if (lambda == 2 & x < 0) {
		cat("Condition 4 \n")
		xt <- -log(1-x)
	}
	return(xt)
}
yjohn.inv <- function(xt, lambda) { #YEO-JOHNSON
	if (lambda == 0) {
		xa <- exp(xt)-1 #COND 2
		xb <- 1 - (((xt*(lambda-2)) + 1) ^ (1/(2-lambda))) #COND 3
		if(is.na(xa) | xa < 0) {x <- xb; cat("Condition 3 \n")}
		if(is.na(xb) | xb >= 0) {x <- xa; cat("Condition 2 \n")}
	} else if (lambda == 2) {
		xa <- ((xt*lambda+1)^(1/lambda))-1 #COND 1
		xb <- 1-exp(-xt) #COND 4
		if(is.na(xa) | xa < 0) {x <- xb; cat("Condition 4 \n")}
		if(is.na(xb) | xb >= 0) {x <- xa; cat("Condition 1 \n")}
	} else {
		xa <- ((xt*lambda+1)^(1/lambda))-1 #COND 1
		xb <- 1 - (((xt*(lambda-2)) + 1) ^ (1/(2-lambda))) #COND 3
		if(is.na(xa) | xa < 0) {x <- xb; cat("Condition 3 \n")}
		if(is.na(xb) | xb >= 0) {x <- xa; cat("Condition 1 \n")}
	}
	return(x)
}

