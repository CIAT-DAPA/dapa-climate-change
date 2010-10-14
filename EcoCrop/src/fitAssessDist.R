#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Description: Fit the data on different distributions (debug mode for now)

library(MASS)
library(nortest)

###DECLARE FUNCTIONS FOR MEASURES OF GOODNESS OF FIT
mae <- function(x) { #x is a 2-column matrix (second column are the fitted values)
	x <- x[which(!is.na(x[,1])) & which(!is.na(x[,2])),]
	maef <- function(m) {abs(m[1]-m[2])}
	maer <- sum(apply(x, 1, maef)) / nrow(x)
	return(maer)
}
armsq <- function(x) { #x is a 2-column matrix
	x <- x[which(!is.na(x[,1])) & which(!is.na(x[,2])),]
	armsqf <- function(m) {(m[1]-m[2])^2}
	armsqr <- sqrt(sum(apply(x, 1, armsqf)) / nrow(x))
	return(armsqr)
}
mre <- function(x) { #x is a 2-column matrix
	x <- x[which(!is.na(x[,1])) & which(!is.na(x[,2])),]
	mref <- function(m) {abs(m[1]-m[2])}
	mrer <- sum(apply(x, 1, mref))/sum(x[,1])
	return(mrer)
}
rrmsq <- function(x) { #x is a 2-column matrix
	x <- x[which(!is.na(x[,1])) & which(!is.na(x[,2])),]
	rrmsqf <- function(m) {(m[1]-m[2])^2}
	rrmsqr <- sqrt(sum(apply(x, 1, rrmsqf)) / nrow(x)) / (sum(x[,1]) / (nrow(x)))
	return(rrmsqr)
}
rrmsq2 <- function(x) { #x is a 2-column matrix
	x <- x[which(!is.na(x[,1])) & which(!is.na(x[,2])),]
	rrmsq2f <- function(m) {(m[1]-m[2])^2}
	rrmsq2r <- sqrt(sum(apply(x, 1, rrmsq2f)) / (sum(x[,1]^2)))
	return(rrmsq2r)
}

#FUNCTION
fitTestDist <- function(x.real, plotqq=T) {
	#1. FIT THE DISTRIBUTION
	z.real <- (x.real-mean(x.real))/sd(x.real)
	d.real <- fitdistr(z.real, densfun="normal")
	#2. CREATE A THEORETICAL ONE USING THE PARAMETERS ("The perfect fit")
	z.theo <- rnorm(n=length(z.real), m=mean(z.real), sd=sd(z.real)) #m=d.real$estimate[1], sd=d.real$estimate[2])
	
	#3. PRODUCE THE QQ-PLOT DATA
	if (plotqq) {
		qqp <- qqplot(z.theo, z.real, plot.it=T)
		abline(0,1)
	} else {qqp <- qqplot(z.theo, z.real, plot.it=F)}
	#4. ASSESS ACCURACY OF FIT USING DATA FROM THE QQPLOT
	qqp.data <- cbind(qqp$x, qqp$y)
	r <- cor(qqp$x,qqp$y)
	eps <- mae(qqp.data)
	eps2 <- armsq(qqp.data)
	sgm <- mre(qqp.data)
	sgm2 <- rrmsq(qqp.data)
	sgm22 <- rrmsq2(qqp.data)
	metrics <- data.frame(r, eps, eps2, sgm, sgm2, sgm22, n=length(x.real))
	
	#5. PERFORM SCHAPIRO-WILKINS & KOLMOGOROV-SMIRNOV TESTS
	ks <- ks.test(z.real, z.theo)
	sw <- shapiro.test(z.real)
	pe <- pearson.test(z.real)
	n.test <- rbind(c("ks", ks$statistic, ks$p.value), cbind("shapiro", sw$statistic, sw$p.value), cbind("pearson", pe$statistic, pe$p.value))
	n.test <- as.data.frame(n.test)
	names(n.test) <- c("TEST", "STATISTIC", "P.VALUE")
	res <- list(METRICS=metrics,HYPOTHESIS.TEST=n.test)
	return(res)
}

#BOX-COX FUNCTION & MODIFICATIONS
default <- function(x, lambda) { #DEFAULT BOX-COX FUNCTION
	if (lambda == 0) {xt <- log(x)} else {xt <- (((x^lambda) - 1) / lambda)}
	return(xt)
}
manly <- function(x, lambda) { #MANLY (1971)
	if (lambda == 0) {xt <- x} else {xt <- (((exp(lambda*x)) - 1) / lambda)}
	return(xt)
}
jdrap <- function(x, lambda) { #JOHN-DRAPER (1980)
	if (x < 0) {sg <- -1} else {sg <- 1}
	if (lambda == 0) {xt <- sg*log(abs(x)+1)} else {xt <- sg*((((abs(x)+1)^lambda)-1)/(log(abs(x)+1)))}
	return(xt)
}
bdoks <- function(x, lambda) { #BICKEL-DOKSUM (1981)
	if (x < 0) {sg <- -1} else {sg <- 1}
	if (lambda == 0) {xt <- log(x)} else if (lambda > 0) {xt <- ((((abs(x))^lambda)*sg - 1) / lambda)} else {xt <- (((x^lambda) - 1) / lambda)}
	return(xt)
}
yjohn <- function(x, lambda) { #YEO-JOHNSON
	if (lambda != 0 & x >= 0) {
		xt <- ((((x+1)^lambda) - 1) / lambda)
	} else if (lambda == 0 & x >= 0) {
		xt <- log(x+1)
	} else if (lambda != 2 & x < 0) {
		xt <- ((((1 - x)^(2 - lambda)) - 1) / (lambda-2))
	} else if (lambda == 2 & x < 0) {
		xt <- -log(1 - x)
	}
	return(xt)
}

#6. NOW USE THE BOX-COX TRANSFORMATION FOR TRANSFORMING THE DISTRIBUTION INTO A NORMAL ONE
boxCox <- function(vec, method="default", ht="ks", seq.start=-1, seq.end=1, seq.step=0.005, wd="./data", verbose=T, plotsteps=T, varname="prec") {
	#Ignore all NAs
	vec <- vec[which(!is.na(vec))]
	nsteps <- (seq.end-seq.start) / seq.step
	vec <- as.matrix(vec)
	#vec <- (vec - mean(vec)) / sd(vec)
	if (!tolower(method) %in% c("default","manly","jdrap","bdoks","yjohn")) {
		stop("Wrong type selection, should be default, manly, jdrap, bdoks, yjohn")
	}
	if (!tolower(ht) %in% c("ks","shapiro","pearson")) {
		stop("Invalid hypothesis test, only supporting ks, shapiro and pearson")
	}
	iter <- 1
	if (verbose) cat("Iterating", nsteps, "times\n")
	for (lmd in seq(seq.start,seq.end,seq.step)) {
		if (verbose) cat(iter, ".", sep="")
		if (method=="default") {
			if (min(vec) <= 0 & iter == 1) {
				a <- abs(min(vec)) + 0.1
				vec <- vec + a
				warning("Since x contained negative or zero values, a base number ", a, " was added to all the data")
			} else {a <- 0}
			tvec <- default(vec, lmd)
		} else if (method=="manly") {
			tvec <- manly(vec, lmd)
		} else if (method=="jdrap") {
			tvec <- apply(vec, 1, jdrap, lmd)
		} else if (method=="bdoks") {
			tvec <- bdoks(vec, lmd)
		} else if (method=="yjohn") {
			tvec <- apply(vec, 1, yjohn, lmd)
		}
		if (plotsteps) {fitDist.res <- fitTestDist(tvec, plotqq=T)} else {fitDist.res <- fitTestDist(tvec, plotqq=F)}
		ht.res <- fitDist.res$HYPOTHESIS.TEST
		ht.sel <- ht.res[which(ht.res$TEST == ht),]
		ht.res.row <- data.frame(ht.sel[2], ht.sel[3], LAMBDA=lmd, R=fitDist.res$METRICS$r, N=fitDist.res$METRICS$n, EPS=fitDist.res$METRICS$eps, EPS2=fitDist.res$METRICS$eps2, SIGMA=fitDist.res$METRICS$sgm, SIGMA2=fitDist.res$METRICS$sgm2, SIGMA22=fitDist.res$METRICS$sgm22)
		if (iter == 1) {ht.res.matrix <- ht.res.row} else {ht.res.matrix <- rbind(ht.res.matrix, ht.res.row)}
		iter <- iter+1
	}
	if (verbose) cat("\n")
	write.csv(ht.res.matrix, paste(wd,"/box-cox-fitting-process-", varname,".csv",sep=""), row.names=F, quote=T)
	htfinal <- read.csv(paste(wd,"/box-cox-fitting-process-", varname, ".csv",sep=""))
	#par(mfrow=c(3,1))
	#plot(htfinal$LAMBDA, htfinal$P.VALUE, xlab="Lambda", ylab="p-value", type="l")
	#plot(htfinal$LAMBDA, htfinal$STATISTIC, xlab="Lambda", ylab="Statistic", type="l")
	#plot(htfinal$LAMBDA, htfinal$R, xlab="Lambda", ylab="R Pearson", type="l")
	
	htfinal$DCRIT <- 1.95*sqrt((htfinal$N*2)/(htfinal$N^2))
	htfinal$SIGN <- htfinal$DCRIT > htfinal$STATISTIC
	htfinal.sig <- htfinal[which(htfinal$SIGN==T),]
	if (nrow(htfinal.sig) == 0) {
		cat("The transformation was unsuccessful, please check the range and steps of the sequencing, or change the type of transformation \n")
		cat("Maximum P.VALUE was", max(htfinal$P.VALUE), "and was found on a lambda of", htfinal$LAMBDA[which(htfinal$P.VALUE == max(htfinal$P.VALUE))], "\n")
		stop()
	}
	final.lambda <- htfinal.sig$LAMBDA[which(htfinal.sig$P.VALUE==max(htfinal.sig$P.VALUE))]
	if (method=="default") {return(list(LAMBDA=final.lambda, ADD.PAR=a))}
	else {return(final.lambda)}
}

#FINAL HISTOGRAM FOR FITTING COMPARISON
histFit <- function(x, lambda, method="default", plotdir="./img", varname="prec") {
	if (!tolower(method) %in% c("default","manly","jdrap","bdoks","yjohn")) {
		stop("Wrong type selection, should be default, manly, jdrap, bdoks, yjohn")
	}
	if (method=="default") {
		if (min(x) <= 0) {
			a <- min(x) + 1
			x <- x + a
			warning("Since x contained negative or zero values, a base number ", a, " was added to all the data")
		}
		xt <- default(x, lambda)
	} else if (type=="manly") {
		xt <- manly(x, lambda)
	} else if (type=="jdrap") {
		xt <- apply(x, 1, jdrap, lambda)
	} else if (type=="bdoks") {
		xt <- bdoks(x, lambda)
	} else if (type=="yjohn") {
		xt <- yjohn(x, lambda)
	}
	h <- hist(xt,breaks=15,plot=F)
	xhist <- c(min(h$breaks),h$breaks)
	yhist<-c(0,h$density,0)
	xfit<-seq(min(xt),max(xt),length=40)
	yfit<-dnorm(xfit,mean=mean(xt),sd=sd(xt))
	jpeg(paste(plotdir, "/bestFit-BoxCox-", varname, ".jpg", sep=""), quality=100, height=600, width=600)
	plot(xhist,yhist,type="s",ylim=c(0,max(yhist,yfit)),xlab="Transformed values", ylab="Density function")
	lines(xfit,yfit,col="red")
	dev.off()
	return(xt)
}

#NEED TO INVERSE BOX-COX FUNCTION AND MODIFICATIONS
default.inv <- function(xt, lambda) { #DEFAULT BOX-COX FUNCTION
	if (lambda == 0) {x <- exp(xt)} else {x <- ((lambda*xt + 1)^(1 / lambda))}
	return(x)
}
manly.inv <- function(xt, lambda) { #MANLY (1971)
	if (lambda == 0) {x <- xt} else {x <- ((log(lambda*xt + 1)) / lambda)}
	return(x)
}
jdrap.inv <- function(xt, lambda) { #JOHN-DRAPER (1980) GIVES TWO OUTCOMES
	if (lambda == 0) {x <- exp(xt) - 1} else {x <- ((xt*lambda+1)^(1/lambda))-1}
	return(list(OUT1=x,OUT2=-x))
}
bdoks.inv <- function(xt, lambda) { #BICKEL-DOKSUM (1981) GIVES TWO OUTCOMES
	if (lambda == 0) {x <- exp(xt)} else if (lambda > 0) {x <- ((xt*lambda + 1)^(1 / lambda))} else {x <- ((lambda*xt + 1) ^ (1 / lambda))}
	if (lambda > 0) {return(list(OUT1=x,OUT2=-x))} else {return(list(OUT1=x,OUT2=x))}
}
yjohn.inv <- function(xt, lambda) { #YEO-JOHNSON
	if (lambda == 0) {
		xa <- exp(xt)-1 #COND 2
		xb <- (1 - (((xt * (lambda - 2)) + 1) ^ (1 / (2 - lambda)))) #COND 3
		if(is.na(xa) | xa < 0) {x <- xb}
		if(is.na(xb) | xb >= 0) {x <- xa}
	} else if (lambda == 2) {
		xa <- (((xt * lambda + 1) ^ (1 / lambda)) - 1) #COND 1
		xb <- (1 - (exp(-xt))) #COND 4
		if(is.na(xa) | xa < 0) {x <- xb}
		if(is.na(xb) | xb >= 0) {x <- xa}
	} else {
		xa <- ((xt * lambda + 1) ^ (1 / lambda)) - 1 #COND 1
		xb <- 1 - (((xt * (lambda - 2)) + 1) ^ (1 / (2 - lambda))) #COND 3
		if(is.na(xa) | xa < 0) {x <- xb}
		if(is.na(xb) | xb >= 0) {x <- xa}
	}
	return(x)
}

#GET CALIBRATION PARAMETERS. GET THESE QUANTILES: 20%, 40%, 60% and 80% FROM THE TRANSFORMED DISTRIBUTION
getCalPars <- function(x.trans, lambda, method="default", a) {
	if (!tolower(method) %in% c("default","manly","jdrap","bdoks","yjohn")) {
		stop("Wrong type selection, should be default, manly, jdrap, bdoks, yjohn")
	}
	if (is.na(lambda)) {
		stop("Invalid value for lambda, please check")
	} else {
		cal.par <- quantile(x.trans, probs=c(0.05,0.2,0.4,0.6,0.8))
		cal.par <- as.matrix(cal.par)
		if (method == "default") {
			if (is.na(a)) {stop("For this function you need to provide an adding parameter, use a=0 if no adding was required in the first place")}
			cal.par.real <- apply(cal.par, 1, default.inv, lambda)
			cal.par.real <- cal.par.real - a
		} else if (method == "manly") {
			cal.par.real <- apply(cal.par, 1, manly.inv, lambda)
			cal.par.real <- cal.par.real
		} else if (method == "jdrap") {
			cal.par.real <- apply(cal.par, 1, jdrap.inv, lambda)
			cal.par.real <- cal.par.real
		} else if (method == "bdoks") {
			cal.par.real <- apply(cal.par, 1, bdoks.inv, lambda)
			cal.par.real <- cal.par.real
		} else if (method == "yjohn") {
			cal.par.real <- apply(cal.par, 1, yjohn.inv, lambda)
			cal.par.real <- cal.par.real
		}
	}
	cal.par.res <- as.data.frame(cbind(cal.par.real, cal.par))
	names(cal.par.res) <- c("REAL.VALUE", "TRANSFORMED.VALUE")
	return(list(PARAMETERS=cal.par.res, LAMBDA=lambda, X.TRANS=x.trans, ADD.PAR=a, METHOD=method))
}
