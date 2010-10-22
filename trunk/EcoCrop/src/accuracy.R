#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Assessing the accuracy of EcoCrop calibration runs using
	#1. Test and train omission rates with S>0, for Psuit, Tsuit, Suit
	#2. RMSQE from 1 vs 0/1 (S>0)
	#3. Maximum entropy, entropy line slope

require(rgdal)
require(raster)

omissionRate <- function(v) {
	z <- v[which(v == 0)]
	nz <- v[which(v != 0)]
	rate <- length(z)/length(v)
	return(rate)
}
ent <- function(v) {
	th <- matrix(c(1:100))
	entCalc <- function(th, k) {
		p <- length(which(k < th)) / length(k)
		a <- length(which(k >= th)) / length(k)
		e <- -p*log(p,2)-a*log(a,2)
		return(e)
	}
	ev <- apply(th, 1, entCalc, v)
	maxe <- max(ev)
	slop <- (max(ev) - min(ev)) / 99
	return(list(MXENT=maxe, SLOPE=slop, MXE_CURVE=ev))
}
rmsqe <- function(v) {
	v <- v*0.01
	tv <- rep(1, times=length(v))
	y <- (v-tv)^2; y <- sqrt(sum(y) / length(v))
	return(y)
}
errDist <- function(v) {
	v <- v*0.01
	tv <- rep(0.5, times=length(v))
	y <- (v-tv)^2
	h <- hist(y^2, breaks=25, plot=F)
	errD <- (length(which(h$counts[2:(length(h$counts)-1)] != 0))) / (length(h$counts)-2)
	return(errD)
}
areaBeyond <- function(rs, v) {
	#Nothing for the moment
}

accMetrics <- function(rs, x) {
	v <- xyValues(rs, x)
	v <- v[which(!is.na(v))]
	or <- omissionRate(v)
	err <- rmsqe(v)
	ed <- errDist(v)
	mxe <- ent(v)
	return(list(METRICS=data.frame(SUIT=mean(v), SUITSD=sd(v), SUITX=max(v), SUITN=min(v), OMISSION_RATE=or, RMSQE=err, ERR_DIST=ed, MAX_ENT=mxe$MXENT, SLOPE=mxe$SLOPE), MXE_CURVE=mxe$MXE_CURVE))
}
