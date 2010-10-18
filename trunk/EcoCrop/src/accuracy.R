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
	return(list(MXENT=maxe, SLOPE=slop))
}
rmsqe <- function(v) {
	v <- v*0.01
	tv <- rep(1, times=length(v))
	vx <- cbind(v, tv)
	y <- (v-tv)^2; y <- sqrt(sum(y) / length(v))
	return(y)
}
accMetrics <- function(rs, x) {
	v <- xyValues(rs, x)
	v <- v[which(!is.na(v))]
	or <- omissionRate(v)
	err <- rmsqe(v)
	mxe <- ent(v)
	return(list(OMISSION_RATE=or, RMSQE=err, MAX_ENT=mxe$MXENT, SLOPE=mxe$SLOPE))
}

ras <- raster("./data/runs/1-sorghum-tmin_tsuitability.asc")
td <- read.csv("./data/test.csv")
td <- cbind(td[,18], td[,17])
m <- accMetrics(ras, td)
