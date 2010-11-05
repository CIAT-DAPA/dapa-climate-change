#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Assessing area with false predictions using buffering
	#1. Calculate the total area that is suitable and is outside the buffer
	#3. Calculate the total area that is suitable and is inside the buffer
	#2. Calculate the total area that is unsuitable and is inside the buffer
	#4. Calculate the total area that is unsuitable and is outside the buffer

require(rgdal)
require(raster)
areaFalse <- function(a, s, b) {
	ta <- sum(a[which(!is.na(a[]))])
	#Area suitable (!=0) AND outside (!=1) the buffer
	a.sui.out <- s
	a.sui.out[which(b[] == 1)] <- NA; a.sui.out[which(s[] == 0)] <- NA
	a.sui.out[which(!is.na(a.sui.out[]))] <- 1; a.sui.out <- a*a.sui.out
	aso <- sum(a.sui.out[which(!is.na(a.sui.out[]))]); faso <- aso / ta

	#Area suitable (!=0) AND inside (!=0) the buffer
	a.sui.in <- s
	a.sui.in[which(b[] == 0)] <- NA; a.sui.in[which(s[] == 0)] <- NA
	a.sui.in[which(!is.na(a.sui.in[]))] <- 1; a.sui.in <- a*a.sui.in
	asi <- sum(a.sui.in[which(!is.na(a.sui.in[]))]); fasi <- asi / ta

	#Area unsuitable (==0) AND inside (==1) the buffer
	a.nsu.in <- s
	a.nsu.in[which(b[] != 1)] <- NA; a.nsu.in[which(s[] > 0)] <- NA
	a.nsu.in[which(!is.na(a.nsu.in[]))] <- 1; a.nsu.in <- a*a.nsu.in
	aui <- sum(a.nsu.in[which(!is.na(a.nsu.in[]))]); faui <- aui / ta

	#Area unsuitable (==0) AND outside (!=1) the buffer
	a.nsu.out <- s
	a.nsu.out[which(b[] != 0)] <- NA; a.nsu.out[which(s[] > 0)] <- NA
	a.nsu.out[which(!is.na(a.nsu.out[]))] <- 1; a.nsu.out <- a*a.nsu.out
	auo <- sum(a.nsu.out[which(!is.na(a.nsu.out[]))]); fauo <- auo / ta

	#plot(stack(a.sui.out, a.sui.in, a.nsu.in, a.nsu.out))
	d <- data.frame(METRIC=c("SUI.OUT", "SUI.IN", "UNSUI.OUT", "UNSUI.IN"), AREA=c(aso, asi, auo, aui), FRACTION=c(faso, fasi, fauo, faui)) 
}
