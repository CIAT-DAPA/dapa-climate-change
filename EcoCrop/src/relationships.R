#Julian Ramirez
#eejarv@leeds.ac.uk
#Dec 2010

#Script to compare the suitability rating with area harvested, yield and amount of production
#1. Fraction harvested vs. fraction suitable: 
	#for fraction harvested need to define factor conversion from actual units to km2
#2. Yield vs average, max, min, sum suitability

relationships <- function(infile, area.field, fac=1, yield.field, naValue=-9999, outfile="dull.png") {
	shpData <- read.csv(infile)
	aField <- grep(area.field, colnames(shpData))
	yField <- grep(yield.field, colnames(shpData))
	#data for area comparison
	aData <- cbind(as.numeric(shpData[,aField]), shpData$PS)
	colnames(aData) <- c("Harvested","Suitable")
	aData[which(aData[,1] == naValue),1] <- NA
	aData[,1] <- (aData[,1] * fac) / shpData$PA
	aData <- aData[which(!is.na(aData[,1])),]; aData <- aData[which(!is.na(aData[,2])),]
	#data for yield comparison
	yData <- cbind(as.numeric(shpData[,yField]), shpData$ME, shpData$MZ, shpData$MX, shpData$MN, shpData$SM)
	yData[which(yData[,1] == naValue),1] <- NA
	yData <- yData[which(!is.na(yData[,1])),]; yData <- yData[which(!is.na(yData[,2])),]
	yData <- as.data.frame(yData); colnames(yData) <- c(yield.field,"ME","MZ","MX","MN","SM")
	#now plot the data as xy
	png(outfile, width=2048, height=2048,pointsize=40)
	par(mfrow=c(3,2))
	plot(aData[,1],aData[,2],xlab="Fraction harvested", ylab="Potential fraction suitable", xlim=c(0,0.1),ylim=c(0,1),cex=1.5,pch=20)
	plot(yData[,1],yData$ME,xlab=yield.field,ylab="Average suitability",ylim=c(1,100),cex=1.5,pch=20)
	plot(yData[,1],yData$MZ,xlab=yield.field,ylab="Average suitability (suitable areas)",ylim=c(1,100),cex=1.5,pch=20)
	plot(yData[,1],yData$MX,xlab=yield.field,ylab="Maximum suitability",ylim=c(1,100),cex=1.5,pch=20)
	plot(yData[,1],yData$MN,xlab=yield.field,ylab="Minimum suitability",ylim=c(1,100),cex=1.5,pch=20)
	plot(yData[,1],yData$SM,xlab=yield.field,ylab="Total suitability",cex=1.5,pch=20)
	dev.off()
	return()
}

