#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Find fitting parameters using the mode of the data and 20%, 40% and 45% distanced classes from the mode-class
#Get also the average, or max, min, mode

getParameters <- function(x.real, nb=200, plotit=T, plotdir="./img", gs=1, varname="prec") {
	if (nb < 50) {warning("Imprecise results could come out when using less than 50 calibration classes")}
	
	x.real <- x.real[which(!is.na(x.real))]
	ht <- hist(x.real,breaks=nb, plot=F)
	ct <- cbind(ht$breaks[1:(length(ht$breaks)-1)], ht$breaks[2:length(ht$breaks)], ht$mids, ht$counts)
	ct <- as.data.frame(ct)
	ct$MODECLASS <- ct$V4 == max(ct$V4)
	ct$MEANCLASS <- ct$V1 < mean(x.real) & ct$V2 > mean(x.real)
	ct$MEDIANCLASS <- ct$V1 < median(x.real) & ct$V2 > median(x.real)

	mode.loc <- max(which(ct$MODECLASS == T))
	nc.before <- mode.loc - 1
	nc.after <- length(ht$breaks) - mode.loc
	kill.loc <- mode.loc - round(nc.before*0.95)
	min.loc <- mode.loc - round(nc.before*0.8)
	opmin.loc <- mode.loc - round(nc.before*0.4)
	opmax.loc <- mode.loc + round(nc.after*0.4)
	max.loc <- mode.loc + round(nc.after*0.8)
	
	if (plotit) {
		if (!file.exists(plotdir)) {stop("Check your plotting directory as it doesnt seem to exist")}
		jpeg(paste(plotdir, "/calibration-GS", gs, "-", varname, ".jpg", sep=""), quality=100, height=600, width=600)
		
		plot(density(x.real), main="Density plot and parameter location", xlab=varname)
		lines(c(mean(x.real),mean(x.real)), c(0,1), col="red", lty=1)
		text(mean(x.real), -0.0003, "Mean", cex=0.65)
		lines(c(median(x.real),median(x.real)), c(0,1), col="red", lty=3)
		text(median(x.real), -0.0003, "Med", cex=0.65)
		lines(c(ht$breaks[mode.loc],ht$breaks[mode.loc]),c(0,1),col="red", lty=2)
		text(ht$breaks[mode.loc], -0.0003, "Mod", cex=0.65)
		lines(c(ht$breaks[kill.loc],ht$breaks[kill.loc]),c(0,1),col="darkgreen", lty=1)
		text(ht$breaks[kill.loc], -0.0003, "Kill", cex=0.65)
		lines(c(ht$breaks[min.loc],ht$breaks[min.loc]),c(0,1),col="darkgreen", lty=2)
		text(ht$breaks[min.loc], -0.0003, "Min", cex=0.65)
		lines(c(ht$breaks[opmin.loc],ht$breaks[opmin.loc]),c(0,1),col="green", lty=2)
		text(ht$breaks[opmin.loc], -0.0003, "OpMin", cex=0.65)
		lines(c(ht$breaks[opmax.loc],ht$breaks[opmax.loc]),c(0,1),col="green", lty=2)
		text(ht$breaks[opmax.loc], -0.0003, "OpMax", cex=0.65)
		lines(c(ht$breaks[max.loc],ht$breaks[max.loc]),c(0,1),col="darkgreen", lty=2)
		text(ht$breaks[max.loc], -0.0003, "Max", cex=0.65)
		#legend("top",lty=c(1,3,2,2,2,1),cex=0.8,col=c("red", "red", "red", "green", "darkgreen", "darkgreen"), c("Mean", "Median", "Mode", "Optimum", "Absolute", "Killing"),bg="white")
		dev.off()
	}
	cal.par <- data.frame(GS=gs, VARIABLE=varname, KILL=ht$breaks[kill.loc], MIN=ht$breaks[min.loc], OPMIN=ht$breaks[opmin.loc], OPMAX=ht$breaks[opmax.loc], MAX=ht$breaks[max.loc])
	return(cal.par)
}
