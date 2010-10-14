#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Description: Plot a histogram for a particular growing season, for each of the four variables, and an additional histogram

histPlot <- function(dataset, gs=1, plotdir="", nb=20) {
	if (gs < 1 | gs > 12 | !is.numeric(gs)) {
		stop("Invalid selected growing season, please check")
	}
	plotdata <- dataset[,grep(paste("GS", gs, "_", sep=""), names(dataset))]
	plotdata <- plotdata[which(!is.na(plotdata[,1])),]
	
	pc <- 1
	for (pv in c("prec","tmean","tmin","tmax")) {
		jpeg(paste(plotdir, "/GS", gs, "_", pv, ".jpg", sep=""), quality=100, height=600, width=600)
		hist(plotdata[,pc], breaks=nb, xlab=pv, main=pv)
		dev.off()
		pc <- pc+1
	}
	jpeg(paste(plotdir, "/GS", gs, "temp_.jpg", sep=""), quality=100, height=600, width=600)
	hist(c(plotdata[,2], plotdata[,3], plotdata[,4]), breaks=nb, xlab="temp (C)", main="temperatures")
	dev.off()
	
	return(plotdir)
}