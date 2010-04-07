# Author: Julian Ramirez, dawnpatrolmustaine@gmail.com
# Date :  December 2009
# Version 0.1
# Licence GPL v3

BioCalc <- function(folder, ext='', format='ascii') {
	
	flist <- writeFormats()[,1]
	
	if (format %in% flist) {
		ext <- ext
		fmt <- format
	} else {
		stop('Invalid grid type')
	}
	
	if (folder == '') {
		stop('Must specify a directory')
	} else if (!file.exists(folder)) {
		stop('Specified directory does not exist, try again with a valid directory')
	}
	
	TxList <- LoadMonthlyFiles(folder, ext=ext, varbl='tmax', format=fmt)
	TnList <- LoadMonthlyFiles(folder, ext=ext, varbl='tmin', format=fmt)
	TaList <- LoadMonthlyFiles(folder, ext=ext, varbl='tmean', format=fmt)
	PrList <- LoadMonthlyFiles(folder, ext=ext, varbl='prec', format=fmt)
	
	bio_1 <- p1Calc(TaList, paste(folder, "//", "bio_", 1, ext, sep=""), format=fmt)
	bio_2 <- p2Calc(TxList, TnList, paste(folder, "//", "bio_", 2, ext, sep=""), format=fmt)
	bio_4 <- p4Calc(TaList, paste(folder, "//", "bio_", 4, ext, sep=""), format=fmt)
	bio_5 <- p5Calc(TxList, paste(folder, "//", "bio_", 5, ext, sep=""), format=fmt)
	bio_6 <- p6Calc(TnList, paste(folder, "//", "bio_", 6, ext, sep=""), format=fmt)
	bio_7 <- p7Calc(bio_5, bio_6, paste(folder, "//", "bio_", 7, ext, sep=""), format=fmt)
	bio_3 <- p3Calc(bio_2, bio_7, paste(folder, "//", "bio_", 3, ext, sep=""), format=fmt)
	bio_8 <- p8Calc(PrList, TaList, paste(folder, "//", "bio_", 8, ext, sep=""), format=fmt)
	bio_9 <- p9Calc(PrList, TaList, paste(folder, "//", "bio_", 9, ext, sep=""), format=fmt)
	bio_10 <- p10Calc(TaList, paste(folder, "//", "bio_", 10, ext, sep=""), format=fmt)
	bio_11 <- p11Calc(TaList, paste(folder, "//", "bio_", 11, ext, sep=""), format=fmt)
	bio_12 <- p12Calc(PrList, paste(folder, "//", "bio_", 12, ext, sep=""), format=fmt)
	bio_13 <- p13Calc(PrList, paste(folder, "//", "bio_", 13, ext, sep=""), format=fmt)
	bio_14 <- p14Calc(PrList, paste(folder, "//", "bio_", 14, ext, sep=""), format=fmt)
	bio_15 <- p15Calc(PrList, paste(folder, "//", "bio_", 15, ext, sep=""), format=fmt)
	bio_16 <- p16Calc(PrList, paste(folder, "//", "bio_", 16, ext, sep=""), format=fmt)
	bio_17 <- p17Calc(PrList, paste(folder, "//", "bio_", 17, ext, sep=""), format=fmt)
	bio_18 <- p18Calc(PrList, TaList, paste(folder, "//", "bio_", 18, ext, sep=""), format=fmt)
	bio_19 <- p19Calc(PrList, TaList, paste(folder, "//", "bio_", 19, ext, sep=""), format=fmt)

	cat("", "\n", "Creating the raster stack biostack", "\n")
		
	pb <- pbCreate(19, type='text', style=3)
	for(bg in 1:19){
		if (bg == 1) {
			RasList <- get(paste("bio", bg, sep="_"))
		} else {
			RasList <- c(RasList, get(paste("bio", bg, sep="_")))
		}
		pbStep(pb, bg)
	}
	pbClose(pb)
	biostack <- stack(RasList)
	return(biostack)
	
}