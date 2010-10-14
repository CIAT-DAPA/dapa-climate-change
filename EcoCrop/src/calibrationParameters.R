#Julian Ramirez
#eejarv@leeds.ac.uk
#Oct 2010

#Description: Use the extracted and appended climate data to calculate parameters for each growing season. 4 Columns for each GS will be produced.

calibrationParameters <- function(dataset, gs=6, verbose=T) {
	psCol <- which(names(dataset) == "PREC1")
	peCol <- psCol+11
	tnsCol <- which(names(dataset) == "TMIN1")
	tneCol <- tnsCol + 11
	tsCol <- which(names(dataset) == "TMEAN1")
	teCol <- tsCol + 11
	txsCol <- which(names(dataset) == "TMAX1")
	txeCol <- txsCol + 11
	
	fdc <- which(names(dataset) == "TEST_TRAIN") #Final data column
	output <- dataset[,1:fdc]
	
	if (!is.numeric(gs) | gs <= 0 | gs >= 12) {
		stop("Growing season must be numeric, and between 1 and 11 months")
	}
	
	if (verbose) {
		cat("\n")
		cat("GS Length:", gs, "\n")
	}
	for (g in 1:12) {
		if (verbose) cat(".",g," ",sep="")
		start.month <- g
		end.month <- g + gs - 1
		
		if (end.month > 12) {
			end.month <- end.month - 12
			pp.ex.cols <- c((psCol):(psCol+end.month-1), (psCol+start.month-1):(psCol+11))
			tm.ex.cols <- c((tsCol):(tsCol+end.month-1), (tsCol+start.month-1):(tsCol+11))
			tn.ex.cols <- c((tnsCol):(tnsCol+end.month-1), (tnsCol+start.month-1):(tnsCol+11))
			tx.ex.cols <- c((txsCol):(txsCol+end.month-1), (txsCol+start.month-1):(txsCol+11))
		} else {
			pp.ex.cols <- ((psCol+start.month-1):(psCol+end.month-1))
			tm.ex.cols <- ((tsCol+start.month-1):(tsCol+end.month-1))
			tn.ex.cols <- ((tnsCol+start.month-1):(tnsCol+end.month-1))
			tx.ex.cols <- ((txsCol+start.month-1):(txsCol+end.month-1))
		}
		
		pp <- dataset[,pp.ex.cols]
		pp.res <- apply(pp, 1, pgsCalc)
		output$RES <- pp.res
		names(output)[length(names(output))] <- paste("GS",g,"_P",sep="")
		
		tm <- dataset[,tm.ex.cols]
		tm.res <- apply(tm, 1, tgsCalc)
		output$RES <- tm.res
		names(output)[length(names(output))] <- paste("GS",g,"_T",sep="")
		
		tn <- dataset[,tn.ex.cols]
		tn.res <- apply(tn, 1, tgsCalc)
		output$RES <- tn.res
		names(output)[length(names(output))] <- paste("GS",g,"_N",sep="")
		
		tx <- dataset[,tx.ex.cols]
		tx.res <- apply(tx, 1, tgsCalc)
		output$RES <- tx.res
		names(output)[length(names(output))] <- paste("GS",g,"_X",sep="")
	}
	if (verbose) cat("\n")
	return(output)
}

pgsCalc <- function(datapoint) {
	if (is.na(datapoint[1])) {
		res <- NA
	} else {
		res <- sum(datapoint)
	}
	return(res)
}

tgsCalc <- function(datapoint) {
	if (is.na(datapoint[1])) {
		res <- NA
	} else {
		res <- mean(datapoint)
	}
	return(res)
}

