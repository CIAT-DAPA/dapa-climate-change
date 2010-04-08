#This R script computes the EcoCrop suitability index based on a set of parameters

suitCalc <- function(TaList, TnList, TxList, PrList, Gmin=90,Gmax=90,Tkmp=0,Tmin=10,Topmin=16,Topmax=25,Tmax=35,Rmin=150,Ropmin=300,Ropmax=400,Rmax=600) {
	minAdapt <- 0
	maxAdapt <- 1
	
	#Calculating regression models between Rmin-Ropmin and Ropmax-Rmax
	
	Gmin <- 90
	Gmax <- 90
	Tkmp <- 0
	Tmin <- 10
	Topmin <- 16
	Topmax <- 25
	Tmax <- 35
	Rmin <- 150
	Ropmin <- 300
	Ropmax <- 400
	Rmax <- 600
	
	rainLeftReg <- lsfit(x=c(Rmin,Ropmin), y=c(0,1))
	rainLeftM <- rainLeftReg$coefficients[2]
	rainLeftB <- rainLeftReg$coefficients[1]
	
	rainRightReg <- lsfit(x=c(Ropmax,Rmax), y=c(1,0))
	rainRightM <- rainRightReg$coefficients[2]
	rainRightB <- rainRightReg$coefficients[1]
	
	Gavg <- mean(c(Gmin, Gmax))
	Tkill <- Tkmp + 40
	
	cat("Growing season is", Gavg, "\n")
	
	suitFun <- function(dataPixel) {
		
		PptDataPixel <- dataPixel[1:12]
		TavDataPixel <- dataPixel[13:24]
		TxDataPixel <- dataPixel[25:36}
		TnDataPixel <- dataPixel[37:48]
		
		tSuit <- rep(NA, 12)
		pSuit <- rep(NA, 12)
		cumPpt <- rep(NA, 12)
		
		for (i in 1:12) {
			start.month <- i
			end.month <- i + Gavg - 1
			
			#Temp. iteration
			
			if (TnDataPixel[i] < Tkill) {
				tSuit[i] <- 0
			} else if (TavDataPixel[i] < Tmin) {
				tSuit[i] <- 0
			} else if (TavDataPixel[i] < Topmin) {
				tSuit[i] <- 1 - ((Topmin - TavDataPixel[i]) * (1 / (Topmin - Tmin)))
			} else if (TavDataPixel[i] < Topmax) {
				tSuit[i] <- 1
			} else if (TavDataPixel[i] < Tmax) {
				tSuit[i] <- (Tmax - TavDataPixel[i]) * (1 / (Tmax - Topmax))
			} else {
				tSuit[i] <- 0
			}
			
			#Ppt growing season
			
			end.mth.p <- end.month
			if (end.mth.p > 12) {
				end.mth.p <- end.mth.p - 12
			}
			
			cumPpt[i] <- sum(PptDataPixel[start.month:end.mth.p])
			
			#Precipitation iteration
			
			if (cumPpt[i] < Rmin) {
				pSuit[i] <- 0
			} else if (cumPpt[i] < Ropmin) {
				pSuit[i] <- (rainLeftM) * cumPpt[i] + (rainLeftB)
			} else if (cumPpt[i] < Ropmax) {
				pSuit[i] <- 1
			} else if (cumPpt[i] < Rmax) {
				pSuit[i] <- (rainRightM) * cumPpt + (rainRightB)
			} else {
				pSuit[i] <- NA
			}
		}
		
		#Minimum cumulated temperature and rainfall suitability
		
		ecotf <- rep(NA, 12)
		ecopf <- rep(NA, 12)
		
		for (i in 1:12) {
			start.month <- i
			end.month <- i + Gavg - 1
			
			ecot <- rep(NA, 13)
			ecop <- rep(NA, 13)
			
			ecot[i] <- 1
			ecop[i] <- 0
			
			for (j in start.month:end.month) {
				r.end.mth <- j
				if (r.end.mth > 12) {r.end.mth <- r.end.mth - 12}
				r.nxt.mth <- r.end.mth + 1
				
				if (tSuit[r.end.mth] < ecot[r.end.mth]) {
					ecot[r.nxt.mth] <- tSuit[r.end.mth]
				} else {
					ecot[r.nxt.mth] <- ecot[r.end.mth]
				}
				
				if (pSuit[r.end.mth] > ecop[r.end.mth]) {
					ecop[r.nxt.mth] <- pSuit[r.end.mth]
				} else {
					ecop[r.nxt.mth] <- ecop[r.end.mth]
				}
			}
			
			ecot <- ecot[which(!is.na(ecot[]))]
			ecop <- ecop[which(!is.na(ecot[]))]
			
			ecotf[i] <- min(ecot)
			ecopf[i] <- max(ecop)
		}
		
		precFinSuit <- round(max(ecopf * 100))
		tempFinSuit <- round(max(ecotf * 100))
		finSuit <- round((max(ecopt) * max(ecotf)) * 100)
		
	}
	
	
}