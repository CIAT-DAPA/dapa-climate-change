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
			
			
		}
		
	}
	
	
}