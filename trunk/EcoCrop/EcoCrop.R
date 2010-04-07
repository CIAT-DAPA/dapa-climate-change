#This R script computes the EcoCrop suitability index based on a set of parameters

suitCalc <- function(Gmin=90,Gmax=90,Tkill=0,Tmin=10,Topmin=16,Topmax=25,Tmax=35,Rmin=150,Ropmin=300,Ropmax=400,Rmax=600) {
	minAdapt <- 0
	maxAdapt <- 1
	
	#Calculating regression models between Rmin-Ropmin and Ropmax-Rmax
	
	Gmin <- 90
	Gmax <- 90
	Tkill <- 0
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
	
	
}