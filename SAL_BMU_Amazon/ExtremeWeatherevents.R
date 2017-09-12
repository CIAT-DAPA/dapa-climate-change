


rm(list=ls())
library(Kendall)
source("~/PIK-Projects ----/KIBEX/R-Stuff/Fkt_Heatday.R")

## --- TMAX ----
## +++++++++++++
Dpath="~/Potsdam_LUP/Säkularstation/Potsdam_Tmax_daily/"
filelist <- list.files(Dpath) 

if (F) { ## ----------------------

pdm1 <-  read.csv(paste(Dpath,filelist[1],sep=""),sep=";",dec=",",skip=2)
time1 <- as.Date(pdm1[,1])-as.Date("1893-01-01") 
pdm2 <-  read.csv(paste(Dpath,filelist[2],sep=""),sep=";",dec=",",skip=2)
time2 <- as.Date(pdm2[,1])-as.Date("1893-01-01") 
pdm3 <-  read.csv(paste(Dpath,filelist[3],sep=""),sep=";",dec=",",skip=2)
time3 <- as.Date(pdm3[,1])-as.Date("1893-01-01") 
pdm4 <-  read.csv(paste(Dpath,filelist[4],sep=""),sep=";",dec=",",skip=2)
time4 <- as.Date(pdm4[,1])-as.Date("1893-01-01") 
pdm5 <-  read.csv(paste(Dpath,filelist[5],sep=""),sep=";",dec=",",skip=2)
time5 <- as.Date(pdm5[,1])-as.Date("1893-01-01") 
pdm6 <-  read.csv(paste(Dpath,filelist[6],sep=""),sep=";",dec=",",skip=2)
time6 <- as.Date(pdm6[,1])-as.Date("1893-01-01")

pdmall <- rbind(pdm1[,1:2],pdm2[,1:2],pdm3[,1:2],pdm4[,1:2],pdm5[,1:2],pdm6[,1:2])
timeall <- c(time1,time2,time3,time4,time5,time6)
timeall <- timeall + 1

pdmmax <- cbind(NA,NA,NA,pdmall[,2])
pdmmax[,3] <- as.numeric(substr(pdmall[,1],1,4))
pdmmax[,2] <- as.numeric(substr(pdmall[,1],6,7))
pdmmax[,1] <- as.numeric(substr(pdmall[,1],9,10))

#write(t(pdmmax),paste(Dpath,"pdm_tmax_total.csv",sep=""),ncol=4)

## --- TMEAN ----
## +++++++++++++
Dpath="~/Potsdam_LUP/Säkularstation/Potsdam_Tmean_daily/"
filelist <- list.files(Dpath) 

pdm1 <-  read.csv(paste(Dpath,filelist[1],sep=""),sep=";",dec=",",skip=2)
time1 <- as.Date(pdm1[,1])-as.Date("1893-01-01") 
pdm2 <-  read.csv(paste(Dpath,filelist[2],sep=""),sep=";",dec=",",skip=2)
time2 <- as.Date(pdm2[,1])-as.Date("1893-01-01") 
pdm3 <-  read.csv(paste(Dpath,filelist[3],sep=""),sep=";",dec=",",skip=2)
time3 <- as.Date(pdm3[,1])-as.Date("1893-01-01") 
pdm4 <-  read.csv(paste(Dpath,filelist[4],sep=""),sep=";",dec=",",skip=2)
time4 <- as.Date(pdm4[,1])-as.Date("1893-01-01") 
pdm5 <-  read.csv(paste(Dpath,filelist[5],sep=""),sep=";",dec=",",skip=2)
time5 <- as.Date(pdm5[,1])-as.Date("1893-01-01") 
pdm6 <-  read.csv(paste(Dpath,filelist[6],sep=""),sep=";",dec=",",skip=0)
time6 <- as.Date(pdm6[,1])-as.Date("1893-01-01")

colnames(pdm6[,1:2]) <- colnames(pdm5[,1:2])
pdmall <- rbind(pdm1[,1:2],pdm2[,1:2],pdm3[,1:2],pdm4[,1:2],pdm5[,1:2],pdm6[,1:2])
timeall <- c(time1,time2,time3,time4,time5,time6)
timeall <- timeall + 1

pdmmean <- cbind(NA,NA,NA,pdmall[,2])
pdmmean[,3] <- as.numeric(substr(pdmall[,1],1,4))
pdmmean[,2] <- as.numeric(substr(pdmall[,1],6,7))
pdmmean[,1] <- as.numeric(substr(pdmall[,1],9,10))

## --- TMIN ----
## +++++++++++++
pdm1 <- 0;pdm2 <- 0;pdm3 <- 0;pdm4 <- 0;pdm5 <- 0;pdm6 <- 0;
Dpath="~/Potsdam_LUP/Säkularstation/Potsdam_Tmin_daily/"
filelist <- list.files(Dpath) 

pdm1 <-  read.csv(paste(Dpath,filelist[1],sep=""),sep=";",dec=",",skip=2)
time1 <- as.Date(pdm1[,1])-as.Date("1893-01-01") 
pdm2 <-  read.csv(paste(Dpath,filelist[2],sep=""),sep=";",dec=",",skip=2)
time2 <- as.Date(pdm2[,1])-as.Date("1893-01-01") 
pdm3 <-  read.csv(paste(Dpath,filelist[3],sep=""),sep=";",dec=",",skip=2)
time3 <- as.Date(pdm3[,1])-as.Date("1893-01-01") 
pdm4 <-  read.csv(paste(Dpath,filelist[4],sep=""),sep=";",dec=",",skip=2)
time4 <- as.Date(pdm4[,1])-as.Date("1893-01-01") 
pdm5 <-  read.csv(paste(Dpath,filelist[5],sep=""),sep=";",dec=",",skip=2)
time5 <- as.Date(pdm5[,1])-as.Date("1893-01-01") 

#pdmalltag <- c(as.numeric(substr(pdm1[,1],9,10)),as.numeric(substr(pdm2[,1],9,10)),as.numeric(substr(pdm3[,1],9,10)),as.numeric(substr(pdm4[,1],9,10)),
#			as.numeric(substr(pdm5[,1],9,10)))
#pdmallmon <- c(as.numeric(substr(pdm1[,1],6,7)),as.numeric(substr(pdm2[,1],6,7)),as.numeric(substr(pdm3[,1],6,7)),as.numeric(substr(pdm4[,1],6,7)),
#			as.numeric(substr(pdm5[,1],6,7)))
#pdmallyea <- c(as.numeric(substr(pdm1[,1],1,4)),as.numeric(substr(pdm2[,1],1,4)),as.numeric(substr(pdm3[,1],1,4)),as.numeric(substr(pdm4[,1],1,4)),
#			as.numeric(substr(pdm5[,1],1,4)))

pdm6 <-  read.csv(paste(Dpath,filelist[6],sep=""),sep=";",dec=",",skip=2)
time6 <- as.Date(pdm6[,1])-as.Date("1893-01-01")

colnames(pdm6[,1:2]) <- colnames(pdm5[,1:2])
pdmall <- rbind(pdm1[,1:2],pdm2[,1:2],pdm3[,1:2],pdm4[,1:2],pdm5[,1:2],pdm6[,1:2])
timeall <- c(time1,time2,time3,time4,time5,time6)
timeall <- timeall + 1

pdmmin <- cbind(NA,NA,NA,pdmall[,2])
pdmmin[,3] <- as.numeric(substr(pdmall[,1],1,4))
pdmmin[,2] <- as.numeric(substr(pdmall[,1],6,7))
pdmmin[,1] <- as.numeric(substr(pdmall[,1],9,10))

#pdmalltag <- c(pdmalltag,as.numeric(substr(pdm6[,1],1,2)))
#pdmallmon <- c(pdmallmon,as.numeric(substr(pdm6[,1],4,5)))
#pdmallyea <- c(pdmallyea,as.numeric(substr(pdm6[,1],7,10)))
#pdmalldat <- c(pdm1[,2],pdm2[,2],pdm3[,2],pdm4[,2],pdm5[,2],pdm6[,2])
#timeall <- c(time1,time2,time3,time4,time5,time6)
#timeall <- timeall + 1
#pdmmin <- cbind(pdmalltag,pdmallmon,pdmallyea,pdmalldat)

#write(t(pdmmin),paste(Dpath,"pdm_tmin_total.csv",sep=""),ncol=4)

## --- PREC ----
## +++++++++++++
Dpath="~/Potsdam_LUP/Säkularstation/Potsdam_Prec_daily/"
filelist <- list.files(Dpath) 

pdm1 <-  read.csv(paste(Dpath,filelist[1],sep=""),sep=";",dec=",",skip=2)
time1 <- as.Date(pdm1[,1])-as.Date("1893-01-01") 
pdm2 <-  read.csv(paste(Dpath,filelist[2],sep=""),sep=";",dec=",",skip=2)
time2 <- as.Date(pdm2[,1])-as.Date("1893-01-01") 
pdm3 <-  read.csv(paste(Dpath,filelist[3],sep=""),sep=";",dec=",",skip=2)
time3 <- as.Date(pdm3[,1])-as.Date("1893-01-01") 
pdm4 <-  read.csv(paste(Dpath,filelist[4],sep=""),sep=";",dec=",",skip=2)
time4 <- as.Date(pdm4[,1])-as.Date("1893-01-01") 
pdm5 <-  read.csv(paste(Dpath,filelist[5],sep=""),sep=";",dec=",",skip=2)
time5 <- as.Date(pdm5[,1])-as.Date("1893-01-01") 
pdm6 <-  read.csv(paste(Dpath,filelist[6],sep=""),sep=";",dec=",",skip=2)
time6 <- as.Date(pdm6[,1])-as.Date("1893-01-01")

pdmall <- rbind(pdm1[,1:2],pdm2[,1:2],pdm3[,1:2],pdm4[,1:2],pdm5[,1:2],pdm6[,1:2])
timeall <- c(time1,time2,time3,time4,time5,time6)
timeall <- timeall + 1

pdmprec <- cbind(NA,NA,NA,pdmall[,2])
pdmprec[,3] <- as.numeric(substr(pdmall[,1],1,4))
pdmprec[,2] <- as.numeric(substr(pdmall[,1],6,7))
pdmprec[,1] <- as.numeric(substr(pdmall[,1],9,10))

#write(t(pdmprec),paste(Dpath,"pdm_prec_total.csv",sep=""),ncol=4)

## --- WIND ----
## +++++++++++++
Dpath="~/PIK-Projects ----/Potsdam_LUP/Säkularstation/Potsdam_WindMean_daily/"
filelist <- list.files(Dpath) 

pdm1 <-  read.csv(paste(Dpath,filelist[1],sep=""),sep=";",dec=",",skip=2)
time1 <- as.Date(pdm1[,1])-as.Date("1942-01-01") 
pdm2 <-  read.csv(paste(Dpath,filelist[2],sep=""),sep=";",dec=",",skip=2)
time2 <- as.Date(pdm2[,1])-as.Date("1942-01-01")  
pdm3 <-  read.csv(paste(Dpath,filelist[3],sep=""),sep=";",dec=",",skip=2)
time3 <- as.Date(pdm3[,1])-as.Date("1942-01-01")  
pdm4 <-  read.csv(paste(Dpath,filelist[4],sep=""),sep=";",dec=",",skip=2)
time4 <- as.Date(pdm4[,1])-as.Date("1942-01-01")  

pdmall <- rbind(pdm1[,1:2],pdm2[,1:2],pdm3[,1:2],pdm4[,1:2])
timeall <- c(time1,time2,time3,time4)
timeall <- timeall + 1

pdmwind <- cbind(NA,NA,NA,pdmall[,2])
pdmwind[,3] <- as.numeric(substr(pdmall[,1],1,4))
pdmwind[,2] <- as.numeric(substr(pdmall[,1],6,7))
pdmwind[,1] <- as.numeric(substr(pdmall[,1],9,10))
#write(t(pdmwind),paste(Dpath,"pdm_wind_total.csv",sep=""),ncol=4)

## --- WIND ----
## +++++++++++++
Dpath="~/Potsdam_LUP/Säkularstation/Potsdam_WindMax_daily/"
filelist <- list.files(Dpath) 

pdm1 <-  read.csv(paste(Dpath,filelist[1],sep=""),sep=";",dec=",",skip=2)
time1 <- as.Date(pdm1[,1])-as.Date("1967-01-01") 
pdm2 <-  read.csv(paste(Dpath,filelist[2],sep=""),sep=";",dec=",",skip=2)
time2 <- as.Date(pdm2[,1])-as.Date("1967-01-01")  

pdmall <- rbind(pdm1[,1:2],pdm2[,1:2])
timeall <- c(time1,time2)
timeall <- timeall + 1

pdmwindX <- cbind(NA,NA,NA,pdmall[,2])
pdmwindX[,3] <- as.numeric(substr(pdmall[,1],1,4))
pdmwindX[,2] <- as.numeric(substr(pdmall[,1],6,7))
pdmwindX[,1] <- as.numeric(substr(pdmall[,1],9,10))

#write(t(pdmwindX),paste(Dpath,"pdm_windX_total.csv",sep=""),ncol=4)

## --- SNOW ----
## +++++++++++++
Dpath="~/PIK-Projects ----/Potsdam_LUP/Säkularstation/Potsdam_newSnow_daily/"
filelist <- list.files(Dpath) 

pdm1 <-  read.csv(paste(Dpath,filelist[1],sep=""),sep=";",dec=",",skip=2)
time1 <- as.Date(pdm1[,1])-as.Date("1951-01-02") 
pdm2 <-  read.csv(paste(Dpath,filelist[2],sep=""),sep=";",dec=",",skip=2)
time2 <- as.Date(pdm2[,1])-as.Date("1951-01-02")  
pdm3 <-  read.csv(paste(Dpath,filelist[3],sep=""),sep=";",dec=",",skip=2)
time3 <- as.Date(pdm3[,1])-as.Date("1951-01-02")

pdmall <- rbind(pdm1[,1:2],pdm2[,1:2],pdm3[,1:2])
timeall <- c(time1,time2,time3)
timeall <- timeall + 1

pdmsnow <- cbind(NA,NA,NA,pdmall[,2])
pdmsnow[,3] <- as.numeric(substr(pdmall[,1],1,4))
pdmsnow[,2] <- as.numeric(substr(pdmall[,1],6,7))
pdmsnow[,1] <- as.numeric(substr(pdmall[,1],9,10))
#write(t(pdmsnow),paste(Dpath,"pdm_snow_total.csv",sep=""),ncol=4)

}  ## ------------------------------

######################################################################################
######################################################################################

Dpath <- "~/PIK-Projects ----/Potsdam_LUP/Säkularstation/"
library(Kendall)
pdmmax <- read.csv(paste(Dpath,"pdm_tmax_total.csv",sep=""),sep="")
pdmmin <- read.csv(paste(Dpath,"pdm_tmin_total.csv",sep=""),sep="")
pdmmean <- read.csv(paste(Dpath,"pdm_tmean_total.csv",sep=""),sep="")
pdmprec <- read.csv(paste(Dpath,"pdm_prec_total.csv",sep=""),sep="")
pdmwindX <- read.csv(paste(Dpath,"pdm_windX_total.csv",sep=""),sep="")
pdmwind <- read.csv(paste(Dpath,"pdm_wind_total.csv",sep=""),sep="")
pdmsnow <- read.csv(paste(Dpath,"pdm_snow_total.csv",sep=""),sep="")

pdmmean5113 <- pdmmean[pdmmean[,3]>=1951&pdmmean[,3]<=2013,]
pdmmax5113 <- pdmmax[pdmmax[,3]>=1951&pdmmax[,3]<=2013,]
pdmmin5113 <- pdmmin[pdmmin[,3]>=1951&pdmmin[,3]<=2013,]
pdmprec5113 <- pdmprec[pdmprec[,3]>=1951&pdmprec[,3]<=2013,]		##&pdmprec[,2]>=4&pdmprec[,2]<=9,]
pdmwind5113 <- pdmwind[pdmwind[,3]>=1991&pdmwind[,3]<=2013,]
#pdmwndX6713 <- pdmwindX[pdmwindX[,3]>=1971&pdmwindX[,3]<=2013,]
pdmsnow5113 <- pdmsnow[pdmsnow[,3]>=1951&pdmsnow[,3]<=2013,]

source("~/PIK-Projects ----/KIBEX/R-Stuff/Fkt_Heatday.R")

## DrySpell ~~~~~~~~~~~
DS <- function(ds_tmit,ds_prec,para) {
	# ds_tmit <- pdmmax5113 ; ds_prec <- pdmprec5113 ; para <- c(20,25.6,0.9)
	ds_value <- cbind(ds_tmit[,1:3],NA,NA)
	dat_roh <- cbind(ds_tmit[,4],ds_prec[,4]) 

	for (day in 1:((nrow(ds_prec)-para[1]+1))) {
	help.tmit <- mean(dat_roh[(day:(day+para[1]-1)),1]) 	## gemittelte TagesmittelTemp. in para[1]-Tagen darf para[2] nicht überschreiten
	help.prec <- mean(dat_roh[(day:(day+para[1]-1)),2])	## gemittelte Tagesniederschläge dürfen para[3] nicht überschreiten
	if (help.tmit>=para[2]&help.prec<=para[3]) ds_value[(day:(day+para[1]-1)),4] <- 1  }
	ds_value[is.na(ds_value[,4]),4] <- 0
	dl.l <- 0 ; tag  <- 1
	while (tag <= nrow(ds_prec)) {
	if (ds_value[tag,4]!=0) {dl.l <- dl.l + 1 ; ds_value[((tag-dl.l+1):tag),5] <- dl.l} 
	else { dl.l <- 0 }
	tag <- tag + 1
	}
	ds_value[is.na(ds_value[,5]),5] <- 0
	return(ds_value)
}
CDD <- function(cdd_dat,thres) {
	## cdd_dat <- pdmprec5113 ; thres <- 1
	cdd_info <- matrix(NA,dist(range(cdd_dat[,3]))+1,6)
	for (jahr in range(cdd_dat[,3])[1]:range(cdd_dat[,3])[2]){
		cdd_info[jahr-range(cdd_dat[,3])[1]+1,1] <- jahr
		count_y <- 1							## counter for days in year ii
		count_d <- 0							## counter for drought
		count_max <- 0 ; start_max <- 0 ; end_max <- 0	 	## best value arrays
		first <- which(cdd_dat[,3]==jahr)[1]
		while (cdd_dat[first+count_y-1,3]==jahr & first+count_y <= nrow(cdd_dat) ) {
			if (cdd_dat[first+count_y-1,4]<thres)  {if (count_d==0) {start <- cdd_dat[first+count_y-1,1:2]} ; count_d <- count_d + 1}
			if (cdd_dat[first+count_y-1,4]>=thres) {end <- cdd_dat[first+count_y-1,1:2] 
				if (count_max < count_d) {count_max <- count_d ; start_max <- start ; end_max <- end}
				count_d <- 0
			}
			count_y <- count_y + 1
		}
		cdd_info[jahr-range(cdd_dat[,3])[1]+1,2] <- count_max			## längste Phase ohne P (P<1mm)
		cdd_info[jahr-range(cdd_dat[,3])[1]+1,c(3:4)] <- start_max		## Beginn dieser Trockenphase
		cdd_info[jahr-range(cdd_dat[,3])[1]+1,c(5:6)] <- end_max			## Erster Tag mit P nach Trockenphase
	}
	return(cdd_info)
}
runningmean <- function(vector,lgth,mod) {		#### function to calculate running mean	## vector <- pdm_hrly[,5] ; lgth <- 10
	help <- array(NA,length(vector))
	for.beg <- floor(lgth/2)
	for.end <- nrow(help)-for.beg
	if (mod==1) for (step in (for.beg+1):for.end) { help[step] <-  sum( vector[(step-for.beg):(step+for.beg)],na.rm=T ) }
	if (mod==2) for (step in (for.beg+1):for.end) { help[step] <- mean( vector[(step-for.beg):(step+for.beg)],na.rm=T ) }
	return(help)
}
monthlymean <- function(vector,mode,thres) {		## inclusive info of time	vector <- pdmprec5113
	helpmm <- matrix(NA,1,dist(range(vector[,3]))*12)
	for (year in range(vector[,3])[1]:range(vector[,3])[2]) {
		for (mt in 1:12) {
			if (mode==1) helpmm[(year-range(vector[,3])[1])*12+mt] <- mean(vector[vector[,3]==year&vector[,2]==mt,4])
			if (mode==2) helpmm[(year-range(vector[,3])[1])*12+mt] <- sum(vector[vector[,3]==year&vector[,2]==mt,4])
			if (mode==3) helpmm[(year-range(vector[,3])[1])*12+mt] <- max(vector[vector[,3]==year&vector[,2]==mt,4],na.rm=T)
			if (mode==4) helpmm[(year-range(vector[,3])[1])*12+mt] <- length(which(vector[vector[,3]==year&vector[,2]==mt,4]>=thres))
 	} 	}
	return(helpmm)
}
yearlymean <- function(vector,mode,thres) {		## inclusive info of time
	helpym <- matrix(NA,1,dist(range(vector[,3])))
	for (year in range(vector[,3])[1]:range(vector[,3])[2]) {
		if (mode==1) helpym[(year-range(vector[,3])[1]+1)] <- mean(vector[vector[,3]==year,4])
		if (mode==2) helpym[(year-range(vector[,3])[1]+1)] <- sum(vector[vector[,3]==year,4])
		if (mode==3) helpym[(year-range(vector[,3])[1]+1)] <- max(vector[vector[,3]==year,4],na.rm=T)
		if (mode==4) helpym[(year-range(vector[,3])[1]+1)] <- length(which(vector[vector[,3]==year,4]>=thres))
		if (mode==5) helpym[(year-range(vector[,3])[1]+1)] <- length(which(vector[vector[,3]==year,4]<thres))
 	} 	
	return(helpym)
}
 seasonalmean <- function(vector,mode,thres) {			## inclusive info of time		## vector <- pr.arr[,c(1:3,3+ms)]
	helpym <- matrix(NA,4,dist(range(vector[,3])))		## winter wird für Jahr X mit Dezember des letzten Jahres (x-1) berechnet
	if (mode==1) seasfunc <- function(vect) {mean(vect)}
	if (mode==2) seasfunc <- function(vect) {sum(vect)}
	if (mode==3) seasfunc <- function(vect) {length(which(vect>thres))}
	for (year in (range(vector[,3])[1]+1):range(vector[,3])[2]) {
		helpym[1,(year-range(vector[,3])[1])] <- seasfunc(vector[vector[,3]==year&vector[,2]>=3&vector[,2]<=5,4])	## MAM
		helpym[2,(year-range(vector[,3])[1])] <- seasfunc(vector[vector[,3]==year&vector[,2]>=6&vector[,2]<=8,4])	## JJA
		helpym[3,(year-range(vector[,3])[1])] <- seasfunc(vector[vector[,3]==year&vector[,2]>=9&vector[,2]<=11,4])	## SON
		helpym[4,(year-range(vector[,3])[1])] <- seasfunc(vector[which((vector[,3]==year&(vector[,2]<=2)|vector[,3]==(year-1)&vector[,2]==12)),4])	## DJF
 	} 	
	return(helpym)
 }
level <- function(mkt) {
	lev <- c(0,80,90,95,99,99.9,100)
	ll <- which(lev>(mkt))[1]
	return(lev[ll-1])
}
plotgraf <- function(win,datvector,ylabnam,mainname,axis1list,axis1name,axis2list,dimname,plotcol,trend,mittel,thres) {	## ab jetzt datvector mit zeitspalte 
	##x11(win[1],win[2]);par(mfrow=c(1,1),mar=c(2.8,2.9,1.7,0.5),mgp=c(1.6,0.6,0)) ; fac <- 1.4
	## win <- c(8,3.5); datvector <- yprec; axis1name <- seq(1951,2013,3); axis2list <- c(380,670,50); dimname <- " mm"; plotcol <- "blue"; trend=T; mittel=T; thres <-1000
	## ylabnam <- " Niederschlag (mm)"; axis1list <- c(1,nrow(yprec),3); mainname <- "Jährliche Niederschlagsmenge"
	par(mar=c(2.8,2.9,1.7,0.5),mgp=c(1.6,0.6,0)) ; fac <- 1.4
	plot(1,1,col="white",ylim=c(axis2list[1],fac*axis2list[2]),xlim=c(1,nrow(datvector)),axes=F,xlab="",ylab="")
	points(datvector[,2],type="l",col=plotcol,lwd=0.7,ylab="",ylim=c(axis2list[1],fac*axis2list[2]),cex.main=1.4,xlab="")
	if (trend) {mkt  <- as.numeric((1 - MannKendall(datvector[,2])$sl)*100)
 			helpreg <- lm(datvector[,2]~c(1:nrow(datvector))) ; 	anst <- round(helpreg$coeff[2],2) 
			abline(helpreg$coeff,col=grey(0.5),lwd=2,lty=2)
			pval <- (1-summary(helpreg)$coeff[2,4])*100	}
	abline(h=thres,col="gray",lty=2)
	axis(1,at=seq(axis1list[1],axis1list[2],axis1list[3]),label=axis1name,cex.axis=1.1)
	axis(2,at=seq(axis2list[1],fac*axis2list[2],axis2list[3]),cex.axis=1.1)
	mtext(side=1,line=1.7,text="Jahr",cex=1.1)
	mtext(side=2,line=1.8,text=ylabnam,cex=1.1)
	mtext(side=3,line=0.2,text=mainname,cex=1.2) ; box()
	if (trend) legend("topleft",legend=c(paste("MK Signifikanz: >",level(mkt),"% ",sep=""),paste("lin.Regr.  m: ",anst,dimname,sep="") ),text.col=c("black"),cex=1.1,x.intersp=-0.5,bg="grey")
	#if (mittel) {legend("topright",legend=c(paste("Mittel 1994-2013: ",round(mean(datvector[which(axis1name>=1994&axis1name<=2013)],na.rm=T),1),dimname,sep="")),cex=1.1,bg="grey",x.intersp=-0.5)}
	if (mittel) {legend("topright",legend=c(paste("Mittel 1971-2000: ",round(mean(datvector[which(datvector[,1]>=1971&datvector[,1]<=2000),2],na.rm=T),1),dimname,sep="")),cex=1.1,bg="grey",x.intersp=-0.5)}
}

################################################################################################
################################################################################################

## DrySpells ++++++++++++++++++++++++++++++++++
drysp <- DS(pdmmax5113,pdmprec5113,c(20,25.6,0.9))
drysp.ym <- yearlymean(drysp[,1:4],mode=2)
x11(7,3.5);par(mfrow=c(1,1))
plotgraf(c(14,4.2),cbind(c(1951:2013),drysp.ym)," #-Tage","Anzahl der jährlichen DrySpell-Tage (20,25.6,0.9)",c(1,length(drysp.ym),3),seq(1951,2013,3),c(0,70,5)," Tage","red",T,TRUE,45)

	text(34,80,"Jun-Sep 1983",col="red") ; text(46,75,"Jun-Aug 1995",col="red")
c(1951:2013)[which(drysp.ym>=45)]
drysp[drysp[,3]==2003,]

## DryDays ++++++++++++++++++++++++++++++++++++
dryday5113 <- pdmprec5113 
dryday5113[pdmprec5113[,4]<1,4] <- 1
dryday5113[pdmprec5113[,4]>=1,4] <- 0
plotgraf(c(14,4.2),monthlymean(dryday5113,2)->dat," # Tage","Monatliche Anzahl der trockenen Tage (+ CDD max)",c(1,length(dat),3*12),seq(1951,2013,3),c(0,40,5)," Tage","blue",F,TRUE,35)
cddinfo <- CDD(pdmprec5113,1)
	#points(seq(6.5,length(dat),12),cddinfo[,2],type="p",col="black",pch=14)
	#text(9*12,42,"17.08.-24.09.59",col="red") ; text(21*12,43,"15.07.-23.08.71",col="red") ; text(56*12,38,"24.03.-07.05.07",col="red") ; text(62*12,43,"19.10.-27.11.11",col="red")
plotgraf(c(9,3.5),cddinfo[,2]," # Tage","Längste Trockenphase",c(1,nrow(cddinfo),3),seq(1951,2013,3),c(0,40,5)," Tage","red",T,T,1000)

## WetSpells ++++++++++++++++++++++++++++++++++
prec5d <- runningmean(pdmprec5113[,4],5,1)
plotgraf(c(14,4.2),prec5d,"Niederschlag (mm)","Niederschlag innerhalb von 5 Tagen",c(1,length(prec5d),3*366),seq(1951,2013,3),c(0,100,20)," mm","blue",FALSE,FALSE,100)
	text(1330,120,"22.08.1954",col="red") ; text(10082,125,"08.08.1978",col="red") ; text(18853,115,"12.08.2002",col="red") 
pdmprec5113[which(prec5d>=100),]
cbind(prec5d[which(prec5d>=100)]

## Stürme ++++++++++++++++++++++++++++++++++
#wind <- monthlymean(pdmwind5113,4,8) ; wind <- monthlymean(pdmwind5113,3,8)		
#wind <- monthlymean(pdmwndX6713,4,15) 
wind <- yearlymean(pdmwindX,3,15)	## Monatshöchstwert der stärksten Tagesboe
	##plotgraf(c(14,4.2),cbind(c(1967:2013),wind)," Windstärke (m/s)","Monatsmaximum des Tageshöchstwertes",c(6,length(wind),2*12),seq(1991,2013,2),c(10,30,4)," Tage","black",T,T,32)
plotgraf(c(14,4.2),cbind(c(1967:2013),wind)," Windstärke (m/s)","Tage über dem 98.Perzentile",c(1,length(wind),2),seq(1967,2013,2),c(10,30,4)," Tage","black",T,T,32)
wind.dat <- cbind(rep(1:12,23),rep(1991:2013,each=12),wind)
wind.dat[wind.dat[,3]>=35,]

## Stürme ++++++++++++++++++++++++++++++++++
wind <- yearlymean(pdmwindX,4,23.9)	
plotgraf(c(8,3.5),cbind(c(1967:2013),wind)," # Tage","Tage über 98.Perzentile der Tageswindspitze in Potsdam",c(1,length(wind),2),seq(1967,2013,2),c(0,25,2)," Tage","black",T,T,1000)
##wind.dat <- cbind(rep(1:12,23),rep(1991:2013,each=12),wind)

## Heat Waves ++++++++++++++++++++++++++++++
heatd <- cbind(rep(1951:2013,each=1),NA)
for (year in 1951:2013) {
	heatd[year-1950,2] <- heatday(pdmmax5113[pdmmax5113[,3]==year,4],1,30) }
plotgraf(c(9,3.5),heatd[,2]," # Tage","Jährliche Anzahl Hitzewellentage",c(1,nrow(heatd),3),seq(1951,2013,3),c(0,14,4)," Tage","red",T,T,1000)

## Tropische Nacht ++++++++++++++++++++++++++++++
trop <- cbind(rep(1951:2013,each=1),NA)
for (year in 1951:2013) {
	trop[year-1950,2] <- length(which(pdmmin5113[pdmmin5113[,3]==year,4]>=20)) }
x11(7,3.5);par(mfrow=c(1,1))
plotgraf(c(8,3.5),trop[,c(1,2)]," # Tage","Jährliche Anzahl tropischer Nächte",c(1,nrow(trop),3),seq(1951,2013,3),c(0,4,1)," Tage","red",T,T,1000)
## -->-->  ( 2*sd(trop[,2]) )/mean(trop[,2]) * 100

## Heat Waves ++++++++++++++++++++++++++++++
hotd <- cbind(rep(1951:2013,each=1),NA)
for (year in 1951:2013) {
	hotd[year-1950,2] <- length(which(pdmmax5113[pdmmax5113[,3]==year,4]>=30)) }
x11(7,3.5);par(mfrow=c(1,1))
plotgraf(c(8,3.5),hotd[,c(1,2)]," # Tage","Jährliche Anzahl heißer Tage",c(1,nrow(hotd),3),seq(1951,2013,3),c(0,21,3)," Tage","red",T,T,1000)
## -->-->  mean(hotd[,2]) ; ( 2*sd(hotd[,2]) )/mean(hotd[,2]) * 100
#hotd[hotd[,2]>=20,]

## Cold Waves (Tmean) ++++++++++++++++++++++++++++++
coldw <- cbind(rep(1951:2013,each=1),NA)
for (year in 1951:2013) {
	coldw[year-1950,2] <- length(which(pdmmean5113[pdmmean5113[,3]==year,4]<=0)) }
plotgraf(c(14,4.2),coldw[,2]," # Tage","Jährliche Anzahl der Tage mit Mitteltemp. unter 0°C",c(1,nrow(coldw),3),seq(1951,2013,3),c(10,70,5)," Tage","blue",FALSE,F,70)
coldw[coldw[,2]>=70,]

## Cold Waves (Tmin) ++++++++++++++++++++++++++++++
coldw <- cbind(rep(1951:2013,each=1),NA)
for (year in 1951:2013) {
	coldw[year-1950,2] <- length(which(pdmmin5113[pdmmin5113[,3]==year,4]<=0)) }
plotgraf(c(8,3.5),coldw[,2]," # Tage","Jährliche Anzahl der Tage mit Tiefsttemp. unter 0°C",c(1,nrow(coldw),3),seq(1951,2013,3),c(40,110,5)," Tage","blue",T,T,1000)

## Yearly Precipitation ++++++++++++++++++++++++++++++
yprec <- cbind(rep(1951:2013,each=1),NA)
yprec[,2] <- yearlymean(pdmprec5113,2,1000) 
x11(7,3.5);par(mfrow=c(1,1))
plotgraf(c(8,3.5),yprec[,c(1,2)]," Niederschlag (mm)","Jährliche Niederschlagsmenge",c(1,nrow(yprec),3),seq(1951,2013,3),c(380,670,50)," mm","blue",T,T,1000)

## Yearly Precipitation bei Temp. unter 1°C ++++++++++++++++++++++++++++++
yprec <- cbind(rep(1951:2013,each=1),NA)
yprec[,2] <- yearlymean(pdmprec5113[which(pdmmin5113[,4]<1),],2,1000) 
x11(7,3.5);par(mfrow=c(1,1))
plotgraf(c(8,3.5),yprec[,c(1,2)]," Niederschlag (mm)","Jährliche Niederschlagsmenge bei <1°C (Tmin)",c(1,nrow(yprec),3),seq(1951,2013,3),c(50,250,50)," mm","blue",T,T,1000)

## Number of days with prec über 5mm bei unter 1°C ++++++++++++++++++++++++++++++
yprec <- cbind(rep(1951:2013,each=1),NA)
yprec[,2] <- yearlymean(pdmprec5113[which(pdmmin5113[,4]<1),],4,5) 
x11(7,3.5);par(mfrow=c(1,1))
plotgraf(c(8,3.5),yprec[,c(1,2)]," # Tage","Anzahl Tage mit Prec>=5mm bei <1°C (Tmin)",c(1,nrow(yprec),3),seq(1951,2013,3),c(0,20,2)," Tage","blue",T,T,1000)
 


## Höchste Niederschlagsmenge in 5 Tagen ++++++++++++++++++++++++++++++
r5d <- function(ds_prec,k) {		## ds_prec <- pdmprec5113 ; k <- 5
	runmeank <- array(NA,length(unique(ds_prec[,3]))) 	; cnt <- 0
	for (year in range(ds_prec[,3])[1]:range(ds_prec[,3])[2]) {
		help <- ds_prec[ds_prec[,3]==year,4] ; cnt <- cnt + 1
		runhelp <- array(NA,length(help))
            for (rmk in 1:(length(help)-k+1)) {
                 runhelp[rmk] <- sum(help[rmk:(rmk+k-1)]) }
	 runmeank[cnt]  <- max(runhelp,na.rm=T) }
      return(runmeank)
}
d5prec <- cbind(rep(1951:2013,each=1),NA)
d5prec[,2] <- r5d(pdmprec5113,5) 
plotgraf(c(8,3.5),d5prec[,2]," # Tage","Größte Niederschlagsmenge in 5 Tagen",c(1,nrow(d5prec),3),seq(1951,2013,3),c(20,110,10)," mm","blue",T,T,1000)

## Yearly Max Temperature ++++++++++++++++++++++++++++++
ytemp <- cbind(rep(1951:2013,each=1),NA)
ytemp[,2] <- yearlymean(pdmmax5113,1,1000) 
x11(8,4);par(mfrow=c(1,1))
plotgraf(c(8,3.5),ytemp[,c(1,2)],"°C","Jahresgemittelte Tageshöchsttemperatur",c(1,nrow(ytemp),3),seq(1951,2013,3),c(11.5,11.5,0.5)," °C","red",T,T,1000)

## Yearly Mean Temperature ++++++++++++++++++++++++++++++
ytemp <- cbind(rep(1951:2013,each=1),NA)
ytemp[,2] <- yearlymean(pdmmean5113,1,1000) 
x11(8,4);par(mfrow=c(1,1))
plotgraf(c(8,3.5),ytemp[,c(1,2)],"°C","Jahresgemittelte Tageshöchsttemperatur",c(1,nrow(ytemp),3),seq(1951,2013,3),c(7,8,0.5)," °C","red",T,T,1000)

## Monthly Mean Temperature and Precipitation ++++++++++++++++++++++++++++++
mmtemp <- cbind(c(1:12),NA)
mmprec <- cbind(c(1:12),NA)
for (mm in 1:12) {
	mmtemp[mm,2] <- round(mean(pdmmean5113[which(pdmmean5113[,3]>=1971&pdmmean5113[,3]<=2000&pdmmean5113[,2]==mm),4]),1) 
	mmprec[mm,2] <- round(sum(pdmprec5113[which(pdmmean5113[,3]>=1971&pdmmean5113[,3]<=2000&pdmmean5113[,2]==mm),4])/30,1)}



## Saisonale Temperature ++++++++++++++++++++++++++++++
ytemp <- cbind(rep(1952:2013,each=1))
ytemp <- cbind(ytemp,t(seasonalmean(pdmmax5113,1,1000)))
x11(6.5,10);par(mfrow=c(4,1))
plotgraf(c(7,3.5),ytemp[,c(1,2)]," °C","Tageshöchsttemperatur Frühling",c(1,nrow(ytemp),3),seq(1952,2013,3),c(11,13.5,0.5)," °C","red",T,T,1000)
plotgraf(c(7,3.5),ytemp[,c(1,3)]," °C","Tageshöchsttemperatur Sommer",c(1,nrow(ytemp),3),seq(1952,2013,3),c(20.5,20.5,0.5)," °C","red",T,T,1000)
plotgraf(c(7,3.5),ytemp[,c(1,4)]," °C","Tageshöchsttemperatur Herbst",c(1,nrow(ytemp),3),seq(1952,2013,3),c(10,13.6,0.5)," °C","red",T,T,1000)
plotgraf(c(7,3.5),ytemp[,c(1,5)]," °C","Tageshöchsttemperatur Winter",c(1,nrow(ytemp),3),seq(1952,2013,3),c(-3,6.6,0.5)," °C","red",T,T,1000)

## Saisonale Niederschläge ++++++++++++++++++++++
yprec <- cbind(rep(1952:2013,each=1))
yprec <- cbind(yprec,t(seasonalmean(pdmprec5113,2,1000)))
x11(6.5,10);par(mfrow=c(4,1))		
plotgraf(c(8,3.5),yprec[,c(1,2)]," mm","Niederschlagsmenge Frühling",c(1,nrow(yprec),3),seq(1952,2013,3),c(60,215,20)," mm","blue",T,T,1000)
plotgraf(c(8,3.5),yprec[,c(1,3)]," mm","Niederschlagsmenge Sommer",c(1,nrow(yprec),3),seq(1952,2013,3),c(60,300,20)," mm","blue",T,T,1000)
plotgraf(c(8,3.5),yprec[,c(1,4)]," mm","Niederschlagsmenge Herbst",c(1,nrow(yprec),3),seq(1952,2013,3),c(50,215,20)," mm","blue",T,T,1000)
plotgraf(c(8,3.5),yprec[,c(1,5)]," mm","Niederschlagsmenge Winter",c(1,nrow(yprec),3),seq(1952,2013,3),c(50,160,20)," mm","blue",T,T,1000)

## Yearly Heavy Rain Events ++++++++++++++++++++++++++++++
yhre <- cbind(rep(1951:2013,each=1),NA)
yhre[,2] <- yearlymean(pdmprec5113,4,10) 
x11(8,4);par(mfrow=c(1,1))
plotgraf(c(8,3.5),yhre[,c(1,2)]," # Tage","Jährliche Anzahl Starkregentage (>10 mm)",c(1,nrow(yhre),3),seq(1951,2013,3),c(4,20,1)," Tage","blue",T,T,1000)

## Yearly dry days ++++++++++++++++++++++++++++++
ydd <- cbind(rep(1951:2013,each=1),NA)
ydd[,2] <- yearlymean(pdmprec5113,5,1) 
plotgraf(c(8,3.5),ydd[,2]," # Tage","Jährliche Anzahl Trockentage",c(1,nrow(ydd),3),seq(1951,2013,3),c(220,230,20)," Tage","blue",T,T,1000)

## Neuschnee ++++++++++++++++++++++++++++++
plotgraf(c(14,4.2),cbind(c(1951:2013),yearlymean(pdmsnow5113,2,1000)->snowdat)," mm","Jährliche Neuschneesumme",c(1,length(snowdat),3),seq(1951,2013,3),c(0,140,5)," mm","blue",T,T,500)
snowdat <- cbind(rep(1:12,33),rep(1981:2013,each=12),snowdat)
snowdat[snowdat[,3]>=50,]

## STARKREGEN ++++++++++++++++++++++++++++++
precyear <- yearlymean(pdmprec5113,4,10)	
precyearmtmax <- monthlymean(pdmprec5113,3,0)
plotgraf(c(14,4.2),precyearmtmax," # Tage","Höchster monatlicher Tagesniederschlag",c(1,length(precyearmtmax),12*3),seq(1951,2013,3),c(0,80,3)," Tage","blue",FALSE,F,50)
precdat <- cbind(rep(1:12,63),rep(1951:2013,each=12),precyearmtmax)
precdat[precdat[,3]>=50,]
pdmprec5113[pdmprec5113[,4]>=50,]

precyear <- yearlymean(pdmprec5113,4,16.4)	
plotgraf(c(14,4.2),cbind(c(1951:2013),precyear)," # Tage","Anzahl Starkregentage (>=16.4mm)",c(1,length(precyear),3),seq(1951,2013,3),c(0,10,2)," Tage","blue",T,T,50)


## Tägliche Niederschläge ++++++++++++++++++++++++++++++
plotgraf(c(8,3.5),pdmprec5113[,4]," mm","Tägliche Niederschlagsmenge in Potsdam",c(180,nrow(pdmprec5113),3*365),seq(1951,2012,3),c(0,80,5)," mm","blue",F,T,40)
sekul.prec <- pdmprec5113
hist(sekul.prec[sekul.prec[,3]>=1971&sekul.prec[,3]<=2000,4]->sekul.prec7100,main="Verteilung des Niederschlags in Potsdam",xlab="Niederschlag in mm",ylab="Häufigkeit") ; quantile(sekul.prec7100,0.999) ## 97%== 9.9 mm 99%==16.4mm

## nur Niederschlagstage
sekul.onlyprec <- pdmprec5113[pdmprec5113[,4]>=1,]

x11(7,3.5);par(mfrow=c(1,1),mar=c(2.7,3.0,2.6,0.4),mgp=c(2.4,0.7,0))
hist(sekul.onlyprec[sekul.onlyprec[,3]>=1971&sekul.onlyprec[,3]<=2000,4]->sekul.onlyprec7100,main="Verteilung des Niederschlags in Potsdam",100,xlab="",ylab="") 
mtext(side=1,line=1.6,text="Niederschlag in mm",cex=1.2)
mtext(side=2,line=1.8,text="Häufigkeit",cex=1.2) ; box()
mtext(side=3,line=0,"1971-2000 - Darstellung: nur Tage mit Niederschlag",cex=0.8)
abline(v=quantile(sekul.prec7100,0.999),col="red");text(39,750,"99.9th Perzentile",adj=0)
quantile(sekul.onlyprec7100,0.99)





thelp1 <- pdmmin[pdmmin[,3]>=1994&pdmmin[,3]<=2013,]
thelp2 <- pdmmax[pdmmax[,3]>=1994&pdmmax[,3]<=2013,]
tall <- cbind(thelp1,thelp2[,4])
write(t(tall),"~/Potsdam_LUP/Potsdam_TMIN_TMAX.txt",ncol=5)



