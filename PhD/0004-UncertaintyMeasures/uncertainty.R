#Extract GCM anomaly values for a given point and do density plots
#F:\climate_change\IPCC_CMIP3\SRES_A1B\anomalies\bccr_bcm2_0\2010_2039
library(raster)
rm(list=ls()); g<-gc(T)

extractGCMData <- function(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:9, sres="A1B", x=76.981201, y=29.668963) {
	
	#Defining input directory
	sres <- paste("SRES_", sres, sep="")
	sresDir <- paste(bDir, "/", sres, "/anomalies", sep="")
	
	#Listing the GCMs
	gcmList <- list.files(sresDir)
	
	#Defining if dividing according to variable
	if (variable == "prec") {
		divide <- F
	} else {
		divide <- T
	}
	
	#Create data frame for target site
	xy <- data.frame(X=x, Y=y)
	
	for (gcm in gcmList) {
		cat("Processing model", gcm, "\n")
		dataDir <- paste(sresDir, "/", gcm, "/", period, sep="")
		
		#Looping through months to extract data, for those cases in which more than 1 month is specified
		if (length(month) > 1) {
			for (mth in month) {
				#Defining and reading first raster
				in.raster <- paste(dataDir, "/", variable, "_", mth, ".asc", sep="")
				in.rs <- raster(in.raster)
				
				#Creating list for stack
				if (mth == month[1]) {
					in.stk <- in.rs
				} else {
					in.stk <- c(in.stk,in.rs)
				}
			}
			
			#Now creating stack and calculating mean/total
			in.stk <- stack(in.stk)
      
			#Create output raster by dividing or keeping the sum of the values
			out.rs <- sum(in.stk)
			if (divide) {out.rs <- out.rs / length(month)}
			
		} else {
			in.raster <- paste(dataDir, "/", variable, "_", mth, ".asc", sep="")
			out.rs <- raster(in.raster)
		}
		
		#Now extract the values of the specified cell
		value <- extract(out.rs, xy)
		
		if (gcm == gcmList[1]) {
			value.list <- value
		} else {
			value.list <- c(value.list, value)
		}
	}
	return (value.list)
}

#Shannon's entropy function, vector is first discretised
shannon.e <- function(pix.data, ndiv=8) {
  #Calculate range and interval
  pix.range <- max(pix.data) - min(pix.data)
  int.lg <- pix.range / ndiv
  
  #generate breaks (equally distant) and calculate probabilities for each break
  for (d in 1:ndiv) {
    if (d == 1) {
      ll <- min(pix.data)
      ul <- ll+int.lg
      ncases <- length(which(pix.data >= ll & pix.data < ul))
      hi <- (ncases/length(pix.data))*log(ncases/length(pix.data))
    } else if (d == ndiv) {
      ll <- ul
      ul <- max(pix.data)
      ncases <- length(which(pix.data >= ll & pix.data <= ul))
      hi <- (ncases/length(pix.data))*log(ncases/length(pix.data))
    } else {
      ll <- ul
      ul <- ll+int.lg
      ncases <- length(which(pix.data >= ll & pix.data <= ul))
      hi <- (ncases/length(pix.data))*log(ncases/length(pix.data))
    }
    
    #cat("Interval", d, "from", ll, "to", ul, "\n")
    #cat(ncases, "cases \n")
    
    if (d == 1) {
      classes <- data.frame(INT=d, LL=ll, UL=ul, CASES=ncases)
      H <- hi
    } else {
      classes <- rbind(classes, c(d,ll,ul,ncases))
      H <- c(H,hi)
    }
    
  }
  H.total <- -(sum(H,na.rm=T))
  
  return(list(CLASSES=classes, Hvect=H, H=H.total))
}

wd <- "F:/PhD-work/climate-data-assessment/gcm-uncertainties"
setwd(wd)

sites <- read.csv("sites.csv")

for (site in sites$Site) {
  cat("\n")
  cat("Processing", paste(site), "\n")
  
  ctry <- sites$Country[which(sites$Site == site)]
  loc.x <- sites$x[which(sites$Site == site)]
  loc.y <- sites$y[which(sites$Site == site)]
  
  prec.data <- extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:8, sres="A1B", x=loc.x, y=loc.y)
  prec.h <- shannon.e(prec.data, ndiv=8)$H
  
  temp.data <- extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="tmean", period="2020_2049", month=6:8, sres="A1B", x=loc.x, y=loc.y)
  temp.h <- shannon.e(temp.data, ndiv=8)$H
  
  nposit <- length(which(prec.data > 0))
  nnegat <- length(which(prec.data < 0))
  
  if (mean(prec.data) < 0) {ag.prec <- nnegat/length(prec.data)} else {ag.prec <- nposit/length(prec.data)}
  
  out.data <- data.frame(
    Country=ctry, 
    Site=site, 
    x=loc.x, 
    y=loc.y, 
    Mean.prec=mean(prec.data), 
    Max.prec=max(prec.data),
    Min.prec=min(prec.data),
    Range.prec=max(prec.data)-min(prec.data)
    StD.prec=sd(prec.data), 
    CV.prec=sd(prec.data)/mean(prec.data)*100, 
    AG.prec=ag.prec,
    H.prec=prec.h,
    Mean.temp=mean(temp.data), 
    Max.temp=max(temp.data),
    Min.temp=min(temp.data),
    Range.temp=max(temp.data)-min(temp.data)
    StD.temp=sd(temp.data), 
    CV.temp=sd(temp.data)/mean(temp.data)*100, 
    H.temp=temp.h
  )
  
  if (site == sites$Site[1]) {
    out.all <- out.data
  } else {
    out.all <- rbind(out.all, out.data)
  }
  
}

