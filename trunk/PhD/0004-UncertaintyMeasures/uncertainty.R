#Extract GCM anomaly values for a given point and do density plots
#F:\climate_change\IPCC_CMIP3\SRES_A1B\anomalies\bccr_bcm2_0\2010_2039
library(raster)
rm(list=ls()); g<-gc(T)

extractGCMData <- function(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:9, sres="A1B", x=76.981201, y=29.668963) {
	
#   bDir <- "F:/climate_change/IPCC_CMIP3"
#   variable <- "prec"
#   period <- "2020_2049"
#   month <- 6:9
#   sres <- "A1B"
#   x <- 76.981201
#   y <- 29.668963
  
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
  ndiv <- 8
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

kar.data <- extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:9, sres="A1B", x=76.981201, y=29.668963)
kar.h <- shannon.e(kar.data,ndiv=8)

kar.data.t <- extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="tmean", period="2020_2049", month=6:9, sres="A1B", x=76.981201, y=29.668963)
kar.h.t <- shannon.e(kar.data.t,ndiv=8)

tim.data <- extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:9, sres="A1B", x=-77.250, y=2.917)
tim.h <- shannon.e(tim.data,ndiv=8)

tim.data.t <- extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="tmean", period="2020_2049", month=6:9, sres="A1B", x=-77.250, y=2.917)
tim.h.t <- shannon.e(tim.data.t,ndiv=8)

