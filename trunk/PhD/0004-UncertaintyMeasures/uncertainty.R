#Extract GCM anomaly values for a given point and do density plots
#F:\climate_change\IPCC_CMIP3\SRES_A1B\anomalies\bccr_bcm2_0\2010_2039

extractGCMData <- function(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:9, sres="A1B", x=76.981201, y=29.668963) {
	
	#Defining input directory
	sres <- "A1B"; sres <- paste("SRES_", sres, sep="")
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
			for (m in month) {
				if (m < 10) {mth <- paste("0",m,sep="")} else {mth <- paste(m)}
				
				#Defining and reading first raster
				in.raster <- paste(dataDir, "/", variable, "_", mth, ".asc", sep="")
				in.rs <- raster(in.raster)
				
				#Creating list for stack
				if (m == month[1]) {
					in.stk <- in.rs
				} else {
					in.stk <- list(in.stk,in.rs)
				}
			}
			
			#Now creating stack and calculating mean/total
			in.stk <- stack(in.stk)
			
			#Create output raster by dividing or keeping the sum of the values
			out.rs <- sum(in.stk)
			if (divide) {out.rs <- out.rs / length(month)}
			
		} else {
			m <- month
			if (m < 10) {mth <- paste("0",m,sep="")} else {mth <- paste(m)}
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


pix.data <- extractGCMData(bDir="F:/climate_change/IPCC_CMIP3", variable="prec", period="2020_2049", month=6:9, sres="A1B", x=76.981201, y=29.668963)

#generate breaks
pix.range <- max(pix.data) - min(pix.data)


hist(pix.data)
dev.new(); plot(density(pix.data))



