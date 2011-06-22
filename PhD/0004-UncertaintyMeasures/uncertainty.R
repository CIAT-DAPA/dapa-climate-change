#Extract GCM anomaly values for a given point and do density plots
#F:\climate_change\IPCC_CMIP3\SRES_A1B\anomalies\bccr_bcm2_0\2010_2039

bDir <- "F:/climate_change/IPCC_CMIP3"
period <- "2020_2049"
month <- 1 #This is month
variable <- "prec" #tmean, tmin, tmax

sres <- "A1B"; sres <- paste("SRES_", sres, sep="")
sresDir <- paste(bDir, "/", sres, "/anomalies", sep="")

gcmList <- list.files(sresDir)


#Defining if dividing according to variable
if (variable == "prec") {
	divide <- F
} else {
	divide <- T
}

#Define target site
x <- 76.981201
y <- 29.668963

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
	} else {
		m <- month
		if (m < 10) {mth <- paste("0",m,sep="")} else {mth <- paste(m)}
		in.raster <- paste(dataDir, "/", variable, "_", mth, ".asc", sep="")
		in.stk <- raster(in.raster)
	}
	
	#Create output raster by dividing or keeping the sum of the values
	out.rs <- sum(in.stk)
	if (divide) {out.rs <- out.rs / length(month)}
	
	#Now extract the values of the specified cell
	value <- extract(out.rs, xy)
	
}

