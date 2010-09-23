source("extractByPointArea.R")
source("zipRead.R")

calc <- c("richness", "turnover")
type <- c("plants", "aves")

bdir <- "C:/CIAT_work/COP_CONDESAN"
summDir <- paste(bdir, "/summaries", sep="")
shpDir <- paste(bdir, "/protected_areas", sep="")

for (ct in calc) {
	cat("\n")
	cat("Extracting for", ct, "\n")
	
	for (tp in type) {
		
		cat("...-Calculating", tp, "\n")
		
		rsDir <- paste(summDir, "/", ct, "-", tp, "/all-genera", sep="")
		rsList <- list.files(rsDir, pattern=".gz")
		
		outMetricsDir <- paste(summDir, "/", ct, "-", tp, "/APMetrics", sep="")
		if (!file.exists(outMetricsDir)) {
			dir.create(outMetricsDir)
		}
		
		for (rsf in rsList) {
			cat("...-...-", rsf, "\n")
			
			rs <- zipRead(rsDir, rsf)
			ext <- valueByPolygon(rs, paste(shpDir, "/AP_CAN_EDIT_WGS84_SPart.shp", sep="")) #AP_CAN_BOL AP_CAN_EDIT_WGS84_SPart
			
			outFName <- gsub(".asc.gz", ".csv", rsf)
			
			write.csv(ext, paste(outMetricsDir, "/", outFName, sep=""), quote=F, row.names=F)
			
			rm(ext)
		}
	}
}

cat("Done! \n")

#rs <- raster("C:/CIAT_work/COP_CONDESAN/summaries/richness-aves/all-genera/richness_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_Prevalence.asc")
#df <- "C:/CIAT_work/COP_CONDESAN/protected_areas/AP_CAN_BOL.shp"

#ext <- valueByPolygon(rs, df)

 