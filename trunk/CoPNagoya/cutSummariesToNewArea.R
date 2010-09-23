source("cutNewDomain.R")
source("zipRead.R")
source("zipWrite.R")

calc <- c("richness", "turnover")
type <- c("plants", "aves")

bdir <- "C:/CIAT_work/COP_CONDESAN"
summDir <- paste(bdir, "/summaries", sep="")
mskDir <- paste(bdir, "/maskData/AAIGrids", sep="")

msk <- raster(paste(mskDir, "/andes_finalDomain_25m.asc", sep=""))

for (ct in calc) {
	cat("\n")
	cat("Extracting for", ct, "\n")
	
	for (tp in type) {
		
		cat("...-Calculating", tp, "\n")
		
		rsDir <- paste(summDir, "/", ct, "-", tp, "/all-genera", sep="")
		rsList <- list.files(rsDir, pattern=".gz")
		
		outCutDir <- paste(summDir, "/", ct, "-", tp, "/all-genera-newDomain", sep="")
		if (!file.exists(outCutDir)) {
			dir.create(outCutDir)
		}
		
		for (rsf in rsList) {
			cat("...-...-", rsf, "\n")
			
			rs <- zipRead(rsDir, rsf)
			rso <- cutPRGrid(rs, msk)
			
			rso <- zipWrite(rso, outCutDir, rsf)
		}
	}
}

cat("Done! \n")

#rs <- raster("C:/CIAT_work/COP_CONDESAN/summaries/richness-aves/all-genera/richness_baseline_20C3M_WorldClim-2_5min-bioclim_1950_2000_Prevalence.asc")
#df <- "C:/CIAT_work/COP_CONDESAN/protected_areas/AP_CAN_BOL.shp"

#ext <- valueByPolygon(rs, df)

 