require(sp)
require(rgdal)
require(raster)
require(maptools)

gcmChars <- read.csv("gcm_chars.csv")

cat("\n")
cat("GCM List\n")
cat("\n")
for (r in 1:nrow(gcmChars)) {
	cat(paste(r, ". ", gcmChars$model[r], sep=""), "\n")
}
cat("\n")

gcmList <- c(1:5) 								#A parameter to define
gcmChars <- gcmChars[gcmList,]

indir <- "F:/climate_change/IPCC_CMIP3" 		#A parameter to define
procdir <- "C:/CIAT_work/Workspace"				#A parameter to define
scenario <- "SRES_A1B" 							#A parameter to define
type <- "anomalies" 							#A parameter to define
period <- "2010_2039" 							#A parameter to define

worldshapefile <- "F:/Administrative_boundaries/adminFiles/10m-admin-0-countries.shp" #A parameter to define
sh <- readShapePoly(worldshapefile)

inputDir <- paste(indir, "/", scenario, sep="")
typeDir <- paste(inputDir, "/", type, sep="")

varList <- c("tmean", "prec")

cz <- min(gcmChars$cellsize)
nc <- 360/cz
nr <- 180/cz
rs <- raster(ncol=nc, nrow=nr)
xy <- xyFromCell(rs, 1:ncell(rs))

wt <- 5
rel <- ncol(rs)/nrow(rs)
ht <- wt/rel

np <- 2
ht <- ht/np

pdf("outputFigs.pdf", width=wt, height=ht)
par(mfrow=c(1,np))

gcmctr <- 1

for (gcm in gcmChars$model) {
	
	cat("Processing", paste(gcm), "\n")
	
	gcmDir <- paste(typeDir, "/", gcm, sep="")
	if (file.exists(gcmDir)) {
		#The entire process here
		
		periodDir <- paste(gcmDir, "/", period, sep="")
		
		for (vr in varList) {
			
			cat("Variable", vr, "\n")
			
			for (m in 1:12) {
				
				cat("Month", m, "\n")
				
				if (type != "anomalies" & m < 10) {mth <- paste(0, m, sep="")} else {mth <- m}
				
				ras <- raster(paste(periodDir, "/", vr, "_", mth, ".asc", sep=""))
				vals <- xyValues(ras, xy)
				
				if (m == 1) {
					resVals <- vals
				} else {
					resVals <- resVals + vals
				}
			}
			
			if (vr == "tmean") {
				p1 <- resVals / 12
			} else {
				p12 <- resVals
			}
		}
		
		p1r <- raster(rs)
		p1r[] <- p1
		rm(p1)
		
		p12r <- raster(rs)
		p12r[] <- p12
		rm(p12)
		
		plot(p1r, col=heat.colors(1000), main=paste(gcm, "-Temperature", sep=""))
		plot(sh, add=T)
		
		plot(p12r, col=rainbow(1000), main=paste(gcm, "-Precipitation", sep=""))
		plot(sh, add=T)
		
		if (gcmctr == 1) {
			p1 <- p1r
			p12 <- p12r
		} else {
			p1 <- p1+p1r
			p12 <- p12+p12r
		}
		
		gcmctr <- gcmctr+1
		
	} else {
		cat("The model ", gcm, "was not available for this scenario \n")
	}
	
}

p1 <- p1 / nrow(gcmChars)
p12 <- p12 / nrow(gcmChars)

plot(p1, col=heat.colors(1000), main=paste("MultiModel-Temperature", sep=""))
plot(sh, add=T)

plot(p12, col=heat.colors(1000), main=paste("MultiModel-Precipitation", sep=""))
plot(sh, add=T)

dev.off()
