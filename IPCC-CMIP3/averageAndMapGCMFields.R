#Julian Ramirez, dawnpatrolmustaine@gmail.com
#ot <- mapGCMFields(c(1,2:24), "F:/", "C:/CIAT_work/_tools/packageTesting", "SRES_A1B", "anomalies", "2010_2039", xn=-85, xx=-35, yn=-20, yx=13, wt=5, "C:/CIAT_work/World_Shapefile/Countries/world_adm0.shp", temp=T, prec=F, writeRasterFiles=F)

require(sp)
require(rgdal)
require(raster)
require(maptools)
gpclibPermit()

gcmChars <- read.csv("gcm_chars.csv")

cat("\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("XXXXXXXXXXXXXX AVERAGE AND MAP GCM FIELDS XXXXXXXXXXXXXXXX \n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")
cat("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\n")

cat("\n")
cat("GCM List\n")
cat("\n")
for (r in 1:nrow(gcmChars)) {
	cat(paste(r, ". ", gcmChars$model[r], sep=""), "\n")
}
cat("\n")

cat("\n")
cat("IMPORTANT INFORMATION!!! \n")
cat("\n")
cat("The function mapGCMFields has become available, use it as follows \n")
cat("\n")
cat("mapGCMFields(gcmList, drive, procdir, scenario, type, period, xn=-180, xx=180, yn=-90, yx=90, wt=5, worldshapefile, temp=T, prec=T) \n")
cat("\n")
cat("\n")
cat("Where: \n")
cat("*** [gcmList] is a numeric vector with the number of the GCMs to be displayed, eg. c(1,2,4,6,10,20). See GCM list above \n")
cat("*** [drive] is the network unit drive letter where the data is stored (eg. W:/) \n")
cat("*** [procdir] is the folder where you want to store your output charts (default is C:/ for windows versions and /home/username/ for linux distros) \n")
cat("*** [scenario] can be 20C3M, SRES_A1B, SRES_A2, or SRES_B1 \n")
cat("*** [type] can be anomalies, or actual \n")
cat("*** [period] can be 2010_2039, 2020_2049, 2030_2059, 2040_2069, 2050_2079, 2060_2089, 2070_2099\n")
cat("*** [xn], [xx], [yn], [yx] are the limits (xmin, xmax, ymin, ymax) of the bounding box you intend to plot \n")
cat("*** [wt] is the width in inches of the plot (multi-page PDF plots) \n")
cat("*** [worldshapefile] is a shapefile of administrative boundaries of the world, not used if none provided \n")
cat("*** [temp=T/F] logical, will map temperature data if TRUE \n")
cat("*** [prec=T/F] logical, will map precipitation data if TRUE \n")
cat("*** [writeRasterFiles=T/F] logical, will write raster files for each model and the MMM if TRUE \n")
cat("\n")

mapGCMFields <- function(gcmList, drive, procdir, scenario, type, period, xn=-30, xx=30, yn=-40, yx=40, wt=5, worldshapefile, temp=T, prec=T, writeRasterFiles=T) {
	#Checking the list makes sense
	
	if (!is.numeric(gcmList)) {
		stop("gcmList should be a numeric vector of length >= 1")
	}
	
	if (max(gcmList) > nrow(gcmChars)) {
		stop("Number of available models (24) exceeded")
	}
	
	if (length(gcmList) > nrow(gcmChars)) {
		cat("Length in gcmList is greater than the number of available models, using all models instead \n")
		gcmList <- c(1:24)
	}
	
	gcmChars <- gcmChars[gcmList,]
	
	#Checking the input dir exists
	
	indir <- paste(drive, "/climate_change/IPCC_CMIP3", sep="")
	if (!file.exists(indir)) {
		stop("Input dir does not exist, check drive letter")
	}
	
	if (!file.exists(procdir)) {
		cat("Working directory does not exist, using default (C:/ in windows and /home/user/ in linux \n")
		if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
			procdir <- "C:/"
		} else if (substr( R.Version()$platform, 1, 9) == "x86_64-pc" ) {
			procdir <- "C:/"
		} else {
			whoami <- system(paste("whoami"), intern=T)
			procdir <- paste("/home/", usr, sep="")
		}
	}
	
	#Checking the scenario is supported
	scenario <- toupper(scenario)
	if (!scenario %in% c("20C3M","SRES_A1B","SRES_A2","SRES_B1")) {
		stop('Scenario', scenario, ' is not supported')
	}
	
	#Checking the type is supported, and changing it slightly
	type <- tolower(type)
	if (!type %in% c("anomalies","actual")) {
		stop('Type', type, ' is not supported')
	}
	
	if (scenario == "20C3M" & type == "anomalies") {
		stop('No anomalies are possible for 20C3M scenario')
	}
	
	if (type == "actual") {
		type <- "filled"
	}
	
	#Checking the period is supported
	if (!period %in% c("2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099")) {
		stop('Period', period, ' is not supported')
	}
	
	#Checking numerical parameters
	if (!is.numeric(xn)) {stop("xn must be a numeric value")}
	if (!is.numeric(xx)) {stop("xx must be a numeric value")}
	if (!is.numeric(yn)) {stop("yn must be a numeric value")}
	if (!is.numeric(yx)) {stop("yx must be a numeric value")}
	if (!is.numeric(wt)) {stop("wt must be a numeric value")}
	
	#Checking the boundaries of the bbox
	if (xn < -180) {
		cat("Minimum X is lower than -180, using -180 as default \n")
		xn <- -180
	}
	if (xx > 180) {
		cat("Maximum X is greater than 180, using 180 as default \n")
		xx <- 180
	}
	if (yn < -90) {
		cat("Minimum Y is lower than -90, using -90 as default \n")
		yn <- -90
	}
	if (yx > 180) {
		cat("Maximum Y is greater than 90, using 90 as default \n")
		yx <- 180
	}
	
	#Checking consistency of the width
	if (wt < 5) {
		stop('Plot width too small, use values between 4 and 15 inches')
	} else if (wt > 15) {
		stop('Plot width too large, use values between 4 and 15 inches')
	}
	
	#Selecting np based on variables switches
	if (prec & temp) {
		np <- 2
		varList <- c("tmean", "prec")
		suf <- "TP"
	} else if (prec & !temp) {
		np <- 1
		varList <- c("prec")
		suf <- "P"
	} else if (!prec & temp) {
		np <- 1
		varList <- c("tmean")
		suf <- "T"
	} else if (!prec & !temp) {
		stop("At least one of the variables needs to be ON")
	}
	
	cat("\n")
	cat("Selected models are: \n")
	cat("\n")
	for (r in 1:nrow(gcmChars)) {
		cat(paste(r, ". ", gcmChars$model[r], sep=""), "\n")
	}
	cat("\n")
	
	if (file.exists(worldshapefile)) {
		cat("Reading world shapefile... \n")
		sh <- readShapePoly(worldshapefile)
	} else {
		cat("World shapefile not provided, so wont be used... \n")
	}
	
	inputDir <- paste(indir, "/", scenario, sep="")
	typeDir <- paste(inputDir, "/", type, sep="")

	#Creating the base raster
	cat("Creating the base raster... \n")
	cz <- min(gcmChars$cellsize)
	nc <- 360/cz
	nr <- 180/cz
	rs <- raster(ncol=nc, nrow=nr)
	xy <- xyFromCell(rs, 1:ncell(rs))

	#This is for calculating the width and height of the map
	xt <- extent(c(xn,xx,yn,yx))
	rsa <- crop(rs, xt)
	rel <- ncol(rsa)/nrow(rsa)
	
	ht <- wt/rel
	ht <- ht/np
	
	#Relationship between width and pointsize (change for smaller, or greater font size)
	pzrel <- 15/12
	pz <- wt/pzrel

	#Creating the figure (PDF format)
	cat("Creating the figure with characteristics... \n")
	pdf(paste(procdir, "/Figs_", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".pdf", sep=""), width=wt, height=ht, pointsize=pz)
	par(mfrow=c(1,np), ps=pz)

	gcmctr <- 1
	
	for (gcm in gcmChars$model) {
		
		cat("\n")
		cat("Processing", toupper(paste(gsub("_", "-",gcm))), "\n")
		
		gcmDir <- paste(typeDir, "/", gcm, sep="")
		
		if (file.exists(gcmDir)) {
			#The entire process here
			
			periodDir <- paste(gcmDir, "/", period, sep="")
			
			for (vr in varList) {
				
				cat("   .Averaging over monthly", vr,"\n")
				
				for (m in 1:12) {
					
					#cat("Month", m, "\n")
					
					if (type != "anomalies" & m < 10) {mth <- paste(0, m, sep="")} else {mth <- m}
					
					ras <- raster(paste(periodDir, "/", vr, "_", mth, ".asc", sep=""))
					vals <- xyValues(ras, xy)
					
					if (m == 1) {
						resVals <- vals
					} else {
						resVals <- resVals + vals
					}
				}
				
				if (prec & temp) {
					if (vr == "tmean") {
						p1 <- resVals / 12
					} else {
						p12 <- resVals
					}
				} else if (temp & !prec) {
					p1 <- resVals / 12
				} else if (!temp & prec) {
					p12 <- resVals
				} else {
					stop("At least one of the variables needs to be ON")
				}
			}
			
			#Assigning a title for each gcm
			assign(paste("title", gcmctr, sep=""), toupper(paste(gsub("_", "-",gcm))))
			
			cat("   .Temperature and Rainfall rasters \n")
			
			if (temp) {
				assign(paste("p1r", gcmctr, sep=""), raster(rs))
				assign(paste("p1r", gcmctr, sep=""), setValues(get(paste("p1r", gcmctr, sep="")), p1))
				assign(paste("p1r", gcmctr, sep=""), crop(get(paste("p1r", gcmctr, sep="")), xt))
				
				if (gcmctr == 1) {
					exVals <- getValues(get(paste("p1r", gcmctr, sep="")))
					theListt <- exVals[which(!is.na(exVals))]
				} else {
					exVals <- getValues(get(paste("p1r", gcmctr, sep="")))
					theListt <- c(theListt, exVals[which(!is.na(exVals))])
				}
				
				rm(p1)
				rm(exVals)
				
				if (writeRasterFiles) {
					rName <- paste(procdir, "/AIIGrid_AMT", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_", gcm, "_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".asc", sep="")
					p1r <- writeRaster(p1r, rName, format='ascii', overwrite=T)
					rm(rName)
				}
			}
			
			if (prec) {
				assign(paste("p12r", gcmctr, sep=""), raster(rs))
				assign(paste("p12r", gcmctr, sep=""), setValues(get(paste("p12r", gcmctr, sep="")), p12))
				assign(paste("p12r", gcmctr, sep=""), crop(get(paste("p12r", gcmctr, sep="")), xt))
				
				if (gcmctr == 1) {
					exVals <- getValues(get(paste("p12r", gcmctr, sep="")))
					theListp <- exVals[which(!is.na(exVals))]
				} else {
					exVals <- getValues(get(paste("p12r", gcmctr, sep="")))
					theListp <- c(theListp, exVals[which(!is.na(exVals))])
				}
				
				rm(p12)
				rm(exVals)
				if (writeRasterFiles) {
					rName <- paste(procdir, "/AIIGrid_TAR", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_", gcm, "_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".asc", sep="")
					p12r <- writeRaster(p12r, rName, format='ascii', overwrite=T)
					rm(rName)
				}
			}
			
			if (gcmctr == 1) {
				cat("   .MultiModel stack (1) \n")
				if (temp) {
					p1List <- get(paste("p1r", gcmctr, sep=""))
				}
				
				if (prec) {
					p12List <- get(paste("p12r", gcmctr, sep=""))
				}
			} else {
				cat("   .MultiModel stack \n")
				if (temp) {
					p1List <- c(p1List, get(paste("p1r", gcmctr, sep="")))
				}
				
				if (prec) {
					p12List <- c(p12List, get(paste("p12r", gcmctr, sep="")))
				}
			}
			
			gcmctr <- gcmctr+1
			
		} else {
			cat("The model ", gcm, "was not available for this scenario \n")
		}
		
	}

	cat("\n")
	cat("Finalizing MultiModel calculations \n")

	fun <- function(x) { sd(x) }
	
	if (temp) {
		cat("   .Temperature mean \n")
		p1m <- mean(stack(p1List))
		tzlim <- c(min(theListt), max(theListt))
		cat("   .Temperature std \n")
		p1sd <- calc(stack(p1List),fun)
	}
	
	if (prec) {
		cat("   .Precipitation mean \n")
		p12m <- mean(stack(p12List))
		pzlim <- c(min(theListp), max(theListp))
		cat("   .Precipitation std \n")
		p12sd <- calc(stack(p12List),fun)
	}
	
	cat("\n")
	cat("Plotting \n")
	
	totgcms <- gcmctr-1
	
	for (gcmctr in 1:totgcms) {
		if (temp) {
			plot(get(paste("p1r", gcmctr, sep="")), col=colorRampPalette(c('yellow', 'red'))(500), main=get(paste("title", gcmctr, sep="")), sub="Mean temperature (°C)", zlim=tzlim)
			if (file.exists(worldshapefile)) {
				plot(sh, add=T)
			}
		}
		
		if (prec) {
			plot(get(paste("p12r", gcmctr, sep="")), col=rainbow(1000), main=get(paste("title", gcmctr, sep="")), sub="Total precipitation (mm/year)", zlim=pzlim)
			if (file.exists(worldshapefile)) {
				plot(sh, add=T)
			}
		}
	}
	
	if (temp) {
		plot(p1m, col=colorRampPalette(c('yellow', 'red'))(500), main="MultiModelMean", sub="Mean temperature (°C)", zlim=tzlim)
		if (file.exists(worldshapefile)) {
			plot(sh, add=T)
		}
		plot(p1sd, col=topo.colors(1000), main="MultiModelSD", sub="Deviation (°C)")
		if (file.exists(worldshapefile)) {
			plot(sh, add=T)
		}
		
		#Write the raster files based on input logical condition
		if (writeRasterFiles) {
			rName <- paste(procdir, "/AIIGrid_AMT", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_MMM_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".asc", sep="")
			p1m <- writeRaster(p1m, rName, format='ascii', overwrite=T)
			rm(rName)
			
			rName <- paste(procdir, "/AIIGrid_AMT", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_MMSD_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".asc", sep="")
			p1sd <- writeRaster(p1sd, rName, format='ascii', overwrite=T)
			rm(rName)
		}
	}
	
	if (prec) {
		plot(p12m, col=rainbow(1000), main="MultiModelMean", sub="Total precipitation (mm/year)", zlim=pzlim)
		if (file.exists(worldshapefile)) {
			plot(sh, add=T)
		}
		plot(p12sd, col=topo.colors(1000), main="MultiModelSD", sub="Deviation (mm/year)")
		if (file.exists(worldshapefile)) {
			plot(sh, add=T)
		}
		
		#Write the raster files based on input logical condition
		if (writeRasterFiles) {
			rName <- paste(procdir, "/AIIGrid_TAR", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_MMM_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".asc", sep="")
			p12m <- writeRaster(p12m, rName, format='ascii', overwrite=T)
			rm(rName)
			
			rName <- paste(procdir, "/AIIGrid_TAR", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_MMSD_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".asc", sep="")
			p12sd <- writeRaster(p12sd, rName, format='ascii', overwrite=T)
			rm(rName)
		}
	}

	dev.off()

	cat("\n")
	cat("Done! \n")
	return(paste(procdir, "/Figs_", xn, "WE", xx, "WE", yn, "NS", yx, "NS_", type, "_", gsub("_","",scenario), "_", gsub("_", "-", period), "_", suf, ".pdf", sep=""))
}