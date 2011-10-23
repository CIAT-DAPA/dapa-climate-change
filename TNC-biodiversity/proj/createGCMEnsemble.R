#Script to create a GCM ensemble
require(rgdal); require(raster)

env.dir.in <- "/mnt/GIS-HD716/TNC_global_plants/data/proj"
env.dir.out <- "/data1/TNC/env"

sres <- "A1B"
period <- "2040_2069"
ensemble <- "a1b_2050_ensemble"

if (!file.exists(paste(env.dir.out,"/",ensemble,sep=""))) {
	dir.create(paste(env.dir.out,"/",ensemble,sep=""))
}

zipList <- list.files(env.dir.in,pattern=paste(sres,"_",period,"_",sep=""))

for (i in 1:19) {
	gcount <- 1
	cat("\n")
	if (!file.exists(paste(env.dir.out,"/",ensemble,"/bio_",i,".asc",sep=""))) {
		for (zipFile in zipList) {
			cat("Loading file", i, "inside", zipFile, "\n")
			
			zipFile.path <- paste(env.dir.in,"/",zipFile,sep="")
			zz <- unz(zipFile.path, paste("bio_",i,".asc",sep=""), "r")
			
			#Reading in grid info
			nc <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
			nr <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
			xll <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
			yll <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
			cz <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
			nas <- as.numeric(scan(zz, what="character", nlines=1, quiet=T)[2])
			
			#Read in data
			dta <- scan(zz, na.string=paste(nas), quiet=T)
			close(zz)
			
			#Drawing the extent 
			xur <- xll + cz*nc
			yur <- yll + cz*nr

			#Creating the raster and filling it with data
			rs <- raster(ncol=nc, nrow=nr, xmn=xll, xmx=xur, ymn=yll, ymx=yur)
			rs[] <- dta
			
			#Summarising
			if (gcount == 1) {
				rs.mean <- rs
			} else {
				rs.mean <- rs.mean + rs
			}
			
			gcount <- gcount + 1
			rm(rs); g=gc()
		}
		
		cat("Calculating mean and writing \n")
		rs.mean <- round(rs.mean/length(zipList),0)
		writeRaster(rs.mean, paste(env.dir.out,"/",ensemble,"/bio_",i,".asc",sep=""),format='ascii',overwrite=T)
		rm(rs.mean); g=gc()
	}
}

