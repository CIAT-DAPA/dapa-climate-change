Extract values to points from surfaces generated
listasc <- paste(outName, "/crossval/", spID, "_", 0:24, ".asc", sep="")
data <- lapply(listasc, FUN=raster)
coords <- read.csv(backFileSwd)[2:3]

for(i in 1:length(listasc)){
	data_mat <- extract(data[[i]], coords)
	testMatrix <- cbind(coords, data_mat)
	names(testMatrix)[3] <- paste(spID, "_", i-1, sep="")
	write.csv(testMatrix,paste(outName, "/crossval/", spID, "_", i-1, ".csv", sep=""), row.names=F)
	}
	
