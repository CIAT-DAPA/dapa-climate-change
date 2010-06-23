#Script to manage databases
#based on a list of species will select the species from a group and will print that into a new data frame
#with the following format ID, spname, lon, lat

#CDC species name field is: 		$especie
#CONDESAN species name field is: 	$Nombre
#GBIF species name field is:		$specie

#CDC longitude and latitude fields are:			$longitud	$latitud
#CONDESAN longitude and latitude fields are:	$Longitud	$Latitud
#GBIF longitude and latitude fields are:		$lon		$lat

cat("\n")
uniqueNamesList <- read.csv("speciesListForModeling.csv") #ID,Species

dbList <- c("cdc.csv","condesan.csv","gbif.csv")
dbC <- 1
for (db in dbList) {
	cat("Processing database", db, "\n")
	rs <- read.csv(db)
	
	if (db == "cdc.csv") {
		dta <- rs[which(rs$especie %in% uniqueNamesList$Species),]
		dta2 <- as.data.frame(cbind(rep(db,times=nrow(dta)),rep(99999,times=nrow(dta)),paste(dta$especie),dta$longitud,dta$latitud))
	} else if (db == "condesan.csv") {
		dta <- rs[which(rs$Nombre %in% uniqueNamesList$Species),]
		dta2 <- as.data.frame(cbind(rep(db,times=nrow(dta)),rep(99999,times=nrow(dta)),paste(dta$Nombre),dta$Longitud,dta$Latitud))
	} else if (db == "gbif.csv") {
		dta <- rs[which(rs$specie %in% uniqueNamesList$Species),]
		dta2 <- as.data.frame(cbind(rep(db,times=nrow(dta)),rep(99999,times=nrow(dta)),paste(dta$specie),dta$lon,dta$lat))
	}
	names(dta2) <- c("Source","IDSpecies","Species","Lon","Lat")
	
	dta2 <- merge(dta2, uniqueNamesList)
	dta2$IDSpecies <- dta2$ID
	dta2 <- dta2[,1:(ncol(dta2)-1)]
	
	write.csv(dta, paste("selectedSPP-", db, sep=""), row.names=F, quote=T)
	rm(dta)
	
	if (dbC == 1) {
		fDataset <- dta2
		rm(dta2)
	} else {
		fDataset <- rbind(fDataset,dta2)
		rm(dta2)
	}
	
	dbC <- dbC+1
}

write.csv(fDataset,"andean-species-data.csv", row.names=F, quote=F)
rm(fDataset)