####################################################
#Count n records per species in Andean species databaes
####################################################

cat("\n")

rs <- read.csv("/modeling-data/andean-species-data-sampleArea.csv")

dbList <- c("cdc.csv","condesan.csv","gbif.csv")
dbC <- 1
for (db in dbList) {
	dbData <- rs[which(rs$Source == db),]
	taxlist <- unique(dbData$IDSpecies)
	
	counter <- 1
	
	for (i in taxlist) {
		cat(counter, "of", length(taxlist), paste("(",round(counter/length(taxlist)*100,2),"%)",sep=""), "\n")
		
		spData <- dbData[which(dbData$IDSpecies == i),]
		
		nrec <- nrow(spData)
		nrecAndes <- sum(spData$AndesMountain)
		nrecAndesADM <- sum(spData$AndesADM0)
		taxname <- spData$Species[1]
		
		olst <- c(paste(db),i,paste(taxname),nrec,nrecAndes,nrecAndesADM)
		
		if (i == taxlist[1]) {
			omx <- olst
		}
		else {
			omx <- rbind(omx, olst)
		}
		counter <- counter+1
	}
	row.names(omx) <- 1:nrow(omx)
	res <- as.data.frame(omx)
	names(res) <- c("Source","SpeciesID","Species","Occurrences","AndesOccurrences","AndesADM0Occurrences")
	write.csv(res,paste("taxlist-", db, sep=""),row.names=F,quote=F)
}
