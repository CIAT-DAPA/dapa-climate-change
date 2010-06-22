####################################################
#Count n records per species in GBIF Andean queries
####################################################

rs <- read.csv("andes_aves.csv")
taxlist <- unique(rs$id_specie)
counter <- 1

for (i in taxlist) {
	cat(counter, "of", length(taxlist), "\n")
	nrec <- length(which(rs$id_specie == i))
	taxname <- rs$specie[which(rs$id_specie == i)][1]
	if (i == taxlist[1]) {
		reclist <- nrec
		taxlistname <- paste(taxname)
	}
	else {
		reclist <- c(reclist,nrec)
		taxlistname <- c(taxlistname, paste(taxname))
	}
	counter <- counter+1
}
res <- as.data.frame(cbind(paste(taxlist),paste(taxlistname),as.numeric(reclist)))
names(res) <- c("SpeciesID","Species","occurrences")
write.csv(res,"gbif-aves-taxlist.csv",row.names=F,quote=F)


rs <- read.csv("andes_plantae.csv")
taxlist <- unique(rs$id_specie)
counter <- 1

for (i in taxlist) {
	cat(counter, "of", length(taxlist), "\n")
	nrec <- length(which(rs$id_specie == i))
	taxname <- rs$specie[which(rs$id_specie == i)][1]
	if (i == taxlist[1]) {
		reclist <- nrec
		taxlistname <- paste(taxname)
	}
	else {
		reclist <- c(reclist,nrec)
		taxlistname <- c(taxlistname, paste(taxname))
	}
	counter <- counter+1
}
res <- as.data.frame(cbind(paste(taxlist),paste(taxlistname),as.numeric(reclist)))
names(res) <- c("SpeciesID","Species","occurrences")
write.csv(res,"gbif-plantae-taxlist.csv",row.names=F,quote=F)
