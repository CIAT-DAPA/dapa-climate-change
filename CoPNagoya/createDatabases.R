#Script to manage databases
#based on a list of species will select the species from a group and will print that into a new data frame
#with the following format ID, spname, lon, lat

#CDC species name field is: 		$especie
#CONDESAN species name field is: 	$Nombre
#GBIF species name field is:		$specie

uniqueNamesList <- read.csv("speciesListForModeling.csv")

dbList <- c("cdc.csv","condesan.csv","gbif.csv")

for (db in dbList) {
	
}