#Julian Ramirez
#eejarv@leeds.ac.uk
#Dec 2010

#Script to validate presence and absence of the crop against suitability rating at different administrative levels
#1. input should be a shapefile and a suitability raster
#2. then extract all the values from the shapefile corresponding to the particular field that 
#   indicates presence of absence
#3. create a data-frame that has 
	#a. all the features in the shapefile,
	#b. the suitability rating (max, min, mean, sd) in one column (and derived presence/absence)
	#b1. percent suitable 
	#c. the feature presence, absence or NA corresponding value
	#d. calculate accuracy metrics
		#i. false negative rate: percent of features being predicted as if the crop was not suitable, but being
		#	marked as with the crop in national statistics, to the total number of available features to assess
		#ii. true positive rate: percent of features being predicted as suitable and also marked as suitable 
		#	 to the total number of available features to assess
		#iii. true negative rate: if possible, using absences, calculate percent of features being predicted to
		#	  not have the crop, and in fact not having the crop to the percent of features to assess
		#iv. false positive rate cannot be calculated since this is a climate-based potential suitability map

require(maptools)
require(raster)
require(rgdal)

eval_ps <- function(rsl,eval_rs) {
  pa_rsl <- rsl; pa_rsl[which(rsl[]>0)] <- 1 #bin the prediction
  
  met <- xyFromCell(eval_rs,1:ncell(eval_rs))
  met <- cbind(met,PRE=extract(pa_rsl,met))
  met <- cbind(met,OBS=extract(eval_rs,1:ncell(eval_rs))); met <- as.data.frame(met)
  met <- met[which(!is.na(met$PRE)),]; met <- met[which(!is.na(met$OBS)),] #get rid of NAs
  
  #get the values *1 is observed and 2 is prediction
  ntp <- length(which(met$PRE > 0 & met$OBS == 1))
  tpr <- ntp/length(which(met$OBS == 1))
  #false negative rate
  nfp <- length(which(met$PRE == 0 & met$OBS == 1))
  fpr <- nfp/length(which(met$OBS == 1))
  #true negative rate (if absences are available)
  if (length(which(met$OBS == 0)) != 0) {
  	ntn <- length(which(met$PRE > 0 & met$OBS == 0))
  	tnr <- ntn / length(which(met$OBS == 0))
  } else {tnr <- NA}
  
  rm(met); g=gc(); rm(g)
  met.final <- data.frame(TPR=tpr, FPR=fpr, TNR=tnr)
  return(met.final)
}


