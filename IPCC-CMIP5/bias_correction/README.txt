###########################################################
# Readme Bias Correction of daily CMIP5 data		  #
# CCAFS-Climate < www.ccafs-climate.org >		  #
# Author : Jaime Tarapues, Carlos Navarro, Julián Ramírez #
# Date   : Jun 2016					  #
# Contact: j.e.tarapues@cgiar.org			  #
###########################################################

0. Description

Daily GCM data calibrated using observations (Reanalysis) and bias correction approaches: 'delta' (change factor), 'nudging' (bias correction) and quantile mapping. These types of methods could be more widely adopted for assessing calibration methodologies for crop modelling. 


1. Abbreviations used for methods 

OBS: Observation data
GCM: Global Climate Model data
TS: Time serie
SH: Bias Correction approach excluding variability
BC: Bias Correction approach including variability
DEL: Change Factor approach excluding variability
CF: Change Factor approach including variability
QM: Quantile mappong approach including variability


2. Variable units 

LABEL 	VARIABLE 			UNITS			
prec 	precipitation 			milimeters	
tmean	daily mean temperature 		celsius degrees
tmin	daily minimun temperature 	celsius degrees
tmax	daily maxmium temperature 	celsius degrees
srad	solar radiation			megajoule per square metre 


3. Files structure

- Levels
	\.\bc_<lon>_<lat>.zip\<method>\<bc file>.tab
	\.\bc_<lon>_<lat>.zip\stats\<chart type>\<image file>.tif

- BC Filename
	<method abb>_ts_<scenario>_<lon>_<lat>.tab

- Columns BC file
	date		obs	gcm1	gcm2	...
	YYYY-MM-DD	value	value	value	...

	(Missing  values are given as NA)
	

4. Main References of Methods 

1) Hawkins, E., Osborne, T. M., Ho, C. K., & Challinor, A. J. (2013). Calibration and bias correction of climate projections for crop modelling: An idealised case study over Europe. Agricultural and Forest Meteorology, 170(0), 19-31. http://doi.org/http://dx.doi.org/10.1016/j.agrformet.2012.04.007
2) Gudmundsson L; Bremnes JB; Haugen JE; Engen-Skaugen T. (2012). Technical Note: Downscaling RCM precipitation to the station scale using statistical transformations - a comparison of methods.  Hydrology and Earth System Sciences 16: 3383???3390. doi: 10.5194/hess-16-3383-2012.