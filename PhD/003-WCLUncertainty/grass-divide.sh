#!/bin/bash

DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/cross-validation


g.mapset -c mapset=wcl_uncertainties_$part

for part in {1..10}
do
	for fold in {1..10}
	do
		for var in tmin tmax tean
		do
			
			#Calculating annual mean and total
			echo "Calculating annual average or sum"
			OUT_ANN=$var\_p$part\_f$fold\_ann
			
			#Now divide the file
			r.mapcalc "temp=$OUT_ANN/12"
			
			#Erase original file
			g.remove rast=$OUT_ANN
			
			#Now rename temporal file back to normal
			g.rename rast=temp,$OUT_ANN
			
		done
	done
done
