#!/bin/bash

DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/cross-validation

for part in {1..10}
do
	g.mapset mapset=wcl_uncertainties_$part
	for fold in {1..10}
	do
		for var in tmin tmax tean
		do
			for month in {1..12}
			do
				RAST=$var\_p$part\_f$fold\_m$month
				
				if [ $month == 1 ]
				then
					monthList=$RAST
				else
					monthList=$monthList,$RAST
				fi
			done
				#Calculating annual mean and total
				echo "Calculating annual average of part $part fold $fold variable $var and month $month"
				OUT_ANN=$var\_p$part\_f$fold\_ann
				#Erase original file
				eval `g.findfile element=cell file=$OUT_ANN`
				if [ $file ]
				then
					g.remove rast=$OUT_ANN
				fi
				
				r.series in=$monthList out=$OUT_ANN method=average
			
		done
	done
done
