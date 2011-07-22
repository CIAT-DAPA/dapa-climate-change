#!/bin/bash

#Check the range of the imported rasters and write that into a file for later reading and checking in R or Excel

DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/cross-validation/verification
if [ ! -d $IN_PATH ] 
then
	mkdir -p $IN_PATH
fi

for part in {1..10}
do
	g.mapset mapset=wcl_uncertainties_$part
	for fold in {1..10}
	do
		for var in rain tmin tmax tean
		do
			VERFILE=$IN_PATH/$var\_log.txt
			for month in {1..12}
			do
				#Variable definitions
				RAST=$var\_p$part\_f$fold\_m$month
				
				echo
				echo "Processing month $month fold $fold of part $part of variable $var"
				
				eval `g.findfile element=cell file=$RAST`
				if [ $file ]
				then
					VALUES=$(r.describe -rn map=$RAST)
					VALUES=$( echo $VALUES | awk '{gsub("thru","",$0);print}')
					VALUES=$( echo $VALUES | awk '{gsub(" ",",",$0);print}')
					ROW=$(echo $RAST,$VALUES)
					echo $ROW >> $VERFILE
				fi
			done
				
			#Calculating annual mean and total
			echo "Calculating annual average or sum"
			ANN_RAST=$var\_p$part\_f$fold\_ann
			#Do it if the GRASS raster file does not exist
			eval `g.findfile element=cell file=$ANN_RAST`
			if [ $file ]
			then
				VALUES=$(r.describe -rn map=$ANN_RAST)
				VALUES=$( echo $VALUES | awk '{gsub("thru","",$0);print}')
				VALUES=$( echo $VALUES | awk '{gsub(" ",",",$0);print}')
				ROW=$(echo $ANN_RAST,$VALUES)
				echo $ROW >> $VERFILE
			fi
		done
	done
done
