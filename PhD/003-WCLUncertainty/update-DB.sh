#!/bin/bash

#Input paths
DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/cross-validation

for part in {1..10}
do
	for fold in {1..10}
	do
		for var in rain tmin tmax tean
		do
			for month in {1..12}
			do
				echo "Checking part $part fold $fold month $month for $var"
				
				ST_FILE=$IN_PATH/$var/part-$part/fold-$fold/merged/status.m.$month.merge
				
				if [ -f $ST_FILE ]
				then
					echo "The status file exists, updating the database!"
					mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; UPDATE wclun SET merge_start=NOW() WHERE part=$part AND fold=$fold AND month=$month AND variable='$var';"
					mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; UPDATE wclun SET merge_fin=NOW() WHERE part=$part AND fold=$fold AND month=$month AND variable='$var';"
				fi
			done
		done
	done
done
