#!/bin/bash

#$1 = part
#$2 = variable
#$3 = month (or can be ann for annual mean/total), must be m1..12, or ann

if [ ! $1 ] || [ ! $2 ]
then
	exit
fi

var=$1
month=$2

#Drive and input paths
DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/cross-validation

g.mapset -c mapset=totals

for part in {1..10}
do
	for fold in {1..10}
	do
		#Do if the status file exists (i.e. the merge is done)
		echo
		echo "Processing month $month fold $fold of part $part of variable $var"
		
		#Variable definitions, and database check
		RAST=$var\_p$part\_f$fold\_$month@wcl_uncertainties_$part
		monthDB=$(echo $month | sed 's\m\\g')
		
		if [ $month != ann ]
		then
			STATUS=$(mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; SELECT merge_fin FROM wclun WHERE part=$part AND fold=$fold AND month=$monthDB AND variable='$var';")
		else
			STATUS=$(mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; SELECT calc_fin FROM wclun_ttl WHERE part=$part AND fold=$fold AND variable='$var';")
		fi
		
		if [ ${#STATUS} != 4 ]
		then
			echo "The file exists!"
			
			#Do if the dataset exists
			eval `g.findfile element=cell file=$RAST`
			if [ $file ]
			then
				#Create a list for mean and std of the 10 rasters of a certain fold
				
				if [ $fold == 1 ] & [ $part == 1 ]
				then
					rList=$RAST
				else
					rList=$rList,$RAST
				fi
				
			else
				echo "The GRASS dataset does not exist"
				exit
			fi
		fi
	done
done

#Calculating annual mean and total
echo "Calculating total and std"
OUT_MEAN=$var\_$month\_mean
OUT_STD=$var\_$month\_std
#Do it if the GRASS raster file does not exist
eval `g.findfile element=cell file=$OUT_STD`
if [ ! $file ]
then
	#Checking if all the monthly GRASS files do exist
	if [ ${#rList} -ge 1300 ]
	then
		echo "Average and STD"
		r.series in=$rList out=$OUT_MEAN method=average
		r.series in=$rList out=$OUT_STD method=stddev
	else
		echo "Number of folds is not 10, so cannot calculate avg and std"
	fi
fi
