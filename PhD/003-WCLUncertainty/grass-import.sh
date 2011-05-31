#!/bin/bash

#$1 is the part (there's a mapset for each part, each mapset would contain 10-folds of each of the 12 monthly rasters

if [ ! $1 ] || [ ! $2 ]
then
	exit
fi

part=$1
TMP_PATH=$2

#Drive and input paths
DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/cross-validation

#Temporary path where data is copied before import
if [ ! -d $TMP_PATH ] 
then
	mkdir -p $TMP_PATH
fi

g.mapset -c mapset=wcl_uncertainties_$part

#for part in 1 #{1..10}
#do
	for fold in {1..10}
	do
		for var in rain tmin tmax tean
		do
			for month in {1..12}
			do
				#Variable definitions
				IN_FILE=$IN_PATH/$var/part-$part/fold-$fold/merged/$var\_$month.zip
				ST_FILE=$IN_PATH/$var/part-$part/fold-$fold/merged/status.m.$month.merge
				OUT_FILE=$TMP_PATH/$var\_$month\.zip
				OUT_ASC=$TMP_PATH/$var\_$month\.asc
				OUT_RAST=$var\_p$part\_f$fold\_m$month
				
				#Do if the status file exists (i.e. the merge is done)
				echo
				echo "Processing month $month fold $fold of part $part of variable $var"
				
				#Checking the database
				STATUS=$(mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; SELECT merge_fin FROM wclun WHERE part=$part AND fold=$fold AND month=$month AND variable='$var';")
				
				if [ ${#STATUS} != 4 ]
				then
					echo "The merged file exists!"
					
					 #Do if the dataset does not exist
					eval `g.findfile element=cell file=$OUT_RAST`
					if [ ! $file ]
					then
						#Update the status of grassin_fin in the database to started
						mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; UPDATE wclun SET grassin_start=NOW() WHERE part=$part AND fold=$fold AND month=$month AND variable='$var';"
						
						echo "The GRASS dataset does not exist, so making it"
						#If the temporal stuff exists then delete
						echo "Cleaning temporary files if necessary"
						if [ -f $OUT_FILE ]
						then
							rm $(ls $TMP_PATH/*)
						fi
						
						#Copying to temporal
						echo "Copying..."
						cp $IN_FILE $OUT_FILE
						
						#Local unzip into the temporal folder
						echo "Unzipping..."
						unzip $OUT_FILE -d $TMP_PATH/
						
						#Import into GRASS
						echo "GRASS import..."
						r.in.gdal -o in=$OUT_ASC out=$OUT_RAST
						
						echo "Cleaning temporals..."
						rm $(ls $TMP_PATH/*)
						
						#Update the status of grassin_start in the database to started
						mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; UPDATE wclun SET grassin_fin=NOW() WHERE part=$part AND fold=$fold AND month=$month AND variable='$var';"
					else
						echo "The GRASS dataset already exists"
					fi
					
					#Creating a list for totalling/averaging over a year
					if [ $month == 1 ]
					then
						monthList=$OUT_RAST
					else
						monthList=$monthList,$OUT_RAST
					fi
				fi
			done
				
			#Calculating annual mean and total
			echo "Calculating annual average or sum"
			OUT_ANN=$var\_p$part\_f$fold\_ann
			#Do it if the GRASS raster file does not exist
			eval `g.findfile element=cell file=$OUT_ANN`
			if [ ! $file ]
			then
				#Checking if all the monthly GRASS files do exist
				if [ ${#monthList} -ge 170 ]
				then
					#Update database on total (calc_start) to started
					mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; UPDATE wclun_ttl SET calc_start=NOW() WHERE part=$part AND fold=$fold AND variable='$var';"
					
					if [ $var == rain ]
					echo "Annual means or totals..."
					then
						r.series in=$monthList out=$OUT_ANN method=sum
					else
						r.series in=$monthList out=$OUT_ANN method=average
					fi
					
					mysql --skip-column-names -ujramirez -pramirez2009 -e"USE dapaproc; UPDATE wclun_ttl SET calc_fin=NOW() WHERE part=$part AND fold=$fold AND variable='$var';"
				else
					echo "Number of months is not 12, so cannot calculate annual mean"
				fi
			fi
		done
	done
#done
