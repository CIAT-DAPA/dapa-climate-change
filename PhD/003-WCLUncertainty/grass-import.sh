#!/bin/bash

#$1 is the part (there's a mapset for each part, each mapset would contain 10-folds of each of the 12 monthly rasters

part=$1
DRIVE=/mnt/GIS-HD717
IN_PATH=$DRIVE/CCAFS/climate-data-assessment/wcl-uncertainties/outputs/cross-validation
TMP_PATH=/data1/jramirez/tmp

g.mapset -c mapset=wcl_uncertainties_$part

#for part in 1 #{1..10}
	#do
		for fold in {1..10}
			do
				for var in rain tmin tmax tmean
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
								if [ -f $ST_FILE ]
								then
									echo "The status file exists!"
									 #Do if the dataset does not exist
									eval `g.findfile element=cell file=$OUT_RAST`
									if [ ! $file ]
									then
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
								if [ ${#monthList} == 170 ]
								then
									if [ $var == rain ]
									echo "Annual means or totals..."
									then
										r.series in=$monthList out=$OUT_ANN method=sum
									else
										r.series in=$monthList out=$OUT_ANN method=average
									fi
								else
									echo "Number of months is not 12, so cannot calculate annual mean"
								fi
							fi
					done
			done
#	done
