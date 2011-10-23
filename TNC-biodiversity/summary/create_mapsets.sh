#!/bin/bash

# This will create individual mapsets based on a template, for all the folders
# in which projections are organised. These mapsets are further used when calling
# the script batch_current.v2.sh

#list files in folder
#$1 (first argument in command line) is c_2000_current | a2_2050_ensemble | a1b_2050_ensemble

MODEL=lam_5k_$1
BASE=/data1/TNC/results
DATA_PATH=$BASE/$MODEL
GISDB_PATH=$BASE/grass

#if path exists then copy $MODEL as a GRASS location
if [ ! -d "$DATA_PATH" ]
then
	echo "Data path is not valid"
	return 0
else
	echo "Copying template location"
	cp -r $GISDB_PATH/template $GISDB_PATH/$MODEL
fi


#loop through folders and create them (copy from the base mapset)
for i in $(ls $DATA_PATH)
do
	if [ ! -d "$GISDB_PATH/s$i" ]
	then
		echo "Creating mapset s$i"
		cp -r $GISDB_PATH/$MODEL/tmp $GISDB_PATH/$MODEL/s$i
	fi
done



