#!/bin/bash

INDIR=/media/DATA/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather/shuf-pert

for INFOL in p_yield_climate p_yield_season
do
	IN_DIR=$INDIR/$INFOL
	cd $IN_DIR
	for INNER in *
	do
		WTH_DIR=$IN_DIR/$INNER/WTH
		echo "process: " $WTH_DIR
		if [ -d $WTH_DIR ]
		then
			echo "removing: " $WTH_DIR
			rm -r $WTH_DIR
		fi
	done
	cd $INDIR
done
