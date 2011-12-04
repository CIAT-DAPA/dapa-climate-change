#!/bin/bash

#This script will loop through the perturbed and shuffled folders to produce daily files used the pre-compiled FORTRAN program
#dailyGen. The process will be done in parallel
#
#Julian Ramirez-Villegas / UoL / CIAT / CCAFS / Nov 2011

MAX_NPROC=$1
QUEUE=""

#These are the parallelsing functions
function queue {
	QUEUE="$QUEUE $1"
	NUM=$(($NUM+1))
}

function regeneratequeue {
	OLDREQUEUE=$QUEUE
	QUEUE=""
	NUM=0
	for PID in $OLDREQUEUE
	do
		if [ -d /proc/$PID  ] ; then
			QUEUE="$QUEUE $PID"
			NUM=$(($NUM+1))
		fi
	done
}

function checkqueue {
	OLDCHQUEUE=$QUEUE
	for PID in $OLDCHQUEUE
	do
		if [ ! -d /proc/$PID ] ; then
			regeneratequeue # at least one PID has finished
			break
		fi
	done
}

function createFiles {
	
	# Arguments that were passed to the function
	folder=$1	 # Folder where the process will be done
	experiment=$2      #experiment in question
	inner_fol=$3

	#./dailyGen /media/DATA/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather/shuf-pert/p_prec_climate/p-0_s-12/
	./dailyGen $folder/$experiment/$inner_fol/
}

#This is the processing bit
DATA_DIR=/media/DATA/CIAT_work/GLAM/PNAS-paper/DSSAT-PNUT/GJ-weather/shuf-pert
PROG_DIR=/home/jramirez/work/DSSAT-PNUT/dailyGen

cd $DATA_DIR
for INFOL in s_prec_all s_prec_wyear s_prec_years s_tmax_all s_tmax_wyear s_tmax_years s_tmin_all s_tmin_wyear s_tmin_years
do
	IN_DIR=$DATA_DIR/$INFOL
	cd $IN_DIR
	for FFOL in *
	do
		cd $PROG_DIR
		createFiles $DATA_DIR $INFOL $FFOL &
		echo "DONE:" $DATA_DIR / $INFOL / $FFOL
		PID=$!
		queue $PID
		
		while [ $NUM -ge $MAX_NPROC ]; do
			checkqueue
			sleep 0.4
		done
		
		sleep 0.1
	done
done

