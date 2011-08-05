#!/bin/bash

# script to calculate convex hulls for all species with 10 or more points.
# Usage sh chull.sh 10
# to start 10 instances at the same time

MAX_NPROC=$1
QUEUE=""

# Table name
TABLE=la_$RES\_$MODEL

# function to run procces in parallel
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

# function to make a chull
function chull {
id=$1
  psql -U model1 -d gisdb -c "UPDATE ConvexHulls SET HasChull=1,geom=(SELECT ST_SetSRID(ST_AsText(ST_ConvexHull(ST_Collect(geom))),4326) from Points where SpeciesID='$id') where SpeciesID='$id'"
}

##################
# load variabiles
# get first ID
ID=$(psql -U model1 -d gisdb -t -c "Select SpeciesID from ConvexHulls where HasChull IS NULL and NumberOfPoints > 9 ORDER BY random() limit 1")
	
while [ -n "$ID" ]
do
	# run maxent $i &
	chull $ID &

	PID=$!
	queue $PID

	while [ $NUM -ge $MAX_NPROC ]; do
		checkqueue
		sleep 0.4
	done
		
	sleep 0.1
	ID=$(psql -U model1 -d gisdb -t -c "Select SpeciesID from ConvexHulls where HasChull IS NULL and NumberOfPoints > 9 ORDER BY random() limit 1")
done





