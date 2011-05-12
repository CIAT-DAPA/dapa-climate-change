#!/bin/bash

# $1 containts the name of SRES,YEAR and GCM
# $2 is the number of cores to start

MAX_NPROC=$1
HOST=$2


# Table name
TABLE=SecondTraining

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

# function to run a model and update its status to the database
function runmodel {
	
 # Arguments that were passed to the function
 id=$1		    # id of the species that is being processed
 results=$(echo ${id:0:4}) 
 maxent="lib/maxent/"
 max=2048
 host=$2     # mysql server
 table=$3
	
 # shortcut
 base="/mnt/GIS-HD716/TNC_global_plants/results/training/$results/$id"

 # update statuts as startedo
 mysql --skip-column-names -umodel1 -pmaxent -h$host -e"use tnc;UPDATE $table SET started=NOW() where species_id=$id;"
	
 # make a directory for this species for this run
 mkdir $base/training
 # only run maxent if a lambda file exists

 # run maxent
 
 java -mx${max}m -jar $maxent/maxent.jar nowarnings outputdirectory=$base/training samplesfile=$base/sp.swd environmentallayers=$base/bg.swd -a -z nopictures -P plots=false
   

 mysql --skip-column-names -umodel1 -pmaxent -h$host -e"use tnc; UPDATE $table SET finished=NOW() where species_id=$id;"
	
}

##################
# load variabiles

echo "creating table ...."
mysql --skip-column-names -umodel1 -pmaxent -h$HOST -e"use tnc; \
create table $TABLE \
(species_id int, started timestamp null default null, finished timestamp null default null, exit_status varchar(32));\
 nsert into $TABLE (species_id) select species_id from species where la=1 and where size < 10;"



# run modells for all species
echo "starting with maxent"

# get first ID
ID=$(mysql --skip-column-names -umodel1 -pmaxent -h$HOST -e"use tnc; select species_id from $TABLE where started is null limit 1;")
	
# run first species, so that java cached rasters are created 
runmodel $ID $HOST $TABLE
	
# get second ID
ID=$(mysql --skip-column-names -umodel1 -pmaxent -h $HOST -e"use tnc; select species_id from $TABLE where started is null limit 1;")

while [ -n "$ID" ]
do
 # run maxent $i &
 runmodel $ID $MODEL $RESULTS_HOST $MAXENT $MAXRAM $ENVDATA_HOST $HOST $RES $TABLE &

 PID=$!
 queue $PID

 while [ $NUM -ge $MAX_NPROC ]; do
  checkqueue
  sleep 0.4
 done
		
 sleep 0.1
 ID=$(mysql --skip-column-names -umodel1 -h$HOST -pmaxent -e"use tnc; select species_id from $TABLE where started is null limit 1;")

done
	

