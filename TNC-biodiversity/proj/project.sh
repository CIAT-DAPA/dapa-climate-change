#!/bin/bash

# $1 containts the name of SRES,YEAR and GCM
# $2 is the number of cores to start

MODEL=$1
MAX_NPROC=$2
HOST=$3
QUEUE=""

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
	model=$2	  # name of the model (i.e. SRES_year_gcm)
	results=$3	# directory where all results are stored
	maxent=$4	  # directory where maxent is located
	max=$5		  # max ram available per maxent instance
	envdata=$6	# directory were the environmental data is located
	host=$6     # mysql server
	
	# get part number from database
	part=$(mysql --skip-column-names -umodel1 -pmaxent -h$host -e"use tnc;SELECT part FROM species WHERE species_id=$id")

	# update statuts as startedo
	mysql --skip-column-names -umodel1 -pmaxent -e"use tnc;UPDATE $model SET started=NOW() where species_id=$id;"
	
	# make a directory for this species for this run
	mkdir $results/$model/part.$part/$id

	# only run maxent if a lambda file exists
	if [ -f lambdas/part.$part/$id.lambdas ]
	then
		# run maxent
		java -mx${max}m -cp $maxent/maxent.jar density.Project lambdas/part.$part/$id.lambdas $envdata/$model $results/$model/part.$part/$id/$model -a visible=false writeclampgrid=false warnings=false 
	
		# rescale the reuslts
		gdal_translate -scale 0 1 1 255 -ot byte -a_nodata 0 -q -co "COMPRESSION=lzw" $results/$model/part.$part/$id/$model.asc $results/$model/part.$part/$model.tif

		# clean up
		rm -r $results/$model/part.$part/$id

		# update statuts as finished
	
		mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; UPDATE $model SET finished=NOW() where species_id=$id;"
	else
		mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; UPDATE $model SET exit_status='no lambda file' where species_id=$id;"
	fi
	
}

##################
# load variabiles

if [ -f variables.sh ]
then
  . variables.sh
else
  echo "can not find variables"
  exit
fi

if [ -f "$ENVDATA/$MODEL.zip" ]
then
	# only unzip env data if its not already done
	echo "checking evn data .... "
	if [ ! -d "$ENVDATA/$MODEL" ]
	then
		unzip $ENVDATA/$MODEL.zip -d $ENVDATA/$MODEL
		
		# rename grids
		for i in $(ls $ENVDATA/$MODEL)
		do
			mv $ENVDATA/$MODEL/$i $ENVDATA/$MODEL/$(echo $i | sed 's/_//')
		done
	fi
	
	# create the table this shoul only be done if the table doesnt not exists yet
	echo "checking database .... "
	test=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; show tables;" | grep $MODEL)

	if [ -z "$test" ]
	then
		mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; \
		create table $MODEL \
		(species_id int, started timestamp null default null, finished timestamp null default null, exit_status varchar(32));\
                insert into $MODEL (species_id) select species_id from species;"
	fi
	
	# mkdir for the models
	mkdir $RESULTS/$MODEL
	
	# for each part make a folder
	for i in $(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; select distinct(part) from species;")
	do
		mkdir $RESULTS/$MODEL/part.$i
	done

	# run modells for all species
	echo "starting with maxent"

	# get first ID
	ID=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; select species_id from $MODEL where started is null limit 1;")
	
	# run first species, so that java cached rasters are created 
	runmodel $ID $MODEL $RESULTS $MAXENT $MAXRAM $ENVDATA $HOST
	
	# get second ID
	ID=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; select species_id from $MODEL where started is null limit 1;")

	while [ -n "$ID" ]
	do
		# run maxent $i &
		runmodel $ID $MODEL $RESULTS $MAXENT $MAXRAM $ENVDATA $HOST &

		PID=$!
		queue $PID

		while [ $NUM -ge $MAX_NPROC ]; do
			checkqueue
			sleep 0.4
		done
		
		sleep 0.1
		ID=$(mysql --skip-column-names -umodel1 -pmaxent -e"use tnc; select species_id from $MODEL where started is null limit 1;")

	done
	
	rm -r $ENVDATA/$MODEL
else
  echo "no env data found"
  exit
fi




