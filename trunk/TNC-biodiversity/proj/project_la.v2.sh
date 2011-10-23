#!/bin/bash

# the bash script variables.sh needs to be modified
# $1 containts the name of SRES,YEAR and GCM
# $2 is the number of cores to start
#models are: c_2000_current | a2_2050_ensemble | a1b_2050_ensemble

MODEL=$1
MAX_NPROC=$2
#HOST=$3
#RES=$4
QUEUE=""

##################
# load variabiles

if [ -f variables.sh ]
then
  echo "loading variables ..."
  . variables.sh
else
  echo "can not find variables"
  exit
fi


# Table name
TABLE=lam_$RES\_$MODEL

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
	id=$1		    # id of species that is being processed
	model=$2	  # name of model (i.e. SRES_year_gcm)
	results=$3	# directory where all results are stored
	maxent=$4	  # directory where maxent is located
	max=$5		  # max ram available per maxent instance
	envdata=$6	# directory were the environmental data is located
	host=$7     # server where mysql database is (i.e. flora.ciat.cgiar.org)
	res=$8      #resolution of projected grids (i.e. 5k)
	table=$9    #table in mysql database that stores the results
	
	# shortcut
	base=$results/$table
	
	# get part number from database
	#part=$(mysql --skip-column-names -umodel1 -pmaxent -h$host -e"use tnc;SELECT part FROM species WHERE species_id=$id")

	# update statuts as started
	mysql --skip-column-names -umodel1 -pmaxent -h$host -e"use tnc;UPDATE $table SET started=NOW() where species_id=$id;"
	nf=$(echo ${id:0:4})
	
	#make nf directory if it does not exist
	if [ ! -d "$results/$table/$nf" ]
	then
	  echo "mk prefix dir..."
	  mkdir -p $results/$table/$nf
	fi
	
	# make a directory for this species for this run
	mkdir $base/$nf/$id

	# only run maxent if a lambda file exists
	if [ -f $results/model1/lambdas/$nf/$id.lambdas ]
	then
		# run maxent
		java -mx${max}m -cp $maxent/maxent.jar density.Project $results/model1/lambdas/$nf/$id.lambdas $envdata/$model $base/$nf/$id/$model -a visible=false writeclampgrid=false warnings=false
	
		# rescale the reuslts
		gdal_translate -scale 0 1 1 255 -ot byte -a_nodata 0 -q -co "COMPRESS=lzw" $base/$nf/$id/$model.asc $base/$nf/$id.tif

		# clean up
		rm -r $base/$nf/$id

		# update statuts as finished
	
		mysql --skip-column-names -umodel1 -pmaxent -h$host -e"use tnc; UPDATE $table SET finished=NOW() where species_id=$id;"
	else
		mysql --skip-column-names -umodel1 -pmaxent -h$host -e"use tnc; UPDATE $table SET exit_status='no lambda file' where species_id=$id;"
	fi
	
}


#Multi process
if [ -f "$ENVDATA_SRV/$MODEL.zip" ]
then
	# only unzip env data if its not already done
	echo "checking env data .... "
	if [ ! -d "$ENVDATA_HOST/$MODEL" ]
	then
	  echo "cp env var..."
	  cp -v $ENVDATA_SRV/$MODEL.zip $ENVDATA_HOST/$MODEL.zip
	  unzip $ENVDATA_HOST/$MODEL.zip -d $ENVDATA_HOST/$MODEL
	fi
	
	# rename grids if they are not renamed already
	if [ ! -f "$ENVDATA_HOST/$MODEL/bio1.asc" ]
	then
		echo "renaming and cropping to la"
		for i in $(ls $ENVDATA_HOST/$MODEL)
		do
			gdal_translate -of 'AAIGrid' -projwin -118 35 -33 -60 $ENVDATA_HOST/$MODEL/$i $ENVDATA_HOST/$MODEL/c$i
			mv $ENVDATA_HOST/$MODEL/c$i $ENVDATA_HOST/$MODEL/$(echo $i | sed 's/_//')
			rm $ENVDATA_HOST/$MODEL/$i
		done
	fi
	
	# create the table this shoul only be done if the table doesnt not exists yet
	echo "checking database .... "
	test=$(mysql --skip-column-names -umodel1 -pmaxent -h$HOST -e"use tnc; show tables;" | grep ^$TABLE)

	if [ -z "$test" ]
	then
	  echo "creating table ...."
		mysql --skip-column-names -umodel1 -pmaxent -h$HOST -e"use tnc; \
		create table $TABLE \
		(species_id int, started timestamp null default null, finished timestamp null default null, exit_status varchar(32));\
                insert into $TABLE (species_id) select species_id from species where la=1;"
	fi
	
	# mkdir for the models
	mkdir -p $RESULTS_HOST/$TABLE
	
	# for each part make a folder
	#for i in $(mysql --skip-column-names -umodel1 -pmaxent -h $HOST -e"use tnc; select distinct(part) from species;")
	#do
	#	mkdir $RESULTS_HOST/$TABLE/part.$i
	#done

	# run modells for all species
	echo "starting with maxent"

	# get first ID
	ID=$(mysql --skip-column-names -umodel1 -pmaxent -h$HOST -e"use tnc; select species_id from $TABLE where started is null limit 1;")
	NF=$(echo ${ID:0:4})
	
	# run first species, so that java cached rasters are created 
	runmodel $ID $MODEL $RESULTS_HOST $MAXENT $MAXRAM $ENVDATA_HOST $HOST $RES $TABLE
	
	# get second ID
	ID=$(mysql --skip-column-names -umodel1 -pmaxent -h $HOST -e"use tnc; select species_id from $TABLE where started is null limit 1;")
	
	COUNT=1
	while [ -n "$ID" ]
	do
		echo "sending for species $ID"
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
		
		# stopping for first test, uncomment for testing
		COUNT=$(echo $COUNT+1 | bc)
		#if [ $COUNT -eq 10 ]
		#then
		#	echo "first test done"
		#	return 0
		#fi
		
	done
	
	#remove env data dir
	#rm -r $ENVDATA/$MODEL
else
  echo "no env data found"
  exit
fi




