#!/bin/bash

#MAX_NPROC=10
#SRC_PS=/home/jramirez/dapa-climate-change/PhD/0005-dqs/scripts
#BDIR=/andromeda_data1/jramirez/dqs/EcoCrop-GNUT
#SRC_EC=/home/jramirez/dapa-climate-change/EcoCrop
#p,seasonal,prec,0,1065

MAX_NPROC=$1
SRC_PS=$2
BDIR=$3
SRC_EC=$4
QUEUE=""

IFS=,

##################
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
	SRC_DIR_PS=$1	# dir where sources are located
	BD=$2	  		# base directory
	SRC_DIR=$3		# dir where EcoCrop.R source file is located
	TYPE=$4	  		# type of run (p/s)
	VAR=$5		  	# variable (prec/tmean/tmin)
	SCALE=$6		# scale of run (seasonal/spatial)
	SEED=$7     	# random seed
	PVAL=$8      	# perturbed value (can be NA)
	
	# shortcut
	Rscript --vanilla --no-save $SRC_DIR_PS/EcoCrop-PSBatchRun.R $BD $SRC_DIR_PS $SRC_DIR $TYPE $VAR $SCALE $SEED $PVAL
}


COUNT=1
while read TY SC VA P S
do
	echo "sending for $TY / $SC / $VA / $P / $S "
	# run maxent $i &
	runmodel $SRC_PS $BDIR $SRC_EC $TY $VA $SC $S $P &
	
	PID=$!
	queue $PID
	
	while [ $NUM -ge $MAX_NPROC ]; do
		checkqueue
		sleep 0.4
	done
	
	sleep 0.1
	
	# stopping for first test, uncomment for testing
	COUNT=$(echo $COUNT+1 | bc)
	if [ $COUNT -eq 10 ]
	then
		echo "first test done"
		return 0
	fi
done


