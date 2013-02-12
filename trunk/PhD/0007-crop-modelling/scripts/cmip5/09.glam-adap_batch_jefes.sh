#!/bin/bash

LIM_A=$1
LIM_B=$2
GCM_ID=$3
SCR_ID=$4

#get process id based on name of screen
PID=`screen -list | grep ${SCR_ID} | cut -f1 -d'.' | sed 's/\W//g'`

#make processing directory if it doesnt exist
if [ ! -d "~/workspace/cmip5_adap/process_${HOST}_${PID}" ]
then
	mkdir ~/workspace/cmip5_adap/process_${HOST}_${PID}
fi

cd ~/workspace/cmip5_adap/process_${HOST}_${PID}

#remove run script if it exists
if [ -f "run.R" ]
then
	rm -vf run.R
fi

#copy run file from local svn repo
cp -vf ~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts/cmip5/09.glam-adap_batch_jefes.R run.R

#run R in batch for desired stuff
R CMD BATCH --vanilla --slave "--args lim_a=$LIM_A lim_b=$LIM_B gcm_id=$GCM_ID" run.R /dev/tty

#remove processing directory again
cd ~/workspace/cmip5_adap
rm -rvf process_${HOST}_${PID}

