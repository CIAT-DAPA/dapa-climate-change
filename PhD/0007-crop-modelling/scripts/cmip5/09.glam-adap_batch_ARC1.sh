#!/bin/bash

#$ -l h_rt=00:30:00
#$ -l h_vmem=1.5G
#$ -l cputype=intel
#$ -cwd -V
#$ -m be

LIM_A=$1
LIM_B=$2
GCM_ID=$3
#SCR_ID=$4

#get process id based on name of screen
#PID=`screen -list | grep ${SCR_ID} | cut -f1 -d'.' | sed 's/\W//g'`
PID=${LIM_A}_${LIM_B}_${GCM_ID}
THOST="arc1"

#make processing directory if it doesnt exist
if [ ! -d "~/workspace/cmip5_adap/process_${THOST}_${PID}" ]
then
	mkdir ~/workspace/cmip5_adap/process_${THOST}_${PID}
fi

cd ~/workspace/cmip5_adap/process_${THOST}_${PID}

#remove run script if it exists
if [ -f "run.R" ]
then
	rm -vf run.R
fi

#copy run file from local svn repo
cp -vf ~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts/cmip5/09.glam-adap_batch_ARC1.R run.R

#run R in batch for desired stuff
R CMD BATCH --vanilla --slave "--args lim_a=$LIM_A lim_b=$LIM_B gcm_id=$GCM_ID" run.R /dev/tty

#remove processing directory again
cd ~/workspace/cmip5_adap
rm -rvf process_${THOST}_${PID}

