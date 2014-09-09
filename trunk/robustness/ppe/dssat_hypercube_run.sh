#!/bin/bash

#$ -l h_rt=01:30:00
#$ -l h_vmem=2G
#$ -cwd -V

ME=$1
IRUN=$2
ILOC=$3
CSMODEL=$4

#get process id based on name of screen
PID=${ME}_${IRUN}_${ILOC}
THOST="arc1"

cd ~/quest-for-robustness/scratch/runfiles

#copy run script if it does not exist
if [ ! -f "run_dssat.R" ]
then
	cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/ppe/dssat_hypercube_run.R run_dssat.R
fi

#run R in batch for desired stuff
#TMPDIR=~/workspace/tmp
R CMD BATCH --vanilla --slave "--args csmodel='$CSMODEL' me_i=$ME loc_i=$ILOC hrun_i=$IRUN" run_dssat.R ~/quest-for-robustness/scratch/outfiles/dssat_${CSMODEL}_out_${THOST}_${PID}.out

#remove junk
rm -f ~/quest-for-robustness/scratch/outfiles/dssat_${CSMODEL}_out_${THOST}_${PID}.out

