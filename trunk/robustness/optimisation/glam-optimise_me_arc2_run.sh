#!/bin/bash

#$ -l h_rt=04:00:00
#$ -l h_vmem=1G
#$ -cwd -V

ME=$1
ITER=$2
INUM=$3
JNUM=$4

#get process id based on name of screen
PID=${ME}_${ITER}_${INUM}_${JNUM}
THOST="arc2"

cd ~/quest-for-robustness/scratch/runfiles

#copy run script if it does not exist
if [ ! -f "run.R" ]
then
	cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/glam-optimise_me_arc2_run.R run.R
fi

#run R in batch for desired stuff
R CMD BATCH --vanilla --slave "--args me_i=$ME iter=$ITER i=$INUM j=$JNUM" run.R ~/quest-for-robustness/scratch/outfiles/out_${THOST}_${PID}.out

#remove junk
rm -f ~/quest-for-robustness/scratch/outfiles/out_${THOST}_${PID}.out

