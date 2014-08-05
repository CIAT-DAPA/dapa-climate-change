#!/bin/bash

#$ -l h_rt=01:30:00
#$ -l h_vmem=2G
#$ -cwd -V

ME=$1
ITER=$2
INUM=$3
JNUM=$4

#get process id based on name of screen
PID=${ME}_${ITER}_${INUM}_${JNUM}
THOST="arc2"

#define crop model and number of parameters MAXINUM
CSMODEL=MZCER045

cd ~/quest-for-robustness/scratch/runfiles

#copy run script if it does not exist
if [ ! -f "run_dssat.R" ]
then
  cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/dssat-optimise_me_arc2_run.R run_dssat.R
fi

#run R in batch for desired stuff
TMPDIR=~/workspace/tmp
R CMD BATCH --vanilla --slave "--args csmodel='$CSMODEL' me_i=$ME iter=$ITER i=$INUM j=$JNUM" run_dssat.R ~/quest-for-robustness/scratch/outfiles/dssat_${CSMODEL}_out_${THOST}_${PID}.out

#remove junk
rm -f ~/quest-for-robustness/scratch/outfiles/dssat_${CSMODEL}_out_${THOST}_${PID}.out

