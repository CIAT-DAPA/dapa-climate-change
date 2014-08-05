#!/bin/bash

#arguments. Only the mega-environment number (1-10)
ME=$1

#host name
THOST="arc2"

#go to robustness directory
cd ~/quest-for-robustness/scratch/runfiles

#number of items in queue, and total number of possible processes
NPROC=$(qstat -u earjr | grep run_cscer | wc -l)
NPROCMAX=50

#define crop model and number of parameters MAXINUM
CSMODEL=MZCER045
MAXINUM=16

#loop the iterations
for ITER in {1..10}
do
  for INUM in $(seq 1 $MAXINUM)
	do
		#determine MAXJNUM
		if [ ! -f ~/quest-for-robustness/scratch/maxjnum/dssat_${CSMODEL}_maxjnum_${ITER}_${INUM}.txt ]
		then
			if [ ! -f maxjnum_dssat.R ]
			then
				cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/dssat-optimise_me_arc2_jnum.R maxjnum_dssat.R
			fi
			R CMD BATCH --vanilla --slave "--args csmodel='$CSMODEL' iter=$ITER i=$INUM" maxjnum_dssat.R /dev/tty
		fi
		MAXJNUM=$(< ~/quest-for-robustness/scratch/maxjnum/dssat_${CSMODEL}_maxjnum_${ITER}_${INUM}.txt)
		
		echo 
		echo --------- ${MAXJNUM} JNUM processes to submit ------------
		echo 
		
		for JNUM in $(seq 1 $MAXJNUM)
		do
			#process name
			TPID=${ME}_${ITER}_${INUM}_${JNUM}
			
			#copy the script first
			if [ ! -f run_cscer.sh ]
			then
				cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/dssat-cscer_optimise_me_arc2_run.sh run_cscer.sh
				chmod 755 run_cscer.sh
			fi
			
  			#do the model run only if procfile does not exist
  			if [ ! -f ~/quest-for-robustness/scratch/procfiles/dssat_${CSMODEL}_out_${ME}_${ITER}_${INUM}_${JNUM}.proc ]
  			then
  				echo ----------------------------------------------------------
				echo ---- submitting ${TPID} -----
				echo ---- ${NPROC} processess queueing currently -----------
				echo ----------------------------------------------------------
	  			qsub run_cscer.sh $ME $ITER $INUM $JNUM
	  		else
  				echo ----------------------------------------------------------
				echo ---- ${TPID} need not be submitted -----
				echo ---- ${NPROC} processess queueing currently -----------
				echo ----------------------------------------------------------
	  		fi
  			
  			#count number of submitted processes. Never let the queue be above NPROCMAX
  			#this avoids submitting too many jobs simultaneously
  			NPROC=$(qstat -u earjr | grep run_cscer | wc -l)
  			while [ ${NPROC} -ge ${NPROCMAX} ]
  			do
  				echo Idle process.
  				NPROC=$(qstat -u earjr | grep run_cscer | wc -l)
  				sleep 60
  			done
  		done
  		
  		#one all processes are submitted need to check when they will all be completed,
  		#when so then need to run the collate script
  		NPROC=$(qstat -u earjr | grep run_cscer | wc -l)
  		while [ ${NPROC} -gt 0 ]
  		do
  			echo Idle process. Currently at ${NPROC}
  			NPROC=$(qstat -u earjr | grep run_cscer | wc -l)
  			sleep 60
  		done
  		
  		#count number of .proc files, if below MAXJNUM then dont collate
  		NPROCFIL=$(ls -l ~/quest-for-robustness/scratch/procfiles/dssat_${CSMODEL}_out_${ME}_${ITER}_${INUM}_*.proc | grep .proc | wc -l)
  		if [ ${NPROCFIL} -eq ${MAXJNUM} ]
  		then
	  		#now run the collate script from the driver
  			cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/dssat-optimise_me_arc2_collate.R collate_dssat.R
			
			#run R in batch for desired stuff; /dev/tty will display output in screen
			R CMD BATCH --vanilla --slave "--args csmodel='$CSMODEL' me_i=$ME iter=$ITER i=$INUM" collate_dssat.R /dev/tty
		else
			echo Error. Some submitted jobs have failed
			exit 1
		fi
  	done
done

