#!/bin/bash

#arguments. Only the mega-environment number (1-10)
ME=$1

#host name
THOST="arc2"

#go to robustness directory
cd ~/quest-for-robustness/scratch/runfiles

#number of items in queue, and total number of possible processes
NPROC=$(qstat -u earjr | grep run.sh | wc -l)
NPROCMAX=50

#loop the iterations
for ITER in {1..10}
do
	for INUM in {1..47}
	do
		#determine MAXJNUM
		if [ ! -f ~/quest-for-robustness/scratch/maxjnum/maxjnum_${ITER}_${INUM}.txt ]
		then
			if [ ! -f maxjnum.R ]
			then
				cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/glam-optimise_me_arc2_jnum.R maxjnum.R
			fi
			R CMD BATCH --vanilla --slave "--args iter=$ITER i=$INUM" maxjnum.R /dev/tty
		fi
		MAXJNUM=$(< ~/quest-for-robustness/scratch/maxjnum/maxjnum_${ITER}_${INUM}.txt)
		
		echo 
		echo --------- ${MAXJNUM} JNUM processes to submit ------------
		echo 
		
		for JNUM in $(seq 1 $MAXJNUM)
		do
			#process name
			TPID=${ME}_${ITER}_${INUM}_${JNUM}
			
			#copy the script first
			if [ ! -f run.sh ]
			then
				cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/glam-optimise_me_arc2_run.sh run.sh
				chmod 755 run.sh
			fi
			
  			#do the model run only if procfile does not exist
  			if [ ! -f ~/quest-for-robustness/scratch/procfiles/out_${ME}_${ITER}_${INUM}_${JNUM}.proc ]
  			then
  				echo ----------------------------------------------------------
				echo ---- submitting ${TPID} -----
				echo ---- ${NPROC} processess queueing currently -----------
				echo ----------------------------------------------------------
	  			qsub run.sh $ME $ITER $INUM $JNUM
	  		else
  				echo ----------------------------------------------------------
				echo ---- ${TPID} need not be submitted -----
				echo ---- ${NPROC} processess queueing currently -----------
				echo ----------------------------------------------------------
	  		fi
  			
  			#count number of submitted processes. Never let the queue be above NPROCMAX
  			#this avoids submitting too many jobs simultaneously (max is 170 JNUM anyway)
  			NPROC=$(qstat -u earjr | grep run.sh | wc -l)
  			while [ ${NPROC} -ge ${NPROCMAX} ]
  			do
  				echo Idle process.
  				NPROC=$(qstat -u earjr | grep run.sh | wc -l)
  				sleep 60
  			done
  		done
  		
  		#one all processes are submitted need to check when they will all be completed,
  		#when so then need to run the collate script
  		NPROC=$(qstat -u earjr | grep run.sh | wc -l)
  		while [ ${NPROC} -gt 0 ]
  		do
  			echo Idle process. Currently at ${NPROC}
  			NPROC=$(qstat -u earjr | grep run.sh | wc -l)
  			sleep 60
  		done
  		
  		#count number of .proc files, if below MAXJNUM then dont collate
  		NPROCFIL=$(ls -l ~/quest-for-robustness/scratch/procfiles/out_${ME}_${ITER}_${INUM}_*.proc | grep .proc | wc -l)
  		if [ ${NPROCFIL} -eq ${MAXJNUM} ]
  		then
	  		#now run the collate script from the driver
  			cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/glam-optimise_me_arc2_collate.R collate.R
			
			#run R in batch for desired stuff; /dev/tty will display output in screen
			R CMD BATCH --vanilla --slave "--args me_i=$ME iter=$ITER i=$INUM" collate.R /dev/tty
		else
			echo Error. Some submitted jobs have failed
			exit 1
		fi
  	done
done

