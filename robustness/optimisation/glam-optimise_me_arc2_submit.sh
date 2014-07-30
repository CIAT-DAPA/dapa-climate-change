#!/bin/bash

#arguments. Only the mega-environment number (1-10)
ME=$1

#host name
THOST="arc2"

#go to robustness directory
cd ~/quest-for-robustness

#number of items in queue, and total number of possible processes
NPROC=$(qstat -u earjr | grep run.sh | wc -l)
NPROCMAX=17

#loop the iterations
for ITER in {1..10}
do
	for INUM in {1..47}
	do
		for JNUM in {1..17}
		do
			#process name
			TPID=${ME}_${ITER}_${INUM}_${JNUM}
			
			echo ----------------------------------------------------------
			echo ----------------------------------------------------------
			echo ---- submitting ${TPID} -----
			echo ---- ${NPROC} processess queueing currently -----------
			echo ----------------------------------------------------------
			echo ----------------------------------------------------------
			
			#copy the script first
			if [ ! -f "./run.sh" ]
			then
				cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/glam-optimise_me_arc2_run.sh run.sh
				chmod 755 run.sh
			fi
			
  			#do the model run
  			qsub run.sh $ME $ITER $INUM $JNUM
  			
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
  		NPROC=$(qstat -u eejarv | grep run.sh | wc -l)
  		while [ ${NPROC} -gt 0 ]
  		do
  			echo Idle process.
  			NPROC=$(qstat -u earjr | grep run.sh | wc -l)
  			sleep 60
  		done
  		
  		#now run the collate script from the driver
  		cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/optimisation/glam-optimise_me_arc2_collate.R collate.R
		
		#run R in batch for desired stuff; /dev/tty will display output in screen
		R CMD BATCH --vanilla --slave "--args me_i=$ME iter='$ITER' i=$INUM" collate.R /dev/tty
  	done
done

