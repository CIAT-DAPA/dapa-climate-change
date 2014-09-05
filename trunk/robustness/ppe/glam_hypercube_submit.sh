#!/bin/bash

#arguments. Only the mega-environment number (1-10)
ME=$1

#host name
THOST="arc1"

#go to robustness directory
cd ~/quest-for-robustness/scratch/runfiles

#number of items in queue, and total number of possible processes
NPROC=$(qstat -u earjr | grep run.sh | wc -l)
NPROCMAX=100

#determine and loop locations
if [ ! -f ~/quest-for-robustness/scratch/maxjnum/maxjnum_${ITER}_${INUM}.txt ]
then
	if [ ! -f maxjnum.R ]
	then
		cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/ppe/glam_hypercube_iloc.R maxiloc.R
	fi
	R CMD BATCH --vanilla --slave "--args me_i=$ME" maxiloc.R /dev/tty
fi
MAXILOC=$(< ~/quest-for-robustness/scratch/maxiloc/maxiloc_${ME}.txt)

echo 
echo --------- ${MAXILOC} locations to submit ------------
echo 

for IRUN in {1..10000}
do
	for ILOC in $(seq 1 $MAXILOC)
	do
		#process name
		TPID=${ME}_${IRUN}_${ILOC}
		
		#copy the script first
		if [ ! -f run.sh ]
		then
			cp -vf ~/Repositories/dapa-climate-change/trunk/robustness/ppe/glam_hypercube_run.sh run.sh
			chmod 755 run.sh
		fi
		
		#create out folder if it does not exist
		if [ ! -d ~/quest-for-robustness/scratch/procfiles/out_${ME}_${IRUN} ]
		then
			$(mkdir ~/quest-for-robustness/scratch/procfiles/out_${ME}_${IRUN})
		fi
		
		#do the model run only if procfile does not exist
		if [ ! -f ~/quest-for-robustness/scratch/procfiles/out_${ME}_${IRUN}/out_${ME}_${IRUN}_${ILOC}.proc ]
		then
			echo ----------------------------------------------------------
			echo ---- submitting ${TPID} -----
			echo ---- ${NPROC} processess queueing currently -----------
			echo ----------------------------------------------------------
			qsub run.sh $ME $IRUN $ILOC
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
	NPROCFIL=$(ls -l ~/quest-for-robustness/scratch/procfiles/out_${ME}_${IRUN}/out_${ME}_${IRUN}_*.proc | grep .proc | wc -l)
	if [ ${NPROCFIL} -eq ${MAXILOC} ]
	then
		#remove runfiles
		rm -f ~/quest-for-robustness/scratch/runfiles/run.sh.e*
		rm -f ~/quest-for-robustness/scratch/runfiles/run.sh.o*
	else
		echo Error. Some submitted jobs have failed
		exit 1
	fi
done

