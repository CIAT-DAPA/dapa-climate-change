#!/bin/bash

#arguments. These must be actual GCM_ENS, and actual GLAM ensemble member
GCM=$1
EXP=$2

#host name
THOST="arc1"

#base directory in SEE NFS
BASE_DIR=/nfs/a17/eejarv/PhD-work/crop-modelling/GLAM/model-runs/GNUT

#update the svn code
#cd ~/Repositories
#svn up dapa-climate-change
cd ~/workspace

#number of items in queue, and total number of possible processes
NPROC=$(qstat -u eejarv | grep run.sh | wc -l)
NPROCMAX=65

#loop the grid cells
for LOC in 291 324 326 327 328 358 359 360 361 363 364 392 393 394 399 400 401 402 428 429 430 431 432 433 434 435 436 437 464 465 466 467 468 471 472 499 500 501 502 503 504 505 529 530 531 532 533 534 535 536 537 538 539 564 565 566 567 568 569 570 571 572 573 574 578 579 600 601 602 603 604 605 606 607 608 609 610 613 614 615 635 636 637 638 639 640 641 642 643 644 645 647 648 649 650 651 652 653 671 672 673 674 675 676 677 678 679 680 682 683 684 685 686 687 709 710 711 712 713 714 715 718 719 720 721 744 745 746 747 748 749 750 751 753 754 755 779 780 781 782 783 784 785 786 787 788 789 814 815 816 817 818 819 820 821 822 823 850 851 852 853 854 855 856 885 886 887 888 889 890 891 920 921 922 923 924 925 926 957 958 959 960 961 992 993 994 995 1027 1028 1029 1030 1063 1064 1065 1098
do
	#process name
	TPID=${GCM}_${EXP}_${LOC}
	
	#count existing files
	NFILK=$(ls -1 ~/workspace/cmip5_adap/_process/exp-${EXP}_${GCM} | grep RCP_loc-${LOC} | wc -l)
	echo ---- ${TPID} has ${NFILK} .proc files
	
	if [ ${NFILK} -ne 16 ]
	then
		echo ----------------------------------------------------------
		echo ----------------------------------------------------------
		echo ---- submitting ${TPID} -----
		echo ---- ${NPROC} processess queueing currently -----------
		echo ----------------------------------------------------------
		echo ----------------------------------------------------------
		
		#make local copy directory if it doesnt exist
		if [ ! -d "~/workspace/localcopy/copy_${THOST}_${TPID}" ]
		then
			mkdir ~/workspace/localcopy/copy_${THOST}_${TPID}
		fi
		cd ~/workspace/localcopy/copy_${THOST}_${TPID}
		
		#copy the base data
		scp see-gw-01:${BASE_DIR}/adapt/data/arc1_data.RData .
		
		#create directory for output files
		if [ ! -d "./cmip5_all" ]
		then
			mkdir ./cmip5_all
		fi
		
		#copy the output files from server
		for RUNTYPE in rcp_allin rcp_bcrain rcp_sh rcp_del
		do
			for TCO in {1..4}
			do
				scp -r see-gw-01:${BASE_DIR}/runs/cmip5_all/exp-${EXP}_outputs/${GCM}/${RUNTYPE}_CO2_p${TCO}_${LOC} ./cmip5_all
			done
		done
		
		#create directory for weather files
		if [ ! -d "./weather" ]
		then
			mkdir ./weather
		fi
		
		#copy weather files
		for WTYPE in cmip5_rcp45 cmip5_rcp45_bc cmip5_rcp45_del cmip5_rcp45_sh
		do
			if [ ! -d "./weather/${WTYPE}" ]
			then
				mkdir ./weather/${WTYPE}
			fi
			scp -r see-gw-01:${BASE_DIR}/inputs/ascii/wth-${WTYPE}/${GCM}/rfd_${LOC} ./weather/${WTYPE}/.
			scp -r see-gw-01:${BASE_DIR}/inputs/ascii/wth-${WTYPE}/${GCM}/irr_${LOC} ./weather/${WTYPE}/.
		done
		
		#copy soil and sowing date files
		if [ ! -d "./other" ]
		then
			mkdir ./other
		fi
		scp see-gw-01:${BASE_DIR}/inputs/ascii/sow/sowing_${LOC}_irr.txt ./other/.
		scp see-gw-01:${BASE_DIR}/calib/exp-${EXP}_outputs/gridcells/fcal_${LOC}/opt_fcal_${LOC}.txt ./other/.
		scp see-gw-01:${BASE_DIR}/inputs/ascii/soil/soiltypes_${LOC}.txt ./other/.
		scp see-gw-01:${BASE_DIR}/inputs/ascii/soil/soilcodes_${LOC}.txt ./other/.
		
		#go to processing directory
		cd ~/workspace/cmip5_adap
		
		#copy the script first
		if [ ! -f "./run.sh" ]
		then
			cp -vf ~/Repositories/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts/cmip5/09.glam-adap_batch_ARC1.sh run.sh
			chmod 755 run.sh
		fi
		
  		#do the model run
  		qsub run.sh $LOC $GCM $EXP
  		
  		#count number of submitted processes. Never let the queue be above NPROCMAX
  		NPROC=$(qstat -u eejarv | grep run.sh | wc -l)
  		while [ ${NPROC} -ge ${NPROCMAX} ]
  		do
  			echo Idle process.
  			NPROC=$(qstat -u eejarv | grep run.sh | wc -l)
  			sleep 60
  		done
  	else
  		echo ---- No need to submit ${TPID}
  	fi
done

