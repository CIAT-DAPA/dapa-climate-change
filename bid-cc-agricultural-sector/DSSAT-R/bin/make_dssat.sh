#!/bin/bash
#Arguments. These must be either ifort or gfortran
FCOMP=${1}

#base directory in SEE NFS
BASE_DIR=$(pwd)

#compile as instructed
if [ ${FCOMP} == "ifort" ]
then
	cd ${BASE_DIR}/csm45_1_23_src_ifort
	
	#clean up and re-make
	make clean && make
	
	#delete previous compiled program as needed
	cd ${BASE_DIR}/csm45_1_23_bin_ifort
	if [ -f DSCSM045.EXE ]
	then
		rm -f DSCSM045.EXE
	fi
	
	#copy back and set permissions
	cp -rf ./../csm45_1_23_src_ifort/csm45 DSCSM045.EXE
	chmod 755 DSCSM045.EXE
elif [ ${FCOMP} == "gfort" ]
then
	cd ${BASE_DIR}/csm45_1_23_src_ifort
	
	#clean up and re-make
	make clean && make
	
	#delete previous compiled program as needed
	cd ${BASE_DIR}/csm45_1_23_bin_ifort
	if [ -f DSCSM045.EXE ]; 
	then
		rm -f DSCSM045.EXE
	fi
	
	#copy back and set permissions to execution
	cp -rf ./../csm45_1_23_src_ifort/csm45 DSCSM045.EXE
	chmod 755 DSCSM045.EXE
else
	echo 'only ifort or gfort options allowed'
fi

cd ${BASE_DIR}
