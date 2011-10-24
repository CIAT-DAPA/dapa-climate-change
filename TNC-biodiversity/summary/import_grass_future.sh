#!/bin/bash
##########################################################################
# In one location
# provide name of the run as [region]_[res]_[sres]_[year]_[gcm]
# run in the folder of the run!
# 
# 1. Create a location for each run
# 2. Create a mapset for each 4digits code of species. One with original croped maps and one with thresholded
# 3. Symlink the current to the future so we can calcualte turnovers etc. 
##########################################################################

# Start GRASS shell up at location and mapset

####
# Batch wrapper

#list files in folder
#$1 (first argument in command line) is c_2000_current | a2_2050_ensemble | a1b_2050_ensemble

function runbatch_future {
  folder=$1
  location=$2
  export GRASS_BATCH_JOB=/data1/TNC/results/$location/$folder/batch_future.v2.rw.sh
  /usr/bin/grass64 /data1/TNC/results/grass/$location/s$folder
  unset GRASS_BATCH_JOB
}

function controlBatch {

  NLOCATION=$1
  PROC_FOLDER=$2

  # go to the new location
  cd /data1/TNC/results/$NLOCATION

  # run processes current
  runbatch_future $PROC_FOLDER $NLOCATION
}

while read folder 
do
  echo "doing folder $folder"
  MODEL=lam_5k_$1
  cp /data1/TNC/src/summary/batch_future.v2.sh /data1/TNC/results/$MODEL/$folder/batch_future.v2.rw.sh
  chmod 777 /data1/TNC/results/$MODEL/$folder/batch_future.v2.rw.sh
  
  controlBatch $MODEL $folder
  rm /data1/TNC/results/$MODEL/$folder/batch_future.v2.rw.sh
done




