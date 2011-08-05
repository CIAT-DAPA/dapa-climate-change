#!/bin/bash

# Script is automatically excecuted on each slave with worker nodes

# create tmp dir structure

mkdir -p tnc_tmp/results
mkdir -p tnc_tmp/src/lib/maxent
mkdir -p tnc_tmp/src/scripts/proj
mkdir -p tnc_tmp/data/env
mkdir -p tnc_tmp/logs

# get src files
scp flora:tnc/src/lib/maxent/maxent.zip tnc_tmp/src/lib/maxent
unzip -d tnc_tmp/src/lib/maxent tnc_tmp/src/lib/maxent/maxent.zip

scp flora:tnc/src/scripts/proj/proj_slave.sh tnc_tmp/src/scripts/proj

scp -r flora:tnc/data/env/proj tnc_tmp/data/env

for i in $(ls tnc_tmp/data/env/proj/*.zip)

do
  dir=$(echo $i | cut -f1 -d.)
  unzip -d $dir $i
  rm $i
done
  

# Rename grids
for i in $(ls tnc_tmp/data/env/proj/*/A*.asc)
do
  mv $i $(echo $i | sed 's/A[B12]\{1,2\}_20[24]0_mean_bio_\([0-9]\{1,2\}\)/bio\1/')
done
