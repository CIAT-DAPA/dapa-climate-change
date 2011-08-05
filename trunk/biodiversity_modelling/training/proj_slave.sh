#!/bin/bash

# script to train maxent model on slave node

# 1. Species id
modelprojectid=$1
env=$2
spid=$3
runmodelid=$4

# 2. mkdir and get files needed
mkdir -p tnc_tmp/results/proj/$spid
scp flora:tnc/results/$runmodelid/${spid:0:4}/$spid.zip tnc_tmp/results/proj/$spid

unzip -d tnc_tmp/results/proj/$spid tnc_tmp/results/proj/$spid/$spid.zip

rm tnc_tmp/results/proj/$spid/$spid.zip

if ! java -mx1024m -cp tnc_tmp/src/lib/maxent/maxent.jar density.Project tnc_tmp/results/proj/$spid/${spid}_0.lambdas tnc_tmp/data/env/proj/$env tnc_tmp/results/proj/$modelprojectid -a visible=false writeclampgrid=false warnings=false
then
  ssh flora "echo 1 > tnc/tmp/${modelprojectid}.txt"
  exit
fi
        
# rescale the reuslts
if ! gdal_translate -scale 0 1 1 255 -ot byte -a_nodata 0 -q -co "COMPRESS=lzw" tnc_tmp/results/proj/$modelprojectid.asc tnc_tmp/results/proj/$modelprojectid.tif
then 
  ssh flora "echo 2 > tnc/tmp/${modelprojectid}.txt"
  exit
fi

# compress again to save an other 10%
if ! gzip tnc_tmp/results/proj/$modelprojectid.tif
then
  ssh flora "echo 3 > tnc/tmp/${modelprojectid}.txt"
  exit
fi

if ! scp tnc_tmp/results/proj/$modelprojectid.tif.gz flora:tnc/results/proj/
then
  ssh flora "echo 4 > tnc/tmp/${modelprojectid}.txt"
  exit
fi

ssh flora "echo 0 > tnc/tmp/${modelprojectid}.txt"

rm -r tnc_tmp/results/proj/$spid
rm -r tnc_tmp/results/proj/${modelprojectid}*

