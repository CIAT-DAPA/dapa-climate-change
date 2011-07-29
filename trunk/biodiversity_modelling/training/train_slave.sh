#!/bin/bash

# script to train maxent model on slave node

# 1. Species id
id=$1
TMP_FROM_SLAVE=$2

# 2. mkdir and get files needed
mkdir -p tnc_tmp/results/$id
scp flora:tnc/data/species/species_swd/${id:0:4}/$id/$id.zip tnc_tmp/results/$id

unzip -d tnc_tmp/results/$id tnc_tmp/results/$id/$id.zip

rm tnc_tmp/results/$id/$id.zip

java -mx1024m -jar tnc_tmp/src/lib/maxent/maxent.jar nowarnings outputdirectory=tnc_tmp/results/$id samplesfile=tnc_tmp/results/$id/$id.swd environmentallayers=tnc_tmp/results/$id/background.swd -a -z nopictures plots=false replicates=10

zip -j tnc_tmp/results/$id/$id.zip tnc_tmp/results/$id/*.lambdas tnc_tmp/results/$id/maxentResults.csv

ssh flora mkdir -p tnc/$RUNID/${id:0:4}/
scp tnc_tmp/results/$id/$id.zip flora:tnc/$RUNID/${id:0:4}/

rm -r tnc_tmp/results/$id

