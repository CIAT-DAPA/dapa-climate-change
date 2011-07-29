#!/bin/bash

# script to train maxent model on slave node

# 1. Species id
id=$1
TMP_FROM_SLAVE=$2

# 2. mkdir and get files needed
mkdir -p tnc/results/$id
scp flora:tnc/data/species/species_swd/${id:0:4}/$id/$id.zip tnc/results/$id

unzip -d tnc/results/$id tnc/results/$id/$id.zip

rm tnc/results/$id/$id.zip

java -mx1024m -jar tnc/src/lib/maxent/maxent.jar nowarnings outputdirectory=tnc/results/$id samplesfile=tnc/results/$id/$id.swd environmentallayers=tnc/results/$id/background.swd -a -z nopictures plots=false replicates=10

zip -j tnc/results/$id/$id.zip tnc/results/$id/*.lambdas tnc/results/$id/maxentResults.csv

ssh flora mkdir -p tnc/$RUNID/${id:0:4}/
scp tnc/results/$id/$id.zip flora:tnc/$RUNID/${id:0:4}/

rm -r tnc/results/$id

