#!/bin/bash

# script to train maxent model on slave node

# 1. Species id
id=$1

# 2. mkdir and get files needed
mkdir -p tnc/results/$id
scp flora:tnc/data/species_swd/${id:0:4}/$id/$id.zip tnc/results/$id

unzip tnc/results/$id
java -mx1024m -jar tnc/src/lib/maxent.jar nowarnings outputdirectory=tnc/results/$id samplesfile=tnc/results/$id.swd environmentallayers=$base/bg.swd -a -z nopictures -P plots=false replicates=10
