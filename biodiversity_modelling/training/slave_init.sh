#!/bin/bash

# Script is automatically excecuted on each slave with worker nodes

# create tmp dir structure

mkdir -p tnc_tmp/results
mkdir -p tnc_tmp/src/lib/maxent
mkdir -p tnc_tmp/src/scripts/training
mkdir -p tnc_tmp/logs

# get src files
scp flora:tnc/src/lib/maxent/maxent.zip tnc_tmp/src/lib/maxent
unzip -d tnc_tmp/src/lib/maxent tnc/src/lib/maxent/maxent.zip

scp flora:tnc/src/scripts/training/train_slave.sh tnc_tmp/src/scripts/training
