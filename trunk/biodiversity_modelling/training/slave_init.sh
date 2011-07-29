#!/bin/bash

# Script is automatically excecuted on each slave with worker nodes

# create tmp dir structure

mkdir -p tnc/results
mkdir -p tnc/src/lib/maxent
mkdir -p tnc/src/scripts/training
mkdir -p tnc/logs

# get src files
scp flora:tnc/src/lib/maxent/maxent.zip tnc/src/lib/maxent
unzip -d tnc/src/lib/maxent tnc/src/lib/maxent/maxent.zip

scp flora:tnc/src/scripts/training/train_slave.sh tnc/src/scripts/training
