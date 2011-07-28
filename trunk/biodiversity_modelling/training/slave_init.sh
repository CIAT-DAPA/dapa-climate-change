#!/bin/bash

# Script is automatically excecuted on each slave with worker nodes

# create tmp dir structure

mkdir -p tnc/results
mkdir -p tnc/src/lib
mkdir -p tnc/src/scripts
mkdir -p tnc/logs

# get src files
scp flora:tnc/src/lib/maxent/maxent.zip tnc/src/lib
unzip tnc/src/lib/maxent.zip

scp flora:tnc/src/lib/scripts/train_slave.sh tnc/src/scripts
