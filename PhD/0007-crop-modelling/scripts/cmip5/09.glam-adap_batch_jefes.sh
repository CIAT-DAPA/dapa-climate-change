#!/bin/bash

LIM_A=$1
LIM_B=$2
GCM_ID=$3
SCR_ID=$4

PID=`screen -list | grep $SCR_ID | cut -f1 -d'.' | sed 's/\W//g'`
mkdir ~/workspace/process_$PID
cd ~/workspace/

R CMD BATCH --vanilla --slave "--args lim_a=$LIM_A lim_b=$LIM_B gcm_id=$GCM_ID" 09.glam-adap_batch_jefes.R /dev/null
