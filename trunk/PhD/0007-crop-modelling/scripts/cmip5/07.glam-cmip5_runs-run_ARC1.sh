#!/bin/bash

#$ -l h_rt=5:00:00
#$ -l h_vmem=5G
#$ -l cputype=*
#$ -cwd -V
#$ -m be

G_INI=$1
G_END=$2

cd ~/PhD-work/_tools/dapa-climate-change/trunk/PhD/0007-crop-modelling/scripts/cmip5/
R CMD BATCH --slave --no-save "--args g_ini=$G_INI g_end=$G_END" 07.glam-cmip5_runs-run_ARC1.R ~/workspace/run.Rout
