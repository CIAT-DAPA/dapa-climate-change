#!/bin/env python

import os, sys, glob

if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\00-scripts\04_marksim\04-MarksimBatch.py X:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\datos_diarios\_dat_files D:\cenavarro\ms D:\cenavarro\ms\tmp 1 1 baseline"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
tmp_dir = sys.argv[3]
start = sys.argv[4]
end = sys.argv[5]
rcp = sys.argv[6]

if not os.path.exists(tmp_dir):
	os.system('mkdir ' + tmp_dir)
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# script to run marksim
script = os.getcwd() + "\\runing_interpolations.py"

# where marksim located
marksim = os.getcwd() + "\\lib\\marksim.zip"

# where the results will be copied to
# rcplist = sorted(os.listdir(dirbase))
# for rcp in rcplist:
	
foldlist = sorted(os.walk(dirbase + "\\" + rcp).next()[1])
for fold in foldlist[int(start)-1:int(end)]:

	if not os.path.exists(dirout + "\\" + fold + "\\baseline_" + fold + ".zip"): 

		print "Compressing " + rcp + " " + fold
		# Compress .dat files
		
		inZip = dirbase + "\\" + rcp + "\\" + fold + ".zip"
		if not os.path.exists(inZip):
			
			os.system("7za a -mmt=12 " + inZip + " " + dirbase + "\\" + rcp + "\\" + fold + "\\*.*")
			# os.remove(dirbase + "\\" + rcp + "\\" + fold)

		print "Running MS for " + rcp + " " + fold
		# Run MarkSim routine
		diroutFold = dirout + "\\" + fold
		os.system("python " + script + " " + inZip + " " + diroutFold + " " + marksim + " " + tmp_dir + " " + fold)

		