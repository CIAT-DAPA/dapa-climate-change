#!/bin/env python

import os, sys

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python 04-MarksimBatch.py U:\ALPACAS\Plan_Regional_de_Cambio_Climatico_Orinoquia\01-datos_clima\datos_diarios\baseline D:\CIAT\ms D:\CIAT\ms\tmp"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
tmp_dir = sys.argv[3]
if not os.path.exists(tmp_dir):
	os.system('mkdir ' + tmp_dir)
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

# script to run marksim
script = os.getcwd() + "\\runing_interpolations.py"

# where marksim located
marksim = os.getcwd() + "\\lib\\marksim.zip"

# where the results will be copied to
rcplist = sorted(os.listdir(dirbase))
for rcp in rcplist:
	
	foldlist = sorted(os.listfiles(dirbase + "\\" + rcp))
	for fold in foldlist:
	
		# Compress .dat files
		dsList = glob.glob(dirbase + "\\" + rcp + "\\" + fold + "\\*")
		inZip = dirbase + "\\" + rcp + "\\" + fold + ".zip"
		for ds in dsList:
			os.system("7za a -mmt=12 " + inZip + " " + ds)
		os.remove(fl)

		# Run MarkSim routine
		diroutFold = dirout + "\\" + fold
		os.system("python " + script + " " + inZip + " " + diroutFold + " " + marksim + " " + tmp_dir)


# os.system("python " + script + " " + dirbase + "\\baseline.zip " + dirout + " " + marksim + " " + tmp_dir)

# # Future 
# yearlist = sorted(os.listdir(dirbase))
# for year in yearlist:
	# gcmlist = sorted(os.listdir(dirbase + "\\" + year))
	# for gcm in gcmlist:
		# os.system("python " + script + " " + dirbase + "\\" + str(year) + "\\" + gcm[:-4] + ".zip " + dirout + " " + marksim + " " + tmp_dir)
