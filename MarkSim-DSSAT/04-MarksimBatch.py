#!/bin/env python

import os, sys

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: "
	print "   - python 04-MarksimBatch.py D:\CIAT\Workspace\jperez\MS D:\CIAT\Workspace\jperez\MS D:\CIAT\Workspace\jperez\tmp"
	sys.exit(1)

dirbase = sys.argv[1]
dirout = sys.argv[2]
tmp_dir = sys.argv[3]
if not os.path.exists(tmp_dir):
	os.system('mkdir ' + tmp_dir)
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
	

# set Pathes

# script to run marksim
script = os.getcwd() + "\\runing_interpolations.py"

# where marksim located
marksim = os.getcwd() + "\\lib\\marksim.zip"

# where the results will be copied to
yearlist = sorted(os.listdir(dirbase))
for year in yearlist:
	gcmlist = sorted(os.listdir(dirbase + "\\" + year))
	for gcm in gcmlist:
		os.system("python " + script + " " + dirbase + "\\" + str(year) + "\\" + gcm[:-4] + ".zip " + dirout + " " + marksim + " " + tmp_dir)
