#!/bin/env python

import os
import sys

# set Pathes
# script to run marksim
script = "L:\\MarkSimGCM\\datamanagement\\src\\scripts\\1_runing_interpolations.py"
# where marksim located
marksim = "L:\\MarkSimGCM\\datamanagement\\src\\lib\\marksim.zip"
# where the results will be copied to
out_file = "L:\\MarkSimGCM\\results"

# read params
try:
  f = open(sys.argv[1], "r")
except:
  print "count not open list of files"
  sys.exit(1)

# run marskim
for file in f:
  (year, gcm) = file.split(",")
  year = year.rstrip()
  gcm = gcm.rstrip()
  os.system("python L:\\MarkSimGCM\datamanagement\\src\\scripts\\1_runing_interpolations.py L:\\MarkSimGCM\\MarkSim_data\\" + year + "\\" + gcm + ".zip " + out_file + " L:\\MarkSimGCM\\datamanagement\\src\\lib\\marksim.zip ")
