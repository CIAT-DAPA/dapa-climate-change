# ----------------------------------------------------------------------------------------------
# Descrition : Une en un solo txt varios archivos de texto
# Author : Carlos Navarro
# Date: 3/23/2012
#------------------------------------------------------------------------------------------------

import os, sys, glob

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python MergeTextFiles.py <dirbase>"
	print "   - ie: python MergeTextFiles.py E:\Worldclim_interpolations\outputs_mexico"
	sys.exit(1)

dirbase = sys.argv[1]

variablelist = "rain", "tmax", "tmin"
for variable in variablelist:
	dirvar = dirbase + "\\" + variable
	
	folds = os.listdir(dirvar)
	for fold in folds:
		file = fold + "\\tile-1\\rain_metrics.csv"
		data = open(file)
		out = open(dirbase + "\\metrics_" + variable + ".csv", 'a')
		for l in data:
		  print(l, file=out)
		data.close()
		out.close()