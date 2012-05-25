# ----------------------------------------------------------------------------------------------
# Descrition : Une en un solo txt varios archivos de texto
# Author : Carlos Navarro
# Date: 3/23/2012
#------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

os.system('cls')

if len(sys.argv) < 2:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Join_Field.py <dirbase> <inityear> <finalyear> <outdir>"
	print "   - ie: python JoinField.py D:\Workspace\request_miguel\points_extract\_extract_WC"
	sys.exit(1)

dirbase = sys.argv[1]

# variablelist = "rain", "tmax", "tmin"
# for variable in variablelist:
	# dirvar = dirbase + "\\" + variable
	
	# folds = os.listdir(dirvar)
	# for fold in folds:
		# file = fold + "\\tile-1\\rain_metrics.csv"
		# data = open(file)
		# out = open(dirbase + "\\metrics_" + variable + ".csv", 'a')
		# for l in data:
		  # print(l, file=out)
		# data.close()
		# out.close()
		
dbfList = sorted(glob.glob(dirbase + "\\*.dbf"))
for dbf in dbfList:
	InData = dirbase + "\\" + "bio_1.dbf"
	if not os.path.basename(dbf)[-9:] == "bio_1.dbf":
		fields = os.path.basename(dbf)[:-4].split("_")[-2:]
		gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
		print dbf + " joined"