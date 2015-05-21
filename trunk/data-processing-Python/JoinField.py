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
	print "   - ie: python JoinField.py DD:\Documents\Desktop\reproj_soils_gaza"
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
		
shpList = sorted(glob.glob(dirbase + "\\*.shp"))
for shp in shpList:
	InData = dirbase + "\\" + "classes.dbf"
	# fields = os.path.basename(dbf)[:-4].split("_")[-2:]
	gp.joinfield (shp, "LAND SUITA", InData, "Suitabilit", "Value")
	print shp + " joined"