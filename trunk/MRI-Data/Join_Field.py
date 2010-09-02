# ----------------------------------------------------------------------------------------------
# Descrition : Une en una sola database (.dbf) los datos extraídos por meses de los valores MRI,
# El output es un shape anual con doce campos correspondientes a cada
# uno de los meses, con nombre "var_YYYY_MM".
# Author : Carlos Navarro
#------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 3:
	os.system('cls')
	print "\n Too few args"
	print "   - Sintaxis: python Join_Field.py <dirbase> <inityear> <finalyear> <outdir>"
	print "   - ie: python Join_Field.py D:\MRI_Analysis\prec_monthly 1979 2003 D:\MRI_Analysis\Joined\prec_monhtly"
	sys.exit(1)

dirbase =sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
dirout = sys.argv[4]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)

os.system('cls')

gp.workspace = dirbase 

for year in range(inityear, finalyear + 1, 1):

	for month in range (2, 12 + 1, 1):

		if month < 10:
			# Get a list of shps 
			dsList = gp.ListFeatureClasses("out_" + str(year) + "_0" + str(month), "all")
			InData = dirbase + "\\out_" + str(year) + "_01.shp"
			lista = ""
			for ds in dsList:
				# Process: Join
				gp.joinfield (InData, "STATION_ID", ds, "STATION_ID", "RASTERVALU")
				print ds
		else:
			dsList = gp.ListFeatureClasses("out_" + str(year) + "_" + str(month), "all")
			InData = dirbase + "\\out_" + str(year) + "_01.shp"
			lista = ""
			for ds in dsList:
				# Process: Join
				gp.joinfield (InData, "STATION_ID", ds, "STATION_ID", "RASTERVALU")
				print ds

print "done"
