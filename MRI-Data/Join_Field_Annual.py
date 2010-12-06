# ----------------------------------------------------------------------------------------------
# Descrition : Une en una sola database (.dbf) los datos extraidos por meses de los valores MRI,
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
	print "   - ie: python Join_Field_Annual.py D:\MRI_Analysis\Extracted\prec_anual 1980 2003 prec"
	sys.exit(1)

dirbase =sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]

os.system('cls')

gp.workspace = dirbase 

for year in range(inityear, finalyear + 1, 1):
	InData = dirbase + "\\" + variable + "_1979.shp"
	JoinData = dirbase + "\\" + variable + "_" + str(year) + ".shp"
	print "----> Joining " + variable + " " + str(year)
	gp.joinfield (InData, "STATION_ID", JoinData, "STATION_ID", "RASTERVALU")
print "done"
