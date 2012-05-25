#-----------------------------------------------------------
# Description: Promedia los grids temperatura de los datos MRI
# Author: Carlos Navarro
# Date: 06/09/10
#-----------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Mean_temp_MRI1.py D:\MRI_grids 1979 2003 tmean SP0A"
	sys.exit(1)

# Arguments
dirbase = sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
variable = sys.argv[4]
period = sys.argv[5]

dirout = dirbase + "\\" + variable + "_monthly\\" + period
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
	
# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

gp.OverWriteOutput = 1

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~"
print "   SUM GRIDS   "
print "~~~~~~~~~~~~~~~"
print "\n"

for year in range(inityear, finalyear + 1, 1):

	for month in range (1, 12 + 1, 1):

		if month < 10 and gp.Exists(dirbase + "\\" + "tmean1" + "\\" + period + "\\OUT_" + str(year) + "0" + str(month) + "010000"):
			gp.workspace = dirbase + "\\" + "tmean1" + "\\" + period + "\\OUT_" + str(year) + "0" + str(month) + "010000"
			print "--->...processing : " + dirbase + "\\" + variable + "\\" + period + "\\OUT_" + str(year) + "0" + str(month)

			# Get a list of grids in the workspace of each folder
			print "\t ..listing grids"
			dsList = gp.ListDatasets(variable + "*", "all")
			lista = ""
			for ds in dsList:
				lista = lista + ';' + ds 
			LISTA = "\"" + lista[1:] + "\""
			print LISTA
			OutRaster = dirout + "\\" + variable + "_" + str(year) + "0" + str(month)
			
			# Process: Cell Statistics...
			print "\t ..averaging"
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
			print "\t ..done!!"

		if month > 9 and gp.Exists(dirbase + "\\" + "tmean1" + "\\" + period + "\\OUT_" + str(year) + str(month) + "010000"):
			gp.workspace = dirbase + "\\" + "tmean1" + "\\" + period + "\\OUT_" + str(year) + str(month) + "010000"
			print "--->... processing:" + dirbase + "\\" + variable + "\\" + period + "\\OUT_" + str(year) + str(month)

			# Get a list of grids in the workspace of each folder
			print "\t ..listing grids"
			dsList = gp.ListDatasets(variable + "*", "all")
			lista = ""
			for ds in dsList:
				lista = lista + ';' + ds 
			LISTA = "\"" + lista[1:] + "\""
			print LISTA
			OutRaster = dirout + "\\" + variable + "_" + str(year) + str(month)

			# Process: Cell Statistics...
			print "\t ..averaging"
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
			print "\t ..done!!"

print "Done!!!!"
