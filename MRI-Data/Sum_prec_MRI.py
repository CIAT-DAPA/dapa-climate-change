#-----------------------------------------------------------
# Description: Suma los grids precipitacin de los datos MRI
# Author: Carlos Navarro
# Date: 25/08/10
#-----------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Sum_prec_MRI.py D:\MRI_grids\prec\SP0A 1979 2003 D:\MRI_grids\prec_monthly\SP0A"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
dirout = sys.argv[4]
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

		if month < 10 and gp.Exists(dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"):
			gp.workspace = dirbase + "\\OUT_" + str(year) + "0" + str(month) + "010000"
			print "--->...processing : " + dirbase + "\\OUT_"+ str(year) + "0" + str(month)

			# Get a list of grids in the workspace of each folder
			print "\t ..listing grids"
			dsList = gp.ListDatasets("prec*", "all")
			lista = ""
			for ds in dsList:
				lista = lista + ';' + ds 
			LISTA = "\"" + lista[1:] + "\""
			print LISTA
			OutRaster = dirout + "\\prec_" + str(year) + "0" + str(month)
			
			# Process: Cell Statistics...
			print "\t ..summing"
			gp.CellStatistics_sa(LISTA, OutRaster, "SUM")
			print "\t ..done!!"

		if month > 9 and gp.Exists(dirbase + "\\OUT_" + str(year) + str(month) + "010000"):
			gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
			print "--->... processing:" + dirbase + "\\OUT_" + str(year) + str(month) 

			# Get a list of grids in the workspace of each folder
			print "\t ..listing grids"
			dsList = gp.ListDatasets("prec*", "all")
			lista = ""
			for ds in dsList:
				lista = lista + ';' + ds 
			LISTA = "\"" + lista[1:] + "\""
			print LISTA
			OutRaster = dirout + "\\prec_" + str(year) + str(month)

			# Process: Cell Statistics...
			print "\t ..summing"
			gp.CellStatistics_sa(LISTA, OutRaster, "SUM")
			print "\t ..done!!"

print "Done!!!!"
