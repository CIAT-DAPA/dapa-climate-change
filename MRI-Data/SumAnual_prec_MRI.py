#-----------------------------------------------------------
# Description: Suma los grids precipitacin de los datos MRI
# Author: Carlos Navarro
# Date: 25/08/10
#-----------------------------------------------------------

import arcgisscripting, os, sys
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python SumAnual_prec_MRI.py F:\MRI_grids\_extract_ColPlains\prec_monthly\SP0A 1979 2003 F:\MRI_grids\_extract_ColPlains\prec_annual prec"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
inityear = int(sys.argv[2])
finalyear = int(sys.argv[3])
dirout = sys.argv[4]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
variable = sys.argv[5]
	
# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

os.system('cls')

gp.workspace = dirbase

print "\n"
print "~~~~~~~~~~~~~~~"
print "   SUM GRIDS   "
print "~~~~~~~~~~~~~~~"
print "\n"

for month in range (1, 12 + 1, 1): 
	if month < 10:
		lista = ""
		print "\t ..listing grids"
		for year in range(inityear, finalyear + 1, 1):
			
			# Get a list of grids in the workspace 
			
			# dsList = gp.ListDatasets("prec_" + str(year) + "*", "all")
			ds = gp.workspace + "\\" + variable + "_" + str(year) + "0" + str(month)
			print "\t  " + str(year) + " " + os.path.basename(ds)
			lista = lista + ';' + ds 
		LISTA = "\"" + lista[1:] + "\""
		OutRaster = dirout + "\\" + variable + "_" + str(month)
		if not gp.Exists(OutRaster):
			# Process: Cell Statistics...
			print "\t ..meaning"
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
			print "\t ..done!!"

	if month > 9:
		lista = ""
		print "\t ..listing grids"
		for year in range(inityear, finalyear + 1, 1):
			
			# Get a list of grids in the workspace 
			
			# dsList = gp.ListDatasets("prec_" + str(year) + "*", "all")
			ds = gp.workspace + "\\" + variable + "_" + str(year) + str(month)
			print "\t  " + str(year) + " " + os.path.basename(ds)
			lista = lista + ';' + ds 
		LISTA = "\"" + lista[1:] + "\""
		OutRaster = dirout + "\\" + variable + "_" + str(month)
		if not gp.Exists(OutRaster):
			# Process: Cell Statistics...
			print "\t ..meaning"
			gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
			print "\t ..done!!"
print "Done!!!!"
