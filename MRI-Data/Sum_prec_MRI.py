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
	print "   - ie: python Sum_prec_MRI.py K:\MRIData\MRI_grids\SP0A\tmean 1979 2003 F:\MRI_grids\_extract_ColPlains\tmean_monthly\SP0A tmean"
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

			OutRaster = dirout + "\\" + variable + "_" + str(year) + "0" + str(month)
			if not gp.Exists(OutRaster):
				# Get a list of grids in the workspace of each folder
				print "\t ..listing grids"
				dsList = gp.ListDatasets("*", "all")
				lista = ""
				for ds in dsList:
					lista = lista + ';' + ds 
				LISTA = "\"" + lista[1:] + "\""
				print LISTA
				
				
				# Process: Cell Statistics...
				if variable == "prec":
					print "\t ..summing prec"
					gp.CellStatistics_sa(LISTA, OutRaster, "SUM")
				else: 
					print "\t ..meaning " + variable
					gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
				print "\t ..done!!"

		if month > 9 and gp.Exists(dirbase + "\\OUT_" + str(year) + str(month) + "010000"):
			gp.workspace = dirbase + "\\OUT_" + str(year) + str(month) + "010000"
			print "--->... processing:" + dirbase + "\\OUT_" + str(year) + str(month) 

			OutRaster = dirout + "\\" + variable + "_" + str(year) + str(month)
			if not gp.Exists(OutRaster):
				# Get a list of grids in the workspace of each folder
				print "\t ..listing grids"
				dsList = gp.ListDatasets("*", "all")
				lista = ""
				for ds in dsList:
					lista = lista + ';' + ds 
				LISTA = "\"" + lista[1:] + "\""
				print LISTA
				

				# Process: Cell Statistics...
				
				if variable == "prec":
					print "\t ..summing prec"
					gp.CellStatistics_sa(LISTA, OutRaster, "SUM")
				else: 
					print "\t ..meaning " + variable
					gp.CellStatistics_sa(LISTA, OutRaster, "MEAN")
				print "\t ..done!!"

print "Done!!!!"
