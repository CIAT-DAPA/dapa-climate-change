#-----------------------------------------------------------
# Description: Suma los grids precipitacin de los datos MRI
# Author: Carlos Navarro
# Date: 25/08/10
#-----------------------------------------------------------

import arcgisscripting, os, sys, glob
gp = arcgisscripting.create(9.3)

if len(sys.argv) < 4:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Summary.py E:\RCM_data E:\RCM_data\summary p"
	sys.exit(1)

# Arguments
dirbase =sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
variable = sys.argv[3]

# Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

gp.OverWriteOutput = 1

os.system('cls')

print "\n"
print "~~~~~~~~~~~~~~~"
print "   SUM GRIDS   "
print "~~~~~~~~~~~~~~~"
print "\n"

modellist = sorted(os.listdir(dirbase))
for model in modellist:
	periodlist = sorted(os.listdir(dirbase + "\\" + model))
	for period in periodlist:
		ascList = glob.glob(dirbase + "\\" + model + "\\" + period + "\\" + variable + "*.asc")
		lista = ""
		for asc in ascList:
			lista = lista + ';' + asc 
		LISTA = "\"" + lista[1:] + "\""
		print LISTA
		
		diroutstats = dirout + "\\" + model
		if not os.path.exists(diroutstats):
			os.system('mkdir ' + diroutstats)
		

		
		if variable == "p":
			print period, model, "mean"
			gp.CellStatistics_sa(LISTA, diroutstats + "\\av_" + variable + "_" + period, "SUM")
		else: 
			print period, model, "mean"
			gp.CellStatistics_sa(LISTA, diroutstats + "\\av_" + variable + "_" + period, "MEAN")

		gp.CellStatistics_sa(LISTA, diroutstats + "\\av_" + variable + "_" + period, "MEAN")
		print period, model, "range"
		gp.CellStatistics_sa(LISTA, diroutstats + "\\rng_" + variable + "_" + period, "RANGE")
		print period, model, "std"
		gp.CellStatistics_sa(LISTA, diroutstats + "\\std_" + variable + "_" + period, "STD")
		print period, model, "cv"
		InExpression = diroutstats + "\\std_" + variable + "_" + period + " / " + diroutstats + "\\av_" + variable + "_" + period
		gp.SingleOutputMapAlgebra_sa(InExpression, diroutstats + "\\cv" + variable + "_" + period)
		
print "Done!!!!"
