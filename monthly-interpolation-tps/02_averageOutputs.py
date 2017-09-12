# -------------------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro, Jaime Tarapues
# Date: 2014
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated, anomalies or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)
# python 02_averageOutputs.py D:\cenavarro\col-cormacarena\monthly-interpolations-llanos\outputs_l98 D:\cenavarro\col-cormacarena\monthly-interpolations-llanos\outputs_l98\average 1 25

#Syntax 
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"	
	print "   dirbase	: Root folder where are storaged ascii data"
	print "   dirout	: Output folder of averaged data"
	print "   variable	: variable"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
# variable = sys.argv[3]
tile = sys.argv[3]
nfolds = sys.argv[4]
# mthst = sys.argv[6]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average 							   "
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"	

# for tile in range(1, int(ntiles) +1, 1):
varlist = "rain", "tmax", "tmin"

dirout_tile = dirout 
if not os.path.exists(dirout_tile):
	os.system('mkdir ' + dirout_tile)

for variable in varlist:

	for month in range (1, 12 + 1, 1):

		lista = ""
		for fold in range(1, int(nfolds) +1 , 1):
			lista = lista + ";" + dirbase + "\\" + variable + "\\fold-" + str(fold) + "\\tile-" + str(tile)  + "\\" + variable + "_" + str(month) + ".asc"
		LIST = "\"" + lista[1:] + "\""	
		# print LIST
		
		if variable == "rain":
			OutRasterMean = dirout_tile + "\\prec_" + str(month)
		else:
			OutRasterMean = dirout_tile + "\\" + variable + "_" + str(month)
			
		# Cell statistic function
		if not gp.Exists(OutRasterMean):
			
			print "\t", "tile-" + str(tile), "month-" + str(month), variable, "avg calcs"
			
			if not gp.Exists(OutRasterMean + "_t"):
				gp.CellStatistics_sa(LIST, OutRasterMean + "_t", "MEAN")
			
			if variable == "rain":
				InExpression = "Con(Int(Floor(" + OutRasterMean + "_t + 0.5)) < 0, 0," + OutRasterMean + "_t)"
			else:
				InExpression = "Int(Floor(" + OutRasterMean + "_t + 0.5))"
			
			gp.SingleOutputMapAlgebra_sa(InExpression, OutRasterMean)
			gp.delete_management(OutRasterMean + "_t")
			print "\t done"
			
		else:
		
			print "\t", "tile-" + str(tile), "month-",str(month), variable, "avg calcs"
			print "\t done"

		# if not gp.Exists(OutRasterSTD):
			# gp.CellStatistics_sa(LIST, OutRasterSTD + "_t", "STD")
			# InExpression = "Int(Floor(" + OutRasterSTD  + "_t + 0.5))"
			# gp.SingleOutputMapAlgebra_sa(InExpression, OutRasterSTD)
			# gp.delete_management(OutRasterSTD + "_t")
			# print "\t", "tile-" + str(tile), "fold-" + str(fold), " std calcs done"
		# else:
			# print "\t", "tile-" + str(tile), "fold-" + str(fold), " std calcs done"			
	
print "Process done!!!" 