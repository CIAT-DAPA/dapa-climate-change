# -------------------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro, Jaime Tarapues
# Date: 2014
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated, anomalies or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: 11-average-gcm-ensmble.py <dirbase> <dirout> <scenario> <resolution> <period> <mask> <models>"
	print "   - ex: python 11-average-gcm-ensmble.py T:\gcm\cmip5\downscaled T:\gcm\cmip5\downscaled\ensemble rcp26 30s 2020_2049 ALL"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder of averaged data"
	print "   rcp		: IPCC Emission Escenario"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   period	: Future 30yr interval"	
	print "   mask		: This grid limites calculated to a specific region"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
rcp = sys.argv[3]
resolution = sys.argv[4]
period = sys.argv[5]
models = sys.argv[6]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average " + " " + str(resolution) + " " + rcp
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"


if models == "ALL":
	modellist = sorted(os.listdir(dirbase + "\\" + rcp + "\\global_" + resolution))
else: 
	modellist = models.split(",")
	
	
diroutMean = dirout + "\\" + rcp+ "\\global_" + resolution + "\\" + str(period) 
if not os.path.exists(diroutMean):
	os.system('mkdir ' + diroutMean)	

print "\nAvailable models: " + str(modellist)

variablelist = ["bio","tmin","tmax","tmean","prec","cons_mths"]

for variable in variablelist:


	if variable == "bio":
		num = 19
	else:
		num = 12
	for month in range (1, num + 1, 1):
	
		print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		print "     Ensemble " + " " + rcp + " "  + variable + "_" + str(month) + " "  + str(period) 
		print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
	
		lista = ""
		for model in modellist:

			#Set workspace
			gp.workspace = dirbase + "\\" + rcp + "\\global_" + resolution + "\\" + model + "\\r1i1p1\\" + period
			
			if variable == "cons_mths":
				raster = gp.workspace + "\\" + variable
			else:
				raster = gp.workspace + "\\" + variable + "_" + str(month)

			print ".. processing", model, os.path.basename(raster)

			OutRasterMean =  diroutMean + "\\" + os.path.basename(raster)
			OutRasterSTD =  diroutMean + "\\" + os.path.basename(raster) + "_std"			

			lista = lista + ";" + raster
		LIST = "\"" + lista[1:] + "\""	
			
		# Cell statistic function
		if not gp.Exists(OutRasterMean) and not gp.Exists(OutRasterMean + "_"):
			gp.CellStatistics_sa(LIST, OutRasterMean + "_", "MEAN")
			InExpression = "Int(Floor(" + OutRasterMean + "_ + 0.5))"
			gp.SingleOutputMapAlgebra_sa(InExpression, OutRasterMean)
			gp.delete_management(OutRasterMean + "_")
			print "\t",os.path.basename(raster), "calcs done"
		else:
			print "\t",os.path.basename(raster), "calcs done"			


		if not gp.Exists(OutRasterSTD):
			if not gp.Exists(OutRasterSTD[:-1] + "_"):

				gp.CellStatistics_sa(LIST, OutRasterSTD[:-1] + "_", "STD")
				InExpression = "Int(Floor(" + OutRasterSTD[:-1]  + "_ + 0.5))"
				gp.SingleOutputMapAlgebra_sa(InExpression, OutRasterSTD)
				gp.delete_management(OutRasterSTD[:-1]  + "_")
				
			print "\t",os.path.basename(OutRasterSTD), "calcs done"
		else:
			print "\t",os.path.basename(OutRasterSTD), "calcs done"			
			
print "Process done!!!"    