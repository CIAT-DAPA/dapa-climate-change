# -------------------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro, Jaime Tarapues
# Date: 2014
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated, anomalies or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)
# python 11-average-gcm-ensmble.py T:\gcm\cmip5\downscaled D:\jetarapues\cmip5_process rcp26 30s 2020_2049 ALL
# python 11-average-gcm-ensmble.py T:\gcm\cmip5\downscaled D:\jetarapues\cmip5_process rcp26 30s 2040_2069 ALL
# python 11-average-gcm-ensmble.py T:\gcm\cmip5\downscaled D:\jetarapues\cmip5_process rcp26 30s 2060_2089 ALL
#Syntax 
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: 11-average-gcm-ensmble.py <dirbase> <dirout> <scenario> <resolution> <period> <mask> <models>"
	print "   - ex: python 11-average-gcm-ensmble.py T:\data\gcm\cmip5\downscaled T:\data\gcm\cmip5\downscaled\ensemble rcp60 30s 2020_2049 ALL"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder of averaged data"
	print "   rcp		: IPCC Emission Escenario"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   period	: Future 30yr interval"	
	print "   mask		: This grid limites calculated to a specific region"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirtemp = sys.argv[2]
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
	
dirout = dirbase +"\\ensemble"
diroutMean = dirout + "\\" + rcp+ "\\global_" + resolution + "\\" + str(period) 
if not os.path.exists(diroutMean):
	os.system('mkdir ' + diroutMean)	

dirtempPeriod = dirtemp + "\\ensemble\\" + rcp+ "-global_" + resolution + "-" + str(period) 
if not os.path.exists(dirtempPeriod):
	os.system('mkdir ' + dirtempPeriod)	
	
print "\nAvailable models: " + str(modellist)

variablelist = ["bio","prec","cons_mths"]#["bio","tmin","tmax","tmean","prec","cons_mths"]
variablelistCopy = ["tmin","tmax","tmean"]


for variable in variablelistCopy:
	if variable == "bio":
		num = 19
	elif variable == "cons_mths":
		num = 1
	else:
		num = 12
	for month in range (1, num + 1, 1):
	
		if variable == "cons_mths":
			raster = diroutMean + "\\" + variable
		else:
			raster = diroutMean + "\\" + variable + "_" + str(month)

		OutRasterMean =  diroutMean + "\\" + os.path.basename(raster)
		OutRasterSTD =  diroutMean + "\\" + os.path.basename(raster) + "_std"	
		if not gp.Exists(dirtempPeriod+ "\\" + os.path.basename(raster)):
			# gp.delete_management(OutRasterMean)
			gp.CopyRaster_management(OutRasterMean,dirtempPeriod+ "\\" + os.path.basename(raster),"#","#","0","NONE","NONE","#")
			print ".... Copied "+ OutRasterMean

		if not gp.Exists(dirtempPeriod+ "\\" + os.path.basename(raster) + "_std"):
			# gp.delete_management(OutRasterSTD)
			gp.CopyRaster_management(OutRasterSTD,dirtempPeriod+ "\\" + os.path.basename(raster) + "_std","#","#","0","NONE","NONE","#")
			print ".... Copied "+ OutRasterSTD

		print " Copied  Ensemble " + rcp + " "  + variable + "_" + str(month) + " "  + str(period) +"\n"

# Replace a layer/table view name with a path to a dataset (which can be a layer file) or create the layer/table view within the script
# The following inputs are layers or table views: "tmean_5_std"
# arcpy.CopyRaster_management("tmean_5_std","D:/jetarapues/cmip5_process/ensemble/rcp26-global_30s-2020_2049/tmean_5_std","#","#","0","NONE","NONE","#","NONE","NONE")

for variable in variablelist:
	checkfile=dirtempPeriod+"-"+variable+".txt"
	
	# if not os.path.isfile(checkfile):
	if variable == "bio":
		num = 19
	elif variable == "cons_mths":
		num = 1
	else:
		num = 12
	for month in range (1, num + 1, 1):
	
		print "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
		print "     Ensemble " + " " + rcp + " "  + variable + "_" + str(month) + " "  + str(period) 
		print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"
		
		
		if variable == "cons_mths":
			rasterVar = variable
		else:
			rasterVar = variable + "_" + str(month)
			
		OutRasterMean =  dirtempPeriod + "\\" + os.path.basename(rasterVar)
		OutRasterSTD =  dirtempPeriod + "\\" + os.path.basename(rasterVar) + "_std"
		if not gp.Exists(OutRasterMean) and not gp.Exists(OutRasterSTD):
			
			lista = ""
			for model in modellist:

				#Set workspace
				gp.workspace = dirbase + "\\" + rcp + "\\global_" + resolution + "\\" + model + "\\r1i1p1\\" + period
				raster = gp.workspace + "\\" + rasterVar
				# if not gp.Exists(OutRasterMean) and not gp.Exists(OutRasterMean + "_"):
				print ".. processing", model, os.path.basename(raster)
				lista = lista + ";" + raster
				# if not gp.Exists(OutRasterSTD) and not gp.Exists(OutRasterSTD + "_"):
				# print ".. processing", model, os.path.basename(raster)
				# lista = lista + ";" + raster				
			LIST = "\"" + lista[1:] + "\""	
				
			# Cell statistic function
			# if not gp.Exists(OutRasterMean) and not gp.Exists(OutRasterMean + "_"):
			gp.CellStatistics_sa(LIST, OutRasterMean + "_", "MEAN")
			InExpression = "Int(Floor(" + OutRasterMean + "_ + 0.5))"
			gp.SingleOutputMapAlgebra_sa(InExpression, OutRasterMean)
			gp.delete_management(OutRasterMean + "_")
			print "\t",os.path.basename(raster), "calcs done"
			# else:
				# print "\t",os.path.basename(raster), "calcs done"			


			# if not gp.Exists(OutRasterSTD):
				# if not gp.Exists(OutRasterSTD[:-1] + "_"):

			gp.CellStatistics_sa(LIST, OutRasterSTD[:-1] + "_", "STD")
			InExpression = "Int(Floor(" + OutRasterSTD[:-1]  + "_ + 0.5))"
			gp.SingleOutputMapAlgebra_sa(InExpression, OutRasterSTD)
			gp.delete_management(OutRasterSTD[:-1]  + "_")

			print "\t",os.path.basename(OutRasterSTD), "calcs done"
			# else:
				# print "\t",os.path.basename(OutRasterSTD), "calcs done"			
				
			outFile = open(checkfile, "a")
			outFile.write(rasterVar+"\n")
		else:
			print "  done "+os.path.basename(rasterVar)+" ,"+ os.path.basename(rasterVar) + "_std"
	# else:
		# print "  done "+variable
	outFile.close()
		
gp.workspace = dirtempPeriod
rasters = gp.ListRasters("*", "GRID")
listRaster=[]
for raster in rasters:
    # print raster
	listRaster.append(raster);

countRaster=len(listRaster)
if countRaster == 136:
	os.system('rmdir /s /q ' + diroutMean)	
	os.system("robocopy " + dirtempPeriod+" "+ diroutMean+" /s /z")	
	os.system('rmdir /s /q ' + dirtempPeriod)	
else:
	print "Error numero de rasters: "+countRaster+" =>"+dirtempPeriod
			
print "Process done!!!" 