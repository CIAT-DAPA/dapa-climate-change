# -----------------------------------------------------------
# Author: Carlos Navarro
# Date: April 13th 2011
# Pourpose: Make a raster layers from GCM downscaled datasets
# -----------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 6:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python GCMLayers.py \\172.22.33.33\Geodata\climate_change\IPCC_CMIP3 D:\climate_change\IPCC_CMIP3\_layers A1B 10min downscaled"
	sys.exit(1)

#Set variables 
dirbase = sys.argv[1]
dirout = sys.argv[2]
if not os.path.exists(dirout):
	os.system('mkdir ' + dirout)
scenario = sys.argv[3]
resolution = sys.argv[4]
type = sys.argv[5]

#Clear screen
os.system('cls')


print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Make Layers " + type + " " + str(resolution) + " SRES_" + str(scenario)
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of scenarios and models
periodDc = {"2010_2039": "2020s", "2020_2049": "2030s", "2030_2059": "2040s", "2040_2069": "2050s", "2050_2079": "2060s", "2060_2089": "2070s", "2070_2099": "2080s"}
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))

for model in modellist:
	
	for period in sorted(periodDc):
	
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		print "\n---> Processing: " + "SRES_" + scenario + " " + type + " " + str(resolution) + " " + model + " " + period + "\n"
		diroutlayer = dirout + "\\SRES_" + scenario + "\\Global_" + str(resolution)
		if not os.path.exists(diroutlayer):
			os.system('mkdir ' + diroutlayer)
			
		#Get a list of raster into the workspace
		rasters = sorted(gp.ListRasters("", "GRID"))
		for raster in rasters:

			if not os.path.exists(diroutlayer + "\\" + raster + "-SRES_" + str(scenario) + "-" + str(model) + "-" + str(periodDc [period]) + "-" + str(resolution) + ".lyr"):
				print "  Making layer of " + model + " " + raster + " "  + period + " " + "SRES_" + scenario + " " + type + " " + str(resolution) 
				TmpLyr = raster + "-SRES_" + str(scenario) + "-" + str(model) + "-" + str(periodDc [period]) + "-" + str(resolution)
				gp.MakeRasterLayer_management(raster, TmpLyr)
				
				OutLyr = diroutlayer + "\\" + raster + "-SRES_" + str(scenario) + "-" + str(model) + "-" + str(periodDc [period]) + "-" + str(resolution) + ".lyr"
				gp.savetolayerfile(TmpLyr, OutLyr)

			else:
				print "  Existing Layer " + model + " " + raster + " "  + period + " " + "SRES_" + scenario + " " + type + " " + str(resolution) 
print "Process done!!!"    
