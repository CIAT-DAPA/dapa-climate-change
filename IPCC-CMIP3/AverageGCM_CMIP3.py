# -------------------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: June 16th 2011
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated, anomalies  or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: AverageGCM_CMIP3.py <dirbase> <dirout> <scenario> <resolution> <period> <mask>"
	print "   - ex: python AverageGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace a1b 30s 2010_2039 D:\Workspace\mask\mask"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder of averaged data"
	print "   sres		: IPCC Emission Escenario"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   period	: Future 30yr interval"	
	print "   mask		: This grid limites calculated to a specific region"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
res = sys.argv[4]
period = sys.argv[5]
mask = sys.argv[6]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average " + " " + str(resolution) + " " + sres
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of sress and models
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution)))
print "\nAvailable models: " + str(modellist)

# Looping around periods
for period in periodlist:
	
	# Seek for variables in the first model
	gp.workspace = dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\bccr_bcm2_0\\2010_2039"

	# Get a list of variables
	variablelist = gp.ListRasters("*", "GRID")

	# Create output directories
	diroutMean = dirout + "\\" + sres  + "_" + str(resolution) + "_ensemble\\" + period
	if not os.path.exists(diroutMean):
		os.system('mkdir ' + diroutMean)

	# Looping around grids
	for variable in variablelist:		
		
		print "\t.. processing", os.path.basename(variable)
		
		# Define outputs
		OutRasterMean =  diroutMean + "\\" + variable
		OutRasterSTD =  diroutMean + "\\" + variable + "_std"
		
		if not gp.Exists(OutRasterSTD):
		
			lista = ""
			for model in modellist:
				raster = dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period + "\\" + variable
				lista = lista + ";" + raster
			LIST = "\"" + lista[1:] + "\""		
		
			# Set extent of mask
			gp.toolbox = "SA"
			gp.Extent = mask
			gp.SnapRaster = mask
			gp.Mask = mask

			# Cell statistic function
			gp.CellStatistics_sa(LIST, OutRasterMean, "MEAN")
			gp.CellStatistics_sa(LIST, OutRasterSTD, "STD")

			print os.path.basename(variable), "calcs done"
		
		else:
		
			print os.path.basename(variable), "calcs done"
			
print "Process done!!!"    
