# -------------------------------------------------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: June 16th 2011
# Purpose: Get an average and the standar variation surfaces from Donwscaled, dissaggregated, anomalies or interpolated GCM data
# -------------------------------------------------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax 
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: <AverageGCM_CMIP3.py> <dirbase> <dirout> <mask> <sres> <resolution> <period> <wildcard>"
	print "   - ex: python AverageGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace\output D:\Workspace\mask a1b 30s 2010_2039 ALL"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder of averaged data"
	print "   mask		: This grid limites calculated to a specific region"	
	print "   sres		: IPCC Emission Escenario. The possibilities are a1b, a2, b1"
	print "   resolution: Units Resolution in arcminutes. The possibilities are 30s 2_5min 5min 10min"
	print "   periods	: Future 30yr periods. If you want to choose some periods, enter them separated by commas without spaces. E.g.: 2010_2039,2020_2049,2030_2059. Use 'ALL' to process all the periods"
	print "	  wildcard  : Search files with matching key name. E.g. if you want all precipitation data (prec_1, prec_2, ..., prec_n), you must write PREC. Use ALL to convert all data in the workspace"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
mask = sys.argv[3]
sres = sys.argv[4]
resolution = sys.argv[5]
period = sys.argv[6]
wildcard = sys.argv[7]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Average " + " " + str(resolution) + " " + sres
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of sress and models
if period == "ALL":
	periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
else:	
	periodlist = period.split(",")


modellist = sorted(os.listdir(dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution)))
gp.AddMessage( "\nAvailable models: " + str(modellist) )
gp.AddMessage( "Periods: " + str(periodlist) )

# Looping around periods
for period in periodlist:
	
	# Seek for variables in the first model
	gp.workspace = dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\bccr_bcm2_0\\2010_2039"

	# Get a list of variables
	if wildcard == "ALL":
		variablelist = sorted(gp.ListRasters("*", "GRID"))
	else:	
		variablelist = sorted(gp.ListRasters(wildcard + "*", "GRID"))	
	

	# Create output directories
	diroutMean = dirout + "\\" + sres  + "_" + str(resolution) + "_ensemble\\" + period
	if not os.path.exists(diroutMean):
		os.system('mkdir ' + diroutMean)

	# Looping around grids
	for variable in variablelist:		
		
		gp.AddMessage( "\t.. processing " + os.path.basename(variable) )
		
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

			gp.AddMessage( os.path.basename(variable) + " " + "calcs done" )
		
		else:
		
			gp.AddMessage( os.path.basename(variable) + " " + "calcs done" )
			
gp.AddMessage("\n \t ====> DONE!! <====")    
