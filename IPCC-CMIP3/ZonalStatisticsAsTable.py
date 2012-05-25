# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ZonalStatisticsAsTable.py M:\climate_change\IPCC_CMIP3 A1B D:\Workspace\request_miguel\mask\sites.shp D:\Workspace\request_miguel\regions_extract 30s downscaled"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <scenario>, <mask>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "	  mask		: shape with full path and extension"
	print "   dirout	: Out folder"
	print "   resolution: The possibilities are 2_5 min 5min 10min 30s"
	print "   type		: Disaggregated, Interpolated or Downscaled"	
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
scenario = sys.argv[2]
mask = sys.argv[2]
dirout = sys.argv[3]
resolution = sys.argv[5]
type = sys.argv[4]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists 
# periodlist = "2010_2039", "2020_2049", "2040_2069" #"1961_1990", "2070_2099", "2050_2079", "2060_2089", "2030_2059",
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
print "Available models: " + str(modellist)
period = "2010_2039"

for model in modellist:
	# for period in periodlist:
	print "\n---> Processing: " + "SRES_" + scenario + " " + type + " Global_" + str(resolution) + " " + model + " " + period + "\n"
	diroutpoints = dirout + "\\_extract_SRES_" + scenario 
	if not os.path.exists(diroutpoints):
		os.system('mkdir ' + diroutpoints)	
	
	if not os.path.exists(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt"):		
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		rasters = sorted(gp.ListRasters("*", "GRID"))
		
		InPointsFC = mask 
		for raster in rasters:
			outTable = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + os.path.basename(raster) + ".dbf"
			gp.CheckOutExtension("Spatial")
			if not gp.Exists(outTable):
				gp.ZonalStatisticsAsTable_sa(mask, "FID", asc, outTable, "DATA")

	checkTXT = open(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt", "w")
	checkTXT.close()
		
		
print "done!!!" 