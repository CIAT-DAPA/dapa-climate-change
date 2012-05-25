# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# 		First, cut the climate data with cut_process.aml and cut_GCM.aml scripts. 
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 5:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_MaskGCM_for_Analogues.py E:\workspace\Request_Flora\gha_bfa_30s D:\workspace\Request_Flora\tmp E:\workspace\Request_Flora\gha_bfa_30s_asciis 30s"
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
dirtemp = sys.argv[2]
if not os.path.exists(dirtemp):
    os.system('mkdir ' + dirtemp)
dirout = sys.argv[3]
resolution = sys.argv[4]

os.system('cls')
gp.CheckOutExtension("Spatial")
gp.toolbox="management"

# countrylist = "afr", "asi", "auz", "eur", "lat", "nam", "rus", "glo"
# countrylist = "ind", "eth", "bgd", "mli", "sen", "ner", "bfa", "gha", "nep", "uga", "ken", "tza",
# countryDc = {"ind": "67 6 99 37 ", "bgd": "87 19 94 28 ", "mli": "-13 8 6 27 ", "sen": "-19 11 -10 18 ", "ner": "-1 10 17 25 ",
			 # "bfa": "-6.5 8 3 17 ", "gha": "-4 3 2 13 ", "nep": "79 24 89 32 ", "eth": "32 0 49 17 ", "uga": "28 -3 36 6 ", "ken": "33 -7 43 7 ", "tza": "28 -14 41 1 ", }
		 
period = "2020_2049"
modellist = sorted(os.listdir(dirbase + "\\Global_" + str(resolution)))
# res = "0.016667"

print "~~~~~~~~~~~~~~~~~~~~~~"
print "   DTR calculation  	 "  
print "~~~~~~~~~~~~~~~~~~~~~~"

for model in modellist:

	print model, period
	if model == "current":
		gp.workspace = dirbase + "\\Global_" + str(resolution) + "\\" + model
	else:
		gp.workspace = dirbase + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		
	for month in range (1, 12 + 1, 1):

		if not gp.Exists(gp.workspace + "\\dtr_" + str(month)):
			print "\tCalculating month " + str(month)
			InExpression = gp.workspace + "\\tmax_" + str(month) + " - " + gp.workspace + "\\tmin_" + str(month)
			gp.SingleOutputMapAlgebra_sa(InExpression,  gp.workspace + "\\dtr_" + str(month))
					
print "DTR Calculation done!!!"    


print "~~~~~~~~~~~~~~~~~~~~~~"
print "  CONVERT TO ASCII	 "
print "~~~~~~~~~~~~~~~~~~~~~~"


for model in modellist:
	if model == "current":
		# for country in countrylist:
			
		gp.workspace = dirbase + "\\Global_" + str(resolution) + "\\" + model
		print "\n---> Processing: " + model
		print "\n---> Processing: " + model, period
		
		diroutAscii = dirout
		if not os.path.exists(diroutAscii):
			os.system('mkdir ' + diroutAscii)
		
		rasters = gp.ListRasters("*", "GRID")

		for raster in rasters:
			if not os.path.basename(raster)[0:4] == "tmin" or not os.path.basename(raster)[0:4] == "tmax":
				
				if os.path.basename(raster)[0:3] == "bio":
					OutAscii = diroutAscii + "\\" + model + "_" + raster + "_1.asc"
				else:
					OutAscii = diroutAscii + "\\" + model + "_" + raster + ".asc"
				
				print "Converting " + os.path.basename(OutAscii)
				gp.RasterToASCII_conversion(raster, OutAscii)

	else:
		gp.workspace = dirbase + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		print "\n---> Processing: " + model, period
		
		diroutAscii = dirout
		if not os.path.exists(diroutAscii):
			os.system('mkdir ' + diroutAscii)
		
		rasters = gp.ListRasters("*", "GRID")

		for raster in rasters:
			if not os.path.basename(raster)[0:4] == "tmin" or not os.path.basename(raster)[0:4] == "tmax":
				
				if os.path.basename(raster)[0:3] == "bio":
					OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + "_1.asc"
				else:
					OutAscii = diroutAscii + "\\a1b_2020_2049_" + model + "_" + raster + ".asc"
				
				print "Converting " + os.path.basename(OutAscii)
				gp.RasterToASCII_conversion(raster, OutAscii)
print "done!!!"    







