# -------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: 14/12/2011
# Purpose: Resample anomalies, disaggregated, interpolated or downscaled surfaces
# -------------------------------------------------------------------------------

import arcgisscripting, os, sys, string
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 8:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: ResampleGCM_CMIP3.py <dirbase> <dirout> <scenario> <out resolution> <period> <method>"
	print "	  - ie: python ResampleGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace a1b 60 2010_2039 NEAREST"
	print "   dirbase	: Root folder where are storaged GCM data"
	print "   dirout	: Output folder of averaged data"
	print "   sres		: IPCC Emission Escenario"
	print "   res		: Units Resolution in arcminutes"
	print "   period	: Future 30yr interval"	
	print "   method	: NEAREST Nearest neighbor assignment This is the default"
	print "			      BILINEAR Bilinear interpolation"
	print "				  CUBIC Cubic convolution"
	print "				  MAJORITY Majority resampling"
	sys.exit(1)

# Set variables
dirbase = sys.argv[1]
dirout = sys.argv[2]
sres = sys.argv[3]
res = sys.argv[4]
period = sys.argv[5]
mask = sys.argv[6]
switch = sys.argv[7]

# Clean screen
os.system('cls')

#Check out Spatial Analyst extension license
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
print "     Cut GCM " + " " + str(resolution) + " " + sres
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of sress and models
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution)))
print "\nAvailable models: " + str(modellist)

# Looping around periods
for period in periodlist:

	# for model in modellist:
	for model in modellist:
		
		# Set workspace by each model
		gp.workspace = dirbase + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		
		# Define check file
		checkFile = dirout + "\\sres_" + sres + "\\" + model + "-" + period + "-resample-done.txt"
		if not os.path.exists(checkFile):
			
			print "\n Processing: ", sres, period, model + "\n"
			
			# Define and create output directory
			diroutGrd = dirout + "\\sres_" + sres + "\\Global" + model + "\\" + period
			if not os.path.exists(diroutGrd):
				os.system('mkdir ' + diroutGrd)

			#Get a list of raster in workspace
			rasters = sorted(gp.ListRasters("*", "GRID"))
			for raster in rasters:
				
				# Define out ESRI-Grid
				outGrd = diroutGrd + "\\" + raster
				
				if not gp.Exists(outGrd):
					
					# Cut function
					gp.Resample_management(gp.workspace + "\\" + raster, outGrd, res / 60, '"' + method + '"')	
					print "\t", raster, period, model "cutted"
									
			checkFile = open(checkFile, "w")
			checkFile.close()
			print "\n " + sres + " " + period + " " + model + " processed\n"
		else:
			print "\n " + sres + " " + period + " " + model + " processed\n"

print "\n \t Process done!!"  