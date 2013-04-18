# -----------------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extract values for anomalies, disaggregated, interpolated or downscaled surfaces
# -----------------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   Syntax	: ExtractValuesGCM_CMIP3.py <dirbase> <dirout> <scenario> <resolution> <period> <points>"
	print "	  - ex: python ExtractValuesGCM_CMIP3.py \\dapadfs\data_cluster_4\gcm\cmip3\downscaled D:\Workspace a1b 30s 2010_2039 D:\Workspace\mask\points.shp"
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
print "     Extract Values GCMs " + " " + str(resolution) + " " + sres
print "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"

#Get lists of sress and models
periodlist = "2010_2039", "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase  + "\\sres_" + sres + "\\Global_" + str(resolution)))
print "Available models: " + str(modellist)

# Looping around periods
for period in periodlist:

	# for model in modellist:
	for model in modellist:
		
		# Set workspace by each model
		gp.workspace = dirout + "\\sres_" + sres + "\\Global" + model + "\\" + period + "\\sres_" + sres + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		
		# Define output directory and file
		diroutSam = dirout + "\\sres_" + sres + "\\Global" + model + "\\" + period
		outFile = diroutSam + "\\extracts-" + sres + "-" + model + "-" + period + ".dbf"
		
		if not os.path.exists(outFile):
			
			print "\n Processing: ", sres, period, model + "\n"
			
			# Create output directory
			if not os.path.exists(diroutSam):
				os.system('mkdir ' + diroutSam)

			#Get a list of raster in workspace
			rasters = sorted(gp.ListRasters("*", "GRID"))
			for raster in rasters:
				
				# Set output points
				outPoints = diroutSam + "\\" + sres + "_" + model + "_" + period + "_" + os.path.basename(raster) + ".dbf"
				if not gp.Exists(outPoints) and not os.path.basename(raster).split("_")[0] == "tmean":
					# Sample function
					gp.Sample_sa(raster, mask, outPoints, "")
				print "\t", raster, period, model "sampled"
					
			# Get a list of dbfs 
			dbfList = sorted(glob.glob(diroutSam + "\\" + sres + "_" + model + "_" + period + "*.dbf"))
			for dbf in dbfList:
				inData = diroutSam + "\\" + sres + "_" + model + "_" + period + "_bio_1.dbf"
				if not os.path.basename(dbf)[-9:] == "bio_1.dbf":
					fields = os.path.basename(dbf)[:-4].split("_")[-2:]
					gp.joinfield (inData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
					print "\t", os.path.basename(dbf) + " joined"
					os.remove(dbf)
			
			# Rename output file
			os.rename(inData, outFile)
			
			# Remove trash files
			xmlList = sorted(glob.glob(diroutSam + "\\*.xml"))
			for xml in xmlList:
				os.remove(xml)

			print "\n " + sres + " " + period + " " + model + " processed\n"
			
		else:
		
			print "\n " + sres + " " + period + " " + model + " processed\n"

print "\n \t Process done!!"  