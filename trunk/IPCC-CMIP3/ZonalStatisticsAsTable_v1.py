# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Purpose: This script calculates the mean of GCM data for a specific regions
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string,glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python ZonalStatisticsAsTable_v1.py M:\climate_change\IPCC_CMIP3 A2 D:\Workspace\PNUMA\Zonas\mun_zon.dbf D:\Workspace\PNUMA\Zonal_Statistical_Zones 30s downscaled"
	print "   Syntax	: <Extract_MaskGCM.py>, <dirbase>, <scenario>, <mask>, <dirout>, <resolution>, <type>"
	print "   dirbase	: Root folder where are storaged the datasets"
	print "   scenario	: A1B, A2 or B1"
	print "	  mask		: shape with full path and extension"
	print "   dirout	: Out folder"
	print "   resolution: The possibilities are 2_5min 5min 10min 30s"
	print "   type		: Disaggregated, Interpolated or Downscaled"	
	sys.exit(1)

#Set variables
dirbase = sys.argv[1]
scenario = sys.argv[2]
mask = sys.argv[3]
dirout = sys.argv[4]
resolution = sys.argv[5]
type = sys.argv[6]

os.system('cls')
gp.CheckOutExtension("Spatial")

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists 
periodlist = "2020_2049", "2040_2069" #"1961_1990", "2070_2099", "2050_2079", "2060_2089", "2030_2059",
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
print "Available models: " + str(modellist)
# period = "2020_2049"

diroutpoints = dirout
if not os.path.exists(diroutpoints):
	os.system('mkdir ' + diroutpoints)	

for period in periodlist:
	for model in modellist:
	
		# for period in periodlist:
		print "\n---> Processing: " + "SRES_" + scenario + " " + type + " Global_" + str(resolution) + " " + model + " " + period + "\n"

		if not os.path.exists(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt"):		
			gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
			
			rasters = sorted(gp.ListRasters("*", "GRID"))
			
			InPointsFC = mask 
			for raster in rasters:
				if not os.path.basename(raster).split("_")[0] == "tmean":
					outTable = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + os.path.basename(raster) + ".dbf"
					gp.CheckOutExtension("Spatial")
					if not gp.Exists(outTable):
						print raster
						gp.ZonalStatisticsAsTable_sa(mask, "FID", raster, outTable, "DATA")
					else:
						print raster,"processed"
		
			checkTXT = open(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt", "w")
			checkTXT.close()
		
		
for period in periodlist:
	for model in modellist:
		
		print "\n---> Joining: " + scenario + " " + type + " Global_" + str(resolution) + " " + model + " " + period + "\n"
		# Get a list of dbfs 
		dbfList = sorted(glob.glob(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "*.dbf"))
		for dbf in dbfList:
			InData = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_bio_1.dbf"
			if not os.path.basename(dbf)[-9:] == "bio_1.dbf":
				# fields = os.path.basename(dbf)[:-4].split("_")[-2:]
				gp.joinfield (InData, "FID", dbf, "FID", "MEAN")
				print dbf + " joined"
				# os.remove(dbf)

		xmlList = sorted(glob.glob(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "*.xml"))
		for xml in xmlList:
			os.remove(xml)
	
	

		
		
print "done!!!" 