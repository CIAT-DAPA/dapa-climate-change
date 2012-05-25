# ---------------------------------------------------------------------------------
# Author: Carlos Navarro
# Date: September 13th, 2010
# Purpose: Extraction by mask, diseggregated, interpolated or downscaled surfaces
# Note: If process is interrupted, you must be erase the last processed period
# ----------------------------------------------------------------------------------

import arcgisscripting, os, sys, string, glob
gp = arcgisscripting.create(9.3)

#Syntax
if len(sys.argv) < 7:
	os.system('cls')
	print "\n Too few args"
	print "   - ie: python Extract_PointsGCM_MS.py O:\\climate_change\\IPCC_CMIP3 A2 D:\Workspace\Beatriz\point_total.shp D:\Workspace\Beatriz 2_5min downscaled"
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
mask = sys.argv[3]
dirout = sys.argv[4]

resolution = sys.argv[5]
type = sys.argv[6]

os.system('cls')

print "~~~~~~~~~~~~~~~~~~~~~~"
print " EXTRACT BY MASK GCM  "
print "~~~~~~~~~~~~~~~~~~~~~~"

#Get lists 
periodlist = "2010_2039" , "2040_2069", # "2020_2049", "2030_2059", "2040_2069", "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
variablelist = "tmin", "tmax", "prec"
print "Available models: " + str(modellist)

for model in modellist:
	for period in periodlist:
	
		print "\n---> Processing: " + "SRES_" + scenario + " " + type + " Global_" + str(resolution) + " " + model + " " + period + "\n"
		diroutpoints = dirout + "\\_extract_" + "SRES_" + scenario 
		if not os.path.exists(diroutpoints):
			os.system('mkdir ' + diroutpoints)	
		
		if not os.path.exists(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt"):
			for month in range (1, 12 + 1, 1):
				for variable in variablelist:
					
					gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
					raster = gp.workspace + "\\" + variable + "_" + str(month) #sorted(gp.ListRasters(variable + "*", "GRID"))
					
					# for raster in rasters:
					#OutRaster = diroutraster + "\\" + raster
					#gp.clip_management(raster,"-100 -60 -30 23 ",OutRaster)
					InPointsFC = mask 
					OutPointsFC = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + os.path.basename(raster) + ".dbf"

					if not os.path.exists(OutPointsFC):
						print "\tExtracting .. " + os.path.basename(raster)
						#Check out Spatial Analyst extension license
						gp.CheckOutExtension("Spatial")

						#Process: Cell Statistics...
						gp.Sample_sa(raster, InPointsFC, OutPointsFC, "")
					else:
						print "\t" + os.path.basename(raster) + " extracted"
			
			print "\n"
			for month in range (1, 12 + 1, 1):
				for variable in variablelist:
					dbf = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + variable + "_" + str(month) + ".dbf"

					if not os.path.basename(dbf)[-10:] == "tmin_1.dbf":
						print "\tJoining .. " + os.path.basename(dbf)
						InData = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_tmin_1.dbf"
						fields = os.path.basename(dbf)[:-4].split("_")[-2:]
						gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
						os.remove(dbf)
			
			xmlList = sorted(glob.glob(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "*.xml"))
			for xml in xmlList:
				os.remove(xml)
			
			checkTXT = open(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt", "w")
			checkTXT.close()
print "done!!!" 