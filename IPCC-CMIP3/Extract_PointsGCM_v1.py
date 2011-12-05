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
	print "   - ie: python Extract_PointsGCM_v1.py M:\climate_change\IPCC_CMIP3 A1B E:\Workspace\Osana\point_usa.shp E:\Workspace\Osana 30s downscaled"
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
# periodlist = "2020_2049", "2040_2069", "2070_2099" #"#"2010_2039"#,#, "2030_2059", , "2050_2079", "2060_2089", "2070_2099"
modellist = sorted(os.listdir(dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution)))
# variablelist = "prec", "tmax", "tmean", "tmin"
# variable = "bio"
print "Available models: " + str(modellist)

for model in modellist[6:]:
	# for period in periodlist:
	period = "2020_2049"
	print "\n---> Processing: " + "SRES_" + scenario + " " + type + " Global_" + str(resolution) + " " + model + " " + period + "\n"
	diroutpoints = dirout + "\\_extract_SRES_" + scenario 
	if not os.path.exists(diroutpoints):
		os.system('mkdir ' + diroutpoints)	
	
	if not os.path.exists(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt"):		
		gp.workspace = dirbase + "\\SRES_" + scenario + "\\" + type + "\\Global_" + str(resolution) + "\\" + model + "\\" + period
		rasters = sorted(gp.ListRasters("*", "GRID"))
		
		InPointsFC = mask 
		for raster in rasters:
			OutPointsFC = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_" + os.path.basename(raster) + ".dbf"
			gp.CheckOutExtension("Spatial")

			#Process: Sample...
			if not gp.Exists(OutPointsFC) and not os.path.basename(raster).split("_")[0] == "tmean":
				gp.Sample_sa(raster, InPointsFC, OutPointsFC, "")
				print "\t" + os.path.basename(raster) + " extracted"
			else:
				print "\t" + os.path.basename(raster) + " extracted"
		
		# Get a list of dbfs 
		dbfList = sorted(glob.glob(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "*.dbf"))
		for dbf in dbfList:
			InData = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_bio_1.dbf"
			if not os.path.basename(dbf)[-9:] == "bio_1.dbf":
				fields = os.path.basename(dbf)[:-4].split("_")[-2:]
				gp.joinfield (InData, "mask", dbf, "mask", fields[0] + "_" + fields[1])
				print dbf + " joined"
				# os.remove(dbf)

		xmlList = sorted(glob.glob(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "*.xml"))
		for xml in xmlList:
			os.remove(xml)

		checkTXT = open(diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + period + "_done.txt", "w")
		checkTXT.close()

# for model in modellist:
	# for period in periodlist:
		# diroutpoints = dirout + "\\_extract_3_" + "SRES_" + scenario 
		# dbf = diroutpoints + "\\SRES_" + scenario + "_" + model + "_" + "2010_2039" + "_prec_1.dbf"
		# print "\tJoining .. " + os.path.basename(dbf)
		# InData = diroutpoints + "\\SRES_" + scenario + "_" + model + "_2010_2039_" + "prec_1.dbf"
		# if gp.Exists(dbf):
			# gp.joinfield (InData, "mask", dbf, "mask")
							
# for model in modellist:
	# print model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" + model + "\n" 

			
print "done!!!" 